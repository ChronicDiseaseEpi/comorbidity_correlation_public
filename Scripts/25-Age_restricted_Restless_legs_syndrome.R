#' ---
#' title: "25: Age restricted IPD restless legs syndrome"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script runs the MVP model on simulated community IPD that has been
# age restricted to match the age distribution of the trials.

# All model scripts (02-12 & 15-25) are stand alone so they can be run from command line

# File path for cmdstan installation will likely need altered, see the cmdstanr 
# installation guide on how to do this: 
# https://mc-stan.org/cmdstanr/articles/cmdstanr.html



# Set up -----------------------------------------------------------------------

source("Scripts/00-Functions_and_packages.R")

set_cmdstan_path("/opt/cmdstanr/cmdstan-2.29.2")



# Data -------------------------------------------------------------------------

restlegs_f <- read_csv("Data/Restless_legs_syndrome_Female_age_restricted_IPD.csv")
restlegs_m <- read_csv("Data/Restless_legs_syndrome_Male_age_restricted_IPD.csv")
restlegs  <- na.omit(rbind(restlegs_m, restlegs_f))
rm(restlegs_m, restlegs_f)

# Do not need to take sample as n < 20,000

# New columns: intercept, recode sex to binary
restlegs <- restlegs %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L)) # Male = 1

# Recode Y -> 1, N -> 0
response <- restlegs[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
restlegs_data_list <- list("D" = 6, 
                         "K" = 3, # Intercept, sex and age
                         "N" = nrow(restlegs), # Number of participants
                         "y" = as.matrix(response),
                         "x" = as.matrix(restlegs[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

condition <- unique(restlegs$index_condition)
condition <- str_replace_all(condition, " ", "_")

run_mod2(run_info = paste0("0311-Age_restricted_IPD_", condition), 
        data_list = restlegs_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/age_restricted_model_fits/")
