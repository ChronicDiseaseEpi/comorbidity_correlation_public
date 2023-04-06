#' ---
#' title: "24: Age restricted IPD psoriasis"
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

psoria_f <- read_csv("Data/Psoriasis_Female_age_restricted_IPD.csv")
psoria_m <- read_csv("Data/Psoriasis_Male_age_restricted_IPD.csv")
psoria  <- na.omit(rbind(psoria_m, psoria_f))
rm(psoria_f, psoria_m)

# Take random sample of 20000
psoria <- sample_n(psoria, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
psoria <- psoria %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L)) # Male = 1

# Recode Y -> 1, N -> 0
response <- psoria[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
psoria_data_list <- list("D" = 6, 
                      "K" = 3, # Intercept, sex and age
                      "N" = nrow(psoria), # Number of participants
                      "y" = as.matrix(response),
                      "x" = as.matrix(psoria[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

condition <- unique(psoria$index_condition)

run_mod2(run_info = paste0("0310-Age_restricted_IPD_", condition), 
        data_list = psoria_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/age_restricted_model_fits/")
