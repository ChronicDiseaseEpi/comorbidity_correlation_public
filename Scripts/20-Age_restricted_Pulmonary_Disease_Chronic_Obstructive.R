#' ---
#' title: "20: Age restricted IPD pulmonary disease CO"
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

# Packages 
source("Scripts/00-Functions_and_packages.R")

# Set cmdstan path
set_cmdstan_path("/opt/cmdstanr/cmdstan-2.29.2")



# Data -------------------------------------------------------------------------

pulmd_f <- read_csv("Data/Pulmonary_Disease_Chronic_Obstructive_Female_age_restricted_IPD.csv")
pulmd_m <- read_csv("Data/Pulmonary_Disease_Chronic_Obstructive_Male_age_restricted_IPD.csv")
pulmd  <- na.omit(rbind(pulmd_m, pulmd_f))
rm(pulmd_f, pulmd_m)

# Index condition
cond <- unique(pulmd$index_condition)
cond <- cond %>%
  str_remove_all(",") %>%
  str_replace_all(" ", "_")

# Take random sample 
pulmd <- sample_n(pulmd, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
pulmd <- pulmd %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L)) # Male = 1

# Recode Y -> 1, N -> 0
response <- pulmd[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
pulmd_data_list <- list("D" = 6, 
                         "K" = 3, # Intercept, sex and age
                         "N" = nrow(pulmd), # Number of participants
                         "y" = as.matrix(response),
                         "x" = as.matrix(pulmd[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod2(run_info = paste0("0306-Age_restricted_IPD_", cond), 
        data_list = pulmd_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/age_restricted_model_fits/")
