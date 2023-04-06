#' ---
#' title: "16: Age restricted IPD benign hyperplasia"
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

benign <- read_csv("Data/Benign_Prostatic_Hyperplasia_Male_age_restricted_IPD.csv")
benign <- na.omit(benign)

# Do not need to take random sample as n < 20,000

# Condition 
cond <- unique(benign$index_condition)
cond <- cond %>% 
  str_replace_all(" ", "_")
  
# New columns: intercept, recode sex to binary
benign <- benign %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L)) # Male = 1

# Recode Y -> 1, N -> 0
response <- benign[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
benign_data_list <- list("D" = 6, 
                         "K" = 2, # Intercept and age
                         "N" = nrow(benign), # Number of participants
                         "y" = as.matrix(response),
                         "x" = as.matrix(benign[, c("interc", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0302-Age_restricted_IPD_", cond), 
        data_list = benign_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/age_restricted_model_fits/")
