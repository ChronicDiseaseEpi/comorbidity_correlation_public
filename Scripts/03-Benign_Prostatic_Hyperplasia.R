#' ---
#' title: "03: Benign prostatic hyperplasia IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for benign prostatic
# hyperplasia (only has single sex - males)

# All model scripts (02-12 & 15-25) are stand alone so they can be run from command line

# File path for cmdstan installation will likely need altered, see the cmdstanr 
# installation guide on how to do this: 
# https://mc-stan.org/cmdstanr/articles/cmdstanr.html



# Set up -----------------------------------------------------------------------

# Packages 
source("Scripts/00-Functions_and_packages.R")

# Set cmdstan path
set_cmdstan_path("/opt/cmdstanr/cmdstan-2.29.2")



# DATA ------------------------------------------------------------------------

# Data - only have for males 
ben_hyperp_m <- read_csv("Data/Benign_Prostatic_Hyperplasia_Male_ipd.csv")
ben_hyperp_m <- na.omit(ben_hyperp_m) # 0

# Extract condition
cond <- unique(ben_hyperp_m$index_condition)
cond <- str_replace_all(cond, " ", "_")

# New columns: intercept, recode sex to binary
ben_hyperp_m <- ben_hyperp_m %>% 
  mutate(interc = 1)

# Recode Y -> 1, N -> 0
response <- ben_hyperp_m[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format - sex removed as only males  
ben_data_list <- list("D" = 6, # Number of response vars
                      "K" = 2, # Intercept, age
                      "N" = nrow(ben_hyperp_m), # Number of participants
                      "y" = as.matrix(response),
                      "x" = as.matrix(ben_hyperp_m[, c("interc", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0102_IPD_", cond), 
        data_list = ben_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
