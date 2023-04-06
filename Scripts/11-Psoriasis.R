#' ---
#' title: "11: Psoriasis IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for psoriasis

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

# Data 
psoria_m <- read.csv("Data/Psoriasis_Male_ipd.csv")
psoria_f <- read.csv("Data/Psoriasis_Female_ipd.csv")
psoria <- na.omit(rbind(psoria_m, psoria_f))
rm(psoria_f, psoria_m)

# Extract condition
cond <- unique(psoria$index_condition)

# Take random sample of 20000
psoria <- sample_n(psoria, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
psoria <- psoria %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- psoria[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
psoria_data_list <- list("D" = 6, 
                         "K" = 3, # Intercerpt, sex, age
                         "N" = nrow(psoria), # Number of participants
                         "y" = as.matrix(response),
                         "x" = as.matrix(psoria[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0110_IPD_", cond), 
        data_list = psoria_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
