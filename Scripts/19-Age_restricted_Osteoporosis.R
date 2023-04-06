#' ---
#' title: "19: Age restricted IPD osteoporosis"
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

ostpor_m <- read_csv("Data/Osteoporosis_Male_ipd.csv")
ostpor_f <- read_csv("Data/Osteoporosis_Female_ipd.csv")
ostpor <- na.omit(rbind(ostpor_m, ostpor_f))
rm(ostpor_f, ostpor_m)

# Index condition
cond <- unique(ostpor$index_condition)

# Take random sample 
ostpor <- sample_n(ostpor, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
ostpor <- ostpor %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L)) # Male = 1

# Recode Y -> 1, N -> 0
response <- ostpor[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
ostpor_data_list <- list("D" = 6, 
                         "K" = 3, # Intercept, sex and age
                         "N" = nrow(ostpor), # Number of participants
                         "y" = as.matrix(response),
                         "x" = as.matrix(ostpor[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod2(run_info = paste0("0305-Age_restricted_IPD_", cond), 
        data_list = ostpor_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/age_restricted_model_fits/")
