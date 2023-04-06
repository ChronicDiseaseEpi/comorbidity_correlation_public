#' ---
#' title: "09: Asthma IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for asthma. 

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
asth_m <- read.csv("Data/Asthma_Male_ipd.csv")
asth_f <- read.csv("Data/Asthma_Female_ipd.csv")
asth <- na.omit(rbind(asth_m, asth_f))
rm(asth_f, asth_m)

# Extract condition 
cond <- unique(asth$index_condition)

# Take a random sample (to reduce memory overhead and allow saving)
asth <- sample_n(asth, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
asth <- asth %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- asth[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
asth_data_list <- list("D" = 6, 
                       "K" = 3, # Intercerpt, sex, age
                       "N" = nrow(asth), # Number of participants
                       "y" = as.matrix(response),
                       "x" = as.matrix(asth[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0108_IPD_", cond), 
        data_list = asth_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
