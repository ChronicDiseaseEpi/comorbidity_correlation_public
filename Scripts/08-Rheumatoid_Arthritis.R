#' ---
#' title: "08: Rheumatoid arthritis IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD  for rheumatoid arthritis.

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
rhuarth_m <- read.csv("Data/Rheumatoid_arthritis_Male_ipd.csv")
rhuarth_f <- read.csv("Data/Rheumatoid_arthritis_Female_ipd.csv")
rhuarth <- na.omit(rbind(rhuarth_m, rhuarth_f))
rm(rhuarth_f, rhuarth_m)

# Extract condition
cond <- unique(rhuarth$index_condition)
cond <- str_replace(cond, " ", "_")

# N < 20000 so do not need to take random sample 

# New columns: intercept, recode sex to binary
rhuarth <- rhuarth %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- rhuarth[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
rhuarth_data_list <- list("D" = 6, 
                          "K" = 3, # Intercerpt, sex, age
                          "N" = nrow(rhuarth), # Number of participants
                          "y" = as.matrix(response),
                          "x" = as.matrix(rhuarth[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0107_IPD_", cond), 
        data_list = rhuarth_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
