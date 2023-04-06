#' ---
#' title: "12: Restless legs syndrome IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for restless legs syndrome

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
restleg_m <- read.csv("Data/Restless_legs_syndrome_Male_ipd.csv")
restleg_f <- read.csv("Data/Restless_legs_syndrome_Female_ipd.csv")
restleg <- na.omit(rbind(restleg_m, restleg_f))
rm(restleg_f, restleg_m)

# Extract condition
cond <- unique(restleg$index_condition)
cond <- str_replace_all(cond, " ", "_")

# n < 20000 so do not need to take random sample

# New columns: intercept, recode sex to binary
restleg <- restleg %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- restleg[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
restleg_data_list <- list("D" = 6, 
                          "K" = 3, # Intercerpt, sex, age
                          "N" = nrow(restleg), # Number of participants
                          "y" = as.matrix(response),
                          "x" = as.matrix(restleg[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0111_IPD_", cond), 
        data_list = restleg_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
