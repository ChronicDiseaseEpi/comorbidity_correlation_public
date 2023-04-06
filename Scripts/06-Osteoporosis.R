#' ---
#' title: "06: Osteoaporosis IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for osteoporosis.

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
ostpo_m <- read.csv("Data/Osteoporosis_Male_ipd.csv")
ostpo_f <- read.csv("Data/Osteoporosis_Female_ipd.csv")
ostpo <- na.omit(rbind(ostpo_m, ostpo_f))
rm(ostpo_f, ostpo_m)

# Extract condition
cond <- unique(ostpo$index_condition)

# Take random sample of 20000
ostpo <- sample_n(ostpo, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
ostpo <- ostpo %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- ostpo[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
ostp_data_list <- list("D" = 6, 
                  "K" = 3, # Intercerpt, sex, age
                  "N" = nrow(ostpo), # Number of participants
                  "y" = as.matrix(response),
                  "x" = as.matrix(ostpo[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0105_IPD_", cond), 
        data_list = ostp_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
