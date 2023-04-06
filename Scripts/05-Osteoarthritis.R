#' ---
#' title: "05: Osteoarthritis IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for osteoarthritis 

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
ost_arth_m <- read.csv("Data/Osteoarthritis_Male_ipd.csv")
ost_arth_f <- read.csv("Data/Osteoarthritis_Female_ipd.csv")
ost_arth   <- na.omit(rbind(ost_arth_m, ost_arth_f))
rm(ost_arth_f, ost_arth_m)

# Extract condition
cond <- unique(ost_arth$index_condition)

# Take a random sample (to reduce memory overhead and allow saving)
ost_arth <- sample_n(ost_arth, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
ost_arth <- ost_arth %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- ost_arth[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
ost_arth_data_list <- list("D" = 6, 
                           "K" = 3, # Intercerpt, sex
                           "N" = nrow(ost_arth), # Number of participants
                           "y" = as.matrix(response),
                           "x" = as.matrix(ost_arth[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0104_IPD_", cond), 
        data_list = ost_arth_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
