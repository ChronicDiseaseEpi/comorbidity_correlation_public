#' ---
#' title: "02: Type 2 Diabetes IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for type 2 diabetes. 

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
diab_f <- read_csv("Data/Diabetes_Mellitus_Type_2_Female_ipd.csv")
diab_m <- read_csv("Data/Diabetes_Mellitus_Type_2_Male_ipd.csv")
diab <- na.omit(bind_rows(diab_f, diab_m))
rm(diab_m, diab_f)

# Extract condition
cond <- unique(diab$index_condition)
cond <- str_remove_all(cond, ",")
cond <- str_replace_all(cond, " ", "_")

# Take a random sample (to reduce memory overhead and allow saving)
diab <- sample_n(diab, 20000, replace = FALSE)

# New columns: intercept, recode sex to binary
diab <- diab %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- diab[3:8]
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
diab_data_list <- list("D" = 6, 
                       "K" = 3, # Intercerpt, sex, age
                       "N" = nrow(diab), # Number of participants
                       "y" = as.matrix(response),
                       "x" = as.matrix(diab[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0101_IPD_", cond), 
        data_list = diab_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
