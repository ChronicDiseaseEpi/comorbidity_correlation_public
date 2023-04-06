#' ---
#' title: "10: Dementia IPD model run"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---


# This script runs the MVP model on the simulated IPD for dementia. 

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
dem_m <- read_csv("Data/Dementia_any_Male_ipd.csv")
dem_f <- read_csv("Data/Dementia_any_Female_ipd.csv")
dem <- na.omit(rbind(dem_m, dem_f))
rm(dem_m, dem_f)

# Extract condition 
cond <- unique(dem$index_condition)
cond <- cond %>%
  str_replace_all(" ", "_") %>%
  str_remove_all("[()]")

# Less than 20,000 patients so don't need to take random sample 

# New columns: intercept, recode sex to binary
dem <- dem %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L))

# Recode Y -> 1, N -> 0
response <- dem[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
dem_data_list <- list("D" = 6, 
                      "K" = 3, # Intercerpt, sex, age
                      "N" = nrow(dem), # Number of participants
                      "y" = as.matrix(response),
                      "x" = as.matrix(dem[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

run_mod(run_info = paste0("0109_IPD_", cond), 
        data_list = dem_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/model_fits/")
