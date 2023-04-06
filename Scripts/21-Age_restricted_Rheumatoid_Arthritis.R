#' ---
#' title: "21: Age restricted IPD rheumatoid arthritis"
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

source("Scripts/00-Functions_and_packages.R")

set_cmdstan_path("/opt/cmdstanr/cmdstan-2.29.2")



# Data -------------------------------------------------------------------------

rheu_arth_f <- read_csv("Data/Rheumatoid_arthritis_Female_age_restricted_IPD.csv")
rheu_arth_m <- read_csv("Data/Rheumatoid_arthritis_Male_age_restricted_IPD.csv")
rheu_arth  <- na.omit(rbind(rheu_arth_m, rheu_arth_f))
rm(rheu_arth_f, rheu_arth_m)

# Don't need to take sample as n < 20,000

# New columns: intercept, recode sex to binary
rheu_arth <- rheu_arth %>% 
  mutate(interc = 1,
         sex = if_else(sex == "Male", 1L, 0L)) # Male = 1

# Recode Y -> 1, N -> 0
response <- rheu_arth[3:8]
response <- as.data.frame(response)
response[] <- if_else(response == "Y", 1L, 0L)

# Data in stan format 
rheu_arth_data_list <- list("D" = 6, 
                        "K" = 3, # Intercept, sex and age
                        "N" = nrow(rheu_arth), # Number of participants
                        "y" = as.matrix(response),
                        "x" = as.matrix(rheu_arth[, c("interc", "sex", "age_sim")]))



# SAMPLE POSTERIOR --------------------------------------------------------------

condition <- unique(rheu_arth$index_condition)
condition <- condition %>% str_replace_all(" ", "_")

run_mod2(run_info = paste0("0307-Age_restricted_IPD_", condition), 
        data_list = rheu_arth_data_list, 
        stanmodel_file = "Scripts/Multivariate_probit_model.stan",
        save_path = "Outputs/age_restricted_model_fits/")
