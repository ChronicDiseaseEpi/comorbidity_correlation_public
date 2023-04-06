#' ---
#' title: "27: Combine non restricted and age restricted model outputs"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script combines the community non-restricted and age restricted model dataframes 
# (made in scripts 13 and 26) into a single data frame



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data: Omega summaries 
not_rstrct_sum <- read_csv("Outputs/VM_unrestricted_community_omega_summaries.csv")
rstrct_sum     <- read_csv("Outputs/VM_age_restricted_community_omega_summaries.csv")

# Data#: Omega draws
not_rstrct_draws <- readRDS("Outputs/VM_unrestricted_community_omega_draws.RDS")
rstrct_draws     <- readRDS("Outputs/VM_age_restricted_community_omega_draws.RDS")



# Combine omega summaries and save as single df --------------------------------

cmnty_sum <- not_rstrct_sum %>% 
  bind_rows(rstrct_sum) %>% 
  mutate(repo = if_else(repo == "SAIL", "Community", repo))

# Checks 
unique(cmnty_sum$Var1) # 10
unique(cmnty_sum$Var2) # 10
unique(cmnty_sum$comorbidity) # 47
map(cmnty_sum, ~ sum(is.na(.x))) # NAs present where it is ok 

saveRDS(cmnty_sum, "Outputs/VM_community_omega_summaries.RDS")



# Combine omega draws and save as single df ------------------------------------

cmnty_draws <- not_rstrct_draws %>% 
  bind_rows(rstrct_draws) %>% 
  mutate(repo = if_else(repo == "SAIL", "Community", repo))

# Checks 
unique(cmnty_draws$comorbidity) # 84
map(cmnty_draws, ~ sum(is.na(.x))) # 0 

saveRDS(cmnty_draws, "Outputs/VM_community_omega_mcmc_draws.RDS")
