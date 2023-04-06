#' ---
#' title: "14: Age restrict simulated IPD"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script reads in simulated IPD data from the 01-simulate_community_ipd script
# it then restricts the min and max age of participants to match those in the trials
# for the same index condition.



# Set up ------------------------------------------------------------------------

# Load in packages and functions
source("Scripts/00-functions_and_packages.R")

# Data
cmnty <- read.csv("Created_metadata/SAIL_common_6_comorbidities.csv", row.names = 1)
tr_cond_age <- read_csv("Created_metadata/trial_conditions_age_distribution.csv")



# Restrict simulated community data --------------------------------------------

# List file paths for simulated community data
sim_ipd_paths <- list.files("Data", full.names = TRUE)# [-c(INSERT INDEXES)] # Add indexes to remove files that are not model fits

# Select only file paths for conditions we have in trials 
trial_conds <- unique(tr_cond_age$condition) # 16
tr_inds <- c() 

for (i in seq_along(trial_conds)) {
  index <- grep(trial_conds[i], sim_ipd_paths)
  tr_inds <- c(tr_inds, index)
  unique(tr_inds)
}

sim_ipd_paths <- sim_ipd_paths[tr_inds] # 21

# Apply function to all IPD dfs
map(sim_ipd_paths, ~ restrict_ipd(.x))



# Check it worked --------------------------------------------------------------

# get non restricted age distributions
get_ages <- function(paths) {
  
  ipd <- read_csv(paths)
  cond <- unique(ipd$index_condition)
  sexx <- unique(ipd$sex)
  min_age <- min(ipd$age_sim, na.rm = T)
  max_age <- max(ipd$age_sim, na.rm = T)
  
  output <- data.frame(condition = cond,
                       sex = sexx,
                       min_age = min_age,
                       max_age = max_age)
}

# Unrestricted ipd ages 
ipd_ages <- map_dfr(sim_ipd_paths, ~ get_ages(.x))

# Restricted IPD ages 
res_ipd <- list.files("Data", full.names = T)[c(-1, -9)]
rstrc_ipd_ages <- map_dfr(res_ipd, ~ get_ages(.x))

# Compare 
ipd_ages <- ipd_ages %>% 
  mutate(condition = condition %>% str_remove_all(",") %>% str_remove_all("[()]") %>% str_replace_all(" ", "_")) %>%
  rename(ipd_min_age = min_age,
         ipd_max_age = max_age)

rstrc_ipd_ages <- rstrc_ipd_ages %>% 
  mutate(condition = condition %>%  str_remove_all(",") %>% str_remove_all("[()]") %>% str_replace_all(" ", "_")) %>%
  rename(rstrct_ipd_min_age = min_age,
         rstrct_ipd_max_age = max_age)

cmp <- ipd_ages %>%
  inner_join(tr_cond_age %>% select(min_age, max_age, condition, sex), by = c("condition", "sex")) %>%
  inner_join(rstrc_ipd_ages %>% select(rstrct_ipd_min_age, rstrct_ipd_max_age, condition, sex), by = c("condition", "sex")) 
