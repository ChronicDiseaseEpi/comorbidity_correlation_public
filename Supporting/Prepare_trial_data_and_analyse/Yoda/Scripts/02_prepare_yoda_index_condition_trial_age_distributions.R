# This script Cleans the yoda trial data and produces two tables giving the 
# age distributions per trial and per index condition 


# Set up -----------------------------------------------------------------------

# Packages 
source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# Data 
age_by_sex     <- readRDS("Supporting/Prepare_trial_data/Yoda/Outputs/baseline_related_summaries_yoda.Rds")$agebysex
como_prop_smry <- readRDS("Supporting/Prepare_trial_data/Yoda/Outputs/baseline_related_summaries_yoda.Rds")$como_prp_smry
sail_cmnty     <- read.csv("Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv")



# Trial age distribution -------------------------------------------------------

# Add condition to age by sex
age_by_sex <- age_by_sex %>%
  inner_join(como_prop_smry %>% select(condition, nct_id, trial), by = "trial") %>%
  mutate(sex = if_else(male == TRUE, "male", "female")) %>% 
  rename(minimum = minumum)

# Match index condition names to community index condition names 
cmnty_ind <- unique(sail_cmnty$condition)
unique(age_by_sex$condition)

age_by_sex <- age_by_sex %>% 
  mutate(condition = case_when(condition == "rheumatoid arthritis" ~ cmnty_ind[1],
                               condition == "Diabetes Mellitus, Type 2" ~ cmnty_ind[6],
                               condition == "Alzheimer's Disease" ~ cmnty_ind[5],
                               TRUE ~ as.character(condition))) %>%
  mutate(condition = str_remove_all(condition, "'"),
         condition = str_replace_all(condition, " ", "_"))

setdiff(unique(age_by_sex$condition), unique(sail_cmnty$condition)) # OK!

# "Crohns_disease"         "Psoriatic_arthritis"   
# "ankylosing_spondylitis" "Ulcerative_colitis"

#### THERE IS A 0 IN MINIMUM AGE - NEEDS FIXED!!!
# CAPSS-381, Migraine, NCT00212810, female

age_by_sex <- age_by_sex %>% 
  mutate(minimum = if_else(minimum == "0", "18", minimum))

# Save
write_csv(age_by_sex, "Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_trial_age_distribution.csv")



# Index condition age distribution ---------------------------------------------

age_by_sex$mean <- as.numeric(age_by_sex$mean)
age_by_sex$`standard deviation` <- as.numeric(age_by_sex$`standard deviation`)
age_by_sex$minimum <- as.numeric(age_by_sex$minimum)
age_by_sex$maximum <- as.numeric(age_by_sex$maximum)

trial_age_distr <- age_by_sex %>%
  group_by(condition, sex) %>% 
  summarise(n_trials = n(),
            age_min = round(min(minimum), 2),
            age_max = round(max(maximum), 2),
            age_mean = round(mean(mean), 2),
            age_sd = round(sd(`standard deviation`, na.rm = TRUE), 2)) %>%
  mutate(age_sd = if_else(is.na(age_sd), 0, age_sd))

# Save
write_csv(trial_age_distr, "Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_index_cond_age_distribution.csv")


