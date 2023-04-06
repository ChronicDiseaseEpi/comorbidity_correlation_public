# 20_extract_index_condition_age_distributions
# Age distributions of index conditions p/sex and p/trial
# Saved outputs will be used to restrict simulated community IPD index conditions
# so that the age ranges match those in the trials
# cleaned_conmed_defined_comorbidity data has already been restricted to only 
# index conditions found in the community (SAIL) (13/9/22)
# have 38 trials with 9 unique index conditions



# Packages 
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

# Data  
vivli_como <- read.csv("Processed_data/cleaned_conmed_defined_comorbidity.csv", row.names = 1)
common_cmnty_como <- read.csv("Supporting/Prepare_trial_data/Vivli/Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv", row.names = 1)

# Remove trial TKASYR322_402 due to poor model diagnostics
vivli_como <- vivli_como %>% filter(!trial %in% "TKASYR322_402")


# Age distributions ------------------------------------------------------------


# Index conditions age distribution p/sex
ind_cond_age_rng <- vivli_como %>%
  group_by(condition, sex) %>%
  summarise(age_mean = round(mean(age, na.rm = TRUE), 2),
            age_std = round(sd(age, na.rm = TRUE), 2),
            age_min = round(min(age, na.rm = TRUE), 2),
            age_max = round(max(age, na.rm = TRUE), 2),
            n_patients = n()) %>% 
  mutate(repo = "Vivli")

write_csv(ind_cond_age_rng, "Supporting/Prepare_trial_data/Vivli/Created_metadata/vivli_index_condition_age_distribution_by_sex.csv")


# Index conditions age distribution
ind_cond <- vivli_como %>%
  group_by(condition) %>%
  summarise(age_mean = round(mean(age, na.rm = TRUE), 2),
            age_std = round(sd(age, na.rm = TRUE), 2),
            age_min = round(min(age, na.rm = TRUE), 2),
            age_max = round(max(age, na.rm = TRUE), 2),
            n_patients = n(),
            n_trials = n_distinct(trial)) %>% 
  mutate(repo = "Vivli")

write_csv(ind_cond, "Supporting/Prepare_trial_data/Vivli/Created_metadata/vivli_index_condition_age_distribution.csv")


# Trail and index condition age distribution p/sex
trial_age_rng <- vivli_como %>%
  group_by(condition, sex, trial) %>%
  summarise(age_mean = round(mean(age, na.rm = TRUE), 2),
            age_std = round(sd(age, na.rm = TRUE), 2),
            age_min = round(min(age, na.rm = TRUE), 2),
            age_max = round(max(age, na.rm = TRUE), 2),
            n_patients = n()) %>% 
  mutate(repo = "Vivli")

write_csv(trial_age_rng, "Supporting/Prepare_trial_data/Vivli/Created_metadata/vivli_trial_age_distribution_by_sex.csv")


# Trail and index condition age distribution p/sex
trial <- vivli_como %>%
  group_by(condition, trial) %>%
  summarise(age_mean = round(mean(age, na.rm = TRUE), 2),
            age_std = round(sd(age, na.rm = TRUE), 2),
            age_min = round(min(age, na.rm = TRUE), 2),
            age_max = round(max(age, na.rm = TRUE), 2),
            n_patients = n(),
            both_sexes = if_else(length(unique(sex)) == 2, "Yes", "No")) %>% 
  mutate(repo = "Vivli")

write_csv(trial, "Supporting/Prepare_trial_data/Vivli/Created_metadata/vivli_trial_age_distribution.csv")


