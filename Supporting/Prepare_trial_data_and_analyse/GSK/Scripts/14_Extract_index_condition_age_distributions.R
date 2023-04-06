# 14_Extract_index_condition_age_distributions

# This script extracts the age distributions of each index condition and gsk trial. 



# Set up -----------------------------------------------------------------------

path <- "//campus.gla.ac.uk/SSD_Dept_Data_7/HAW/HAWPublic/PublicHealth/ClinicaltrialMultimorbidityGSK/1732/1732/"
source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

# Data 
cleaned_gsk <- read.csv("GSK_processed_data/cleaned_gsk_conmed_defined_comorbidities.csv", row.names = 1)



# Age distribution for each index condition ------------------------------------

ind_cond_sex_age_distr <- cleaned_gsk %>% 
  group_by(condition, sex) %>% 
  summarise(n_patients = n(),
            min_age  = min(age),
            max_age  = max(age),
            mean_age = round(mean(age), 2),
            sd_age   = round(sd(age), 2)) %>%
  mutate(repo = "GSK")

# Save 
write_csv(ind_cond_sex_age_distr, "Created_metadata/gsk_index_condition_age_distribution.csv")


gsk_trial_age_distr <- cleaned_gsk %>% 
  group_by(condition, sex, trial) %>% 
  summarise(n = n(),
            min_age  = min(age),
            max_age  = max(age),
            mean_age = round(mean(age), 2),
            sd_age   = round(sd(age), 2)) %>% 
  mutate(repo = "GSK")

# Save 
write_csv(gsk_trial_age_distr, "Created_metadata/gsk_trial_age_distribution.csv")

 
