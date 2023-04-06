# This script calculates proportion of each comorbidity in each trial



# Set up -----------------------------------------------------------------------

# Packages 
source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# Data 
yoda <- readRDS("Processed_data/cleaned_yoda_conmed_defined_comorbidities.Rds")



# Calculate proportion of each comorbidity p/trial -----------------------------

# Remove trail 28431754DIA3008 which we removed from analysis
tr <- unique(yoda$trial)
tr <- tr[-23]
yoda <- yoda %>% filter(trial %in% tr)

# Calculate column sums 
como_col_sums <- yoda %>% 
  group_by(trial, nct_id) %>%
  summarise(across(antacids:skin, ~sum(.x)))

# Add n_patients
tr_n <- yoda %>% 
  group_by(trial) %>% 
  summarise(n_patients = n())

como_col_sums <- como_col_sums %>% inner_join(tr_n, by = "trial")

# Calculate proportions
como_prop <- como_col_sums %>%
  group_by(trial, nct_id) %>%
  summarise(across(antacids:skin, ~ .x/n_patients)) %>%
  ungroup()

# Add condition
conds <- yoda %>%
  group_by(trial, condition) %>%
  summarise(n = n()) %>% 
  select(-n)

como_prop <- como_prop %>% left_join(conds)

# Add n_patients
como_prop <- como_prop %>% left_join(tr_n, by = "trial")

# Save 
write_csv(como_prop, "Supporting/Prepare_trial_data/Yoda/Outputs/comorbidity_proportions_by_trial.csv")


