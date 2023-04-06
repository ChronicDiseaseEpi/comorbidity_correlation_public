# Model results from Yoda need to be exported to be combined with the model results 
# from Vivli and GSK. However, Yoda has a max export file size (5MB) which the model 
# results were far greater than. Hence, we had to manually compress the results into 
# smaller files for export. This script does that.



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# Data 
om_draws <- readRDS("Results/omega_correlations/yoda_omega_draws.RDS")



# Compress files ---------------------------------------------------------------

# Trail summary: trial, condition and nct id
om_draws_smry <- om_draws %>% 
  distinct(trial, condition, nct_id)
write_csv(om_draws_smry, "Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_nct_id_trial_id.csv")

# Can now remove trial and condition 
om_draws <- om_draws %>% 
  select(-trial, -condition)

# Shorten omega 
om_draws <- om_draws %>% 
  mutate(Omega = str_remove(Omega, "Omega") %>% str_trim() %>% str_sub(2) %>% str_remove("\\.")) 

# Omega - Comorbidity labels 
omega_lbls <- om_draws %>% 
  distinct(nct_id, Omega, comorbidity)
write_csv(omega_lbls, "Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_omega_comorbidity_combo_lbls.csv")

# Check all estimates = 1 are 1:1 comorbidity correlations 
om_draws %>% 
  filter(estimate == 1) %>% 
  distinct(Omega, comorbidity)

# Remove 1:1 combinationss
om_draws <- om_draws %>% 
  filter(!estimate == 1)

# Remove comorbidity 
om_draws <- om_draws %>% 
  select(-comorbidity)

# Round and turn into whole numbers
om_draws <- om_draws %>% 
  mutate(estimate = 10000*round(estimate,4))

# Label nct_id 
nct_id_lbl <- om_draws %>% 
  distinct(nct_id) %>% 
  arrange(nct_id) %>% 
  mutate(nct_shrt = seq_along(nct_id))
write_csv(nct_id_lbl, "Supporting/Prepare_trial_data/Yoda/Created_metadta/yoda_nct_id_lbl_lkp.csv")

# Turn into named vector
nct_id_vct <- nct_id_lbl$nct_shrt
names(nct_id_vct) <- nct_id_lbl$nct_id

# Replace nct_id wioth shortened version
om_draws$nctshrt <- nct_id_vct[om_draws$nct_id]
om_draws <- om_draws %>% 
  select(-nct_id)

# Reduce to distinct comorbidity combinations
a <- combn(1:6, 2)
a <- apply(a, 2, function(x) paste(x, collapse = ""))

om_draws <- om_draws %>% 
  filter(Omega %in% a)

# Make wide and remove iteration (1 row = 1 iteration)
om_draws_wide <- om_draws %>% 
  spread(Omega, estimate) %>% 
  select(-iteration)

# Save in 5 parts (5 MB max export size)
write_csv(om_draws_wide %>% filter(nctshrt %in% 1:6), "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials1_6.csv")
write_csv(om_draws_wide %>% filter(nctshrt %in% 7:12), "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials7_12.csv")
write_csv(om_draws_wide %>% filter(nctshrt %in% 13:18), "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials13_18.csv")
write_csv(om_draws_wide %>% filter(nctshrt %in% 19:22), "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials19_22.csv")
write_csv(om_draws_wide %>% filter(nctshrt %in% 23:25), "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials23_25.csv")


