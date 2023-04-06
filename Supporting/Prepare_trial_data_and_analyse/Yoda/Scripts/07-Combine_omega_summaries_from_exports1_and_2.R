# This script recreates the Yoda draws df as it had to be compresses manually for export

#### IMPORTANT !!!

# Need to remove 3 trials- one needs to be fully removed and 2 replaced with a new export 
# dropping trial 28431754DIA3008 (model wouldn't fit well)
# replace 2 trials: DIA3005 & 28431754DIA3014
# At end should have 24 trials 



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# Data 
om_sum_exp1 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_trial_omega_summaries.csv")
om_sum_exp2 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_summaries_run2.csv")



# Remove trials from export 1 --------------------------------------------------

# Trials to remove and replace 
rm_trial   <- "28431754DIA3008"
rep_trials <- c("DIA3005", "28431754DIA3014")

n_distinct(om_sum_exp1$trial) # 25
om_sum_exp1 <- om_sum_exp1 %>% filter(!trial %in% c(rm_trial, rep_trials))
n_distinct(om_sum_exp1$trial) # 22



# Join exports 1 and 2 ---------------------------------------------------------

om_sum_final <- om_sum_exp1 %>% bind_rows(om_sum_exp2)

n_distinct(om_sum_final$trial) # 24 

om_sum_final <- om_sum_final %>%
  mutate(repo = if_else(repo == "YODA_run2", "YODA", repo))

# Save 
write_csv(om_sum_final, "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_summary_final.csv")
