# Due to forgetting to export the look up tables with the second export need to 
# the correct nctshrt to trial by calculating the mean omega estimates for each trial 
# using MCMC draws and comparing with the omega summary table for the same 2 trials
# which lukily has the correct and full trial names. 
# Note some additiona processing required like rounding and dividing by 10000
# to reverse the manula compression that was carried out in Yoda prior to exporting. 



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# 2nd export data
draws_exp2 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_run2_compressed-DO_NOT_USE_WRONG_NCT.csv")
sum_exp2 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_summaries_run2.csv")

# Look up tables (from 1st export)
nct_trial_id <- read_csv("Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_nct_id_trial_id.csv")
nct_id_lbl <- read_csv("Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_nct_id_lbl_lkp.csv")



# Calculate mean omega correlation from MCMC samples ---------------------------

# Arrange by trial and add iteration column 
draws_exp2 <- draws_exp2 %>% 
  arrange(nctshrt) %>% 
  mutate(iteration = rep(1:1000, times = 6))

# Calculate column means for each trial 
est <- draws_exp2 %>%
  group_by(nctshrt) %>%
  summarise(across("12":"56", ~ mean(.x)))

# Pivot to long and reverse some of the compression 
est_long <- est %>%
  pivot_longer("12":"56", names_to = "como combo", values_to = "mean") %>%
  mutate(mean = round(mean/10000, 2))



# Compare with omega summary and overwrite file to correct ---------------------

# Remove 1:! correlations and examine only useful columns 
xmn_sum <- sum_exp2 %>% 
  select(trial, comorbidity, mean) %>%
  filter(!mean == 1)


# From visual comparison I am 100% positive (complete match across correlations) that:
# nctshrt 1 = DIA3005
# nctshrt 2 = 28431754DIA3014


# Get full nct_id by filtering for trial 
trials <- c("DIA3005", "28431754DIA3014")
ncts <- nct_trial_id %>% filter(trial %in% trials)

# Add correct nct short
ncts <- ncts %>% inner_join(nct_id_lbl)



# Recode nctshrt in draws dataframe to match look up tables from the first export 
correct_draws <- draws_exp2 %>% 
  mutate(nctshrt = if_else(nctshrt == 1, 18, 24)) %>% 
  select(-iteration)

# Save  
write_csv(correct_draws, "Supporting/Prepare_trial_data/Yoda/Outputs/Correct_yoda_omega_draws_run2_compressed.csv") # USE THIS


