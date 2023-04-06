# This script combines the 2 different exports from Yoda. 



## 1st export ## 

# Omega summaries for all trials were exported in a single csv file titled:
#  "yoda_trial_omega_summaries.csv"

# Omega draws for all trials were manually compressed and exported in 5 files:
#  "yoda_omegadraws_trials1_6.csv"
#  "yoda_omegadraws_trials7_12.csv"
#  "yoda_omegadraws_trials13_18.csv"
#  "yoda_omegadraws_trials19_22.csv"
#  "yoda_omegadraws_trials23_25.csv"

# Trail 28431754DIA3008 needs to be removed from both omega summaries and omega 
# draws from export 1 as diagnostics indicated major fitting issues

# Trials DIA3005 and 28431754DIA3014 need to be removed from 1st export omega 
# summaries and draws as their models were later found to have many divergent transitions.
# Hence, they need to be replaced using the 2nd export omega draws/summaries 
# which are from the model re-runs that removed the divergent transitions. 



## 2nd export ##

# Omega summaries and omega draws from the 2nd export only contain 2 trials, these are to replace themselves 
# from the first export:
#  "yoda_omega_draws_run2_compressed.csv" - **lkp up tables not exported so wrong nctshrt see below
#  "yoda_omega_summaries_run2.csv"


# ** The corresponding look up tables for the second export were not exported and the look 
# up tables from the 1st export do not match due to having more trials. 
# Run script "01-Identify_unlabeled_trials_from_omega_estimates" first to change 
# the nctshrts to match with the look up tables from the first export. 
# Trials were  identified by comparing calculated mean omega estimates from the 
# draws table with the mean estimates in the omega summary table which have the correct trial names.

# IMPORTANT
# Use script output:   "Processed_data/Correct_yoda_omega_draws_run2_compressed.csv"   
# INSTEAD OF file exported from Yoda: "yoda_omega_draws_run2_compressed.csv". 



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# Look up tables (from 1st export)
nct_trial_id <- read_csv("Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_nct_id_trial_id.csv")
nct_id_lbl   <- read_csv("Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_nct_id_lbl_lkp.csv")
como_combo   <- read_csv("Supporting/Prepare_trial_data/Yoda/Created_metadata/yoda_omega_comorbidity_combo_lbls.csv")

# 1st export (omega draws in 5 parts) - remove 3 trials from draws/summaries
draws1_exp1 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials1_6.csv")
draws2_exp1 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials7_12.csv")
draws3_exp1 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials13_18.csv")
draws4_exp1 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials19_22.csv")
draws5_exp1 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_trials23_25.csv")

# 2nd export omega draws and summaries (3 replacement trials)
draws_exp2 <- read_csv("Supporting/Prepare_trial_data/Yoda/Outputs/Correct_yoda_omega_draws_run2_compressed.csv")



# Combine 1st export of omega draws --------------------------------------------

# Join draws dataframes  
draws_exp1 <- draws1_exp1 %>%
  bind_rows(draws2_exp1,
            draws3_exp1,
            draws4_exp1,
            draws5_exp1)

rm(draws1_exp1, draws2_exp1, draws3_exp1, draws4_exp1, draws5_exp1)

# Add column to identify 1st export
draws_exp1 <- draws_exp1 %>% 
  mutate(repo = "YODA_run1")
  
# Harmonise nct short and correct nct ID to long version 
draws_exp1 <- draws_exp1 %>% 
  rename(nct_shrt = nctshrt) %>% 
  left_join(nct_id_lbl, by = "nct_shrt") %>% 
  select(-nct_shrt)

# Add trial and condition 
draws_exp1 <- draws_exp1 %>%
  left_join(nct_trial_id, by = "nct_id")



# Remove 3 trials (1 deleted fully, 2 replaced later) --------------------------

# Trials to remove and replace 
rm_trial   <- "28431754DIA3008"
rep_trials <- c("DIA3005", "28431754DIA3014")

# Remove 3 trials
n_distinct(draws_exp1$trial) # 25
draws_exp1 <- draws_exp1 %>% filter(!trial %in% c(rm_trial, rep_trials))
n_distinct(draws_exp1$trial) # 22



# Add in 2 replacement trials --------------------------------------------------

# Harmonise nct short and correct nct ID to long version 
draws_exp2 <- draws_exp2 %>% 
  rename(nct_shrt = nctshrt) %>% 
  left_join(nct_id_lbl, by = "nct_shrt") %>% 
  select(-nct_shrt)

# Add trial and condition 
draws_exp2 <- draws_exp2 %>% 
  left_join(nct_trial_id, by = "nct_id")

# Add export 1 and 2 togther 
all_draws <- draws_exp1 %>% 
  bind_rows(draws_exp2)

# Check there are 24 trials 
unique(all_draws$trial)
rm(draws_exp1, draws_exp2)



# Uncompress -------------------------------------------------------------------

# Arrange by nct short then add iteration column 
all_draws <- all_draws %>%
  arrange(nct_id) %>% 
  mutate(iteration = rep(1:1000, times = 72))

# Omega parameters into long format 
all_draws_lng <- all_draws %>%
  pivot_longer(cols = "12":"56", names_to =  "Omega", values_to = "estimate") %>%
  mutate(estimate = estimate/10000)



# Recreate matrix (i.e ad diagonals and mirrored correlations)

# dataframe with rows, cols and vcov, so will need to have gathered and separated your current columns
yd <- all_draws_lng %>% 
  select(nct_id, chain, iteration, estimate, Omega)

yd2 <- yd %>%
  separate(Omega, into = c("rows", "cols"), sep = 1) %>%
  rename(vcov = estimate)

yd_nst <- yd2 %>% 
  nest(data = c(rows, cols, vcov))

# Function to recreate full matrix 
mycount <- 0

CnvrtMatrix <- function(a){
  
  print(mycount)
  mycount <<- mycount +1
  
  ## recovery whole matrix by duplication
  a <- bind_rows(a,
                 a %>%
                   rename(rows = cols, cols = rows)) %>%
    distinct()
  # convert into matrix format
  a <- a %>%
    spread(cols, vcov)
  a_rows <- a$rows
  a$rows <- NULL
  a <- as.matrix(a)
  diag(a) <- 1
  if (any(is.na(a))) warning("Missing values in matrix")
  rownames(a) <- a_rows
  a
}

# Very slow - output saved as intermediate file "list_column_draws_temp.rds"
#yd_nst$data2 <- map(yd_nst$data, CnvrtMatrix)

# Save intermediate to prevent having to run again
#saveRDS(yd_nst, "Supporting/Prepare_trial_data/Yoda/Outputs/list_column_draws_temp.rds")
yd_nst <- readRDS("Supporting/Prepare_trial_data/Yoda/Outputs/list_column_draws_temp.rds")

# Turn matrix into 3 columned df 
yd_nst$data3 <- map(yd_nst$data2, melt)

# Unnest column of dfs
yd_unnst <- yd_nst %>% 
  unnest(cols = data3) %>%
  mutate(Omega = paste0(Var1, Var2)) %>%
  rename(estimate = value) %>%
  select(-c(data, data2, Var1, Var2)) 

# Add comorbidity - NEED TO FILL IN 1:1 AND UPPER MATRIX CORNER
como_combo$Omega <- as.character(como_combo$Omega)

yoda_draws_final <- yd_unnst %>%
  left_join(como_combo, by = c("nct_id", "Omega"))

# Add condition and trial name 
yoda_draws_final <- yoda_draws_final %>%
  left_join(nct_trial_id, by = "nct_id")

# Correct omega names = Omegax.y
yoda_draws_final <- yoda_draws_final %>% 
  mutate(Omega = stri_sub_replace(Omega, 2, 1, value = "."),
         Omega = paste0("Omega.", Omega))

# Add repo column 
yoda_draws_final <- yoda_draws_final %>%
  mutate(repo = "YODA")

# Save 
saveRDS(yoda_draws_final, "Supporting/Prepare_trial_data/Yoda/Outputs/yoda_omega_draws_final.rds") # USE THIS FOR PLOTTING


