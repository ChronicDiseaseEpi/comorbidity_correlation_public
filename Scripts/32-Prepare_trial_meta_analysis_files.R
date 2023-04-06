#' ---
#' title: "32: Prepare trial weighted estimate"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script makes the trial weighted summary dataframe for meta analysis trial comparison 
# It creates 2 dataframes by weighting trial draws by number of trial participants  
#   1) "weighted_meta_draws_temp.rds" - weighted MCMC draws
#   2) "trial_meta_analysis_weighted_omega_summaries.csv" - weighted omega summaries



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
tr_draws  <- readRDS("Outputs/all_trials_omega_draws.RDS")
trial_age <- read_csv("Created_metadata/all_trial_age_distributions.csv")



# Prepare df for weighting ------------------------------------------------------

# Add total number of patients per trial 
trial_n <- trial_age %>% 
  group_by(trial, condition) %>% 
  summarise(n_pat = sum(n_patients))

tr_draws <- tr_draws %>% 
  left_join(trial_n)

# Add total number of patients per index condition
cond_n <- trial_age %>%
  group_by(condition) %>%
  summarise(total_patients = sum(n_patients))

tr_draws <- tr_draws %>% 
  left_join(cond_n)



# Calculate weighted estimate -------------------------------------------------- 

# Weighted draws
tr_draws <- tr_draws %>%
  mutate(w_estimate = estimate*n_pat/total_patients)

# Weighted estimate for each condition by chain and iteration
tr_draws_fin = tr_draws %>% 
  group_by(condition, Omega, comorbidity, chain, iteration) %>% 
  summarise(w_estimate = sum(w_estimate)) %>%
  ungroup()

# Save as temporary object to compare vs original data
saveRDS(tr_draws_fin, "Outputs/trial_meta_analysis_weighted_draws.rds") # Weighted MCMC draws

# Weighted estimate for omega parameters for each condition
tr_draws_fin_smry <- tr_draws_fin %>%
  group_by(condition, Omega, comorbidity) %>% 
  summarise(across(w_estimate, list(mean = mean, 
                                    sd = sd,
                                    CI2.5 = ~ quantile2(.x, probs = 0.025),
                                    CI97.5 = ~ quantile2(.x, probs = 0.975)), .names = "{.fn}")) %>% 
  mutate(across(mean:CI97.5, ~ round(.x, 2))) %>%
  ungroup()



# Correct comorbidity names ----------------------------------------------------

# Split comorbidity combination into separate comorbidities in 2 cols
tr_draws_fin_smry <- tr_draws_fin_smry %>%
  separate(comorbidity, c("Var1", "Var2"), " : ")

# Correct como names
lk_up <- data.frame(new = c("Cardiovascular", "Pain", "Arthritis", "Acid-related disease", 
                            "Asthma", "Anxiety", "Thyroid disorders", "Inflammatory disorders", 
                            "Diabetes", "Osteoporosis"),
                    old = c("CV", "pain", "arthritis", "antacids", "asthma_COPD", 
                            "anxiety", "thyroid", "inflammatory", "diabetes", "osteoporosis"))

# Use look up to add new comos and remove old 
tr_draws_fin_smry <- tr_draws_fin_smry %>% 
  inner_join(lk_up, by = c("Var1" = "old")) %>%
  select(-Var1) %>%
  rename(Var1 = new) %>%
  inner_join(lk_up, by = c("Var2" = "old")) %>%
  select(-Var2) %>%
  rename(Var2 = new)

# Unite Var1 and Var2 back into comorbidity combo
tr_draws_fin_smry <- tr_draws_fin_smry %>%
  unite("comorbidity", c(Var1, Var2), sep = " : ", remove = FALSE)



# Limit to unique comorbidity combinations -------------------------------------

# Add shortened omega 
tr_draws_fin_smry <- tr_draws_fin_smry %>%
  mutate(om_short = str_remove(Omega, "Omega") %>% str_trim() %>% str_sub(2) %>% str_remove("\\."))

# Distinct combos 
a <- combn(1:6, 2)
a <- apply(a, 2, function(x) paste(x, collapse = ""))

# Filter for distinct combos 
tr_draws_fin_smry_unique <- tr_draws_fin_smry %>% 
  ungroup() %>%
  filter(om_short %in% a)

# Remove extra cols and add repo
tr_draws_fin_smry_unique <- tr_draws_fin_smry_unique %>%
  select(-om_short) %>% 
  mutate(repo = "Trial meta weighted")

# Organise comorbidity combiantions alphabetically 
tr_draws_fin_smry_unique <- tr_draws_fin_smry_unique %>% 
  mutate(new_var1 = pmin(Var1, Var2),
         new_var2 = pmax(Var1, Var2))

tr_draws_fin_smry_unique <- tr_draws_fin_smry_unique %>% 
  unite("new_comorbidity", c(new_var1, new_var2), sep = " : ", remove = FALSE)

# Checks
unique(tr_draws_fin_smry_unique$condition)      # 16
unique(tr_draws_fin_smry_unique$Var1)           # 7
unique(tr_draws_fin_smry_unique$Var2)           # 10
unique(tr_draws_fin_smry_unique$comorbidity)    # 44
length(tr_draws_fin_smry_unique$condition)      # 240 
map(tr_draws_fin_smry_unique, ~ sum(is.na(.x))) # 0

unique(tr_draws_fin_smry_unique$new_var1) # 8
unique(tr_draws_fin_smry_unique$new_var2) # 9
unique(tr_draws_fin_smry_unique$new_comorbidity) # 37

# Clean columns 
tr_draws_fin_smry_unique <- tr_draws_fin_smry_unique %>% 
  select(-Var1, - Var2, -comorbidity) %>%
  rename(Var1 = new_var1,
         Var2 = new_var2, 
         comorbidity = new_comorbidity)

# Save 
write_csv(tr_draws_fin_smry_unique, "Outputs/trial_meta_analysis_weighted_omega_summaries.csv") # Weighted omega summaries



