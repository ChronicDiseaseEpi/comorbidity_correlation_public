#' ---
#' title: "30: Combine model outputs form all trials"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script joins all the model outputs (omega summaries and MCMC draws)
# from the models ran for each trial (1 model per trial), and saves them all
# as two single dataframes: omega summaries and omega draws



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Omega summaries
vivli_sum <- read_csv("Supporting/Prepare_trial_data_and_analyse/Vivli/Results/vivli_omega_summaries.csv")
gsk_sum   <- read_csv("Supporting/Prepare_trial_data_and_analyse/GSK/Results/GSK_trial_omega_summaries.csv")
yoda_sum  <- read_csv("Supporting/Prepare_trial_data_and_analyse/Yoda/Outputs/yoda_omega_summary_final.csv")

# Omega draws
vivli_draws <- readRDS("Supporting/Prepare_trial_data_and_analyse/Vivli/Results/vivli_omega_draws.RDS")
gsk_draws   <- readRDS("Supporting/Prepare_trial_data_and_analyse/GSK/Results/GSK_omega_draws.RDS")
yoda_draws  <- readRDS("Supporting/Prepare_trial_data_and_analyse/Yoda/Outputs/yoda_omega_draws_final.rds")

# Age distributions
gsk_tr_age   <- read_csv("Supporting/Prepare_trial_data_and_analyse/GSK/Created_metadata/gsk_trial_age_distribution.csv")
yoda_tr_age  <- read_csv("Supporting/Prepare_trial_data_and_analyse/Yoda/Created_metadata/yoda_trial_age_distribution.csv")
vivli_tr_age <- read_csv("Supporting/Prepare_trial_data_and_analyse/Vivli/Created_metadata/vivli_trial_age_distribution_by_sex.csv")




# Combine trial omega summaries ------------------------------------------------

# Rename CI in vivli to match other 2 repos 
vivli_sum <- vivli_sum %>% 
  rename(CI2.5 = ci2.5, 
         CI97.5 = ci97.5)

# Bind rows 
all_trials_sum <- vivli_sum %>% 
  bind_rows(gsk_sum, yoda_sum)

# Remove all 1:1 correlations 
all_trials_sum <- all_trials_sum %>% 
  filter(Var1 != Var2)

# Make unique comorbidity column 
all_trials_sum <- all_trials_sum %>% 
  mutate(new_var1 = pmin(Var1, Var2),
         new_var2 = pmax(Var1, Var2))

all_trials_sum <- all_trials_sum %>% 
  unite("new_comorbidity", c(new_var1, new_var2), sep = " : ", remove = FALSE)

# Check 
names(all_trials_sum)
n_distinct(all_trials_sum$trial) # 83
unique(all_trials_sum$Var1) # 9
unique(all_trials_sum$Var2) # 8
unique(all_trials_sum$repo) # 3
unique(all_trials_sum$comorbidity) # 37
unique(all_trials_sum$condition) # 16

unique(all_trials_sum$new_var1) # 8
unique(all_trials_sum$new_var2) # 9
unique(all_trials_sum$new_comorbidity) # 37

# Clean up columns
all_trials_sum <- all_trials_sum %>% 
  select(-Var1, -Var2, -comorbidity) %>% 
  rename(Var1 = new_var1,
         Var2 = new_var2,
         comorbidity = new_comorbidity)

# Save
write_csv(all_trials_sum, "Outputs/all_trials_omega_summaries.csv")



# Combine trial omega mcmc draws -----------------------------------------------

all_trials_draws <- vivli_draws %>% 
  bind_rows(gsk_draws, yoda_draws)

n_distinct(all_trials_draws$trial) # 83
names(all_trials_draws)
unique(all_trials_draws$comorbidity) # 84 
unique(all_trials_draws$condition) # 16
unique(all_trials_draws$repo) # 3

# Save 
saveRDS(all_trials_draws, "Outputs/all_trials_omega_draws.RDS")




# All trial age distributions --------------------------------------------------

# Harmonise df names 
vivli_tr_age <- vivli_tr_age %>% 
  rename(mean_age = age_mean,
         sd_age = age_std,
         min_age = age_min,
         max_age = age_max)

gsk_tr_age <- gsk_tr_age %>% 
  rename(n_patients = n)

# Bind rows 
trial_ages <- gsk_tr_age %>%
  bind_rows(yoda_tr_age, vivli_tr_age) %>%
  mutate(sex = if_else(sex == "male", "Male", "Female"))

# Save - by sex 
write_csv(trial_ages, "Created_metadata/all_trial_age_distributions.csv")



# Index condition age distributions --------------------------------------------

ind_cond_age <- trial_ages %>%
  group_by(condition, sex) %>% 
  summarise(n_trial = n(),
            n_patients = sum(n_patients),
            min_age = round(min(min_age), 2),
            max_age = round(max(max_age), 2),
            mean_of_mean_age = round(mean(mean_age), 2),
            sd_age = round(sd(sd_age), 2)) %>%
  mutate(sd_age = if_else(is.na(sd_age), 0, sd_age))

# Save
write_csv(ind_cond_age, "Created_metadata/all_trial_index_condition_age_distribution.csv")


