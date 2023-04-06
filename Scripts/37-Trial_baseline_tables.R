#' ---
#' title: "37: Trial baseline tables"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script produces summary tables for trials that will then be used in script 36



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
tr_om_sum  <- read_csv("Outputs/all_trials_omega_summaries.csv")
cmnty      <- read.csv("Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv")

vivli_prop <- read_csv("Supporting/Prepare_trial_data_and_analyse/Vivli/Outputs/comorbidities_by_trial.csv")
gsk_prop   <- read_csv("Supporting/Prepare_trial_data_and_analyse/GSK/Outputs/GSK_comorbidities_by_trial.csv")
yoda_prop  <- read_csv("Supporting/Prepare_trial_data_and_analyse/Yoda/Outputs/yoda_comorbidity_proportions_by_trial.csv")

viv_n      <- read_csv("Supporting/Prepare_trial_data_and_analyse/Vivli/Created_metadata/vivli_trial_age_distribution_by_sex.csv")
gsk_n      <- read_csv("Supporting/Prepare_trial_data_and_analyse/GSK/Created_metadata/gsk_trial_age_distribution.csv")
yoda_n     <- read_csv("Supporting/Prepare_trial_data_and_analyse/Yoda/Created_metadata/yoda_trial_age_distribution.csv")




# Comorbidity proportions by trial ---------------------------------------------

# Add repo 
vivli_prop <- vivli_prop %>% mutate(repo = "Vivli")
gsk_prop   <- gsk_prop %>% mutate(repo = "GSK") 

# Make yoda into percentages 
yoda_prop <- yoda_prop %>% 
  mutate(repo = "YODA",
         across(antacids:skin, ~ .x*100))

# Join dfs 
all_prop <- vivli_prop %>% bind_rows(gsk_prop, yoda_prop)

# Filter for trials in study 
study_trs <- unique(tr_om_sum$trial)
all_prop <- all_prop %>% filter(trial %in% study_trs)

# Harmonise conditions
unique(all_prop$condition) # 25 conds instead of 16 
unique(tr_om_sum$condition) # names of correct 16 conditions

all_prop <- all_prop %>% 
  mutate(condition = case_when(condition == "Diabetes Mellitus, Type 2; Renal Insufficiency" ~ "Diabetes_Mellitus_Type_2",
                               condition == "Diabetes Mellitus, Type 2;   Hyperglycemia" ~ "Diabetes_Mellitus_Type_2",
                               condition == "Diabetes Mellitus, Type 2, nephropathy" ~ "Diabetes_Mellitus_Type_2", 
                               condition == "Hypertension; Diabetic Nephropathies" ~ "Diabetes_Mellitus_Type_2",
                               condition == "Diabetic Nephropathies" ~ "Diabetes_Mellitus_Type_2",
                               condition == "Diabetes Mellitus" ~ "Diabetes_Mellitus_Type_2",
                               condition == "Diabetes Mellitus, Type 2" ~ "Diabetes_Mellitus_Type_2", 
                               condition == "Type 2 Diabetes Mellitus" ~ "Diabetes_Mellitus_Type_2",
                               condition == "Pulmonary Disease, Chronic Obstructive, Ph" ~ "Pulmonary_Disease_Chronic_Obstructive",
                               condition == "Pulmonary Disease, Chronic Obstructive" ~ "Pulmonary_Disease_Chronic_Obstructive",
                               condition == "Erectile Dysfunction, Benign Prostatic Hyperplasia" ~ "Benign_Prostatic_Hyperplasia",
                               condition == "Alzheimer's Disease" ~ "Dementia_any",
                               condition == "Restless Legs Syndrome" ~ "Restless_legs_syndrome",
                               condition == "Hypertension, Pulmonary" ~ "Pulmonary_Hypertension",
                               condition == "ankylosing spondylitis" ~ "Axial_Spondyloarthritis",
                               condition == "rheumatoid arthritis" ~ "Rheumatoid_arthritis",
                               TRUE ~ as.character(condition) %>% str_remove_all(",") %>% str_replace_all(" ", "_")))

setdiff(unique(all_prop$condition), unique(tr_om_sum$condition)) # matching 
map(all_prop, ~ sum(is.na(.x)))
unique(all_prop$condition) # 16

# Clean dfs            SHOULD SKIN BE REMOVED?
all_prop <- all_prop %>% 
  select(-num_of_como, -company, - medicine, -n_patients)

# Add number of patients per trial
gsk_n <- gsk_n %>% rename(n_patients = n)
viv_n <- viv_n %>% 
  rename(sd_age = age_std,
         mean_age = age_mean,
         min_age = age_min,
         max_age = age_max)
tr_n <- viv_n %>% bind_rows(gsk_n, yoda_n)

trial_n <- tr_n %>% 
  group_by(trial) %>%
  summarise(n = sum(n_patients))

all_prop <- all_prop %>% inner_join(trial_n)

# Save
write_csv(all_prop, "Outputs/comorbidity_proportions_by_trial.csv")




# Total number of patients per condition ---------------------------------------

# Summary for conditions 
cond_n <- tr_n %>% 
  group_by(condition) %>% 
  summarise(n = sum(n_patients),
            age_mean = round(mean(mean_age), 0),
            age_sd = round(mean(sd_age), 0))

# Calculate % male, (LILLY_B3D_MC_GHAC only 1 sex, female)
tr_male <- tr_n %>% filter(sex %in% "male") 
cond_male <- tr_male %>% 
  group_by(condition) %>%
  summarise(male_n = sum(n_patients))

cond_n <- cond_n %>% 
  inner_join(cond_male) %>%
  mutate(male_pcnt = round((male_n/n)*100, 1),
         repo = "Trials") 

# Add number of trials per condition 
num_tr <- all_prop %>% 
  group_by(condition) %>%
  summarise(n_trials = n())

cond_n <- cond_n %>%
  inner_join(num_tr)

# Save 
write_csv(cond_n, "Created_metadata/trial_condition_summary.csv")




# Top 6 Comorbidity proportions by condition -----------------------------------

all_prop <- read_csv("Outputs/comorbidity_proportions_by_trial.csv")
cond_n <- read_csv("Created_metadata/trial_condition_summary.csv")

comos <- unique(cmnty$como)

all_prop <- all_prop %>% 
  select(trial, nct_id, condition, n, all_of(comos))

all_prop_lng <- all_prop %>%
  pivot_longer(names_to = "como", values_to = "proportion", cols = pain:thyroid)

# Filter to same conditions in trial/cmnty 
conds <- unique(all_prop$condition)
cmnty <- cmnty %>% filter(condition %in% conds)

# filter to top 6 for each condition
all_prop_lng <- all_prop_lng %>% semi_join(cmnty, by = c("condition", "como"))

# Turn back into counts
all_prop_lng <- all_prop_lng %>% 
  mutate(count = (n/100)*proportion) %>%
  rename(n_trial = n,
         comorbidity = como)

# Calculate proportions p/ condition
cond_prop <- all_prop_lng %>% 
  group_by(condition, comorbidity) %>%
  summarise(condition_n = sum(n_trial),
            comorbidity_n = round(sum(count), 0),
            como_prop = comorbidity_n/condition_n,
            prop_se = sqrt((como_prop*(1-como_prop))/condition_n)) %>%
  mutate(repo = "Trials",
         como_prop = como_prop*100,
         se_min = como_prop - (1.96*prop_se),
         se_max = como_prop + (1.96*prop_se)) 

# Harmonise comorbidities
lk_up <- data.frame(new_como = c("Cardiovascular", "Pain", "Arthritis", "Acid-related disease", "Asthma", 
                                 "Anxiety", "Thyroid disorders", "Inflammatory disorders", "Diabetes", "Osteoporosis"),
                    comorbidity = c("CV", "pain", "arthritis", "antacids", "asthma_COPD", 
                                    "anxiety", "thyroid", "inflammatory", "diabetes", "osteoporosis"))

cond_prop <- cond_prop %>% inner_join(lk_up)

cond_prop <- cond_prop %>%
  select(-comorbidity) %>%
  rename(comorbidity = new_como) %>%
  arrange(condition, desc(como_prop))

# Save 
write_csv(cond_prop, "Outputs/trial_comorbidity_proportions_by_condition.csv")
  


