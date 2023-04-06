## 18 Covariates_consolidate_dataset ----

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

# Data
cmpnies <- readRDS("../Extract_Data/Processed_data/all_sponsors_not_conmed.Rds")
bmi <- cmpnies$bmi
bp  <- cmpnies$bp
demo <- cmpnies$demo
bsa <- readRDS("../Extract_Data/Processed_data/bsa.Rds")
egfr <- readRDS("../Extract_Data/Processed_data/egfr.Rds")
fib4 <- readRDS("../Extract_Data/Processed_data/fib4.Rds")
hgb <- readRDS("../Extract_Data/Processed_data/hgb.Rds")
rm(cmpnies)

# Number trials in each table
n_distinct(bmi$trial) # 48, 46 (17/10/22)
n_distinct(bp$trial)  # 49, 47 (17/10/22)
n_distinct(demo$trial) # 50, 48 (17/10/22)
n_distinct(bsa$trial) # 48, 46 (17/10/22)
n_distinct(egfr$trial) # 43, 43 (17/10/22)
n_distinct(fib4$trial) # 43, 41 (17/10/22)
n_distinct(hgb$trial) # 43, 41 (17/10/22)


## Standardise egfr to bsa
## Identify those who are "very large" or "very small", standard bsa is 1.73m2
vl_vs <- bmi %>% 
  filter(param == "bmi", value >38 | value <22) %>% 
  distinct(trial, id)

bsa_need <- bsa %>% 
  semi_join(vl_vs) %>% 
  mutate(crct = bsa/1.73) %>% 
  select(trial, id, crct) %>% 
  distinct(trial, id, .keep_all = TRUE)

egfr <- egfr %>% 
  left_join(bsa_need) %>% 
  mutate(crct = if_else(is.na(crct), 1, crct))

egfr <- egfr %>% 
  mutate(result = result * crct)

## Create covariates as single dataset
all_cov <- bind_rows(
  bmi = bmi %>% filter(param == "bmi") %>% select(-nct_id) %>% rename(name = param, result = value),
  bp = bp %>% select(-nct_id) %>% rename(name = param, result = value),
  egfr = egfr %>% select(trial, id, name, result),
  fib4 = fib4 %>% mutate(name = "fib4") %>% select(trial, id, name, result = fib4),
  hgb = hgb %>% select(trial, id, name = param, result = value)
)

all_cov <- all_cov %>% 
  select(company, trial, id, name, result) %>% 
  distinct(company, trial, id, name, .keep_all = TRUE) %>% 
  spread(name, result)

all_cov_smry <- all_cov %>%
  select(trial, bmi:sbp) %>%
  mutate(across(bmi:sbp, is.na)) %>%
  group_by(trial) %>%
  summarise_all(mean) %>%
  ungroup() %>%
  mutate(across(bmi:sbp, ~if_else(is.na(.x), "", "Got")))

## Divide BMI by 10, and dbp, sbp, egfr and hgb by 100       @CHECK 
all_cov %>% 
  mutate(EGFR = if_else(EGFR >=60, 60, EGFR)) %>% 
  summarise_at(vars(bmi:sbp), list(m = mean, s = sd), na.rm = TRUE) 
names(all_cov) <- names(all_cov) %>% str_to_lower()
all_cov_mdl <- all_cov %>%
  inner_join(demo %>% select(trial, id, sex, age)) %>% 
  mutate(hgb_anem = case_when(
    sex == "male" & hgb >130 ~ 0,
    sex == "male" & hgb <=130 ~ 130 -hgb,
    sex == "female" & hgb >120 ~ 0,
    sex == "female" & hgb <=120 ~ 120 -hgb),
    anem = if_else( (sex == "male" & hgb <130) | (sex == "female" & hgb <120), TRUE, FALSE),
    egfr = if_else(egfr >=60, 60, egfr)) %>% 
  mutate_at(vars(dbp, sbp, egfr, hgb_anem), function(x) x/100) %>% 
  mutate(bmi = bmi/10)
saveRDS(list(got_vars = all_cov_smry, all_cov_mdl = all_cov_mdl), "../Extract_Data/Processed_data/model_covs_continuous.Rds")


## Categorise variables for comorbidity count

all_cov_cat <- all_cov %>%
  inner_join(demo %>% select(trial, id, sex)) %>% 
  mutate(obese = bmi >30,
         hyprt = sbp>=140 | dbp >=90,
         renal = egfr <=60,
         liver = fib4 >= 1.45,
         anaem = case_when(
           hgb <130 & sex == "male" ~ TRUE,
           hgb <120 & sex == "female" ~ TRUE,
           TRUE ~ FALSE)) %>% 
  select(-sex) %>% 
  select(trial, id, obese, hyprt, renal, liver, anaem) %>% 
  distinct(trial, id, .keep_all = TRUE)

saveRDS(list(got_vars = all_cov_smry, all_cov_cat = all_cov_cat), "../Extract_Data/Processed_data/model_covs_categorical.Rds")

        
        
