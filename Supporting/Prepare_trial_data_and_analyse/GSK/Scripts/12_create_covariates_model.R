## 12_Covariates_consolidate_dataset ----

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

gsk  <- readRDS("GSK_processed_data/GSK_not_conmed.Rds")
bmi  <- gsk$bmi
bp   <- gsk$bp
demo <- gsk$demo
bsa  <- readRDS("GSK_processed_data/bsa.Rds")
egfr <- readRDS("GSK_processed_data/egfr.Rds")
fib4 <- readRDS("GSK_processed_data/fib4.Rds")
hgb  <- readRDS("GSK_processed_data/hgb.Rds")
rm(gsk)

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
  group_by(trial) %>% 
  summarise_at(vars(bmi:sbp), function(x) if_else(!all(is.na(x)), "Got", "")) %>% 
  ungroup()

## Divide BMI by 10, and dbp, sbp, egfr and hgb by 100
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
saveRDS(list(got_vars = all_cov_smry, all_cov_mdl = all_cov_mdl), "GSK_processed_data/GSK_model_covs_continuous.Rds")


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

saveRDS(list(got_vars = all_cov_smry, all_cov_cat = all_cov_cat), "GSK_processed_data/GSK_model_covs_categorical.Rds")
        
        
