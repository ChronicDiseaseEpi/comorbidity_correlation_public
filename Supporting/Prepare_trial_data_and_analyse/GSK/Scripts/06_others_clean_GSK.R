# 06_others_clean

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

path <- ""

gsk <- readRDS("GSK_processed_data/GSK_transposed.Rds")
gsk$conmed <- NULL



## BMI ----
bmi <- gsk$bmi

names_lkp <- c("bmi" = "bmi", "ht" = "ht", "htstd" = "ht", "wt" = "wt", "wtstd" = "wt",
               "height" = "ht", "weight" = "wt",
               "heightcm" = "ht", "weightkg" = "wt")

setdiff(bmi$param, names_lkp %>% names())
bmi$param <- names_lkp[bmi$param]
bmi$value <- as.double(bmi$value)

bmi <- bmi %>% 
  na.omit() %>% 
  group_by(company, trial, id, param) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

bmi <- bmi %>% 
  spread(key = param, value) %>% 
  mutate(bmi = if_else(is.na(bmi), wt/(ht/100)^2, bmi)) %>% 
  gather("param", "value", -company, -trial, -id)

bmi_xmn <- bmi %>% 
  group_by(trial, param) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  spread(param, value)

write_csv(bmi_xmn, "Outputs/GSK_bmi_htwt_mean_per_trial.csv")

## BI502_392 clearly wrong units, need to go back and check units for this one
## Obvious from bmi, height and weight
gsk$bmi <- bmi
rm(bmi)



## rand ----
rand <- gsk$rand

## Examine different date formats
rand_dt_convert <- rand %>% 
  filter(!is.na(randdt)) %>% 
  distinct(company, trial, randdt) %>% 
  mutate(date_formatted = str_detect(randdt, "[0-9]{4,4}-[0-9]{2,2}-[0-9]{2,2}"),
         num_formatted = str_detect(randdt, "[0-9]{5,5}"))

rand_dt_convert_xmn <- rand_dt_convert %>% 
  group_by(company, trial) %>% 
  summarise(date_formatted = mean(date_formatted),
            num_formatted = mean(num_formatted),
            randdt = randdt[1]) %>% 
  ungroup()

# all dates are y-m-d (eg 2001-01-01)

write_csv(rand_dt_convert_xmn, "Outputs/GSK_date_formats.csv")

rand_xmn <- rand %>% 
  group_by(company, trial) %>% 
  summarise(arm = unique(arm) %>% paste(collapse = ", "),
            randdt = sum(!(is.na(randdt))))

write_csv(rand_xmn, "Outputs/GSK_rand_smry_per_trial.csv")

gsk$rand <- rand
rm(rand)





## Demo ----
demo <- gsk$demo

## Use height to determine sex for 31 trials where thimedhs is not recorded
demo2 <- demo %>% 
  filter(sex %in% 1:2) %>% 
  group_by(trial) %>% 
  mutate(n_sexes = sum(!duplicated(sex))) %>% 
  ungroup() 

# eXamine single sex trials
demo2 %>% filter(n_sexes ==1) %>% distinct(trial, n_sexes, sex)

demo2 <- demo2 %>%
  filter(n_sexes >=2) %>% 
  select(-n_sexes)

ht <- gsk$bmi %>%
  semi_join(demo2 %>% select(trial)) %>% 
  filter(param == "ht") %>% 
  distinct(trial, id, value)

demo3 <- demo2 %>% 
  left_join(ht) %>% 
  group_by(trial, sex) %>% 
  summarise(n = length(sex),
            n_measures = sum(!is.na(value)), 
            ht = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(trial, ht)

demo_lbl <- demo3 %>% 
  group_by(trial) %>% 
  mutate(max_ht = max(ht)) %>% 
  ungroup() %>% 
  mutate(sex_lbl = if_else(ht == max_ht, "M", "F")) 

demo_lbl_compare <- demo_lbl %>% 
  select(sex_lbl, ht, trial) %>% 
  spread(sex_lbl, ht) %>% 
  mutate(M - F)
range(demo_lbl_compare$`M - F`)

demo_lbl_final <- demo_lbl %>%
  distinct(trial, sex, sex_lbl)
rm(demo_lbl, demo_lbl_compare, demo2, demo3)

## Add labels back onto main dataset
demo <- demo %>% 
  left_join(demo_lbl_final) %>% 
  mutate(sex = if_else(!is.na(sex_lbl), sex_lbl, sex)) %>% 
  select(-sex_lbl) %>% 
  ungroup()

demo <- demo %>% 
  mutate(sex = case_when(
    str_to_lower(sex) %in% c("m", "male") ~ "male",
    str_to_lower(sex) %in% c("f", "female") ~ "female",
    sex == "U" ~ NA_character_))

## Demo
demo <- demo %>% 
  semi_join(gsk$rand)

## Correct age bands and >/< to age
age_band <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/reviewED_age_bands_to_age_numeric.csv")


demo <- demo %>% 
  left_join(age_band) %>% 
  mutate(age = if_else(!is.na(age_numeric), as.character(age_numeric), age)) %>% 
  select(-age_numeric)


## Detect de facto top adn bottom of age eligibility
age_mm <- demo %>% 
  mutate(age = as.integer(age)) %>% 
  filter(!is.na(age)) %>%  
  group_by(trial) %>% 
  mutate(min_age = min(age), max_age = max(age))  %>% 
  ungroup()

age_mm <- age_mm %>% 
  group_by(trial, min_age, max_age) %>% 
  summarise(min_age_n = sum(age == min_age),
            max_age_n = sum(age == max_age)) %>% 
  ungroup()

demo2 <- demo %>% 
  mutate(age = as.integer(age)) %>% 
  filter(!is.na(age), !is.na(sex)) %>% 
  mutate(trial_sex = paste(trial, sex, sep = "_")) %>% 
  arrange(trial_sex)

density_age_sex <- tapply(demo2$age, demo2$trial_sex, density)
# tapply(demo2$age, demo2$trial_sex, sd, na.rm = TRUE)
density_age_sex <- map(density_age_sex, ~ .x[c("x", "y")] %>% as.data.frame())
density_age_sex <- bind_rows(density_age_sex, .id = "trial_sex")

demo3 <- demo2 %>% 
  distinct(trial, sex, trial_sex) %>% 
  inner_join(density_age_sex)

## Summarise age and sex results as continuous variable
demo_xmn_cont <- demo2 %>% 
  filter(!is.na(sex), !is.na(age)) %>% 
  group_by(company, trial, sex) %>% 
  summarise_at(vars(age), .funs = c(n = "length", "mean", "sd", "min", "max")) %>% 
  ungroup() 
write_csv(demo_xmn_cont, "Outputs/GSK_demo_smry_per_trial_continuous.csv")

## Summarise age and sex results
demo_xmn <- demo %>% 
  filter(!is.na(sex), !is.na(age)) %>% 
  mutate(age_cut = Hmisc::cut2(age %>%  as.double(), cuts = c(30, seq(40, 90, 10)))) %>% 
  group_by(company, trial, sex, age_cut) %>% 
  summarise(n = sum(!duplicated(id))) %>% 
  ungroup() %>% 
  spread(age_cut, n, fill = 0L)
write_csv(demo_xmn, "Outputs/GSK_demo_smry_per_trial.csv")



## Summarise race/ethnicity
DropCollapse <- function(x) {
  x <- x[!is.na(x) & x!= ""]
  x %>% 
    unique() %>% 
    sort() %>% 
    paste(collapse = ", ")
}

race_xmn <- demo %>%
  filter(!is.na(race), !race == "") %>% 
  distinct(race, trial, smoke) %>% 
  group_by(trial) %>% 
  summarise_all(DropCollapse)  

write_csv(race_xmn, "Outputs/GSK_race_smry_per_trial.csv")

gsk$demo <- demo
rm(demo)
sum(is.na(gsk$demo$age)) # 0 (25/08/22)
gsk$demo %>% filter(race %in% "") %>% count() # 8 (25/08/22)



## Labs ----

# Can get labs to wokr but the labs_failed doesnt work as missing:
# result non stabndard and result character 

labs <- gsk$labs
labs_lbl <- labs %>% 
  distinct(name, label, new_lab)
# write_csv(labs_lbl, "Scratch_data/consolidate_lab_names.csv")
# write_csv(lilly_labs %>% distinct(lbtest), "clipboard")

## Need to recode one GSK trial which uses numbers as the name to its own newlab
labs <- labs %>% 
  mutate(name = if_else(trial == "GSKAR2103413_v02", new_lab, name))

labs_lbls_reviewed <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/reviewED_consolidate_lab_names.csv") %>% 
  distinct(name, new_lab) %>% 
  filter(!is.na(new_lab))

## Relable other and lilly lab results
## NEed to separate urine and serum for lilly labs
labs <- labs %>% 
  select(-new_lab) %>% 
  inner_join(labs_lbls_reviewed) %>% 
  rename(original_name = name, name = new_lab)

# Examine where lab result does not succesfully change to a double format
# For now ignore these, may need to revisit if missing values considerable
# Roche set result to <4 if ALT, AST or SGPT was undetectable, which was only for 10 patients, drop.
labs_num <- labs %>%
  transmute_at(vars(result, ll, ul), as.double) %>% 
  transmute(failed = is.na(result))

labs <- labs %>%
  mutate_at(vars(result, ll, ul), as.double) %>% 
  filter(!is.na(result))
rm(labs_num)

## Aggreagate lab results, slight danger if different assay used with the same trial, id and unit
labs <- labs %>% 
  group_by(company, trial, id, name, original_name, label, unit) %>% # removed fast,
  filter(!is.na(result)) %>% 
  summarise_at(vars(result, ll, ul), mean, na.rm = TRUE) %>% 
  ungroup()

## Examine which emasures were done in each trial
## Split off weight which is a feature in LILLY-B3D-MC-GHAC

labs_count <- labs %>% 
  filter(!name == "weight") %>% 
  group_by(trial, name) %>% 
  count() %>% 
  ungroup() %>% 
  spread(name, n, fill = 0L)

## write this as a summary
write_csv(labs_count, "Outputs/GSK_lab_tests_per_trial.csv")

gsk$labs <- labs
rm(labs, labs_lbl, labs_lbls_reviewed, labs_count)



## BP ----
bp <- gsk$bp

bp <- bp %>% 
  mutate(param = if_else(param %in% c("bpdia", "DBP", "dia", "diabp",
                                      "dia1", "dia2", "dia3",
                                      "diastolic blood pressure"),
                         "dbp",
                         "sbp"),
         value = as.integer(value))

bp <- bp %>% 
  na.omit() %>% 
  group_by(company, trial, id, param) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

bp_xmn <- bp %>% 
  group_by(company, trial, param) %>% 
  summarise(value = mean(value)) %>% 
  spread(param, value)
write_csv(bp_xmn, "Outputs/GSK_blood_pressure_mean_per_trial.csv")

gsk$bp <- bp
rm(bp)




## Medhist - Meddra and Non meddra Medical history data ----

medhist <- gsk$medhist

sum(medhist$meddra == "meddra")     # 64985 (25/08/22)
sum(medhist$meddra == "non-meddra") # 16554 (25/08/22)
sum(is.na(medhist$meddra))          # 0 (25/08/22)
medhist %>% filter(meddra %>% is.na()) %>% distinct(trial) # 0 (25/08/22)

## Review where does not state whether or not it is MedDRA
medhist %>% group_by(trial) %>%
  count(meddra == "meddra")

## 14 trials are meddra
meddra <- medhist %>% filter(meddra == "meddra")
n_distinct(meddra$trial) # 14 (25/08/22)

## Count ones with meddra
meddra_xmn <- meddra %>% 
  distinct(trial, term, id) %>% # removed lltcd,
  group_by(trial) %>% 
  summarise(unique_terms = sum(!duplicated(paste0(term))), # removed lltcd,
            unique_patient = sum(!duplicated(id)),
            entries = length(term)) %>% 
  ungroup() %>% 
  mutate(warning = if_else(unique_terms * unique_patient == entries,
                           "Looks like might have a yes/no structure",
                           ""))

meddra_xmn2 <- meddra %>% 
  distinct(trial, term, id) %>%  # removed lltcd,
  group_by(trial, id) %>%
  count()

meddra_xmn3 <- meddra_xmn2 %>% 
  ungroup() %>% 
  distinct(trial, n) %>% 
  arrange(trial, n) %>% 
  group_by(trial) %>% 
  summarise(`Min` = min(n),
            `Max` = max(n),
            n_values = paste(n, collapse = ", ")) %>% 
  rename(`Range of entries for meddra terms (up to 20)` = n_values)

meddra_xmn3 <- meddra_xmn %>% 
  inner_join(meddra_xmn3) 

# SAVE!
write_csv(meddra_xmn3, "Outputs/GSK_medhist_per_trial.csv")

# Code no longer needed so wrapped in function 
#WrapNonMedhistinFunction <- function() {





non_meddra <- gsk$medhist %>% filter(meddra != "meddra")
## None of these medical history tables have large (ie more than 10 term)
## medical history databases
non_meddra %>% 
  semi_join(meddra_xmn3 %>% 
              filter(unique_terms >10))

tapply(non_meddra$term, non_meddra$trial, function(x) {
  y <- x %>% unique()
  len_y <- min(20, length(y))
  c(sample(y, len_y))
})
tapply(non_meddra$term, non_meddra$trial, function(x) length(unique(x)))

## very messy terms, and lots of them
## All of these trials have ATC coded concomitant medication data, do not attempt to code these diagnoses
conmed <- readRDS("GSK_processed_data/GSK_conmed.Rds") # NEED TO FINISH 04_CONMED_GSK

conmed_xmn <- conmed %>% 
  filter(trial %in% non_meddra$trial) %>% 
  distinct(trial, atc_code) %>% 
  group_by(trial) %>% 
  count()
intersect(non_meddra$trial, conmed$trial)
 

n_distinct(medhist$trial) # expect 22 now 23 (25/08/22)
n_distinct(meddra$trial) # 14 
gsk$medhist <- meddra # Meddra only has 13 trials, OK -> now 14 trials (25/08/22)



## Which tables are present ----

## Summarise number of drug classes per trial
conmed <- readRDS("GSK_processed_data/GSK_conmed.Rds") # NEED TO FINISH 04_CONMED_GSK
rand   <- readRDS("GSK_processed_data/GSK_not_conmed.Rds")$rand

conmed <- conmed %>% 
  semi_join(rand)

conmed_xmn <- conmed %>% 
  group_by(company, trial) %>% 
  summarise(unique_terms = sum(!duplicated(paste0(atc_code))),
            unique_patient = sum(!duplicated(id)),
            entries = length(atc_code),
            three_lvl_class = sum(!duplicated(str_sub(atc_code, 1, 3)))) %>% 
  ungroup() 

write_csv(conmed_xmn, "Outputs/GSK_conmed_per_trial.csv", na = "")

map_int(gsk, ~ sum(.x$trial %>% unique() %>% length()))
# demo    labs medhist      bp     bmi    rand 
# 31      25      14     28      29      31     (25/08/22)

# medhist only has 14 has select only meddra trials

# Examine all,
# rand and demo shoudl be the largest,
map_int(gsk, ~ distinct(.x, company, trial, id) %>%  nrow())
#demo    labs medhist      bp     bmi    rand 
#48950   45258    8919   48197   48562   51917 

# Have restricted to randomised patients for all of the tables, do so now
gsk <- map(gsk, ~ .x %>% semi_join(gsk$rand %>%  select(trial, id)))
map_int(gsk, ~ distinct(.x, company, trial, id) %>%  nrow())
#demo    labs medhist      bp     bmi    rand 
#48950   45258    8917   48197   48562   51917 

check_all_present <- map(gsk, ~ .x %>%  ungroup() %>% distinct(trial))
check_all_present <- bind_rows(check_all_present, .id = "table_name")
check_all_present <- check_all_present %>% 
  mutate(trial = if_else(trial %in% gsk$rand$trial[gsk$rand$company == "ucb"],
                         paste0("ucb", trial), trial))
check_all_present <- check_all_present %>% 
  group_by(table_name) %>%
  # mutate(grp_id = row_number()) %>%
  mutate(n = "") %>% 
  spread(key = table_name, value = n, fill = "X")

conmed_present <- read_csv("Supporting/Prepare_trial_data/GSK/Outputs/GSK_conmed_per_trial.csv")
comment_present <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/reviewED_check_all_present_comments.csv")
check_all_present <- check_all_present %>% 
  mutate(conmed = if_else(trial %in% conmed_present$trial, "", "X"))

check_all_present <- check_all_present %>% 
  left_join(comment_present) %>% 
  mutate(Comment = if_else(is.na(Comment), "", Comment)) %>% 
  select(trial, rand, demo, medhist, conmed, bmi, bp, labs, Comment)%>% 
  select(-rand, -demo)

write_csv(check_all_present, "Outputs/GSK_overview_tables.csv")

## Examine how many participants have data for each table and trial ----
## Participant numbers make sense
check_number_prtcpnts <- map(gsk, ~ .x %>%  ungroup() %>% group_by(trial) %>% summarise(n = sum(!duplicated(id))))
check_number_prtcpnts <- bind_rows(check_number_prtcpnts, .id = "table_name")
check_number_prtcpnts <- check_number_prtcpnts %>% 
  group_by(table_name) %>%
  spread(key = table_name, value = n, fill = "X") 
write_excel_csv(check_number_prtcpnts, "Scratch_data/review_check_num_participants.csv")





## Save as single object for onward analysis ----

## Add trial ids
trial_nctid <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/GSK_trials_in_respository_sas_with_medicine_condition.csv") %>% 
  distinct(nct_id, trial)

# Check differecne in trials 
setdiff(gsk$demo$trial, trial_nctid$trial) 

## check if any not in selected, yes, two trials, known to be rejected
tbls_ids <- map(gsk, ~ .x %>% distinct(trial))
notin <- map(tbls_ids, ~ .x %>%  anti_join(trial_nctid)) %>% bind_rows(.id = "tbl_name")

gsk <- map(gsk, ~ .x %>% inner_join(trial_nctid))
map_int(gsk, ~ sum(.x$trial %>% unique() %>% length()))
# demo    labs medhist      bp     bmi    rand  (25/08/22)
# 31      25      14      28      29      31 

# SAVE
saveRDS(gsk, "GSK_processed_data/GSK_not_conmed.Rds") 
