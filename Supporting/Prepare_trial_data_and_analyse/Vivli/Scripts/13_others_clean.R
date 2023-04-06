# 13_others_clean

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

cmpnies <- readRDS("Processed_data/all_sponsors_transposed.Rds")
cmpnies$conmed <- NULL
lilly <- readRDS("Processed_data/lilly.Rds")
bi_lkp <- read_csv("Created_metadata/bi_trials_lookup_treatment_group_to_label.csv", col_types = cols(.default = "c"))



## BMI -------------------------------------------------------------------------

bmi <- cmpnies$bmi

lilly_bmi <- lilly$bmi %>% 
  rename(id = subjid)  %>% 
  mutate(company = "lilly") %>% 
  mutate(value = as.character(value))


bmi <- bind_rows(bmi, lilly_bmi)

names_lkp <- c("bmi" = "bmi", "ht" = "ht", "htstd" = "ht", "wt" = "wt", "wtstd" = "wt",
               "height" = "ht", "weight" = "wt",
               "heightcm" = "ht", "weightkg" = "wt")

n_distinct(bmi$trial)

setdiff(bmi$param, names_lkp %>% names())
bmi$param <- names_lkp[bmi$param]
bmi$value <- as.double(bmi$value)

bmi_no <- bmi %>% distinct(company, trial)

bmi <- bmi %>% 
  filter(!is.na(value))%>% 
  group_by(company, trial, id, param) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

bmi_no <- bmi_no %>% anti_join(bmi) 

bmi_xmn2 <- bmi %>% group_by(company, trial, param) %>%
  summarise(n = length(value),
            msng = mean(is.na(value)),
            ppt = sum(!duplicated(id))) %>%
  ungroup()

bmi <- bmi %>% 
  spread(key = param, value) %>% 
  mutate(bmi = if_else(is.na(bmi), wt/(ht/100)^2, bmi)) %>% 
  gather("param", "value", -company, -trial, -id)

num_tr <- unique(bmi$trial)

bmi_xmn <- bmi %>% 
  group_by(trial, param) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  spread(param, value)

bmi_n_trials <- unique(bmi$trial)

write_csv(bmi_xmn, "Outputs/bmi_htwt_mean_per_trial.csv")

## BI502_392 clearly wrong units, need to go back and check units for this one
## Obvious from bmi, height and weight
cmpnies$bmi <- bmi
rm(bmi)




## rand ------------------------------------------------------------------------
rand <- cmpnies$rand

lilly_rand <- lilly$rand %>% 
  select(trial, id = subjid, arm = trt, randdt) %>% 
  mutate(company = "lilly", rnddy = NA_character_)

rand <- bind_rows(rand, lilly_rand)
rm(lilly_rand)

## Not randomised
bi_lkp %>%
  mutate(trial = as.character(trial)) %>% 
  rename(arm = tpatt) %>% 
  anti_join(rand)

non_rand <- unique(bi_lkp$trial)

## Randomised
bi_lkp <- bi_lkp %>%
  rename(arm = tpatt) %>% 
  semi_join(rand)

random <- unique(bi_lkp$trial)

rand <- rand %>% 
  mutate(trial = as.character(trial)) %>% 
  left_join(bi_lkp %>% 
              mutate(trial = as.character(trial)) ) %>% 
  mutate(arm = if_else(!is.na(tpattlbl), tpattlbl, arm))




## Examine different date formats, there are two only 5-digit integer or else y-m-d (eg 2001-01-01)
## For the digit one I am going to treat it as a number using 1960-01-01 as the origin as this is the SAS origin date

# All of the randomisation dates are in date formats, n = 36, plus 15 trials where randomisationj date is day. 

rand_xmn <- rand %>% 
  group_by(company, trial) %>% 
  summarise(arm = unique(arm) %>% paste(collapse = ", "),
            randdt = sum(!(is.na(randdt) & is.na(rnddy)))) # 0 is where day is given instead of date

write_csv(rand_xmn, "Outputs/rand_smry_per_trial.csv")

rand_n_trials <- unique(rand$trial)

cmpnies$rand <- rand
rm(rand)




## Demo ------------------------------------------------------------------------

demo <- cmpnies$demo

lilly_demo <- lilly$demo %>% 
  select(trial, id = subjid, age, sex, race, smoke) %>% 
  mutate(company = "lilly")

demo <- bind_rows(demo, lilly_demo)

## Use height to determine sex for 31 trials where thimedhs is not recorded
demo2 <- demo %>% 
  filter(sex %in% 1:2) %>% 
  group_by(trial) %>% 
  mutate(n_sexes = sum(!duplicated(sex))) %>% 
  ungroup() 

demo2 %>% filter(n_sexes ==1) %>% distinct(trial, n_sexes, sex)

demo2 <- demo2 %>%
  filter(n_sexes >=2) %>% 
  select(-n_sexes)

ht <- cmpnies$bmi %>%
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
  semi_join(cmpnies$rand)

## Correct age bands and >/< to age
age_band <- read_csv("Created_metadata/reviewED_age_bands_to_age_numeric.csv")


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

# pdf("Outputs/plot_age_distribution.pdf")
# for(i in unique(demo$trial)){
# print(i)
# plot_age <- ggplot(demo3 %>%  filter(trial == i),
# aes(x = x, y = y)) +
# geom_smooth(se = FALSE)  +
# ggtitle(i)
# print(plot_age)
# }
# dev.off()


## Summarise age and sex results as continuous variable
demo_xmn_cont <- demo2 %>% 
  filter(!is.na(sex), !is.na(age)) %>% 
  group_by(company, trial, sex) %>% 
  summarise_at(vars(age), .funs = c(n = "length", "mean", "sd", "min", "max")) %>% 
  ungroup() 
write_csv(demo_xmn_cont, "Outputs/demo_smry_per_trial_continuous.csv")

## Summarise age and sex results
demo_xmn <- demo %>% 
  filter(!is.na(sex), !is.na(age)) %>% 
  mutate(age_cut = Hmisc::cut2(age %>%  as.double(), cuts = c(30, seq(40, 90, 10)))) %>% 
  group_by(company, trial, sex, age_cut) %>% 
  summarise(n = sum(!duplicated(id))) %>% 
  ungroup() %>% 
  spread(age_cut, n, fill = 0L)
write_csv(demo_xmn, "Outputs/demo_smry_per_trial.csv")



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

write_csv(race_xmn, "Outputs/race_smry_per_trial.csv")

demo_n_trials <- unique(demo$trial)

cmpnies$demo <- demo
rm(demo)




## Labs ------------------------------------------------------------------------
labs <- cmpnies$labs
labs_lbl <- labs %>% 
  distinct(name, label, new_lab)
# write_csv(labs_lbl, "Scratch_data/consolidate_lab_names.csv")
# write_csv(lilly_labs %>% distinct(lbtest), "clipboard")

labs_lbls_reviewed <- read_csv("Created_metadata/reviewED_consolidate_lab_names.csv") %>% 
  distinct(name, new_lab) %>% 
  filter(!is.na(new_lab))

## Relable other and lilly lab results
## NEed to separate urine and serum for lilly labs
labs <- labs %>% 
  select(-new_lab) %>% 
  inner_join(labs_lbls_reviewed) %>% 
  rename(original_name = name, name = new_lab)

lilly_labs <- lilly$labs %>% 
  select(-visit, -sampcdt) %>% 
  rename(name = lbtest) %>% 
  inner_join(labs_lbls_reviewed) %>% 
  rename(original_name = name, name = new_lab)

# Examine where lab result does not succesfully change to a double format
# For now ignore these, may need to revisit if missing values considerable
# Roche set result to <4 if ALT, AST or SGPT was undetectable, which was only for 10 patients, drop.
labs_num <- labs %>%
  transmute_at(vars(result, ll, ul), as.double) %>% 
  transmute(failed = is.na(result))

labs_failed <- labs %>% 
  filter(labs_num$failed) %>% 
  distinct(trial, id, original_name, label, result, result_non_standard, result_character) %>% 
  filter(!(is.na(result) & is.na(result_non_standard) & is.na(result_character))) %>% 
  filter(!result %in% c("NVR", "NOS", "NO VALID RESULT", "", "<4"))

labs_failed %>% 
  filter(!is.na(result_character)) %>% 
  distinct(original_name, result_character)

labs_failed %>% 
  filter(!is.na(result_non_standard)) %>% 
  distinct(original_name, result_non_standard)

labs <- labs %>%
  mutate_at(vars(result, ll, ul), as.double) %>% 
  filter(!is.na(result))
rm(labs_failed, labs_num)
  
## Note lilly lab data already restricted to pre/on-randomisation day samples so can drop timing variables
lilly_labs <- lilly_labs %>% 
  rename(id = subjid, result = lbrn, unit = lbru, ll = lbnrlo, ul = lbnrhi) %>% 
  mutate(label = name,
         fast = NA,
         company = "lilly")



labs <- labs %>% 
  select(-result_non_standard, -result_character)

labs <- bind_rows(labs, lilly_labs)

## Determine which were fasting and convert FPG and GLU gto GLU with a fasting variable alongside
# note in novartis trials 
# c("NVT_SA_ENA713D1301", "NVT_SA_ZOL446H2301", "NVT_SA_ZOL446H2310", 
# "NVT_SA_ZOL446M2309", "NVT_SA_ZOL446O2306") 77 = unknown and 1 = yes
labs <- labs %>% 
  mutate(fast = if_else(name == "FPG" | fast %in% c("Y", "1", "YES"), "yes", "unknown"),
         name = if_else(name == "FPG", "GLU", name))


## Aggreagate lab results, slight danger if different assay used with the same trial, id and unit
labs <- labs %>% 
  group_by(company, trial, id, name, original_name, fast, label, unit) %>% 
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
write_csv(labs_count, "Outputs/lab_tests_per_trial.csv")

labs_n_trials <- unique(labs$trial)
cmpnies$labs <- labs
rm(labs, lilly_labs, labs_lbl, labs_lbls_reviewed, labs_count)




## BP --------------------------------------------------------------------------
## Note no BP for UCB trials
bp <- cmpnies$bp

lilly_bp <- lilly$bp %>% 
  rename(id = subjid) %>% 
  mutate(company = "lilly",
         value = as.character(value))

bp <- bind_rows(bp,
                lilly_bp)

bp <- bp %>% 
  mutate(param = if_else(param %in% c("bpdia", "DBP", "dia", "diabp",
                                      "dia1", "dia2", "dia3",
                                      "diastolic blood pressure"),
                         "dbp",
                         "sbp"),
         value = as.integer(value))

bp <- bp %>% 
 filter(!is.na(value)) %>% 
  group_by(company, trial, id, param) %>% 
  summarise(value = mean(value)) %>% 
  ungroup()

bp_xmn <- bp %>% 
  group_by(company, trial, param) %>% 
  summarise(value = mean(value)) %>% 
  spread(param, value)
write_csv(bp_xmn, "Outputs/blood_pressure_mean_per_trial.csv")

bp_n_trials <- unique(bp$trial)

cmpnies$bp <- bp




## Medhist - Meddra and Non meddra Medical history data ------------------------
medhist <- cmpnies$medhist

## Review where does not state whether or not it is MedDRA
medhist %>% filter(meddra %>% is.na()) %>% distinct(trial)

medhist_lilly <- lilly$medhist

medhist_lilly <- medhist_lilly %>% 
  mutate(company = "lilly",
         id = subjid,
         meddra = "meddra") %>% 
  select(company, trial, id, term = term_no_lltcd, meddra, lltcd) %>% 
  distinct()

## Many lily ones are lltcodes rather than prefertd terms use lookup table
llt_pt <- read.csv("V:/2022_05_12 Scripts for Analysis/llt_code_to_pt_name_lkp.csv")
setdiff(medhist_lilly$lltcd, llt_pt$llt_code)
medhist_lilly <- medhist_lilly %>% 
  left_join(llt_pt %>%  rename(lltcd = llt_code) %>% mutate(lltcd = as.character(lltcd),
                                                             pt_name = as.character(pt_name))) %>% 
  mutate(term = if_else(is.na(pt_name), term, pt_name)) %>% 
  select(-pt_name)
 
## All lilly are meddra
meddra <- bind_rows(medhist %>% filter(meddra == "meddra"),
                    medhist_lilly)

## Count ones with meddra
meddra_xmn <- meddra %>% 
  distinct(trial, term, lltcd, id) %>% 
  group_by(trial) %>% 
  summarise(unique_terms = sum(!duplicated(paste0(term, lltcd))),
            unique_patient = sum(!duplicated(id)),
            entries = length(term)) %>% 
  ungroup() %>% 
  mutate(warning = if_else(unique_terms * unique_patient == entries,
                           "Looks like might have a yes/no structure",
                           ""))

meddra_xmn2 <- meddra %>% 
  distinct(trial, term, lltcd, id) %>% 
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

write_csv(meddra_xmn3, "Outputs/medhist_per_trial.csv")


medhist %>% 
  filter(meddra == "meddra", trial %in% c("BI1160_46")) %>%  distinct(term) %>%  pull() %>% paste(collapse = ", ")


WrapNonMedhistinFunction <- function() {
non_meddra <- cmpnies$medhist %>% filter(meddra != "meddra")
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
conmed_xmn <- conmed %>% 
  filter(trial %in% non_meddra$trial) %>% 
  distinct(trial, atc_code) %>% 
  group_by(trial) %>% 
  count()
intersect(non_meddra$trial, conmed$trial)
}

meddra_n_trials <- unique(meddra$trial)

cmpnies$medhist <- meddra




## Which tables are present ----------------------------------------------------
## Summarise number of drug classes per trial
conmed <- readRDS("Processed_data/all_sponsors_conmed.Rds")

rand <- readRDS("Processed_data/all_sponsors_not_conmed.Rds")$rand

conmed <- conmed %>% 
  semi_join(rand)

conmed_xmn <- conmed %>% 
  group_by(company, trial) %>% 
  summarise(unique_terms = sum(!duplicated(paste0(atc_code))),
            unique_patient = sum(!duplicated(id)),
            entries = length(atc_code),
            three_lvl_class = sum(!duplicated(str_sub(atc_code, 1, 3)))) %>% 
  ungroup() 

write_csv(conmed_xmn, "Outputs/conmed_per_trial.csv", na = "")
## All of the tables have been updated to include lilly and all include UCB
map_lgl(cmpnies, ~ any(.x$company %in% "lilly"))
map_int(cmpnies, ~ sum(.x$trial %>% unique() %>% length()))
#demo    labs medhist      bp     bmi    rand 
#50      43      46      50      49      50    (28/07/22)

#demo    labs medhist      bp     bmi    rand 
#48      41      44      47      46      48    (17/10/22) - after removing 3 roche trials

# Examine all,
# rand and demo shoudl be the largest,
map_int(cmpnies, ~ distinct(.x, company, trial, id) %>%  nrow())
# demo    labs medhist      bp     bmi    rand  
# 43639   41815   40780   46811   52682   43639  (28/07/22) 

# Have restricted to randomised patients for all of the tables, do so now
cmpnies <- map(cmpnies, ~ .x %>% semi_join(cmpnies$rand %>%  select(trial, id)))
map_int(cmpnies, ~ distinct(.x, company, trial, id) %>%  nrow())
# demo    labs medhist      bp     bmi    rand 
# 43639   37860   33595   42744   42028   43639 (28/07/22)

#demo    labs medhist      bp     bmi    rand 
#43637   37858   33593   42742   42026   43637  (17/10/22) - after removing 3 roche trials

check_all_present <- map(cmpnies, ~ .x %>%  ungroup() %>% distinct(trial))
check_all_present <- bind_rows(check_all_present, .id = "table_name")
check_all_present <- check_all_present %>% 
  mutate(trial = if_else(trial %in% cmpnies$rand$trial[cmpnies$rand$company == "ucb"],
         paste0("ucb", trial), trial))
check_all_present <- check_all_present %>% 
  group_by(table_name) %>%
  # mutate(grp_id = row_number()) %>%
  mutate(n = "") %>% 
  spread(key = table_name, value = n, fill = "X")

conmed_present <- read_csv("Outputs/conmed_per_trial.csv")
comment_present <- read_csv("Created_metadata/reviewED_check_all_present_comments.csv")
check_all_present <- check_all_present %>% 
  mutate(conmed = if_else(trial %in% conmed_present$trial, "", "X"))

check_all_present <- check_all_present %>% 
  left_join(comment_present) %>% 
  mutate(Comment = if_else(is.na(Comment), "", Comment)) %>% 
  select(trial, rand, demo, medhist, conmed, bmi, bp, labs, Comment)%>% 
  select(-rand, -demo)

write_csv(check_all_present, "Outputs/overview_tables.csv")

## Examine how many participants have data for each table and trial ----
## Participant numbers make sense
check_number_prtcpnts <- map(cmpnies, ~ .x %>%  ungroup() %>% group_by(trial) %>% summarise(n = sum(!duplicated(id))))
check_number_prtcpnts <- bind_rows(check_number_prtcpnts, .id = "table_name")
check_number_prtcpnts <- check_number_prtcpnts %>% 
  group_by(table_name) %>%
  spread(key = table_name, value = n, fill = "X") 
write_excel_csv(check_number_prtcpnts, "Scratch_data/review_check_num_participants.csv")



## Save as single object for onward analysis ----
# first check the changes
# cmnies_old <- readRDS("Processed_data/all_sponsors_not_conmed.Rds")
# cmpnies_diff <- map2(cmpnies, cmnies_old, ~ anti_join(.x, .y, by = c("trial")))
# map(cmpnies_diff, ~ .x %>% count(trial))

## Add trial ids
trial_nctid <- read.csv("Outputs/vivli_extracted_trial_nct_ID.csv", row.names = 1)

## check if any not in selected, yes, two trials, known to be rejected
tbls_ids <- map(cmpnies, ~ .x %>% distinct(trial))
notin <- map(tbls_ids, ~ .x %>%  anti_join(trial_nctid)) %>% bind_rows(.id = "tbl_name")

cmpnies <- map(cmpnies, ~ .x %>% inner_join(trial_nctid))

saveRDS(cmpnies, "Processed_data/all_sponsors_not_conmed.Rds")




# -----------------
# Missing trials in each table in companies

demo_miss <- setdiff(trial_nctid$trial, cmpnies$demo$trial) 
rand_miss <-setdiff(trial_nctid$trial, cmpnies$rand$trial) 
labs_miss <- setdiff(trial_nctid$trial, cmpnies$labs$trial) 
meddra_miss <- setdiff(trial_nctid$trial, cmpnies$medhist$trial) 
bp_miss <- setdiff(trial_nctid$trial, cmpnies$bp$trial) 
bmi_miss <- setdiff(trial_nctid$trial, cmpnies$bmi$trial) 

n_distinct(cmpnies$demo$trial) # 50
n_distinct(demo_n_trials)      # 50

n_distinct(cmpnies$rand$trial) # 50
n_distinct(rand_n_trials)      # 50

n_distinct(cmpnies$labs$trial) # 43 
n_distinct(labs_n_trials)      # 43

n_distinct(cmpnies$medhist$trial) # 46
n_distinct(meddra_n_trials)       # 46 

n_distinct(cmpnies$bp$trial) # 49
n_distinct(bp_n_trials)      # 50

n_distinct(cmpnies$bmi$trial) # 48
n_distinct(bmi_n_trials)      # 49


# Fianl count of number of trials p/table 
cmp_chk <- readRDS("Processed_data/all_sponsors_not_conmed.Rds")

map_int(cmp_chk, ~ sum(.x$trial %>% unique() %>% length()))
