# 08_apply_conmed_definitions

## Updated to match"Latest_yoda_code_definitions.r" modified on 20180925
## this still includes the skin variable, but drop this in the counting stage

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

## Read in drug lookups
rxnorm_bnf <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/bnf_rxnorm_atc_codes.csv")
rxnorm_bnf <- rxnorm_bnf %>%
  filter(!is.na(str))

## Exclude non randomised participants and merge to trial characteristics
conmed  <- readRDS("GSK_processed_data/GSK_conmed.Rds") %>% select(-body_system, -indication)
gsk <- readRDS("GSK_processed_data/GSK_not_conmed.Rds")
rand <- gsk$rand

# Ensure randomised patients only - no pateints should be dropped
conmed <- conmed %>% 
  semi_join(rand)


# Identify indication and medicine, all conditions have a code,
trial_indic_drug <- read.csv("Supporting/Prepare_trial_data/GSK/Created_metadata/GSK_trials_in_respository_sas_with_medicine_condition.csv", row.names = 1)  
msng_tr          <- setdiff(trial_indic_drug$trial, gsk$demo$trial) # 1 trial is missing, we have it but wrong tables provided 
trial_indic_drug <- trial_indic_drug %>% filter(!trial %in% msng_tr) # remove missing trial

trial_indic_drug %>%  select(trial, medicine, condition)

rand1 <- rand %>% 
  distinct(company, trial) %>% 
  anti_join(trial_indic_drug)

conmed <- conmed %>% 
  inner_join(trial_indic_drug)

# Almost all 5 digit codes
a <- table(conmed$atc_code %>%  str_length()) 
rev(a)
#7     5     4     3     1 
#15020 71428  4321   918    63 
rev(100*a %>%  prop.table()) %>%  cumsum() %>% round(1)
#7     5     4     3     1 
#16.4  94.2  98.9  99.9 100.0 
sum(is.na(conmed$atc_code)) # 0


##### Functions ----
PrintDrugChoices <- function(incld, excld = FALSE, mydf = conmed){
  mydf2 <- mydf %>%
    mutate(incld = incld, excld = excld) %>% 
    group_by(company, trial, id) %>% 
    mutate(excld = any(excld)) %>% 
    ungroup() %>% 
    filter(incld & !excld) %>% 
    group_by(term, route_classify, atc_code) %>% 
    count(sort = TRUE)
  mydf2
}

ApplyMedCriteria <- function(incld, excld = FALSE, print = TRUE, mydf = conmed){
  PrintDrugChoices(incld, excld, mydf) %>%  head(19) %>%  print()
  mydf2 <- mydf %>%
    mutate(incld = incld, excld = excld) %>% 
    group_by(company, trial, id) %>% 
    summarise(present = any(incld) & !any(excld)) 
  print( paste0(round(100* mean(mydf2$present), 1), "%"))
  mydf2
}


############ Define comorbidities based on concomitant medicaitons
## First remove aspirin from all analyses as is used widely as prophylaxis
## Similarly remove amitriptyline as most frequently used for PAIN not for depression, anxiety etc
## Updated this code to be more specific, particularly for aspirin

DropDrugs <- function(mystring = "") {
  y <- rxnorm_bnf %>%
    mutate(term_lower = str_to_lower(str)) %>% 
    select(term_lower, code)
  
  x <- rxnorm_bnf %>%
    filter(str_detect(str, mystring)) %>% 
    mutate(term_lower = str_to_lower(str)) %>% 
    distinct(term_lower, code)
  
  x <- y %>%
    filter(code %in% x$code | term_lower %in% x$term_lower) %>%
    select(term_lower, atc_code = code)
  x
}
# Not combination therapies eg (statin and aspirin) as only want to exclude prophylactic use as 
# a single agent

aspirin_codes <- c("A01AD05", "B01AC06", "N02BA01")
aspirin <- rxnorm_bnf %>% 
  filter(code %in% aspirin_codes) %>% 
  mutate(term_lower = str_to_lower(str)) %>% 
  distinct(term_lower, code)

amitriptyline <- DropDrugs("amitriptyl")
pre_gab_val <- DropDrugs("gabapentin|pregabalin|valproate|valproic acid")

conmed <- conmed %>%
  mutate(term_lower = str_to_lower(term) %>%
           str_replace("/[0-9]{8,8}/", "") %>%
           str_trim()) %>%
  filter(!atc_code %in% c("A01AD05", "B01AC06", "N02BA01"),
         !term_lower %in% aspirin$term_lower)
conmed <- conmed %>%
  anti_join(amitriptyline %>%  select(term_lower)) %>%
  anti_join(amitriptyline %>%  select(atc_code))
conmed <- conmed %>%
  anti_join(pre_gab_val %>%  select(term_lower))  %>% 
  anti_join(pre_gab_val %>%  select(atc_code))


## Antacids
# Note only antacid codes are A02A, A02B, A02X and the last is an empty category
antacids_included <- conmed$atc_code  %>% str_sub(1, 4) %in% c("A02A", "A02B") |
  conmed$atc_code %>% str_sub(1, 3) == "A02"
antacids_excluded <- (conmed$atc_code  %>% str_sub(1, 4) %in% "M01A") |
  (conmed$atc_code  %>% str_sub(1, 3) %in% "B01")
conmed_ant <- ApplyMedCriteria(antacids_included, antacids_excluded)


## Diabetes
diabetes_included <- conmed$atc_code  %>% str_sub(1, 3) %in% c("A10")
conmed_diab <- ApplyMedCriteria(diabetes_included)


## thromboembolic
tbe_included <- str_sub(conmed$atc_code, 1, 5) %in% c("B01AA", "B01AE", "B01AF")
conmed_tbe <- ApplyMedCriteria(tbe_included)


## Cardiovascular
cv_codes <- paste0("C0", c(1,2,4,7,8,9))
cv_included <- str_sub(conmed$atc_code, 1, 3) %in% cv_codes
cv_excluded <- str_sub(conmed$atc_code, 1, 4) == "N02C" &
  str_sub(conmed$atc_code, 1, 5) == "C07AA"
# Where has only 4 digits cannot exclude beta-blockers, where only has 3 cannot exclude antimigraine
conmed_cv <- ApplyMedCriteria(cv_included, cv_excluded)


## Urinary incontinence
ur_included <- str_sub(conmed$atc_code, 1, 5) == "G04BD"
conmed_ur <- ApplyMedCriteria(ur_included)


## ED
ed_included <- str_sub(conmed$atc_code, 1, 5) == "G04BE"
conmed_ed <- ApplyMedCriteria(ed_included)


## Urinary incontinence or ED
## If only 4 character code is available define as urinary incontinence or ED
ur_ed_included <- str_sub(conmed$atc_code, 1, 4) == "G04B" & str_length(conmed$atc_code) == 4
conmed_ur_ed <- ApplyMedCriteria(ur_ed_included)


## Prostate
bph_included <- str_sub(conmed$atc_code, 1, 4) == "G04C"
conmed_bph <- ApplyMedCriteria(bph_included)


## Urinary incontinence or ED or BPH, these are tiny proportions, probably ignore
## If only 3 character code is available define as urinary incontinence or ED or BPH
ur_ed_bph_included <- str_sub(conmed$atc_code, 1, 3) == "G04" & str_length(conmed$atc_code) == 3
conmed_ur_ed_bph <- ApplyMedCriteria(ur_ed_bph_included)


## Glaucoma
# drop isosorbide even though classified as S01EX as is not a contemporary drug for glaucoma
glaucoma <- str_sub(conmed$atc_code, 1, 4) == "S01E" & (is.na(conmed$term) | conmed$term != "ISOSORBIDE")
conmed_gl <- ApplyMedCriteria(glaucoma)


## Arthritis and arthralgia
# Exclude FOLIC ACID from this as is very common and (despite class here)
# is not an M01AX code in WHO ATC
art <- ((str_sub(conmed$atc_code, 1, 4) %in% c("M01A", "M01B") |
           str_sub(conmed$atc_code, 1, 3) == "M02")) &
  (is.na(conmed$term) |
     conmed$term != "FOLIC ACID")
conmed_art <- ApplyMedCriteria(art)


## Osteoporosis
ost <- str_sub(conmed$atc_code, 1, 3) == "M05"
conmed_ost <- ApplyMedCriteria(ost)


## Gout
gou <- str_sub(conmed$atc_code, 1, 3) == "M04"
conmed_gou <- ApplyMedCriteria(gou)


## Inflammatory arthropathies etc
inf <- str_sub(conmed$atc_code, 1,5) %in% c("A07EA", "A07EC", "L04AB", "L04AA", 
                                            "L04AX", "M01CB", "M01CC")
conmed_inf <- ApplyMedCriteria(inf)

inf4 <- (!inf) & (str_length(conmed$atc_code  == 3) &
                  str_sub(conmed$atc_code, 1, 3) %in% c("A07", "L04", "D05"))
inf3 <- (!inf) & (str_length(conmed$atc_code == 4) & 
                  str_sub(conmed$atc_code, 1, 4) == "M01C")
conmed_inf4 <- ApplyMedCriteria(inf4|inf3)


## Migraine
mig <- str_sub(conmed$atc_code, 1, 4) == "N02C"
conmed_mig <- ApplyMedCriteria(mig)


## Pain
pai <- str_sub(conmed$atc_code, 1, 4) %in% c("N02A", "N02B") &
  (!str_sub(conmed$atc_code, 1, 5) == "N02BA")
# Already excluded aspirin above
conmed_pai <- ApplyMedCriteria(pai)

pai3 <- !pai & (str_length(conmed$atc_code ==3) & str_sub(conmed$atc_code, 1, 3) == "N02")
#already excluded aspirin
conmed_pai3 <- ApplyMedCriteria(pai3)


## schizophrenia and delusional disorders
sch_include <- str_sub(conmed$atc_code, 1, 4) == "N05A"
sch_exclude <- str_sub(conmed$atc_code, 1, 5) == "N05AB"
conmed_sch <- ApplyMedCriteria(sch_include, sch_exclude)


#  Anxiety and mood disorders
anx <- str_sub(conmed$atc_code, 1, 4) %in% c("N05B", "N05A", "N06A")
# already excluded amitriptyline
conmed_anx <- ApplyMedCriteria(anx)


##  Epilepsy
epi <- str_sub(conmed$atc_code, 1, 3) == "N03"
# Already excluded GABAPENTIN, PREGABALIN AND VALPROATE terms
# WIll also exclude 5-digit code when text is missing
pre_gab_val5 <- str_length(conmed$atc_code ==5) & is.na(conmed$term) & conmed$atc_code == "N03AX"
# Now also exclude benzodiazepine based code, as clearly overdiagnosing epilepsy (17% in one study of
# patients with arthroplasty. Is classified lorazepam as bzd for epillepsy (N03AE, even though this does not appear in 
# in ATC [only clonazepam is licenced here, presumably as only one licenced for this usage]))
bzd_based <-  (str_length(conmed$atc_code >=5) & str_sub(conmed$atc_code, 1, 5) == "N03AE") |
  str_to_lower(str_sub(conmed$term, -4)) == "epam"
epi_excld <- pre_gab_val5 | bzd_based
conmed_epi <- ApplyMedCriteria(epi, epi_excld)


## Parkinsons
pd <- str_sub(conmed$atc_code, 1, 3) == "N04"
conmed_pd <- ApplyMedCriteria(pd)


## Dementia
dem <- str_sub(conmed$atc_code, 1, 4) == "N06D"
conmed_dem <- ApplyMedCriteria(dem)


## Resp
resp <- str_sub(conmed$atc_code, 1, 3) == "R03"
conmed_resp <- ApplyMedCriteria(resp)


## Thyroid
thy <- str_sub(conmed$atc_code, 1, 3) == "H03"
conmed_thy <- ApplyMedCriteria(thy)


## Skin diseases
skn <- str_sub(conmed$atc_code, 1, 4) == "D02A" |
  str_sub(conmed$atc_code, 1, 3) %in% c("D04", "D06", "D07")
conmed_skn <- ApplyMedCriteria(skn)


## Combine_all
a <- list("conmed_ant", "conmed_anx", "conmed_art", "conmed_bph", "conmed_cv", 
          "conmed_dem", "conmed_diab", "conmed_ed", "conmed_epi", "conmed_gl", 
          "conmed_gou", "conmed_inf", "conmed_inf4", "conmed_mig", "conmed_ost", 
          "conmed_pai", "conmed_pai3", "conmed_pd", "conmed_resp", "conmed_sch", 
          "conmed_tbe", "conmed_thy", "conmed_ur", "conmed_ur_ed", "conmed_ur_ed_bph",
          "conmed_skn"
)

conmed_all <- map(a, get)
conmed %>% distinct(company, trial, id) %>% nrow()
# There are 67821 participants with one or more concomittant drug
map_int(conmed_all, nrow) %>% unique()
# all are 67821 size dataframes
names(conmed_all) <- str_replace(a, "conmed_", "")
conmed_all <- map2(conmed_all, names(conmed_all), ~ set_names(.x, 
                                                            c("company", "trial", "id", .y)))

conmed_all <- reduce(conmed_all, inner_join)

## Create more informative labels for conditions
b <- list("ant", "anx", "art", "bph", "cv", "dem", "diab", "ed", "epi", 
          "gl", "gou", "inf", "inf4", "mig", "ost", "pai", "pai3", "pd", 
          "resp", "sch", "tbe", "thy", "ur", "ur_ed", "ur_ed_bph", "skn")

b_lbl <- c('antacids', 'anxiety', 'arthritis', 'prostate', 'CV',
           'dementia', 'diabetes', 'erectile', 'epilepsy', 'glaucoma',
           'gout', 'inflammatory', 'inflammatory4', 'migraine', 'osteoporosis',
           'pain', 'pain3', 'parkinsons', 'asthma_COPD', 'schizophrenia', 
           'thromboembolic', 'thyroid', 'urological', 'urological_or_ed', 'urological_or_ed_or_bph',
           'skin')
smrs_all <- map(b, function(x) tapply(conmed_all[[x]], conmed_all$trial, mean))
names(smrs_all) <- b_lbl

# Apply labels to conmed
b_lkp <- b_lbl
names(b_lkp) <- b
other_names <- setdiff(names(conmed_all), names(b_lkp))
names(other_names) <- other_names
b_lkp <- c(other_names, b_lkp)
names(conmed_all) <- b_lkp[names(conmed_all)]

conmed_all <- trial_indic_drug %>% 
  select(nct_id, medicine, condition, trial) %>% 
  inner_join(conmed_all)


## Add demography data as the denominator
gsk <- readRDS("GSK_processed_data/GSK_not_conmed.Rds")
demo <- gsk$demo
rm(gsk)

conmed_all2 <- demo %>% 
  select(company, trial, id) %>% 
  inner_join(conmed_all %>% distinct(company, trial, nct_id, medicine, condition)) %>% 
  left_join(conmed_all)
setdiff(conmed_all$trial, conmed_all2$trial)
conmed_all <- conmed_all2
rm(conmed_all2)

conmed_all <- conmed_all %>% 
  mutate_at(vars(antacids:skin), function(x) if_else(is.na(x), FALSE, x))
  
## Collapse additional conditions
## clearly very uncommon to have urological 3-level codes so drop for simplicity
conmed_all_final <- conmed_all %>% 
  mutate(pain = pain | pain3,
         inflammatory = inflammatory | inflammatory4) %>% 
  select(-pain3, -inflammatory4, -urological_or_ed,
         -urological_or_ed_or_bph)

## Save processed data
saveRDS(conmed_all, "Scratch_data/GSK_conmed_defined_comorbidity.Rds")

## Examine numbers for irregular definitions ----
conmed_all <- readRDS("Scratch_data/GSK_conmed_defined_comorbidity.Rds")

## Simple examination across all trials
conmed_smry <- conmed_all %>% 
  select(antacids: skin) %>% 
  summarise_all(mean) %>% 
  t()
conmed_smry[,1] <- conmed_smry[,1] *100 %>% round(1)
write.csv(conmed_smry, "GSK_outputs/GSK_comorbidities_overall_before_removing_index.csv")

## set condition to null where it corresponds to the indication
## Note that this has a one to many relationship for coding, but the condition can have more than one 
condition_match <- c("Alzheimer's Disease", "ankylosing spondylitis", "Asthma", 
                     "Atrial Fibrillation, Stroke", "Benign Prostatic Hyperplasia", 
                     "Chronic Idiopathic Urticaria (CIU)", "Diabetes Mellitus", 
                     "Diabetes Mellitus, Type 2", 
                     "Diabetes Mellitus, Type 2; Hypertension",
                     "Diabetes Mellitus, Type 2; Renal Insufficiency", 
                     "Erectile Dysfunction, Benign Prostatic Hyperplasia",
                     "Hypertension, Pulmonary", 
                     "Osteoporosis", "Osteoporosis, Male", "Osteoporosis; Hip Fracture", 
                     "Parkinson Disease", "Psoriasis", "Pulmonary Disease, Chronic Obstructive", 
                     "rheumatoid arthritis", "Systemic Lupus Erythematosus", "Thromboembolism", 
                     "Thromboprophylaxis", "Type 2 Diabetes Mellitus", 
                     "Ulcerative Colitis; Crohn's Disease", 
                     "Venous Thromboembolism",
                     "Crohn's disease",
                     "Ulcerative colitis", 
                     "Psoriatic arthritis",
                     "Migraine",
                     "Osteoarthritis",
                     "Restless Legs Syndrome",
                     "Hypertension", 
                     "Diabetic Nephropathies",
                     "Arthroplasty, Replacement, Knee; Thromboembolism", 
                     "Thromboembolism; Arthroplasty, Replacement, Hip",
                     "Diabetes Mellitus, Type 2;   Hyperglycemia",
                     "Rhinitis, Allergic, Perennial",
                     "Diabetic Nephropathies (Type 2 diabetes)",
                     "Diabetic Nephropathies (Type 2 diabetes); Hypertension",
                     "Pulmonary fibrosis") # done
conmed_all <- conmed_all %>% 
  mutate(condition = case_when(
    condition == "Rheumatoid arthritis" ~ "rheumatoid arthritis",
    condition == "Ankylosing spondylitis" ~ "ankylosing spondylitis",
    condition %in% c("Type 2 diabetes", "Diabetes Mellitus, Type 2;   Hyperglycemia") ~ "Diabetes Mellitus, Type 2",
    condition == "Alzheimer's" ~ "Alzheimer's Disease",
    TRUE ~ condition
  ))

names(condition_match) <- condition_match
condition_match <- as.list(condition_match)
condition_match$`Alzheimer's Disease` <- c("dementia", "schizophrenia")
condition_match$Asthma <- "asthma_COPD"
condition_match$`Atrial Fibrillation, Stroke` <- c("CV", "thromboembolic")
condition_match$`Benign Prostatic Hyperplasia` <- c("prostate", "urological")
condition_match$`Chronic Idiopathic Urticaria (CIU)`<- c("inflammatory", "skin")
condition_match$`Diabetes Mellitus` <- "diabetes"
condition_match$`Diabetes Mellitus, Type 2` <- "diabetes"
condition_match$`Diabetes Mellitus, Type 2; Hypertension` <- c("diabetes", "hypertension", "CV")
condition_match$`Diabetes Mellitus, Type 2; Renal Insufficiency` <- c("diabetes", "renal")
condition_match$`Erectile Dysfunction, Benign Prostatic Hyperplasia` <- c("erectile", "prostate", "urological")
condition_match$`Hypertension, Pulmonary` <- "thromboembolic"
condition_match[c("Osteoporosis", "Osteoporosis, Male", "Osteoporosis; Hip Fracture")]  <- "osteoporosis"
condition_match$`Parkinson Disease` <- "parkinsons"
condition_match$Psoriasis <- c("inflammatory", "skin")
condition_match$`Pulmonary Disease, Chronic Obstructive` <- "asthma_COPD"
condition_match$`Pulmonary Disease, Chronic Obstructive, Ph` <- "asthma_COPD"
condition_match[c("rheumatoid arthritis", "ankylosing spondylitis",
                  "Psoriatic arthritis")] <- map(
                    condition_match[c("rheumatoid arthritis", "ankylosing spondylitis",
                                      "Psoriatic arthritis")],
                    function(x) c("inflammatory", "arthritis"))
condition_match$`Systemic Lupus Erythematosus`<- "inflammatory"
condition_match$Thromboembolism <- "thromboembolic"
condition_match$Thromboprophylaxis <- "thromboembolic"
condition_match$`Type 2 Diabetes Mellitus` <- "diabetes"
condition_match[c("Ulcerative Colitis; Crohn's Disease",
                  "Crohn's disease",
                  "Ulcerative colitis")] <- "inflammatory"
condition_match$`Venous Thromboembolism` <- "thrombembolic"
condition_match$Migraine <- c("pain", "migraine")
condition_match$Osteoarthritis <- c("pain", "arthritis")
condition_match$`Restless Legs Syndrome` <- c("parkinsons")
condition_match$Hypertension <- c("hypertension", "CV")
condition_match$Atherosclerosis <- c("hypertension", "CV")
condition_match$`Heart failure, Congestive and Microalbuminuria` <- c("renal", "CV")
condition_match$`Diabetic Nephropathies` <- c("diabetes", "renal")
condition_match$`Arthroplasty, Replacement, Knee; Thromboembolism` <- c("pain")
condition_match$`Thromboembolism; Arthroplasty, Replacement, Hip` <- c("pain")
condition_match$`Diabetes Mellitus, Type 2;   Hyperglycemia` <- c("diabetes")
condition_match$`Rhinitis, Allergic, Perennial` <- c("asthma_COPD")
condition_match$`Diabetic Nephropathies (Type 2 diabetes)` <- c("diabetes", "renal")
condition_match$`Diabetic Nephropathies (Type 2 diabetes); Hypertension` <- c("diabetes", "CV", "renal")
condition_match$`Pulmonary fibrosis` <- c("inflammatory")


## Check complete allocation
# All index conditiosn included
setdiff(conmed_all$condition, names(condition_match)) # should be empty except for rhinitis

# Output on (16/06/22) = GOOD!:
#[4] "Rhinitis, Allergic, Seasonal"                  
#[5] "Rhinitis, Vasomotor"                           
#[6] "Rhinitis, Allergic, Perennial and Seasonal"    
 

# condition not allocated any matching pairs
condition_match[condition_match == names(condition_match)]# should be empty 

# Mismatch between terms for concomitant conditions, should have renal, thrombembolic and hypertension only
setdiff(unlist(condition_match),
  c("antacids", "anxiety", "arthritis", "prostate", "CV", "dementia", 
    "diabetes", "erectile", "epilepsy", "glaucoma", "gout", "inflammatory", 
    "inflammatory4", "migraine", "osteoporosis", "pain", "pain3", 
    "parkinsons", "asthma_COPD", "schizophrenia", "thromboembolic", 
    "thyroid", "urological", "urological_or_ed", "urological_or_ed_or_bph", 
    "skin"))

# Returns: "hypertension"  "renal"  "thrombembolic" on 16/06/22
# These are index conditions but not comorbities




## Need to loop as changing same object with each iteration
conmed_all_final2 <- conmed_all_final

for (condition_slct in names(condition_match)) {
  print(paste0(condition_slct, " - ",condition_match[[condition_slct]]))
  conmed_all_final2[conmed_all_final$condition %in% condition_slct,
                    names(conmed_all_final2) %in% condition_match[[condition_slct]]
                    ] <- FALSE
}
conmed_all_final <- conmed_all_final2
rm(conmed_all_final2)
saveRDS(conmed_all_final, "GSK_processed_data/GSK_conmed_defined_comorbidity.Rds") 




## Review number of diagnoses with less information; ie inflammatory4, pain3, urological_or_ed
# and urological_or_ed_or_bph
conmed_all_final <- readRDS("GSK_processed_data/GSK_conmed_defined_comorbidity.Rds")
conmed_all <- readRDS("Scratch_data/GSK_conmed_defined_comorbidity.Rds")

not_in_sail_compare <- c("NCT01264939", "NCT01287117", "NCT01292473", "NCT00289198", "NCT00694382")
intersect(not_in_sail_compare, conmed_all_final$nct_id) # "NCT00289198" (25/08/22)

conmed_all_final <- conmed_all_final %>% 
  filter(!nct_id %in% not_in_sail_compare)
conmed_all <- conmed_all %>% 
  filter(!nct_id %in% not_in_sail_compare)

# Still perfect match
all(conmed_all_final$trial == conmed_all$trial & conmed_all_final$id == conmed_all$id)

conmed_all <- conmed_all %>%
  mutate(inflammatory = if_else(conmed_all_final$inflammatory == FALSE, FALSE, inflammatory),
         pain = if_else(conmed_all_final$pain == FALSE, FALSE, pain))

conmed_all <- conmed_all %>%
  mutate(pain3_only = pain & pain3,
         inflammatory4_only = inflammatory & inflammatory4)
sum(conmed_all$pain3_only)

conmed_all %>% 
  select(pain, pain3_only, inflammatory, inflammatory4_only, urological, erectile, urological_or_ed, urological_or_ed_or_bph) %>% 
  summarise_all(sum)

pain_smpl <- conmed_all %>% 
  filter(pain) %>% 
  select(pain, pain3_only) %>% 
  summarise_all(sum)

inf_smpl <- conmed_all %>% 
  filter(inflammatory) %>% 
  select(inflammatory, inflammatory4_only) %>% 
  summarise_all(sum)

all_smpl <- list(pain_smpl, inf_smpl)
all_smpl <- map(all_smpl, ~ as.data.frame(.x)[1, , drop = TRUE] %>%  unlist())
all_smpl <-  map(all_smpl, function(x) paste0(x, " ", round(100*x/x[1],1), "%"))


## For urological need to sum it
uro_smpl <- conmed_all %>% 
  filter(urological|urological_or_ed|urological_or_ed_or_bph) %>% 
  summarise(uro_narrow = sum(urological),
            uro_broad = sum(!urological & (urological_or_ed|urological_or_ed_or_bph)),
            uro_both = sum(urological|urological_or_ed|urological_or_ed_or_bph),
            uro_broad_prop = uro_broad/uro_both)

ed_smpl <- conmed_all %>% 
  filter(erectile|urological_or_ed|urological_or_ed_or_bph) %>% 
  summarise(ed_narrow = sum(erectile),
            ed_broad = sum(!erectile & (urological_or_ed|urological_or_ed_or_bph)),
            ed_both = sum(erectile|urological_or_ed|urological_or_ed_or_bph),
            ed_broad_prop = ed_broad/ed_both)

