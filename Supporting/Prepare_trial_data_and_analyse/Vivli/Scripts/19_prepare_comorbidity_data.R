# 19_prepare_comorbidity_data
# This script harmonises index conditions and comorbidities across trial and
# community settings. 
# 
# Then joins demo graphic data to be used as model covariates. 
# 
# This script produces 2 files: 
#   1) final data for modelling: with each trial with harmonised
#   2) index conditions and comorbidities

# This script: 
# - Harmonise index conditions across trial/community settings
# - Harmonise comorbidities across trial/community settigns
# - Limit comorbidities for each index condition to match those in community 
# - check one row per patient



# Packages 
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

# Data
path        <- "E:/C_analysis_code_meta/Extract_Data/Processed_data/"
cmnty_como  <- read.csv("Supporting/Prepare_trial_data/Vivli/Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv") # Note has additional conditions not present in Vivli
conmed_como <- readRDS(paste0(path, "conmed_defined_comorbidity.Rds"))
demo        <- readRDS(paste0(path, "all_sponsors_not_conmed.Rds"))$demo
con         <- readRDS(paste0(path, "model_covs_continuous.Rds"))$all_cov_mdl



# Prepare data -----------------------------------------------------------------

# Match vivli trial index conditions to community index conditions 
unique(conmed_como$condition) # 19
cmnty_ind <- unique(cmnty_como$condition) # 22

conmed_como <- conmed_como %>%
  mutate(condition = str_remove_all(condition, ";"),
         condition = str_remove_all(condition, ","),
         condition = str_replace_all(condition, " ", "_"))

conmed_como <- conmed_como %>%
  mutate(condition = case_when(condition == "rheumatoid_arthritis" ~ cmnty_ind[2],
                               condition == "Erectile_Dysfunction_Benign_Prostatic_Hyperplasia" ~ cmnty_ind[6],
                               condition == "Type_2_Diabetes_Mellitus" ~ cmnty_ind[8],
                               condition == "Diabetes_Mellitus_Type_2_Renal_Insufficiency" ~ cmnty_ind[8],
                               condition == "Diabetes_Mellitus_Type_2_nephropathy" ~ cmnty_ind[8],
                               condition == "Hypertension_Diabetic_Nephropathies" ~ cmnty_ind[8],
                               condition == "Diabetes_Mellitus_Type_2___Hyperglycemia" ~ cmnty_ind[8],
                               condition == "Diabetic_Nephropathies" ~ cmnty_ind[8],
                               condition == "Hypertension_Pulmonary" ~ cmnty_ind[11],
                               condition == "Diabetes_Mellitus" ~ cmnty_ind[8],
                               condition == "Restless_Legs_Syndrome" ~ cmnty_ind[20],
                               TRUE ~ as.character(condition))) %>% 
  filter(!condition %in% "Chronic_Idiopathic_Urticaria_(CIU)") # REMOVE CIU

setdiff(unique(conmed_como$condition), cmnty_ind) # 0 
setdiff(cmnty_ind, unique(conmed_como$condition))

# "Arthritis_Psoriatic"          "Asthma"                      
# "Atrial_Fibrillation"          "Axial_Spondyloarthritis"     
# "Dementia_any"                 "Erectile_dysfunction"        
#  "Inflammatory_bowel_disease"   "Migraine"                    
# "Psoriasis"                    "Pulmonary_fibrosis"          
# "Systemic_Lupus_Erythematosus" "Thromboembolism"             


# Check for duplicates
conmed_como %>% distinct(trial, id) # Not 1 row per patient per trial  

xmn <- conmed_como %>% 
  group_by(trial, condition) %>%
  summarise(n = n())

dups <- duplicated(conmed_como %>% select(trial, id)) # 106
dups <- conmed_como[dups, c("trial", "id")] 
dup_trs <- unique(dups$trial)  # 1 trial

conmed_minus_dup_trs <- conmed_como %>% filter(!trial %in% dup_trs)
NROW(conmed_como$company) - NROW(conmed_minus_dup_trs$company) # 4256, 1732

conmed_como1 <- conmed_como %>% distinct(id, trial, .keep_all = TRUE)
NROW(conmed_como$company) - NROW(conmed_como1$company) # 2628, 106
n_distinct(conmed_como$trial) # 40
n_distinct(conmed_como1$trial) # 40
conmed_como <- conmed_como1
rm(conmed_como1)

# Check all columns for NAs
xmn_na <- map(conmed_como, ~sum(is.na(.x))) #  0 NAs

# Checks
n_distinct(conmed_como$trial) # 40
unique(conmed_como$condition) # 10 conditions

# [1] "Diabetes_Mellitus_Type_2"             
# [2] "Parkinson_Disease"                    
# [3] "Osteoarthritis"                       
# [4] "Restless_legs_syndrome"               
# [5] "Hypertension"                         
# [6] "Pulmonary_Disease_Chronic_Obstructive"
# [7] "Rheumatoid_arthritis"                 
# [8] "Osteoporosis"                         
# [9] "Pulmonary_Hypertension"               
# [10] "Benign_Prostatic_Hyperplasia"     


# Clean covariate data then join
con_dup <- duplicated(con %>% select(trial, id))
con_dup <- con[con_dup, c("trial", "id")]
con1 <- con %>% distinct(id, trial, .keep_all = TRUE)
NROW(con)-NROW(con1) # 48247 = something very wrong with con 

# Use Demo as con very wrong
demo_dup <- duplicated(demo %>% select(trial, id))
sum(demo_dup) # 3087
demo1 <- demo %>% distinct(id, trial, .keep_all = TRUE)
NROW(demo)-NROW(demo1) # 3087
demo <- demo1
rm(demo1)

# Join covariate data (age and sex) 
dat  <- conmed_como %>% inner_join(demo, by = c("trial", "id", "company", "nct_id"))
dat1 <- conmed_como %>% inner_join(con, by = c("trial", "id"))
rm(dat1, con, con1)

# Something wrong with covariate data (con) 
# use demo for age and sex 

# Make age numeric 
dat$age <- as.numeric(dat$age)

# Remove trials with index conditions not in community 
dat <- dat %>% filter(condition %in% unique(cmnty_ind)) # Removes none as all in community

# Check for NAs
dat_na <- map(dat, ~sum(is.na(.x))) # 3 NAs in age -> remove
age_na <- is.na(dat$age)
dat1 <- dat %>% filter(!is.na(age))
NROW(dat$age) - NROW(dat1$age) # 3
dat <- dat1
rm(dat1)

# SAVE
write.csv(dat, "Processed_data/cleaned_conmed_defined_comorbidity.csv") # USE THIS FOR ANALYSIS


