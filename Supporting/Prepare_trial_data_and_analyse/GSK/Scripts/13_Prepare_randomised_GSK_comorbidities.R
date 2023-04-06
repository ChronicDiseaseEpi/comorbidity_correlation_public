# 13_Prepare_randomised_GSK_comorbdities

# This script reads in GSK comorbidity data and prepares it for MVP model



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

# Data 
conmed_como <- readRDS("GSK_processed_data/GSK_conmed_defined_comorbidity.Rds")
cmnty_como <- read.csv("Supporting/Prepare_trial_data/GSK/Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv", row.names = 1)
cont_dat    <- readRDS("GSK_processed_data/GSK_model_covs_continuous.Rds")$all_cov_mdl

# Unique Index conditions and comorbidities in GSK trials 
unique(conmed_como$condition) # 14 
colnames(conmed_como)[-c(1:6)]



# Prepare data -----------------------------------------------------------------

# Index conditions in community 
cmnty_ind <- unique(cmnty_como$condition)

# Match GSK trial index conditions to community index conditions 
unique(conmed_como$condition)
cmnty_ind


## After chat with David about additional conditions we have on SAIL but hadn't exported 
# Add SLE to community index conditions (10/10/22)
cmnty_ind <- c(cmnty_ind, "Systemic_Lupus_Erythematosus")

# Harmonise names (if re-running check indexes)
conmed_como <- conmed_como %>% 
  mutate(condition = str_remove_all(condition, ","),
         condition = str_replace_all(condition, " ", "_"),
         condition = case_when(condition == "Pulmonary_Disease_Chronic_Obstructive_Ph" ~ cmnty_ind[15],
                               condition == "Alzheimer's_Disease" ~ cmnty_ind[5],
                               condition == "Restless_Legs_Syndrome" ~ cmnty_ind[16],
                               TRUE ~ as.character(condition)))


# Checks 
setdiff(unique(conmed_como$condition), cmnty_ind)
conmed_como %>% group_by(trial, condition) %>% summarise(n = n())
dups <- duplicated(conmed_como %>% select(trial, id))
dups <- conmed_como[dups, c("trial", "id")] # 0 

# Limit GSK conmed defined comorbidities to only trials with index condition in cmnty
gsk_como <- conmed_como %>% filter(condition %in% cmnty_ind)
unique(gsk_como$condition) # 7

# "Pulmonary_Disease_Chronic_Obstructive" "Dementia_any"                         
# "Asthma"                                "Systemic_Lupus_Erythematosus"         
# "Parkinson_Disease"                     "Restless_legs_syndrome"               
# "Hypertension" 

# Add age and sex data to comorbidity data
gsk_como <- gsk_como %>% inner_join(cont_dat) # drops 5 patients 



# Save
write.csv(gsk_como, "GSK_processed_data/cleaned_gsk_conmed_defined_comorbidities.csv")







