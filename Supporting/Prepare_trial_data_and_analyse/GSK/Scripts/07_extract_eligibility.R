# 07_extract_elig

# This script extracts trial eligibility criteria

# Packages
source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

# Folder path
path <- ""

# Read in trials NCT IDs
trial_nctid <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/selected_trials_in_respository_sas_with_medicine_condition.csv")

# Check sponsors and subset for only GSK
unique(trial_nctid$sponsor)
gsk_nctid <- trial_nctid %>% filter(sponsor %in% "GSK")

# Create trial column in same format as trial names in extract scripts
gsk_nctid <- gsk_nctid %>% 
  mutate(trial = str_replace(sponsor_id_sas, "-", ""),
         trial = str_replace_all(trial, c("-" = "_", 
                                          "/" = "_", 
                                          "_V" = "_v")))

# Check difference in trial names 
gsk <- readRDS("GSK_processed_data/GSK_transposed.Rds")
setdiff(gsk$demo$trial, gsk_nctid$trial)

msng_v02 <- c("GSKAVA102670", "GSKAVA102672", "GSKAVA105640", "GSKAVD100521")
msng_gsk <- c("AR1103420", "FFR101816", "ROR104836")

# Correct names where necessary
gsk_nctid <- gsk_nctid %>%
  mutate(trial = if_else(trial == "GSKHGS1006_C1056", paste0(trial, "_v05"), trial),
         trial = if_else(trial == "GSKHGS1006_C1057", paste0(trial, "_v03"), trial),
         trial = if_else(trial %in% msng_v02, paste0(trial, "_v02"), trial),
         trial = if_else(trial %in% msng_gsk, paste0("GSK", trial), trial))

setdiff(gsk$demo$trial, gsk_nctid$trial)
elig <- readxl::read_excel("Clincialtrials_gov_related/ihw_eligibility/eligibility_exclusions_final_calc.xlsx")
elig <- elig %>% 
  filter(nct_id %in% gsk_nctid$nct_id)
