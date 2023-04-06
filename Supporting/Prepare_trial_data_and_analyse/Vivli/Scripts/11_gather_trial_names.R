# 11 gather trial names NCT id for every trial extracted in vivli 
# New script written by Jamie to get all trial names 

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

bi_01 <- c("BI1245_36", "BI1245_48", "BI248_524", "BI248_525", "BI248_622")

bi_new_01b <- c("BI107_210",
                "BI248_629", 
                "BI502_254", "BI502_256", "BI502_316", "BI502_317", 
                "BI502_391", "BI502_392", "BI502_413", "BI502_550",
                "BI502_376", "BI502_396", "BI502_327", "BI502_397",
                "BI1276_1",
                "BI1199_32", "BI1199_34", 
                "BI244_2484")

bi_mi_01c <- "BI1123_11"

bi_all <- c(bi_01, bi_new_01b, bi_mi_01c)

roche_04 <- c("RCHNA25220", "RCHQ4881G", "RCHQ4882G", "RCHQ4883G", "RCHWA19924")

takeda_06 <- c("TKASYR322_305", "TKASYR322_402")

lilly_07 <-c("LILLY_B3D_MC_GHAC", "LILLY_B3D_US_GHBZ", "LILLY_H3S_MC_GGGK", 
             "LILLY_H6D_MC_LVGY", "LILLY_H6D_MC_LVHB", "LILLY_H6D_MC_LVHG", 
             "LILLY_H6D_MC_LVHJ", "LILLY_H6D_MC_LVHR", "LILLY_H6D_MC_LVHS", 
             "LILLY_H6D_MC_LVID", "LILLY_H9X_MC_GBCF", "LILLY_H9X_MC_GBDA", 
             "LILLY_H9X_MC_GBDB", "LILLY_H9X_MC_GBDC", "LILLY_H9X_MC_GBDD",
             "LILLY_H9X_MC_GBDE", "LILLY_H9X_MC_GBDG")

all_trials <- c(bi_all, roche_04, takeda_06, lilly_07)
all_trials <- as.data.frame(all_trials)



# NCT for bi trials 
bi_nct <- data.frame(trial = bi_all)

bi_nct <- bi_nct %>% 
  mutate(ptrns = bi_all %>% 
           str_replace_all(pattern = "_", replacement = ".") %>% 
           str_remove("BI"), # format search name for findfx function
         path = map(ptrns, FindFx), # find folder path, this contains NCT id
         nct_id = str_sub(path, 17, 27)) # extract nct id


# NCT for roche
roche_nct <- data.frame(trial = roche_04)

roche_nct <- roche_nct %>% 
  mutate(ptrns = str_sub(roche_04, -5),
         path = map(ptrns, FindFx),
         nct_id = str_sub(path, 17, 27))


# NCT for takeda
takeda_nct <- data.frame(trial = takeda_06)

tak_ptrn1 <- paste0(str_sub(takeda_06[1], -7), "_IPD")
tak_ptrn2 <- paste0(sub( "_", "", paste0(str_sub(takeda_06[2], -7))), "_DATA_ANALYSIS")

takeda_nct <- takeda_nct %>% 
  mutate(ptrns = c(tak_ptrn1, tak_ptrn2),
         path = map(ptrns, FindFx),
         nct_id = str_sub(path, 17, 27))


# lilly NCT
colnames(all_trials) <- "trial"

trial_nctid <- read_csv("Supporting/Prepare_trial_data/Vivli/Created_metadata/selected_trials_in_respository_sas_with_medicine_condition.csv")

all_lilly <- trial_nctid %>% filter(sponsor %in% c("Lilly", "Lilly.")) 

lilly_nct <- all_lilly %>% 
  mutate(trial = str_replace_all(sponsor_id, "-", "_")) %>%
  select(nct_id, trial) %>% 
  inner_join(all_trials)

setdiff(all_trials$trial, lilly_nct$trial) # Missing "LILLY_H3S_MC_GGGK"

# Add NCT for missing trial "LILLY_H3S_MC_GGGK", NCT found on clinicaltrials.gov
# NCT ID for missing trial = "NCT00670319"
miss_lilly <- data.frame(trial = "LILLY_H3S_MC_GGGK",
                         nct_id = "NCT00670319")

lilly_nct <- bind_rows(lilly_nct, miss_lilly)

vivli_extracted_trials <- bind_rows(bi_nct,
                                    roche_nct,
                                    takeda_nct,
                                    lilly_nct)

extracted_trials_nct_id <- vivli_extracted_trials %>% select(trial, nct_id)

write.csv(extracted_trials_nct_id, "Supporting/Prepare_trial_data/Vivli/Outputs/vivli_extracted_trial_nct_ID.csv")

