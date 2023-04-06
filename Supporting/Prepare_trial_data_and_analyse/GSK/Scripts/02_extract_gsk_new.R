# 02_extract_gsk_new.R

# This is script 1 of 2 that extracts GSK trials
# I have selected only baseline characteristics for randomised participants at this stage, unlike with 01_gsk


source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

MakeNewGSKFolderName <- function(foldername) {
  ## WARNING, I AM USING SUPERASSINGMENT FOR CONVENIENCE
  folder_out <<- paste0("Data/", str_replace_all(foldername, fixed("-"), ""), "/")
  prefix <- str_replace_all(foldername, "^GSK", "gsk_") %>% 
    str_replace_all(fixed("-"), "_")
  prefix <- paste0(prefix, "_")
  prefix <<- prefix
  if(!dir.exists(folder_out)) dir.create(folder_out)
  
  foldername <- paste0("E:/Research Project 1732/files_",
                       str_replace(foldername, "GSK", "GSK-"),
                       "/Files/")
  folder1 <<- paste0(foldername, "Raw Datasets/SAS_raw/")
  foldpath <<- paste0(foldername, "Analysis Ready Datasets/SAS_analysis/")
  print("Created the following folders in the environement immediately above the function")
  list(folder1, foldpath, folder_out)
}

path <- ""
out <- paste0(path, "GSK_trials_extracted\\")
filesuffix1 <- "ardata_sas.zip"
filesuffix2 <- "data_sas.zip"
filesuffix3 <- "rawdata_sas.zip"



foldername <- "GSK101468_169"    # 0 age/race Na ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_")
foldpath <- paste0(path, prefix, filesuffix1)

## All appear to be randmised
## gives visit windows as a dataset, very helpful metadata 
viswin <- read_sas(unz(foldpath, paste0(prefix, "viswin", ".sas7bdat"))) %>% names_to_lower()

visit <- read_sas(unz(foldpath, paste0(prefix, "visit", ".sas7bdat"))) %>% names_to_lower()

## ON or before randomisation date
vist_pre <- visit %>% 
  filter(visit %in% c("SCREENING", "WEEK -1", "WEEK 0", "WEEK 1")) %>% # @screen
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## Note all demo are screening visits == "SCREENING"
demo  <- read_sas(unz(foldpath, paste0(prefix, "demo", ".sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, race, trtgrp)

cmanal <-  read_sas(unz(foldpath, paste0(prefix, "cmanal", ".sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmatc4, cmatccd, cmcomp, cmroutcd, cmroute) %>% 
  distinct()
conmed <- conmed %>% 
  inner_join(vist_pre) %>% 
  filter(basdate >= cmstdt | cmprior == "Y") %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroutcd, cmroute)


medhist <- read_sas(unz(foldpath, paste0(prefix, "medhist", ".sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhpt, mhbodsys, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct()

labs <- read_sas(unz(foldpath, paste0(prefix, "lab", ".sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("SCREENING", "WEEK -1", "WEEK 0", "WEEK 1")) %>% 
  select(-visit)


vitals <-  read_sas(unz(foldpath, paste0(prefix, "vitals", ".sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("SCREENING", "WEEK -1", "WEEK 0", "WEEK 1")) %>% 
  select(subjid, visit, sysbp, diabp) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## SMoking
smoke <- "Cannot find smoking data in protocol, crf or data dictionary"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre, viswin)



foldername <- "GSK101468_204"    # 2 age NA ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_")
foldpath <- paste0(path, prefix, filesuffix1)

## All appear to be randmised
## gives visit windows as a dataset, very helpful metadata 
viswin <- read_sas(unz(foldpath, paste0(prefix, "viswin", ".sas7bdat"))) %>% names_to_lower()

visit <- read_sas(unz(foldpath, paste0(prefix, "visit", ".sas7bdat"))) %>% names_to_lower()

vist_pre <- visit %>% # @SCREEN
  filter(visit %in% c("SCREENING", "BASELINE")) %>% 
  select(usubjid, basdate = visitdt) %>% 
  arrange(usubjid, desc(basdate)) %>% 
  group_by(usubjid) %>% 
  slice(1) %>% 
  ungroup()

## Note all demo are screening visits == "SCREENING"
demo <- read_sas(unz(foldpath, paste0(prefix, "demo", ".sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, race, trtgrp, agescr) %>%
  mutate(age = if_else(is.na(age), agescr, age)) %>% 
  select(-agescr)

cmanal <-  read_sas(unz(foldpath, paste0(prefix, "cmanal", ".sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmstdt, cmprior, cmatc4, cmatccd, cmcomp) %>% 
  distinct()
conmed <- conmed %>% 
  inner_join(vist_pre) %>% 
  filter(basdate >= cmstdt | cmprior == "Y") %>% 
  distinct(usubjid, cmatc4, cmatccd, cmcomp)

medhist <- read_sas(unz(foldpath, paste0(prefix, "medhist", ".sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhterm, mhclass, mhstat, visit) %>% 
  filter(visit %in% "SCREENING", mhstat %in% c("Past", "Current")) %>% 
  distinct()

medhist %>% group_by(mhterm) %>% summarise(n = n()) # 1474 missing, no other column

labs <- read_sas(unz(foldpath, paste0(prefix, "lab", ".sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
   select(usubjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit == "SCREENING") %>% 
  select(-visit)

vitals <-  read_sas(unz(foldpath, paste0(prefix, "vitals", ".sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("BASELINE", "SCREENING")) %>% 
  select(usubjid, visit, sysbp, diabp) %>% 
  group_by(usubjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- read_sas(unz(foldpath, paste0(prefix, "subuse", ".sas7bdat"))) %>% names_to_lower()
smoke <- smoke %>%  
  select(usubjid, susm) %>% 
  distinct() %>% 
  arrange(usubjid, susm) %>% 
  group_by(usubjid) %>% 
  summarise(susm = paste(susm, collapse = "")) %>% 
  ungroup()


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre, viswin)



foldername <- "GSK101468_205"    # 0 na ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") 
foldpath <- paste0(path, prefix, filesuffix1)

## All appear to be randmised
## gives visit windows as a dataset, very helpful metadata 
viswin <- read_sas(unz(foldpath, paste0(prefix, "viswin", ".sas7bdat"))) %>% names_to_lower()

visit <- read_sas(unz(foldpath, paste0(prefix, "visit", ".sas7bdat"))) %>% names_to_lower()

vist_pre <- visit %>% 
  filter(visit %in% c("SCREEN", "BASELINE")) %>% 
  select(usubjid, basdate = visitdt) %>% 
  arrange(usubjid, desc(basdate)) %>% 
  group_by(usubjid) %>% 
  slice(1) %>% 
  ungroup()

## Note all demo are screening visits == "SCREEN"
demo  <- read_sas(unz(foldpath, paste0(prefix, "demo", ".sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, race, trtgrp)

## COnmeds
cmanal <-  read_sas(unz(foldpath, paste0(prefix, "cmanal", ".sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmstdt, cmprior, cmatc4, cmatccd, cmcomp) %>% 
  distinct()
conmed <- conmed %>% 
  inner_join(vist_pre) %>% 
  filter(basdate >= cmstdt | cmprior == "Y") %>% 
  distinct(usubjid, cmatc4, cmatccd, cmcomp)


## Medical history tables, note all are "SCREEN"
medhist <- read_sas(unz(foldpath, paste0(prefix, "medhist", ".sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhpt, mhclass, mhstat, visit) %>% 
  filter(visit %in% "SCREEN", mhstat %in% c("Past", "Current")) %>% 
  distinct()

## Labs data
labs <- read_sas(unz(foldpath, paste0(prefix, "lab", ".sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit == "SCREEN") %>% 
  select(-visit)

## Vitals data (height and weight)
vitals <-  read_sas(unz(foldpath, paste0(prefix, "vitals", ".sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("BASELINE", "SCREEN")) %>% 
  select(usubjid, visit, sysbp, diabp) %>% 
  group_by(usubjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- read_sas(unz(foldpath, paste0(prefix, "subuse", ".sas7bdat"))) %>% names_to_lower()
smoke <- smoke %>%  
  select(usubjid, susm) %>% 
  distinct() %>% 
  arrange(usubjid, susm) %>% 
  group_by(usubjid) %>% 
  summarise(susm = paste(susm, collapse = "")) %>% 
  ungroup()


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre, viswin)



foldername <- "GSK101468_249"    # 0 NA ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") 
foldpath <- paste0(path, prefix, filesuffix1)

# Visit ID code Visit ID description
# 1 Screening
# 2 Repeat 
# Screening
# 3 Baseline
# 4 Day 3 OC
# 4.5 Day 3 LOCF
# 5 Week 1 OC
# 5.5 Week 1 LOCF
# 6 Week 2 OC
# 6.5 Week 2 LOCF
# 7 Week 3 OC
# 7.5 Week 3 LOCF
# 8 Week 4 OC
# 8.5 Week 4 LOCF
# 9 Week 5 OC
# 9.5 Week 5 LOCF
# 9.7 Week 6 Actigraphy -3 days
# 9.8 Week 6 Actigraphy -2 days
# 9.9 Week 6 Actigraphy -1 days
# 10 Week 6 OC
# 10.5 Week 
# 6 LOCF
# 11 Week 7 OC
# 11.5 Week 7 LOCF
# 12 Week 8 OC
# 12.5 Week 8 LOCF
# 12.6 Week 10 OC
# 12.9 Week 10 LOCF
# 13 Week 12 OC
# 13.5 Week 12 LOCF

dose <- read_sas(unz(foldpath, paste0(prefix, "dose", ".sas7bdat"))) %>% names_to_lower()
vist_first_dose <- dose %>% 
  select(pid, visid, visit, dos_bdat) %>% 
  arrange(pid, dos_bdat) %>% 
  distinct(pid, .keep_all = TRUE) %>% 
  select(pid, dos_bdat)

## Note all demo are screening visits == "SCREEN"
demo  <- read_sas(unz(foldpath, paste0(prefix, "dem", ".sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(pid, age, sex, dm_ht, dm_wt, race, trx)

## COnmeds
# Missing cmatc4, cmatccd, cmcomp but equivalents are present 
pcmed <- read_sas(unz(foldpath, paste0(prefix, "pcmed", ".sas7bdat"))) %>% names_to_lower()
conmed <- pcmed %>% 
  select(pid, med_bdat, med_pcb, med_atc3, med_atcc, med_gens, med_syn) %>% # missing  cmatc4, cmatccd, cmcomp
  distinct()
## No 
conmed <- conmed %>% 
  inner_join(vist_first_dose) %>% 
  filter(dos_bdat > med_bdat | med_pcb %in% c("Both", "Prior")) %>% 
  distinct(pid, med_atc3, med_atcc, med_gens, med_syn) # missing cmatc4, cmatccd, cmcomp

## Medical history tables. Based on CRF (page 12) these are all from the screening visit
medhist <- read_sas(unz(foldpath, paste0(prefix, "mhist", ".sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  distinct(pid, mh_pref, mh_body, mh_ncp)


## Labs data, selected only screening samples
labs <- read_sas(unz(foldpath, paste0(prefix, "lab", ".sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(pid, lab_dat, lab_val, lab_unit, lab_prm, lab_prmc, lab_nrhi, lab_nrlo, visit_l) %>% 
  filter(visit_l == "Screening") %>% 
  select(-visit_l)

## Vitals data, includes blood pressure, pulse and weight
vitals <-  read_sas(unz(foldpath, paste0(prefix, "vital", ".sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit_l %in% c("Baseline", "Screening")) %>% 
  select(pid, vit_prm, vit_prmc, vit_unt, vit_val)  %>% 
  group_by(pid, vit_prm, vit_prmc, vit_unt) %>%
  summarise_at(vars(vit_val), mean, na.rm = TRUE) %>%
  ungroup()

## No smoking data (despite having alcohol adn caffeine), tobacco consumption wihtin 7 days in an exclusion criterion
smoke <- "No smoking data"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, dose, pcmed, vist_first_dose)



foldername <- "GSKAVD100521_v02" # 1 race ----

prefix   <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_")
prefix1  <- prefix %>% str_replace("AVD", "") 
foldpath <- paste0(path, prefix1, filesuffix2)
unztr1   <- unzip(foldpath, paste0(prefix1, filesuffix1))
prefix2  <- prefix1 %>% str_replace("v02_", "")


## gives visit windows as a dataset, very helpful metadata 
#viswin <- read_sas(unz(unztr1, paste0(prefix2, "viswin", "_v02.sas7bdat"))) %>% names_to_lower()
viswin <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "viswin", "_v02.sas7bdat"))) %>% names_to_lower()


## Take visit dates prior to or on date of randomisation
visit    <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "visit", "_v02.sas7bdat"))) %>% names_to_lower()
vist_pre <- visit %>% 
  filter(visitnum <= 30 ) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

saveRDS(visit, paste0("GSK_processed_data/", foldername, "_visit.Rds"))


# demo
demo <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "demo", "_v02.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt) 

# Concomitatnt medication
cmanal <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "cmanal", "_v02.sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmpcb, cmstdt, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>% 
  filter(cmpcb %in% c("Prior", "Both Prior & Concomitant")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroutcd)

## Note all are screening visits
medhist <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "mhmet", "_v02.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhmpt, mhmsoc, mhmtst, visit) %>% 
  filter(visit %in% "VISIT 1A SCREEN", mhmtst %in% c("Past", "Current")) %>% 
  distinct(subjid, mhmpt, mhmsoc)

# select(usubjid, mhterm, mhclass, mhstat, visit) %>% 
labs <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "lab", "_v02.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("VISIT 1A SCREEN", "Visit 1a Screen", "Visit 1c Randomization")) %>% 
  select(-visit) %>% 
  group_by(subjid, lbtest, lbtestcd, lbstunit, lbstnrhi, lbstnrlo) %>% 
  summarise(lbstresn = mean(lbstresn, na.rm = TRUE)) %>% 
  ungroup()

# Vitals is missing 
vitals <- tibble(note = "vitals table missing")

smoke <- read_sas(unz("gsk_100521_v02_ardata_sas.zip", paste0(prefix2, "subuse", "_v02.sas7bdat"))) %>% names_to_lower()
smoke <- smoke %>%  
  select(subjid, susmhs)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix1, prefix2, unztr1, foldername, foldpath, visit, viswin, vist_pre, vitals)



foldername <- "GSKCOR103560"     # 14 age NAS/23 race NAs but no other colums ----

prefix   <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>% str_remove("COR")
foldpath <- paste0(path, prefix, filesuffix2)
unztr1   <- unzip(foldpath, paste0(prefix, filesuffix1))


## Some similarites and some differences between folders raw and analysis ready
visit <- read_sas(unz(unztr1, paste0(prefix, "visit", ".sas7bdat"))) %>% names_to_lower()

## Get dates for on or before date of randomisation, last available
vist_pre <- visit %>% 
  filter(visit %in% c("PRESCREEN", "SCREENING", "WEEK 1 WASHOUT", "WEEK 2 WASHOUT", 
                      "WEEK 3 WASHOUT", "PRERANDOMIZATION", "BASELINE/RAND")) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, and  height and weight
demo <- read_sas(unz(unztr1, paste0(prefix, "demo", ".sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt, htstdbl, wtstdbl, bmistdbl, birthdt) 

mod <- lm(age ~ I(birthdt/365.25), data = demo)

demo <- demo %>% 
  mutate(age_imp = round(coef(mod)[1] + coef(mod)[2] * (birthdt/365.5), 0),
         age = if_else(is.na(age), age_imp, age)) %>%
  select(-c(age_imp, birthdt))


## Concomitant medication
cmanal <-  read_sas(unz(unztr1, paste0(prefix, "cmanal", ".sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmpcb, cmtrt1st, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>% 
  filter(cmtrt1st <= 1 | cmpcb %in% c("Both Prior & Concomitant", "Prior")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
medhist <- read_sas(unz(unztr1, paste0(prefix, "medhist", ".sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhpt, mhsoc, mhstat, mhterm_non_meddra = mhterm) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct()

## Labs data
labs <- read_sas(unz(unztr1, paste0(prefix, "lab", ".sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("PRESCREEN", "SCREENING", "WEEK 1 WASHOUT", "WEEK 2 WASHOUT", 
                      "WEEK 3 WASHOUT", "PRERANDOMIZATION", "BASELINE/RAND")) %>% 
  select(-visit)

## Vitals data BP (height and weight in demo)
vitals <-  read_sas(unz(unztr1, paste0(prefix, "vitals", ".sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("PRESCREEN", "SCREENING", "WEEK 1 WASHOUT", "WEEK 2 WASHOUT", 
                      "WEEK 3 WASHOUT", "PRERANDOMIZATION", "BASELINE/RAND")) %>% 
  select(subjid, visit, sysbp_1, sysbp_2, sysbp_3, diabp_1, diabp_2, diabp_3) %>% 
  gather("dia_or_sys", "value", -subjid, -visit) %>% 
  separate(dia_or_sys, into = c("diasys", "measure")) %>% 
  spread(diasys, value) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()


smoke <- "No smoking data in CRF or dat aspecification despite not being an exclusion criterion"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, unztr1, foldername, foldpath, visit, vist_pre)




foldername <- "GSKCOR103561"     # 2 race ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("COR")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\", prefix, str_remove(filesuffix1, ".zip"), "\\")


## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, prefix, "visit", ".Rds")) %>% names_to_lower()

## Get dates for on or before date of randomisation, last available
vist_pre <- visit %>% 
  filter(visit %in% c("PRESCREEN", "SCREEN", "RANDOMIZATION")) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demo", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt, htstdbl, wtstdbl, bmistdbl) 

## Concomitant medication
cmanal <-  readRDS(paste0(foldpath, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmpcb, cmtrt1st, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>% 
  filter(cmtrt1st <= 1 | cmpcb %in% c("Both Prior & Concomitant", "Prior")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
medhist <- readRDS(paste0(foldpath, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhpt, mhsoc, mhstat, mhterm_non_meddra = mhterm) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct()

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("PRESCREEN", "SCREEN", "RANDOMIZATION")) %>% 
  select(-visit)

## Vitals data BP (height and weight in demo)
vitals <-  readRDS(paste0(foldpath, prefix, "vitals", ".Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("PRESCREEN", "SCREEN", "RANDOMIZATION")) %>% 
  select(subjid, visit, sysbp_1, sysbp_2, sysbp_3, diabp_1, diabp_2, diabp_3) %>% 
  gather("dia_or_sys", "value", -subjid, -visit) %>% 
  separate(dia_or_sys, into = c("diasys", "measure")) %>% 
  group_by(subjid, diasys) %>% 
  summarise_at(vars(value), mean, na.rm = TRUE) %>% 
  ungroup() %>% 
  spread(diasys, value)


smoke <- readRDS(paste0(str_replace(foldpath, "ar", "raw"), prefix, "cvrf", ".Rds")) %>% names_to_lower()
smoke <- smoke %>% 
  select(subjid, smkrfscr)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre)




foldername <- "GSKFFR30003"      # 0  ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_replace("FFR", "ffr")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")

## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, prefix, "visit", ".Rds")) %>% names_to_lower()

## Get dates for on or before date of randomisation, last available
## Note that visit 1 is screening, visit 2 is "Baseline / 5 to 21 days after ~ Screening visit"
vist_pre <- visit %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demo", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt, height, weight) 

## Concomitant medication
cmanal <-  readRDS(paste0(foldpath, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmprior, cmstdt, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>%
  inner_join(vist_pre) %>% 
  filter(cmstdt <=  basdate | cmprior %in% c("Y")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
## Note only meddra system organ class appears to have been recorded
medhist <- readRDS(paste0(foldpath, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhsoc = mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct(subjid, mhsoc)

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(-visit)


## Vitals data BP (height and weight in demo)
vitals <-  readRDS(paste0(foldpath, prefix, "vitals", ".Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(subjid, visit, sysbp, diabp) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- "No smoking data, current smoking use is an exclusion criterion"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre)



foldername <- "GSKFFR30006"      # 0  ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_replace("FFR", "ffr")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")


## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, prefix, "visit", ".Rds")) %>% names_to_lower()
## Get dates for on or before date of randomisation, last available
## Note that visit 1 is screening, visit 2 is "Baseline / 7 to 14 days after ~ Screening visit"
vist_pre <- visit %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demo", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt, height, weight) 

## Concomitant medication
cmanal <-  readRDS(paste0(foldpath, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmprior, cmstdt, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>%
  inner_join(vist_pre) %>% 
  filter(cmstdt <=  basdate | cmprior %in% c("Y")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
## Note only meddra system organ class appears to have been recorded
medhist <- readRDS(paste0(foldpath, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhsoc = mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct(subjid, mhsoc)

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(-visit)


## Vitals data BP (height and weight in demo)
vitals <-  readRDS(paste0(foldpath, prefix, "vitals", ".Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(subjid, visit, sysbp, diabp) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- "No smoking data, current smoking use is an exclusion criterion"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre)



foldername <- "GSKFFR30007"      # 2 race ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_replace("FFR", "ffr")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")


## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, prefix, "visit", ".Rds")) %>% names_to_lower()
## Get dates for on or before date of randomisation, last available
## Note that visit 1 is screening, visit 2 is "Baseline / 7 to 14 days after ~ Screening visit"
vist_pre <- visit %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demo", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt, height, weight) 

## Concomitant medication
cmanal <-  readRDS(paste0(foldpath, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmprior, cmstdt, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>%
  inner_join(vist_pre) %>% 
  filter(cmstdt <=  basdate | cmprior %in% c("Y")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
## Note only meddra system organ class appears to have been recorded
medhist <- readRDS(paste0(foldpath, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhsoc = mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct(subjid, mhsoc)

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(-visit)


## Vitals data BP (height and weight in demo)
vitals <-  readRDS(paste0(foldpath, prefix, "vitals", ".Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2")) %>% 
  select(subjid, visit, sysbp, diabp) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- "No smoking data, current smoking use is an exclusion criterion"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre)



foldername <- "GSKFFU111439"     # o ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("FFU")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")

## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, prefix, "visit", ".Rds")) %>% names_to_lower()
## Get dates for on or before date of randomisation, last available
## Note that visit 1 is screening, visit 2 is "Baseline / 7 to 14 days after ~ Screening visit"
vist_pre <- visit %>% 
  filter(visit %in% c("VISIT 1 SCREENING", "VISIT 2 RANDOMISATION")) %>% 
  select(usubjid, basdate = visitdt) %>% 
  arrange(usubjid, desc(basdate)) %>% 
  group_by(usubjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demo", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, race, trtgrp, dmrefdt, height, weight) 

## Concomitant medication
cmanal <-  readRDS(paste0(foldpath, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmprior, cmstdt, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>%
  inner_join(vist_pre) %>% 
  filter(cmstdt <=  basdate | cmprior %in% c("Y")) %>% 
  distinct(usubjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
## mhterm is present as a variable, but has been entirely redacted.
## only meddra system organ class has been provided
medhist <- readRDS(paste0(foldpath, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhsoc = mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct(usubjid, mhsoc)

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(visit %in% c("VISIT 1 SCREENING", "VISIT 2 RANDOMISATION")) %>% 
  select(-visit)

## Vitals data BP (height and weight in demo)
vitals <-  readRDS(paste0(foldpath, prefix, "vitals", ".Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("VISIT 1 SCREENING", "VISIT 2 RANDOMISATION")) %>% 
  select(usubjid, visit, sysbp, diabp) %>% 
  group_by(usubjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- tibble(note = "No smoking data, current smoking use is an exclusion criterion")


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre)



foldername <- "GSK200165_v02"    # 1 race ----

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")

fold <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_", str_sub(filesuffix1, 1, 10), "\\") 

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("_v02")

## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, fold, prefix, "visit", "_v02.Rds")) %>% names_to_lower()
## Get dates for on or before date of randomisation, last available
## Reviewed protocol to identify these visits
vist_pre <- visit %>%
  filter(visit %in% c("PRE-SCREENING", "VISIT 1")) %>%
  select(subjid, basdate = visitdt) %>%
  arrange(subjid, desc(basdate)) %>%
  group_by(subjid) %>%
  slice(1) %>%
  ungroup()

## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, fold, prefix, "demo", "_v02.Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt) 

## Concomitant medication
## Gives up to 6 codes for each term
cmanal <-  readRDS(paste0(foldpath, fold, prefix, "cmanal", "_v02.Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmprior, cmstdt, cmroute, cmroutcd, starts_with("cmatccd"), cmdecod, cmdrgsyn) %>% 
  distinct()

conmed <- conmed %>% 
  mutate(cmterm = if_else(cmdecod == "Multiple Ingredient", cmdrgsyn, cmdecod)) %>% 
  select(-cmdecod, -cmdrgsyn)

conmed <- conmed %>% 
  gather("codepos", "cmatccd", -subjid, -cmprior, -cmstdt, -cmroute, -cmroutcd,-cmterm, na.rm = TRUE) %>% 
  select(-codepos)

conmed <- conmed %>%
  inner_join(vist_pre) %>% 
  filter(cmstdt <=  basdate | cmprior %in% c("Y")) %>% 
  distinct(subjid, cmatccd, cmroute, cmroutcd, cmterm) %>% # missing cmatc4, cmcomp
  arrange(subjid, cmterm) %>% 
  filter(!is.na(cmatccd), ! cmatccd == "")

## Note all are screening visits
## mhterm is present as a variable, but has been entirely redacted.
## only meddra system organ class has been provided
medhist <- readRDS(paste0(foldpath, fold, prefix, "medhist", "_v02.Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhsoc = mhclass, mhterm, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct(subjid, mhsoc, mhterm)

## Labs data, according to protocol, there is none
labs <- tibble(note = "No laboratory testing is required in this study")


## Vitals data BP (height and weight as well as not in demo)
vitals <-  readRDS(paste0(foldpath, fold, prefix, "vitals", "_v02.Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(visit %in% c("PRE-SCREENING", "VISIT 1")) %>% 
  select(subjid, visit, sysbp, diabp, height, weight) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp, height, weight), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- readRDS(paste0(foldpath, fold, prefix, "subuse", "_v02.Rds")) %>% names_to_lower() %>% 
  filter(visit == "VISIT 1") %>% 
  select(subjid, susmhs)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, fold, visit, vist_pre)



foldername <- "GSKAR2103413_v02" # 34 age no birthdate so removed/0 race ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("AR2") %>%
  str_remove("_v02")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")


## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demog", "_v02.Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex,  trtgrp, blht, blwt, bmi, ethnic, randdt, bsdia, bssys, birthdt) %>%
  filter(!birthdt %in% "") %>%
  select(-birthdt)

## Concomitant medication - all cardiovascular or diabets, no ATC coding, ignore
conmed <- "ignore"

## Note all are screening visits. Gives us 14 criteria, one of which is "Other
## relevant ongoing medical conditions prior to randomization" , and the rest are cardiovascular, smoking or diabetes
medhist <- readRDS(paste0(foldpath, prefix, "medhist", "_v02.Rds")) %>% names_to_lower()
medhist_term <- medhist %>% 
  count(mhterm, sort = TRUE)

# Of these, there are 1900 unique terms, the commonest being "redacted", the first non-cv RF is chronic bronchitis (54)
# Followed by COPD 44
# I do not think that analysing these is feasible
medhist_term_other <- medhist %>% 
  filter(mhterm == "Other relevant ongoing medical conditions prior to randomization") %>% 
  count(mhdetail, sort = TRUE)

medhist <- "ignore, 14 blanket terms most of which are cardiometabolic diseases, with one being 'other ...' 
along with a set of 1899 unique terms, not in a controlled vocabulary on the other hand, this is a large trial!"

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", "_v02.Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, lborresn, lborunit, lbtest, lbtestcd, lbornrhi, lbdt) %>% 
  inner_join(demo %>% select(usubjid, randdt)) %>% 
  filter(lbdt <= randdt) %>% 
  select(-lbdt, -randdt) %>% 
  group_by(usubjid, lborunit, lbtest, lbtestcd, lbornrhi) %>% 
  summarise(lborresn = mean(lborresn, na.rm = TRUE)) %>% 
  ungroup()


## Vitals data BP (height and weight in demo)
vitals <-  demo %>% 
  select(usubjid, bssys, bsdia) 
demo <- demo %>% 
  select(-bssys, -bsdia)


smoke <- readRDS(paste0(foldpath, prefix, "medhist", "_v02.Rds")) %>% names_to_lower()
smoke <- smoke %>% 
  select(usubjid, mhterm) %>% 
  filter(mhterm %in% c("Smoker (Current)", "Smoker (Never)"))
smoke <- smoke %>% 
  mutate(value = 1L) %>% 
  spread(mhterm, value, fill = 0L) %>% 
  mutate(smoker = case_when(`Smoker (Current)` ==1L ~ "Current",
                            `Smoker (Never)` == 1L ~ "Never",
                            TRUE ~ "Former")) %>% 
  select(usubjid, smoker)
smoke <- demo %>% 
  select(usubjid) %>% 
  left_join(smoke) %>% 
  mutate(smoker = if_else(smoker %>% is.na(), "Former", smoker))


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, visit, vist_pre, medhist_term, medhist_term_other)



foldername <- "GSKAR1103420"     # 91 age no birthdt so removed/ 0 race ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("AR1") 

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")


## demographics, and  height and weight
demo <- readRDS(paste0(foldpath, prefix, "demog", "_v03", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex,  trtgrp, blht, blwt, bmi, ethnic, randdt, bsdia, bssys) %>% 
  filter(!is.na(age))

## Concomitant medication - all cardiovascular or diabetes, no ATC coding, ignore
conmed <-  conmed <- "ignore"

## Note all are screening visits. Gives us 15 criteria, one of which is "Other
## relevant ongoing medical conditions prior to randomization" , and the rest are cardiovascular, smoking or diabetes
medhist <- readRDS(paste0(foldpath, prefix, "medhist", "_v03", ".Rds")) %>% names_to_lower()
medhist_term <- medhist %>% 
  count(mhterm, sort = TRUE)

# Of these all 9769 "redacted"
medhist_term_other <- medhist %>% 
  filter(mhterm == "Other relevant ongoing medical conditions prior to randomization") %>% 
  count(mhdetail, sort = TRUE)

medhist <- "ignore, 15 blanket terms most of which are cardiometabolic diseases, with one being 'other ...' 
other is all readacted"

## Labs data
labs <- readRDS(paste0(foldpath, prefix, "lab", "_v03", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, lborresn, lborunit, lbtest, lbtestcd, lbornrhi, lbdt) %>% 
  inner_join(demo %>% select(usubjid, randdt)) %>% 
  filter(lbdt <= randdt) %>% 
  select(-lbdt, -randdt) %>% 
  group_by(usubjid, lborunit, lbtest, lbtestcd, lbornrhi) %>% 
  summarise(lborresn = mean(lborresn, na.rm = TRUE)) %>% 
  ungroup()

## Vitals data BP (height and weight in demo)
vitals <-  demo %>% 
  select(usubjid, bssys, bsdia) 
demo <- demo %>% 
  select(-bssys, -bsdia)

smoke <- readRDS(paste0(foldpath, prefix, "medhist", "_v03", ".Rds")) %>% names_to_lower()
smoke <- smoke %>% 
  select(usubjid, mhterm) %>% 
  filter(mhterm %in% c("Smoker (Current)", "Smoker (Never)"))
smoke <- smoke %>% 
  mutate(value = 1L) %>% 
  spread(mhterm, value, fill = 0L) %>% 
  mutate(smoker = case_when(`Smoker (Current)` ==1L ~ "Current",
                            `Smoker (Never)` == 1L ~ "Never",
                            TRUE ~ "Former")) %>% 
  select(usubjid, smoker)
smoke <- demo %>% 
  select(usubjid) %>% 
  left_join(smoke) %>% 
  mutate(smoker = if_else(smoker %>% is.na(), "Former", smoker))


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, medhist_term, medhist_term_other)



foldername <- "GSKFFR101816"     # 0  ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("FFR") 

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")

## Some similarites and some differences between folders raw and analysis ready
visit <- readRDS(paste0(foldpath, prefix, "visit", ".Rds")) %>% names_to_lower()
## Get dates for on or before date of randomisation, last available
## Note that visit 1 is screening, visit 2A is "Priming Visit 1"
vist_pre <- visit %>% 
  filter(visit %in% c("VISIT 1", "VISIT 2A")) %>% 
  select(subjid, basdate = visitdt) %>% 
  arrange(subjid, desc(basdate)) %>% 
  group_by(subjid) %>% 
  slice(1) %>% 
  ungroup()

## demographics, Does not have height and weight
demo <- readRDS(paste0(foldpath, prefix, "demo", ".Rds")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, race, trtgrp, dmrefdt) 

## Concomitant medication
cmanal <-  readRDS(paste0(foldpath, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmprior, cmstdt, cmroute, cmroutcd, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

conmed <- conmed %>%
  inner_join(vist_pre) %>% 
  filter(cmstdt <=  basdate | cmprior %in% c("Y")) %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp, cmroute, cmroutcd)

## Note all are screening visits
## Note only meddra system organ class appears to have been recorded
medhist <- readRDS(paste0(foldpath, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhsoc = mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct(subjid, mhsoc)

## Labs data
labs <- tibble(note = "Laboratory testing is done, but is redacted")

## Vitals data BP
vitals <-  "No height, weight or BP data"

## All smoking are screening visits
smoke <- "No smoking data, tobacco use wwithin the last year is an exclusion criterion"


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, visit, vist_pre, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath, medhist_term, medhist_term_other)



foldername <- "GSKROR104836"     # 0  ----

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")
prefix   <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>% str_remove("ROR") 
fold     <- paste0(prefix, str_sub(filesuffix1, 1, 10), "\\")


demo  <- readRDS(paste0(foldpath, fold, prefix, "demo", ".Rds")) %>% names_to_lower()
# dmrefdt
demo <- demo %>% 
  select(subjid, age, sex, height, weight, race, trtgrp, dmrefdt)

cmanal <-  readRDS(paste0(foldpath, fold, prefix, "cmanal", ".Rds")) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmatc4, cmatccd, cmcomp) %>% 
  distinct()
conmed <- conmed %>% 
  inner_join(demo %>% select(subjid, dmrefdt)) %>% 
  filter(dmrefdt >= cmstdt | cmprior == "Y") %>% 
  distinct(subjid, cmatc4, cmatccd, cmcomp)


## Non meddra
medhist <- readRDS(paste0(foldpath, fold, prefix, "medhist", ".Rds")) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhterm, mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct()
## Drop MHterm as all except cardiovascular are missing
medhist$mhterm <- NULL
labs <- readRDS(paste0(foldpath, fold, prefix, "lab", ".Rds")) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, avisit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo) %>% 
  filter(avisit %in% c("Screen")) %>% 
  select(-avisit)


vitals <-  readRDS(paste0(foldpath, fold, prefix, "vitals", ".Rds")) %>% names_to_lower()
vitals <- vitals %>% 
  filter(avisit %in% c("Screen")) %>% 
  select(subjid, visit, sysbp, diabp) %>% 
  group_by(subjid) %>% 
  summarise_at(vars(sysbp, diabp), mean, na.rm = TRUE) %>% 
  ungroup()

## All smoking are screening visits
smoke <- readRDS(paste0(foldpath, fold, prefix, "subuse", ".Rds")) %>% names_to_lower()
smoke <- smoke %>%  
  select(subjid, susm) %>% 
  distinct() %>% 
  arrange(subjid, susm) %>% 
  group_by(subjid) %>% 
  summarise(susm = paste(susm, collapse = "")) %>% 
  ungroup()


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, prefix, foldername, foldpath)


# Consolidate all files ----
## The following consolidates the data and ensures all matches

## Combine all files into a single dataframe ----
#List folder names
foldernames <- list("GSK101468_169",
                    "GSK101468_204",
                    "GSK101468_205",
                    "GSK101468_249",
                    "GSKAVD100521_v02",
                    "GSKCOR103560",
                    "GSKCOR103561",
                    "GSKFFR30003",
                    "GSKFFR30006",
                    "GSKFFR30007",
                    "GSKFFU111439",
                    "GSK200165_v02",
                    "GSKAR2103413_v02",
                    "GSKAR1103420",
                    "GSKFFR101816",
                    "GSKROR104836")

# Check all have saveddata
a <- map(foldernames, function(foldername) list.files(paste0(path, "GSK_trials_extracted/", foldername), patt = "extract"))
names(a) <- foldernames
a

# Read all data into a big list
all_trials <- map(foldernames, function(foldername){
  assign(foldername, new.env())
  load(paste0(path, "GSK_trials_extracted/", foldername, "/", foldername, "extract.Rdata"), envir = get(foldername))
  as.list(get(foldername))
})
names(all_trials) <- foldernames

## Check all tables are the same, they are
tbl_names <- map(all_trials, names) 
do.call(rbind, tbl_names)


# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)



## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo)

# Rename variables
demo <- map(demo, function(x) {
  names(x)[names(x) %in% c("race", "ethnic")] <- "race"
  names(x)[names(x) %in% c("bmi", "bmistdbl")] <- "bmi"
  names(x)[names(x) %in% c("weight", "wt", "blwt", "wtstdbl", "dm_wt", "weightbl")] <- "wt"
  names(x)[names(x) %in% c("height", "ht", "blht", "htstdbl", "dm_ht", "heightbl")] <- "ht"
  names(x)[names(x) %in% c("pid", "subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("trx","trtgrp")] <- "trt"
  names(x)[names(x) %in% c("randt", "dmrefdt")] <- "randdt"
  x <- x[!names(x) %in% c("visit", "visitnum", "rand_visit", "randdt")]
  x
})

CreateTableNameLabels(demo)
demo <- map(demo, function(current_df) {
  x <- current_df
  x$subjid <- as.character(x$subjid)
  # x$race <- as.character(x$race)
  # x$sex <- as.character(x$sex)
  # if("randt" %in% names(x)) x$randdt <- as.Date(x$randdt)
  # if("smoke" %in% names(x)) x$smoke <-  as.character(x$smoke)
  x}
)
demo <- BindRowsWLabels(demo, 2)
demo_ht_wt <- demo %>% 
  select(trial, subjid, ht, wt, bmi)
demo <- demo %>% 
  select(trial, age, race, sex, id = subjid, trt)

## remove non-randomised # @SCREEN
demo <- demo %>% 
  filter(!trt %in% c("", "Non-Randomised", "Not Randomized"))

sum(is.na(demo$age)) # 0 (25/08/22)
demo %>% group_by(race) %>% summarise(n = n()) # 3 missing (25/08/22)



## Labs table ----

labs <- all_trials$labs

# 2 trials have no lab data
labs$GSK200165_v02  <- NULL
labs$GSKFFR101816 <- NULL

# 11 trials have a common format
a <- CreateTableNameLabels(labs)

labs1 <- labs[c("GSK101468_169", "GSK101468_204", "GSK101468_205", "GSKAVD100521_v02", 
                "GSKCOR103560", "GSKCOR103561", "GSKFFR30003", "GSKFFR30006", 
                "GSKFFR30007", "GSKFFU111439", "GSKROR104836")]

CreateTableNameLabels(labs1)

labs1 <- map(labs1, function(x) {
  names(x)[names(x) %in% c("subjid", "usubjid")] <- "subjid"
  x$subjid <- as.character(x$subjid)
  x
})

CreateTableNameLabels(labs1)

labs1 <- BindRowsWLabels(labs1, 1)

## Atypical 1/3
GSK101468_249 <- labs$GSK101468_249 %>% 
  select(subjid = pid,
         lbstnrhi = lab_nrhi,
         lbstresn = lab_val,
         lbstunit = lab_unit,
         lbtest = lab_prm,
         lbtestcd = lab_prmc) %>% 
  mutate(lbstnrlo = NA_real_,
         trial = "GSK101468_249")

## atypical 2/3
GSKAR2103413_v02 <- labs$GSKAR2103413_v02 %>% 
  select(subjid = usubjid,
         lbstnrhi = lbornrhi,
         lbstresn = lborresn,
         lbstunit = lborunit,
         lbtest,
         lbtestcd) %>% 
  mutate(lbstnrlo = NA_real_,
         trial = "GSKAR2103413_v02",
         lbtestcd = as.character(lbtest))

## atypical 3/3
GSKAR1103420 <- labs$GSKAR1103420 %>% 
  select(subjid = usubjid,
         lbstnrhi = lbornrhi,
         lbstresn = lborresn,
         lbstunit = lborunit,
         lbtest,
         lbtestcd) %>% 
  mutate(lbstnrlo = NA_real_,
         trial = "GSKAR1103420",
         lbtestcd = as.character(lbtest))

labs <- bind_rows(labs1, GSK101468_249, GSKAR2103413_v02, GSKAR1103420)
rm(labs1, GSK101468_249, GSKAR2103413_v02, GSKAR1103420)

## Limit to randomised ' SCREEN
labs <- labs %>% 
  rename(id = subjid) %>% 
  semi_join(demo, by = c("id", "trial"))
  
## Add new lookups for numerically coded lab results and for other new lab results (cf gsk original)
labs %>% 
  filter(trial %in% c("GSKAR2103413_v02", "GSKAR1103420")) %>% 
  group_by(lbtestcd, lbtest) %>% 
  count() %>% 
  arrange(lbtestcd)
# A tibble: 12 x 3
# Groups:   lbtestcd, lbtest [12]
# lbtestcd lbtest                n
# <int> <chr>             <int>
# 1        1 Hemoglobin        11836
# 2        2 Hematocrit        11163
# 3        3 Total WBC         11643
# 4        4 Platelet count    11079
# 5        5 aPTT               8307
# 6        6 INR                7467
# 7        7 ACT                 591
# 8        8 Total Cholesterol  7607
# 9        9 LDL Cholesterol    5393
# 10       10 HDL Cholesterol    5769
# 11       11 Triglycerides      6752
# 12       12 Creatinine        11818

# Some more rows added now 



## Limit to desired tests
labs_slctd <- read_csv(
  "lbtestcd,newlab
  ALT,ALT
  ALAT,ALT
  ALT_PLC,ALT
  AST,AST
  ASAT,AST
  AST_PLC,AST
  CREAT,CREAT
  CRT_PLC,CREAT
  Creatinine,CREAT
  CRTCE_PLR,EGFR
  GLU,GLU
  GLUC_PLC,GLU
  GLUCC,GLU
  GLUCR,GLU
  Serum Glucose, GLU
  GLUCP_PLC,GLU
  HB_BLC,HGB
  HGB,HGB
  Hemoglobin,HGB
  PLAT,PLT
  PLT,PLT
  PLT_BLC,PLT
  Platelet count,PLT
  PLATE,PLT"
)

labs2 <- labs %>% 
  inner_join(labs_slctd)

labs2 <- labs2 %>% 
  select(trial, lbstnrhi, lbstnrlo, lbstresn, lbstunit, lbtest, lbtestcd, id, newlab)

labsagg <- labs2 %>% 
  group_by(trial) %>% 
  count(newlab) %>% 
  spread(newlab, n)

## Manually checked, trials without glucose genuinely have not emasured glucose
labs <- labs2
rm(labsagg, labs2)

# Exclude non-randomised people, has no effect @SCREEN
labs <- labs %>% 
  semi_join(demo) 
n_distinct(labs$trial) # 14 trials as expected as 2 trials missing labs data 


## Concomittant medicines table ----

conmed <- all_trials$conmed
map(conmed, class)

## 2 trials with non-ATC coding and narrow range of drugs -> ignore
conmed$GSKAR2103413_v02
conmed$GSKAR1103420

conmed$GSKAR2103413_v02 <- NULL
conmed$GSKAR1103420    <- NULL
CreateTableNameLabels(conmed)

## 12 trials typical, 2 atypical
# DOuble checked typical ones
# Note that GSKAVD100521V02 is missing route labels but not codes
# GSK101468204 is missing route data of any kind
conmed1 <- conmed[c("GSK101468_169", "GSK101468_204", "GSK101468_205", "GSKAVD100521_v02", 
                    "GSKCOR103560", "GSKCOR103561", "GSKFFR30003", "GSKFFR30006", 
                    "GSKFFR30007", "GSKFFU111439", "GSKROR104836", "GSKFFR101816")]
CreateTableNameLabels(conmed1)
conmed1 <- map(conmed1, function(x) {
  names(x)[names(x) %in% c("subjid","usubjid")] <- "subjid"
  x$subjid <- as.character(x$subjid)
  x
})
conmed1 <- BindRowsWLabels(conmed1, 1)
                  
# Atypical trial 1 of 2 GSK101468_249
GSK101468_249 <- conmed$GSK101468_249
GSK101468_249 <- GSK101468_249 %>% 
  mutate(trial = "GSK101468_249",
         cmcomp = if_else(med_gens == "Multiple Ingredient", med_syn, med_gens),
         cmroutcd = NA,
         cmroute = NA) %>% 
  select(trial,
         cmatc4 = med_atc3,
         cmatccd = med_atcc,
         cmcomp,
         cmroutcd,
         cmroute,
         subjid = pid)

# Atypical trial 2 of 2 GSK200165_v02
GSK200165_v02 <- conmed$GSK200165_v02
GSK200165_v02 <- GSK200165_v02 %>% 
  mutate(trial = "GSK200165_v02",
         cmatc4 = NA,
         subjid = as.character(subjid)) %>% 
  select(trial,
         cmatc4,
         cmatccd,
         cmcomp = cmterm,
         cmroutcd,
         cmroute,
         subjid)

conmed <- bind_rows(conmed1, GSK101468_249, GSK200165_v02) # 14 trials as expected as 2 trials missing conmed table
setdiff(names(tbl_names), conmed$trial) #  "GSKAR2103413_v02" "GSKAR1103420"   (25/08/22)
rm(conmed1, GSK200165_v02, GSK101468_249)



## Medical history table ----
## Note all but two of these tables are meddra, there is one non-meddra text field, which I will separate

medhist <- all_trials$medhist
map(medhist, class)

# 2 trials have no conmed table
medhist$GSKAR2103413_v02
medhist$GSKAR1103420
medhist$GSKAR2103413_v02 <- NULL
medhist$GSKAR1103420     <- NULL

CreateTableNameLabels(medhist)
medhist <- map(medhist, function(x) {
  names(x)[names(x) %in% c("csid", "subjid","usubjid", "pid")] <- "subjid"
  names(x)[names(x) %in% c("mdra_llt", "mhllt")] <- "mhllt"
  names(x)[names(x) %in% c("mdra_sct", "mhsoc", "mh_body", "mhmsoc", "mhbodsys", "mhcat", "mhclass")] <- "mhsoc"
  names(x)[names(x) %in% c("mhoccur", "mhstat", "mh_ncp")] <- "mhstat"
  names(x)[names(x) %in% c("mhcat", "mhclass")] <- "mhcat"
  names(x)[names(x) %in% c("mh_pref", "mhmpt", "mhpt", "mhterm")] <- "mhpt"
  
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("visit", "mhterm_non_meddra", "mhstat")]
  x
})
CreateTableNameLabels(medhist)

## Keep MHSOC as have system even where have nothing else
medhist <- BindRowsWLabels(medhist)

# There are 5350 missing meddra terms 
mssng_term <- medhist %>% filter(mhpt %in% "")
unique(mssng_term$trial) # "GSK101468_204" "GSK101468_205" "GSKCOR103560" "GSKCOR103561" 

medhist <- medhist %>% 
  rename(mhterm = mhpt,
         id = subjid) %>% 
  mutate(meddra = "meddra")
medhist_non_meddra <- all_trials$medhist[c("GSKCOR103560", "GSKCOR103561", "GSKAR2103413_v02")]



## Vitals table ----

vitals <- all_trials$vitals

## 2 without any vitals data
vitals$GSKFFR101816 <- NULL
vitals$GSKAVD100521_v02 <- NULL
CreateTableNameLabels(vitals)

## One vitals is in long format, and also has weight
GSK101468_249 <- vitals$GSK101468_249
GSK101468_249 <- GSK101468_249 %>% 
  filter(! vit_prm == "Pulse") %>% 
  mutate(vit_prmc = case_when(
    vit_prmc == "DIA" ~ "diabp",
    vit_prmc == "SYS" ~ "sysbp",
    vit_prmc == "WT" ~ "wt",
    TRUE ~ NA_character_)) 

GSK101468_249 <- GSK101468_249 %>% 
  select(subjid = pid,
         vit_prmc,
         vit_val) %>% 
  group_by(subjid, vit_prmc) %>% 
  summarise(vit_val = mean(vit_val, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(vit_prmc, vit_val)

GSK101468_249_wt <- GSK101468_249 %>% 
  select(subjid, wt)

GSK101468_249$wt <- NULL

vitals$GSK101468_249 <- NULL

## 1 vitals has height and weight data but is in wide format
GSK200165_v02_wt <- vitals$GSK200165_v02 %>% 
  select(subjid, ht = height, wt = weight)

vitals$GSK200165_v02[ , c("height", "weight")] <- NULL

## 12 of 13 trials are in wide format, BPS only
vitals <- map(vitals, function(x) {
  names(x)[names(x) %in% c("subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("bsdia", "diabp")] <- "diabp"
  names(x)[names(x) %in% c("bssys", "sysbp")] <- "sysbp"
  x$subjid <- as.character(x$subjid)
  x
})
vitals1 <- BindRowsWLabels(vitals, 1)
vitals2 <- bind_rows(vitals1, GSK101468_249 %>%  mutate(trial = "GSK101468_249"))
bp <- vitals2 %>% 
  rename(id = subjid) %>% 
  gather("param", "value", -trial, -id)

## Height, weight and BMI for long format ----
CreateTableNameLabels(all_trials$demo) %>% select(trial, contains("ht"))
# All say cm except , GSK101468249, this looks like cm too
# hist(demo_ht_wt$ht[demo_ht_wt$trial == "GSK101468249"])
# Is height in cm and weight in kg
ExtractLabel(GSK200165_v02_wt)
# Looks like kg
ExtractLabel(GSK101468_249_wt)
rm(GSK101468_249_wt)
## Both already have height and weight in main dataset so ignore in vitals

# hist(GSK101468249_wt$wt)
# three trials have no height and no weight data in demo or elsewhere
demo_ht_wt %>% 
  filter(trial %in% c("GSK200165_v02", "GSKAVD100521_v02", "GSKFFR101816")) %>% 
  distinct(trial, .keep_all = T)

## one with height/weight in vitals already has in demo
demo_ht_wt %>% 
  filter(trial %in% c("GSK200165_v02", "GSK101468_249", "GSKFFR101816")) %>% 
  group_by(trial) %>% 
  slice(1:3) 

demo_ht_wt <- bind_rows(demo_ht_wt %>% filter(!trial %in% c("GSK200165_v02", "GSKAVD100521_v02", "GSKFFR101816")), 
                        GSK200165_v02_wt %>%  mutate(trial = "GSK200165_v02",
                                                    subjid = as.character(subjid)))
                        
#GSKAVD100521_v02_wt %>%  mutate(trial = "GSKAVD100521_v02",
 #                                                   subjid = as.character(subjid)))

## Calculate BMI 
demo_ht_wt<- demo_ht_wt %>% 
  mutate(bmi = wt/(ht/100)^2) %>% 
  rename(id = subjid)
## 15 trials, all which have bmi data
# library(ggplot2)
# plot1 <- ggplot(demo_ht_wt, aes (x = bmi)) + geom_histogram() + facet_wrap(~ trial, scales = "free_y")
# plot1
demo_ht_wt %>% 
  semi_join(demo) %>% 
  group_by(trial) %>% 
  summarise(n = length(bmi),
            missing_bmi = sum(is.na(bmi)),
            missing_wt = sum(is.na(wt)),
            missing_ht = sum(is.na(ht)))
bmi <- demo_ht_wt %>% 
  gather("param", "value", -trial, -id)



## Add smoking onto demo table as per BI format ----
smoke <- all_trials$smoke
map(smoke, class)
smoke_char <- smoke[map_lgl(smoke, is.character)]
smoke_char <- stack(smoke_char) 
names(smoke_char) <- c("smoke", "trial")
smoke_df <- smoke[!map_lgl(smoke, is.character)]
CreateTableNameLabels(smoke_df)

## Convert 0/1 to N/Y
smoke_df$GSKCOR103561$smkrfscr <- ifelse(smoke_df$GSKCOR103561$smkrfscr  == 1, "Y", "N")

smoke_df$GSKFFU111439$subjid <- NA

smoke_df <- map(smoke_df, function(x) {
  names(x)[names(x) %in% c("pid", "subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("smoker",  "susmhs")] <- "smoking_status"
  names(x)[names(x) %in% c("smkrfscr", "susm")] <- "current"
  x$subjid <- as.character(x$subjid)
  # if("current" %in% names(x)) x$current <- as.character(x$current)
  x
})

smoke_df <- BindRowsWLabels(smoke_df)

smoke_df2 <- smoke_df %>% 
  mutate(smoking_status = case_when(
    is.na(smoking_status) & current == "Y" ~ "current smoker",
    is.na(smoking_status) & current == "N" ~ "Former or never smoker",
    TRUE ~ smoking_status)) %>% 
  select(trial, id = subjid, smoke = smoking_status)
  

demo2 <- demo %>% 
  left_join(smoke_df2)

smoke_char_ex_never <- smoke_char %>% 
  filter(smoke %in% c("No smoking data, current smoking use is an exclusion criterion",
                      "No smoking data, tobacco use wwithin the last year is an exclusion criterion")) %>% 
  pull(trial) %>% 
  as.character()
demo2 <- demo2 %>%
  mutate(smoke = if_else(trial %in% smoke_char_ex_never, "former or never", 
                         str_to_lower(smoke) %>% 
                           str_replace_all("smoker|smoked", "") %>% 
                           str_trim()))
demo <- demo2
rm(demo2)

demo %>% group_by(race) %>% summarise(n = n()) # 3 na (24/08/22)
sum(is.na(demo$age)) # 0 na (25/08/22)



## Randomisation as separate table ----
rand <- all_trials$demo
CreateTableNameLabels(rand)
# Rename variables
rand <- map(rand, function(x) {
  names(x)[names(x) %in% c("pid", "subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("trx","trtgrp")] <- "trt"
  names(x)[names(x) %in% c("randt", "dmrefdt")] <- "randdt"
  x <- x[names(x) %in% c("subjid", "trt","randdt")]
  x
})

CreateTableNameLabels(rand)
rand <- map(rand, function(current_df) {
  x <- current_df
  x$subjid <- as.character(x$subjid)
  x}
)
rand <- BindRowsWLabels(rand, 5)

## remove non-randomised # @SCREEN
rand <- rand %>% 
  filter(!trt %in% c("", "Non-Randomised", "Not Randomized"))

rand <- rand  %>% 
  select(trial, id = subjid, trt, randdt)
demo$trt <- NULL
gsk_new <- list(conmed = conmed, demo = demo, labs = labs, medhist = medhist, bp = bp, bmi = bmi, rand = rand)
rm(list = setdiff(ls(), "gsk_new"))

## Rename all so matches BI_new, except conmed which wish to match to gsk ---- 
gsk_new$conmed <- gsk_new$conmed %>% 
  select(trial, 
         id = subjid,
         whoterm = cmatc4,
         who_atcc = cmatccd,
         cmcomp,
         cmdecod = cmcomp,
         cmroutcd,
         cmroute)

gsk_new$labs <- gsk_new$labs %>% 
  select(trial,
         id,
         labnm = lbtestcd,
         labnmx = lbtest,
         labstd = lbstresn,
         labstdu = lbstunit,
         llc = lbstnrlo,
         ulc = lbstnrhi,
         newlab)

## Drop where only SOC is recorded
medhisttermmissing <- gsk_new$medhist %>% 
  group_by(trial) %>% 
  filter(all(is.na(mhterm) | mhterm == "")) %>% 
  pull(trial) %>% 
  unique() %>% 
  sort()

medhisttermmissing # "GSKFFR101816" "GSKFFR30003"  "GSKFFR30006" "GSKFFR30007"  "GSKFFU111439" "GSKROR104836"



# GSK101468204 only has 17 coded terms, all of which are cardiovascular
gsk_new$medhist <- gsk_new$medhist %>% 
  group_by(trial) %>% 
  filter(! all(is.na(mhterm) | mhterm == "")) %>% 
  ungroup() %>% 
  select(trial,
         id,
         mhterm, # try leaving mhterm instead of changing to term (i.e term = mhterm)
         mhsoc,
         meddra)

gsk_new$rand <- gsk_new$rand %>% 
  rename(tpatt = trt)


## SAVE
saveRDS(gsk_new, "GSK_processed_data/gsk_new.Rds")



# CHECK

rm(gsk_new)
gsk_new <- readRDS("GSK_processed_data/gsk_new.Rds")

# Check how many trials, should = 16
n_distinct(gsk_new$conmed$trial)  # 14
n_distinct(gsk_new$demo$trial)    # 16
n_distinct(gsk_new$labs$trial)    # 14
n_distinct(gsk_new$medhist$trial) # 7 (2 missing + dropped 7 non-meddra trials)- note from 1 run back in ~june, now 8 on (25/08/2022)
n_distinct(gsk_new$bp$trial)      # 14
n_distinct(gsk_new$bmi$trial)     # 14
n_distinct(gsk_new$rand$trial)    # 16


medhist <- gsk_new$medhist
sum(is.na(medhist$mhterm)) # 0 (25/08/22)
medhist %>% filter(mhterm %in% "") %>% count() # 5350 (25/08/22)
medhist %>% filter(mhterm %in% "") %>% distinct(trial) #  GSK101468_204, GSK101468_205, GSKCOR103560, GSKCOR103561 (25/08/22)
gsk_new$demo %>% group_by(race) %>% summarise(n = n()) # 3


