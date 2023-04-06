# 01_extract_gsk

# This is script 1 of 2 that extracts GSK trials 

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

path <- ""
out  <- paste0(path, "GSK_trials_extracted\\")
filesuffix1 <- "ardata_sas.zip"
filesuffix2 <- "data_sas.zip"
filesuffix3 <- "rawdata_sas.zip"



foldername <- "GSK200109" # 0 age/race NAs ----

file_prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_")
foldpath <- paste0(path, file_prefix, filesuffix1)


demo  <- read_sas(unz(foldpath, paste0(file_prefix, "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, bmi, race, susmhs, trtgrp, visit, visitnum)

cmanal <- read_sas(unz(foldpath, paste0(file_prefix, "cmanal.sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmistdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmdecodr, cmprior) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhclass, mhstat, mhterm, visit) %>% 
  filter(visit %in% "Screening", mhstat %in% c("Past", "Current")) %>% # @SCREEN 
  distinct()

labs <- read_sas(unz(foldpath, paste0(file_prefix, "lab.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, foldername, foldpath)



foldername <- "GSK200110" # 0 age NAs/race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_")
foldpath <- paste0(path, file_prefix, "ardata_sas.zip")


demo  <- read_sas(unz(foldpath, paste0(file_prefix, "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, bmi, race, susmhs, trtgrp, visit, visitnum)

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal.sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmistdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmdecodr, cmprior) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhclass, mhstat, mhterm, visit) %>% 
  filter(visit %in% "Screening", mhstat %in% c("Past", "Current")) %>% # @SCREEN
  distinct()

labs <- read_sas(unz(foldpath, paste0(file_prefix, "lab.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, foldername, foldpath)



foldername <- "GSK201315" # 0 age NAs/race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_")
foldpath <- paste0(path, file_prefix, filesuffix1)

demo  <- read_sas(unz(foldpath, paste0(file_prefix, "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, bmi, race, susmhs, trtgrp, visit, visitnum) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal.sas7bdat"))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmistdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmdecodr, cmprior) %>% 
  distinct()

## Does have meddra codes after all
medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhclass, mhstat, mhterm = mhpt, visit) %>% 
  filter(visit %in% "Screening", mhstat %in% c("Past", "Current")) %>% # @SCREEN 
  distinct()

#labs <- read_sas(unz(foldpath, paste0(file_prefix, "lab.sas7bdat"))) %>% names_to_lower()
#labs <- labs %>% 
#  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)
#labs <- paste0(foldername, "_NO lab data")
# tiny 15kb lab file with no data in it, although has all the names

labs <- tibble(note = "No lab results although labs table present with names and ID")

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, foldername, foldpath)



foldername <- "GSKMEA115588" # 226 age NAs/ 0 race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKMEA"), "gsk_"), "_")
foldpath <- paste0(path, file_prefix, filesuffix2)

unztrial <- unzip(foldpath, paste0(file_prefix, filesuffix1))
unztrial2 <- unzip(foldpath, paste0(file_prefix, filesuffix3))


demo <- read_sas(unz(unztrial, paste0(file_prefix, "adsl.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, bmi, race, arm, randdt, birthyr) 

sum(is.na(demo$age)) # 226 

# Calculate missing age 
mod  <- lm(age ~ I(birthyr/365.25), data = demo)
demo <- demo %>% 
  mutate(age_imp = round(coef(mod)[1] + coef(mod)[2] * (birthyr/365.25), 0),
         age = if_else(is.na(age), age_imp, age)) %>%
  select(-c(age_imp, birthyr))


cm <-  read_sas(unz(unztrial, paste0(file_prefix, "adcm.sas7bdat"))) %>% names_to_lower()
conmed <- cm %>% 
  select(usubjid, cmstdtc, cmdecod, cmroute, dcl4c, dcl4t, prescrfl, prerunfl, runinfl) %>% 
  distinct()

medhist <- read_sas(unz(unztrial2, paste0(file_prefix, "mh.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhcat, mhoccur, mhterm, visit, visitnum) %>% 
  distinct()

labs <- read_sas(unz(unztrial, paste0(file_prefix, "adlb.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, visitnum, aval, param, paramcd, anrhi, anrlo)

vitals <- read_sas(unz(unztrial, paste0(file_prefix, "advs.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(usubjid, visit, aval, param, paramcd)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cm, conmed, demo, labs, medhist, vitals, unztrial, unztrial2, file_prefix, foldername, foldpath)


foldername <- "GSKAVA102670_v02" # 32 age NAs/ all race missing -> new race var now only 2 missing ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKAVA"), "gsk_"), "_")
file_prefix <- str_replace(file_prefix, "v02_$", "")
foldpath    <- paste0(path, file_prefix, "v02_", filesuffix2)
unztr1      <- unzip(foldpath, paste0(file_prefix, "v02_", filesuffix1))
suffix      <- "_v02.sas7bdat"

demo <- read_sas(unz(unztr1, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, bmi, race = deid_race, trtgrp, dmrefdt, birthdt) %>%
  filter(!is.na(birthdt)) # drop 15 patients 

# Correct missing ages
sum(is.na(demo$age)) # 17

mod <- lm(age ~ I(birthdt/365.25), data = demo)

demo <- demo %>% 
  mutate(age_imp = round(coef(mod)[1] + coef(mod)[2] * (birthdt/365.25), 0),
         age = if_else(is.na(age), age_imp, age)) %>%
  select(-c(birthdt, age_imp))


cmanal <-  read_sas(unz(unztr1, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

medhist <- read_sas(unz(unztr1, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhllt, mhlltcd, mhclass, mhsoc, mhstat, visit) %>% 
  filter(visit %in% "SCREEN", mhstat %in% c("Past", "Current")) %>% # @SCREEN
  distinct()

labs <- read_sas(unz(unztr1, paste0(file_prefix, "lab", suffix))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(unztr1, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)

smoke <- read_sas(unz(unztr1, paste0(file_prefix, "substat", suffix))) %>% names_to_lower()
smoke <- smoke %>%  
  select(subjid, susmhs)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, unztr1, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKAVA102672_v02" # 36 age NAs/ all race missing -> new race var now only 1 missing ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKAVA"), "gsk_"), "_")
file_prefix <- str_replace(file_prefix, "v02_$", "")
foldpath    <- paste0(path, file_prefix, "v02_", filesuffix2)
unztr1      <- unzip(foldpath, paste0(file_prefix, "v02_", filesuffix1))
suffix      <- "_v02.sas7bdat"

demo <- read_sas(unz(unztr1, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, bmi, race = deid_race, trtgrp, dmrefdt, birthdt) %>%
  filter(!is.na(birthdt)) # drops 20 patients 

# Correct missing age 
sum(is.na(demo$age)) # 36 

mod <- lm(age ~ I(birthdt/365.25), data = demo)

demo <- demo %>% 
  mutate(age_imp = round(coef(mod)[1] + coef(mod)[2] * (birthdt/365.25), 0),
         age = if_else(is.na(age), age_imp, age)) %>%
  select(-c(birthdt, age_imp))


cmanal <-  read_sas(unz(unztr1, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

medhist <- read_sas(unz(unztr1, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhllt, mhlltcd, mhclass, mhsoc, mhstat, visit) %>% 
  filter(visit %in% "SCREEN", mhstat %in% c("Past", "Current")) %>% 
  distinct()

labs <- read_sas(unz(unztr1, paste0(file_prefix, "lab", suffix))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(unztr1, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)

smoke <- read_sas(unz(unztr1, paste0(file_prefix, "substat", suffix))) %>% names_to_lower()
smoke <- smoke %>%  
  select(subjid, susmhs)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, unztr1, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKAVA105640_v02" # 59 age Na/ 0 race 11----

file_prefix <- paste0(str_replace(foldername, fixed("GSKAVA"), "gsk_"), "_")
file_prefix <- str_replace(file_prefix, "v02_$", "")
foldpath    <- paste0(path, file_prefix, "v02_", filesuffix2)
unztr1      <- unzip(foldpath, paste0(file_prefix, "v02_", filesuffix1))
suffix      <- "_v02.sas7bdat"


demo  <- read_sas(unz(unztr1, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo  <- demo %>% 
  select(subjid, age, sex, height, weight, bmi, race, trtgrp, dmrefdt, birthdt) %>%
  filter(!is.na(birthdt)) # only 1 and its a randomised patient

# Calculate missing age
sum(is.na(demo$age)) # 58
mod  <- lm(age ~ I(birthdt/365.25), data = demo)
demo <- demo %>% 
  mutate(age_imp = round(coef(mod)[1] + coef(mod)[2] * (birthdt/365.5), 0),
         age = if_else(is.na(age), age_imp, age)) %>% 
  select(- c(birthdt, age_imp))


cmanal <-  read_sas(unz(unztr1, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

medhist <- read_sas(unz(unztr1, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhllt, mhlltcd, mhclass, mhsoc, mhstat, visit) %>% 
  filter(visit %in% "SCREEN", mhstat %in% c("Past", "Current")) %>% 
  distinct()

labs <- read_sas(unz(unztr1, paste0(file_prefix, "lab", suffix))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(unztr1, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)

smoke <- read_sas(unz(unztr1, paste0(file_prefix, "substat", suffix))) %>% names_to_lower()
smoke <- smoke %>%  
  select(subjid, susmhs)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, unztr1, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKB2C112060" # 1 race/ 0 age  ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKB2C"), "gsk_"), "_")
foldpath <- paste0(path, file_prefix, filesuffix1)
suffix <- ".sas7bdat"


demo  <- read_sas(unz(foldpath, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, deid_race, trtgrp) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhterm, mhclass, mhstat, visit) %>% 
  filter(visit %in% "Screening", mhstat %in% c("Past", "Current")) %>% 
  distinct()

# labs <- read_sas(paste0(foldername, file_prefix, "lab", file_suffix)) %>% names_to_lower()
# labs <- labs %>% 
#   select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

labs <- tibble(note = "No lab results although was collected")

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visit, sysbp, diabp)

smoke <- read_sas(unz(foldpath, paste0(file_prefix, "subuse", suffix))) %>% names_to_lower()
smoke <- smoke %>%  
  select(subjid, susmhs)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, smoke, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, smoke, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKDB2113360_v02" # 0 age/race NAs ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKDB2"), "gsk_"), "_")
file_prefix <- str_replace(file_prefix, "v02_$", "")
foldpath <- paste0(path, file_prefix, "v02_", filesuffix1)
suffix <- "_v02.sas7bdat"

demo  <- read_sas(unz(foldpath, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, bmi, race, susmhs, trtgrp) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmistdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmdecodr, cmprior) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhclass, mhstat, mhterm, visit) %>% 
  filter(visit %in% "Screening", mhstat %in% c("Past", "Current")) %>% 
  distinct()

labs <- read_sas(unz(foldpath, paste0(file_prefix, "lab", suffix))) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(usubjid, visit, sysbp, diabp)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKFFA112059" # 0 age/ 2 race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKFFA"), "gsk_"), "_")
foldpath    <- paste0(path, file_prefix, filesuffix1)
suffix      <- ".sas7bdat"

demo <- read_sas(unz(foldpath, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, race, trtgrp) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmprior) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhclass, mhstat, mhterm, visit) %>% 
  filter(visit %in% "Visit 1 (Screening)", mhstat %in% c("Past", "Current")) %>% 
  distinct()

labs <- read_sas(unz(foldpath, paste0(file_prefix, "lab", suffix))) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- tibble(note = "no blood pressure measured from protocol")


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKFFA115285" # 0 age/race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKFFA"), "gsk_"), "_")
foldpath    <- paste0(path, file_prefix, filesuffix1)
suffix      <- ".sas7bdat"

demo <- read_sas(unz(foldpath, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, race, trtgrp) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmprior) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhclass, mhstat, mhterm, visit) %>% 
  filter(visit %in% "Visit 1 (Screening)", mhstat %in% c("Past", "Current")) %>% 
  distinct()

# labs <- read_sas(paste0(foldername, file_prefix, "lab", file_suffix)) %>% names_to_lower()
# labs <- labs %>% 
  # select(usubjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)
labs <- tibble(note = "no lab data in folder, but is in protocol")

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals  %>% 
  select(usubjid, visit, sysbp, diabp)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, suffix, foldername, foldpath)



foldername <- "GSKFFR106080" # 0 age/1 race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKFFR"), "gsk_"), "_")
foldpath    <- paste0(path, file_prefix, filesuffix1)
suffix      <- ".sas7bdat"

demo <- read_sas(unz(foldpath, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, age, sex, height, weight, race, trtgrp) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(subjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmprior) %>% 
  distinct()

medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(subjid, mhclass, mhstat) %>% 
  filter(mhstat %in% c("Past", "Current")) %>% 
  distinct() %>% 
  mutate(mhterm = "Not available in this trial")

labs <- read_sas(unz(foldpath, paste0(file_prefix, "lab", suffix))) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visit, lbstresn, lbstunit, lbtest, lbtestcd, lbstnrhi, lbstnrlo)

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals  %>% 
  select(subjid, visit, sysbp, diabp)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, suffix, foldername, foldpath)








foldername <- "GSKHGS1006_C1056_v05" # 0 age/race/new race var  ----

file_prefix1 <- paste0(str_replace(foldername, fixed("GSKHGS1006_C1056_v05"), "gsk_hgs1006-c1056_v05"), "_")
foldpath     <- paste0(path, file_prefix1, filesuffix2)
file_prefix2 <- paste0(str_replace(foldername, fixed("GSKHGS1006_C1056_v05"), "gsk_110751"), "_")
unztr1       <- unzip(foldpath, paste0(file_prefix1, filesuffix1))
suffix       <- "_v05.sas7bdat"


demo <- read_sas(unz(unztr1, paste0(file_prefix2, "dem", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(csid, age, sex, ht, wt, bmi, race = racegrpc, trt_l) 

cmanal <-  read_sas(unz(unztr1, paste0(file_prefix2, "conmed", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(csid, visit, mdra_llt, mdra_hlt, mdra_sct, sday, whoterm, who_atcc) %>% 
  distinct()

medhist <- read_sas(unz(unztr1, paste0(file_prefix2, "mhx", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(csid, mdra_llt, mdra_hlt, mdra_sct) %>% 
  distinct()

labs1 <- read_sas(unz(unztr1, paste0(file_prefix2, "l_hema", suffix))) %>% names_to_lower()
labs1 <- labs1 %>% 
  select(csid, visit, lressi, lressi_u, ltest, vtest, lnrsi_h, lnrsi_l)

labs2 <- read_sas(unz(unztr1, paste0(file_prefix2, "l_chem", suffix))) %>% names_to_lower()
labs2 <- labs2 %>% 
  select(csid, visit, lressi, lressi_u, ltest, vtest, lnrsi_h, lnrsi_l)

labs <- bind_rows("hema" = labs1, "chem" = labs2, .id = "lab_test_type")
rm(labs1, labs2)

vitals <- read_sas(unz(unztr1, paste0(file_prefix2, "vital", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(csid, visit, bps, bpd)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, unztr1, file_prefix1, file_prefix2, suffix, foldername, foldpath)



foldername <- "GSKHGS1006_C1057_v03" # 0 age/race/ new race var ----

file_prefix1 <- paste0(str_replace(foldername, fixed("GSKHGS1006_C1057_v03"), "gsk_hgs1006-c1057_v03"), "_")
foldpath     <- paste0(path, file_prefix1, filesuffix2)
file_prefix2 <- paste0(str_replace(foldername, fixed("GSKHGS1006_C1057_v03"), "gsk_110752"), "_")
unztr1       <- unzip(foldpath, paste0(file_prefix1, filesuffix1))
suffix       <- "_v03.sas7bdat"

demo  <- read_sas(unz(unztr1, paste0(file_prefix2, "dem", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(csid, age, sex, ht, wt, bmi, race = racegrpc, trt_l) 

cmanal <-  read_sas(unz(unztr1, paste0(file_prefix2, "conmed", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(csid, visit, mdra_llt, mdra_hlt, mdra_sct, sday, whoterm, who_atcc) %>% 
  distinct()

medhist <- read_sas(unz(unztr1, paste0(file_prefix2, "mhx", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(csid, mdra_llt, mdra_hlt, mdra_sct) %>% 
  distinct()

labs1 <- read_sas(unz(unztr1, paste0(file_prefix2, "lhema1", suffix))) %>% names_to_lower()
labs1 <- labs1 %>% 
  select(csid, visit, lressi, lressi_u, ltest, vtest, lnrsi_h, lnrsi_l)

labs2 <- read_sas(unz(unztr1, paste0(file_prefix2, "lchem1", suffix))) %>% names_to_lower()
labs2 <- labs2 %>% 
  select(csid, visit, lressi, lressi_u, ltest, vtest, lnrsi_h, lnrsi_l)

labs3 <- read_sas(unz(unztr1, paste0(file_prefix2, "lhema2", suffix))) %>% names_to_lower()
labs3 <- labs3 %>% 
  select(csid, visit, lressi, lressi_u, ltest, vtest, lnrsi_h, lnrsi_l)

labs4 <- read_sas(unz(unztr1, paste0(file_prefix2, "lchem2", suffix))) %>% names_to_lower()
labs4 <- labs4 %>% 
  select(csid, visit, lressi, lressi_u, ltest, vtest, lnrsi_h, lnrsi_l)

labs <- bind_rows("hema1" = labs1, "chem1" = labs2, 
                  "hema2" = labs3, "chem2" = labs2,
                  .id = "lab_test_type")
rm(labs1, labs2, labs3, labs4)

vitals <- read_sas(unz(unztr1, paste0(file_prefix2, "vital", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(csid, visit, bps, bpd)


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, unztr1, file_prefix1, file_prefix2, suffix, foldername, foldpath)






foldername <- "GSKAC4116135" # 0 age/race ----

file_prefix <- paste0(str_replace(foldername, fixed("GSKAC4"), "gsk_"), "_")
foldpath    <- paste0(path, file_prefix, filesuffix1)
suffix      <- ".sas7bdat"

demo  <- read_sas(unz(foldpath, paste0(file_prefix, "demo", suffix))) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age, sex, height, weight, race, trtgrp, susmhs) 

cmanal <-  read_sas(unz(foldpath, paste0(file_prefix, "cmanal", suffix))) %>% names_to_lower()
conmed <- cmanal %>% 
  select(usubjid, cmstdt, cmprior, cmroutcd, cmroute, cmatc4, cmatccd, cmcomp, cmdecodr, cmprior) %>% 
  distinct()
  
medhist <- read_sas(unz(foldpath, paste0(file_prefix, "medhist", suffix))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhclass, mhstat, mhterm, visit) %>% 
  filter(visit %in% "Screening", mhstat %in% c("Past", "Current")) %>% 
  distinct()

vitals <- read_sas(unz(foldpath, paste0(file_prefix, "vitals", suffix))) %>% names_to_lower()
vitals <- vitals %>% 
  select(usubjid, visit, sysbp, diabp)

labs <- tibble(note = "No lab results although was collected")


# SAVE
if(!dir.exists(paste0(out, foldername))) dir.create(paste0(out, foldername))
save(demo, medhist, conmed, labs, vitals, file = paste0(out, foldername, "/", foldername, "extract.Rdata"))
rm(cmanal, conmed, demo, labs, medhist, vitals, file_prefix, suffix, foldername, foldpath)





foldername <- "GSKDB2113374_v02" # Not missing but wrong tables # ----

prefix <- paste0(str_replace(foldername, fixed("GSK"), "gsk_"), "_") %>%
  str_remove("DB2") %>%
  str_remove("_v02")

foldpath <- paste0(str_sub(path, 0, -11), "unzipped_data\\")

ae <- readRDS(paste0(foldpath, prefix, "ae", "_v02.Rds")) %>% names_to_lower()
copdhis <- readRDS(paste0(foldpath, prefix, "copdhis", "_v02.Rds")) %>% names_to_lower()
ds <- readRDS(paste0(foldpath, prefix, "ds", "_v02.Rds")) %>% names_to_lower()
pft <- readRDS(paste0(foldpath, prefix, "pft", "_v02.Rds")) %>% names_to_lower()
sgrq <- readRDS(paste0(foldpath, prefix, "sgrq", "_v02.Rds")) %>% names_to_lower()
sgrqana <- readRDS(paste0(foldpath, prefix, "sgrqana", "_v02.Rds")) %>% names_to_lower()




# Consolidate all files ----
## The following consolidates the data and ensures all matches

## Combine all files into a single dataframe ----
#List folder names
foldernames <- list("GSK200109", "GSK200110", "GSK201315", "GSKAVA102670_v02", 
                    "GSKAVA102672_v02", "GSKAVA105640_v02", "GSKB2C112060", "GSKDB2113360_v02", 
                     "GSKFFA112059", "GSKFFA115285",
                    "GSKHGS1006_C1056_v05", "GSKHGS1006_C1057_v03", 
                    "GSKMEA115588",
                    "GSKFFR106080",
                    "GSKAC4116135")

# "GSKDB2113374_v02",

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

# Check all table names the same, they are not, some have a smoke table
smoke <- map(all_trials, ~ .x$smoke)
smoke <- smoke[!map_lgl(smoke, is.null)]
all_trials <- map(all_trials, ~ .x[names(.x) != "smoke"])
tbl_names <- map(all_trials, names) 
do.call(rbind, tbl_names)

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)
all_trials$smoke <- smoke
rm(smoke)


# Check how many trials, should = 15
n_distinct(all_trials$conmed)  # 15
n_distinct(all_trials$demo)    # 15
n_distinct(all_trials$labs)    # 14
n_distinct(all_trials$medhist) # 15

n_distinct(all_trials$vitals) # 15
n_distinct(all_trials$smoke) # 4

n_distinct(all_trials$bp)      # 0
n_distinct(all_trials$bmi)     # 0
n_distinct(all_trials$rand)    # 0



## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo)
# Rename age variable
demo <- map(demo, function(x) {
  names(x)[names(x) %in% c("race", "deid_race")] <- "race"
  names(x)[names(x) %in% c("weight", "wt")] <- "wt"
  names(x)[names(x) %in% c("height", "ht")] <- "ht"
  names(x)[names(x) %in% c("csid", "subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("arm", "trt_l","trtgrp")] <- "trt"
  names(x)[names(x) %in% c("randt", "dmrefdt")] <- "randdt"
  x <- x[!names(x) %in% c("visit", "visitnum", "rand_visit")]
  x
})

CreateTableNameLabels(demo)
demo <- map(demo, function(current_df) {
  x <- current_df
  x$subjid <- as.character(x$subjid)
  # x$race <- as.character(x$race)
  x$sex <- as.character(x$sex)
  # if("randt" %in% names(x)) x$randdt <- as.Date(x$randdt)
  # if("smoke" %in% names(x)) x$smoke <-  as.character(x$smoke)
  x}
)
CreateTableNameLabels(demo)
demo <- BindRowsWLabels(demo, 2)

# limit to randomised patients # @SCREEN
table(demo$trt == "")
table(is.na(demo$trt))
table(demo$trt)
not_randomised <- c("", "Run-in failure", "Screen failure", "Pre-screen failure", 
                        "Screen Failure", "Not Randomised", "Not Assigned") # @SCREEN
demo <- demo %>% 
  filter(!trt %in% not_randomised) # @SCREEN

n_distinct(demo$trial) # 15 trials 

sum(is.na(demo$age)) # 0 (23/08/22)
unique(demo$race)
race_xmn <- demo %>%
  group_by(race) %>%
  summarise(n = n()) # only 5 missing now 


## Labs table ----
# 9 of 14 have a standard format except subject ID name, exclude the non-standard ones
labs1 <- all_trials$labs[!names(all_trials$labs) %in% c("GSKHGS1006_C1056_v05",
                                                      "GSKHGS1006_C1057_v03",
                                                      "GSKMEA115588",
                                                      "GSKB2C112060",
                                                      "GSKFFA115285",
                                                      "GSKAC4116135",
                                                      "GSK201315")]

CreateTableNameLabels(labs1)
labs1 <- map(labs1, function(x) {
  names(x)[names(x) %in% c("subjid","usubjid")] <- "subjid"
  x$subjid<- as.character(x$subjid)
  x
})

CreateTableNameLabels(labs1)
labs1 <- BindRowsWLabels(labs1, 1)

# 2 of 10 have standard format
labs2 <- all_trials$labs[names(all_trials$labs) %in% c("GSKHGS1006_C1056_v05",
                                                        "GSKHGS1006_C1057_v03")]
CreateTableNameLabels(labs2)
labs2 <- BindRowsWLabels(labs2, 1)

# 1 of 10 is idiosyncratic
labs3 <- all_trials$labs$GSKMEA115588
labs3$trial <- "GSKMEA115588"

# 4 trials missing lab data
all_trials$labs$GSKB2C112060$note
all_trials$labs$GSKFFA115285$note
all_trials$labs$GSKAC4116135$note
all_trials$labs$GSK201315$note

# Join all 3 together
names_lkp <- c("trial" = "trial",
               "csid" = "subjid",
               "lab_test_type" = "lab_test_type",
               "lnrsi_h" = "lbstnrhi",
               "lnrsi_l" = "lbstnrlo",
               "lressi" = "lbstresn",
               "lressi_u" = "lbstunit",
               "ltest" = "lbtestcd",
               "visit" = "visit",
               "vtest" = "vtest")
names(labs2) <- names_lkp[names(labs2)]
# Note is expecting NAs as characters converted to numbers and some terms
# eg microcytosis lost
labs2[c("lbstnrhi", "lbstnrlo", "lbstresn")] <-
  map(labs2[c("lbstnrhi", "lbstnrlo", "lbstresn")], as.double)
labs1_2 <- bind_rows(labs1, labs2)
a <- ExtractLabel(labs1, return_object = TRUE)
setdiff(names(labs1_2), names(a))
a <- c(a, "lab_test_type" = "lab_test_type", "vtest" = "vtest")
a <- a[names(labs1_2)]
labs1_2 <- MakeLabels(labs1_2, a)

n_distinct(labs1_2$trial) # 10

# Join all 3 together
names_lkp <- c("trial" = "trial",
               "usubjid" = "subjid",
               "anrhi" = "lbstnrhi",
               "anrlo" = "lbstnrlo",
               "aval" = "lbstresn",
               "paramcd" = "lbtestcd",
               "param" = "lbtest",
               "visit" = "visit",
               "visitnum" = "visitnum")
names(labs3) <- names_lkp[names(labs3)]
labs3[c("lbstnrhi", "lbstnrlo", "lbstresn")] <-
  map(labs3[c("lbstnrhi", "lbstnrlo", "lbstresn")], as.double)
labs1_2_3 <- bind_rows(labs1_2, labs3)
a <- ExtractLabel(labs1_2, return_object = TRUE)
a <- c(a, "visitnum" = "visitnum")
labs1_2_3 <- MakeLabels(labs1_2_3, a)
table(labs1_2_3$visit)

n_distinct(labs1_2_3$trial) # 11

## Lab results
labs_visits <- tribble(
  ~"trial", ~"code",
  "GSK200109", "'Screening'",
  "GSK200110", "'Screening'",
  "GSK201315", "'Logs'", ## only 15 measures
  "GSKAVA102670_v02", "c('SCREEN', 'BASELINE')",
  "GSKAVA102672_v02", "c('SCREEN', 'BASELINE')",
  "GSKAVA105640_v02", "c('SCREEN', 'BASELINE')",
  "GSKDB2113360_v02", "c('Screening', 'Baseline')",
  "GSKDB2113374_v02", "c('Screening', 'Baseline')",
  "GSKFFA112059", "c('Visit 1 (Screening)', 'Visit 2 (Week 0)')",
  "GSKHGS1006_C1056_v05", "c('Screening', 'Day 0')",
  "GSKHGS1006_C1057_v03", "c('Screening', 'Day 0')",
  "GSKMEA115588", "c('SCREENING', 'VISIT 2 (WEEK 0)')",
  "GSKFFR106080", "c('SCREENING', 'Randomisation')"
  )
labs_visits <- by(labs_visits, labs_visits$trial, function(x) eval(parse(text = x$code)))
labs_visits <- stack(labs_visits) %>% 
  setNames(c("visit", "trial"))

n_distinct(labs_visits$trial) # 13

labs <- labs1_2_3 # 11
rm(labs1, labs1_2, labs1_2_3, labs2, labs3)

labs <- labs %>%
  semi_join(labs_visits) # 11

## Limit to randomised patients @SCREEN
labs <- labs %>% 
  semi_join(demo) 

n_distinct(labs$trial) # 11

## Select wanted measures
labs_rv <- labs %>% distinct(lbtest, lbtestcd, lab_test_type) %>% 
  group_by(lbtestcd) %>% 
  summarise_all(function(x) paste(x, collapse = " | "))
# write_csv(labs_rv, "Scratch_data/reivew_gsk_labs.csv")
# labs_slctd <- read_csv("Created_metadata/reivewED_gsk_labs.csv") %>%
#   filter(keep ==1) %>%
#   select(lbtestcd, newlab)
labs_slctd <- read_csv(
  "lbtestcd,newlab
  ALT,ALT
  ALT_PLC,ALT
  AST,AST
  AST_PLC,AST
  CREAT,CREAT
  CRT_PLC,CREAT
  CRTCE_PLR,EGFR
  GLU,GLU
  GLUC_PLC,GLU
  GLUCC,GLU
  HB_BLC,HGB
  HGB,HGB
  PLAT,PLT
  PLT,PLT
  PLT_BLC,PLT"
)

labs <- labs %>% 
  inner_join(labs_slctd) 

n_distinct(labs$trial) # 11

# Remove variables no longer needed, eg visit related and test types (eg urine versus blood)
labs <- labs %>% 
  select(trial:lbtestcd, subjid, newlab)



## Concomittant medicines table ----
conmed <- all_trials$conmed
CreateTableNameLabels(conmed)

# 13 tables similar
conmed1 <- conmed[c("GSK200109", "GSK200110", "GSK201315", "GSKAVA102670_v02", 
                    "GSKAVA102672_v02", "GSKAVA105640_v02", "GSKB2C112060", "GSKDB2113360_v02", 
                    "GSKFFA112059", "GSKFFA115285", "GSKFFR106080",
                    "GSKAC4116135")] # @MISSING "GSKDB2113374_v02"
CreateTableNameLabels(conmed1)

# Note some cmistdt Imputed start date are relative(ie numbers) and some dates
# so convert all to charachter
conmed1 <- map(conmed1, function(x) {
  names(x)[names(x) %in% c("subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("cmistdt","cmstdt")] <- "cmistdt"
  x$cmistdt <- as.character(x$cmistdt)
  x$subjid <- as.character(x$subjid)
  x
})

conmed1 <- BindRowsWLabels(conmed1, 1)

# 2 other similar tables, have meddra terms for drugs, just drop these as have WHO terms
# COnsider including in medical hstory
conmed2 <- conmed[c("GSKHGS1006_C1056_v05", "GSKHGS1006_C1057_v03", "GSKMEA115588")]
CreateTableNameLabels(conmed2)

# COnsolidate pre screening, pre-run in and pre-treatment initiation flags 
conmed2$GSKMEA115588$cmprior <-  with(conmed2$GSKMEA115588,
                                      if_else(prescrfl == "Y" | prerunfl == "Y" | runinfl == "Y", "Y", "N"))

conmed2 <- map(conmed2, function(x) {
  names(x)[names(x) %in% c("subjid","usubjid", "csid")] <- "subjid"
  names(x)[names(x) %in% c("dcl4c","who_atcc")] <- "who_atcc"
  names(x)[names(x) %in% c("dcl4t","whoterm")] <- "whoterm"
  x$subjid <- as.character(x$subjid)
  x <- x[,  !names(x) %in% c("mdra_llt", "mdra_hlt", "mdra_sct",
                             "prescrfl", "prerunfl", "runinfl")]
  x
})
CreateTableNameLabels(conmed2)
conmed2 <- BindRowsWLabels(conmed2)

# Create single conmed table
names_lkp <- c(
  "trial" = "trial",
  "subjid" = "subjid",
  "cmatc4" = "whoterm",
  "cmatccd" = "who_atcc",
  "cmcomp" = "cmcomp",
  "cmdecodr" = "cmdecod",
  "cmistdt" = "cmstdtc",
  "cmprior" = "cmprior",
  "cmroutcd" = "cmroutcd",
  "cmroute" = "cmroute"
)
names(conmed1) <- names_lkp[names(conmed1)]
conmed <- bind_rows(conmed1, conmed2)
conmed_lbls <- c(ExtractLabel(conmed1, return_object = TRUE),
      ExtractLabel(conmed2, return_object = TRUE))
conmed_lbls <- conmed_lbls[!duplicated(names(conmed_lbls))]
conmed <- MakeLabels(conmed, conmed_lbls)
rm(conmed1, conmed2)
rm(conmed_lbls)

# Select only randomised patients @SCREEN - lose trials here 
conmed <- conmed %>% 
  semi_join(demo)

# Examine conmed start dates
# the following variables indicate that a medication pre-dates the start of a trial
conmed_priorflag <- conmed %>%
  filter(cmprior %in% c("Y", "N") | !is.na(sday) | trial %in% c("GSKHGS1006_C1056_v05", "GSKHGS1006_C1057_v03"),
         !(whoterm == "" & who_atcc == "")) %>% 
  mutate(cmprior = if_else(is.na(cmprior), "", cmprior),
         sday = if_else(is.na(sday), 999, sday),
         prior_rand = cmprior == "Y" | sday <= 1 | trial %in% c("GSKHGS1006_C1056_v05", "GSKHGS1006_C1057_v03")) %>% 
  filter(prior_rand)
# For 4 trials need to look-up start dates
conmed_nopriorflag <- conmed %>%
  filter(!cmprior %in% c("Y", "N") | is.na(cmprior), is.na(sday), 
         ! trial %in% c("GSKHGS1006_C1056_v05", "GSKHGS1006_C1057_v03"),
         ! trial %in% "GSKB2C112060",
         !(whoterm == "" & who_atcc == ""))
conmed_nopriorflag$trial %>% unique()
# only 61 missing cmprior and only 6 missing date values where cmprior == "N" for GSKB2C112060 so ignore
# for GSKAVA102670_v02, GSKAVA102672_v02 and GSKAVA105640_V02  large numbers of 
# cmprior are missing, only recorded for tiny percentage,
xmn <- all_trials$conmed$GSKB2C112060
xmn <- all_trials$conmed$GSKAVA102670_v02
xmn <- all_trials$conmed$GSKAVA102672_v02
xmn <- all_trials$conmed$GSKAVA105640_V02
table(xmn$cmprior, if_else(is.na(xmn$cmstdt), "missing date", "not missing date"))

conmed_nopriorflag <- conmed_nopriorflag %>% 
  inner_join(demo %>% select(trial, subjid, randdt)) %>% 
  filter(as.double(cmstdtc) <= randdt)

# Conmed selected
conmed<- bind_rows(conmed_priorflag, conmed_nopriorflag) %>% 
  select(-cmstdtc, -cmprior, -sday, -visit, -prior_rand, -randdt)
rm(conmed_nopriorflag, conmed_priorflag)




## Medical history table ----
medhist <- all_trials$medhist
CreateTableNameLabels(medhist)

# Some are MedDRA coded, meddra keep as separate column
CreateTableNameLabels(medhist)
medhist <- map(medhist, function(x) {
  names(x)[names(x) %in% c("csid", "subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("mdra_llt", "mhllt")] <- "mhllt"
  names(x)[names(x) %in% c("mdra_sct", "mhsoc")] <- "mhsoc"
  names(x)[names(x) %in% c("mhoccur", "mhstat")] <- "mhstat"
  names(x)[names(x) %in% c("mhcat", "mhclass")] <- "mhcat"
  x$subjid <- as.character(x$subjid)
  x
})
CreateTableNameLabels(medhist)
medhist <- BindRowsWLabels(medhist, 1)


# Select whether term is meddra or non-meddra and create an indicator for which
medhist <- medhist %>% 
  mutate(meddra = if_else(!is.na(mhllt) | trial %in% c("GSK201315"), "meddra", "non-meddra"),
         mhterm = if_else(!is.na(mhllt), mhllt, mhterm)) 

# Create lookup category tables for meddra and non meddra
meddra_lkp <- medhist %>% 
  filter(meddra == "meddra") %>% 
  distinct(mhllt, mhlltcd, mdra_hlt,  mhsoc)

non_meddra_lkp <-  medhist %>% 
  filter(meddra == "non-meddra") %>% 
  distinct(mhterm, mhcat)

medhist <- medhist %>% 
  select(-mhllt, -mhlltcd, -mdra_hlt, -mhsoc, -mhcat)

# All tables are screening visit tables
table(medhist$visit) %>% addmargins()
table(medhist$mhstat) %>% addmargins()

# Select only current, past or missing for mhstat, missing mhstat's are 
# TWO trials have no mhstat variable GSKHGS1006_C1056_v05 GSKHGS1006_C1057_v03 
# GSKMEA115588 has an empty character string for 4% of entries, ignore
# COnfirmed that these do not have an indicator variable for even/never, and in long format
# so assume are present
# Already limited most tables to current or past and screening visit in earlier processing
medhist <- medhist  %>% 
  filter(is.na(mhstat) | mhstat %in% c("Current", "Past", "Y")) %>% 
  select(-visit, -visitnum, -mhstat)

n_distinct(medhist$trial) # 15
unique(medhist$subjid)
unique(demo$subjid)
unique(labs$subjid)
unique(conmed$subjid)

# Select only randomised patients @SCREEN
medhist <- medhist %>% 
  semi_join(demo)

n_distinct(medhist$trial) # 15

## Explore classification scheme for trials without meddra codes or with limited meddra codes
non_meddra <- medhist %>% 
  filter(meddra == "non-meddra") %>% 
  inner_join(non_meddra_lkp) %>%
  distinct(trial, mhcat) %>% 
  arrange(trial, mhcat) 

non_meddra %>% 
  group_by(trial) %>%
  summarise(n = n(), terms = paste(mhcat, collapse = ", "))

non_meddra_xmn <- non_meddra %>% 
  mutate(mhcat = str_to_lower(mhcat),
         value = "X") %>% 
  spread(mhcat, value, fill = "")
## None of the 9 non-meddra tables have meddra codes
setdiff(non_meddra$trial, medhist$trial[medhist$meddra == "meddra"])
## GSK classes
# There are the same GSK classes across all 9 trials, none map across to my drug-base comorbidity definitions





## Vitals table ----
vitals <- all_trials$vitals
CreateTableNameLabels(vitals)
# "no blood pressure measured from protocol" for 
vitals$GSKFFA112059$note
vitals$GSKMEA115588 #is in one row per observation format

# Most tables
vitals <- vitals [!names(vitals) %in% c("GSKFFA112059", "GSKMEA115588")]
CreateTableNameLabels(vitals)

vitals <- map(vitals, function(x) {
  names(x)[names(x) %in% c("csid", "subjid","usubjid")] <- "subjid"
  names(x)[names(x) %in% c("bpd", "diabp")] <- "diabp"
  names(x)[names(x) %in% c("bps", "sysbp")] <- "sysbp"
  x$subjid <- as.character(x$subjid)
  x
})
CreateTableNameLabels(vitals)
vitals <- BindRowsWLabels(vitals)

vitals_lng <-  all_trials$vitals$GSKMEA115588
vitals_lng$trial <- "GSKMEA115588"
vitals_lng <- vitals_lng %>% 
  mutate(paramcd = str_to_lower(paramcd)) %>% 
  filter(paramcd %in% c("diabp", "sysbp")) %>% 
  select(trial, subjid = usubjid, visit, paramcd, aval) %>% 
  group_by(paramcd) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  spread(key = paramcd, value = aval) %>% 
  select(-index)

vitals <- bind_rows(vitals, vitals_lng)
table(vitals$visit)

# Select only pre-randomisation visits @SCREEN
vits_visits <- tribble(
  ~"trial", ~"code",
  "GSK200109", "c('Screening', 'Baseline')",
  "GSK200110", "c('Screening', 'Baseline')",
  "GSK201315", "c('Screening', 'Baseline')",
  "GSKAVA102670_v02", "c('SCREEN', 'BASELINE')",
  "GSKAVA102672_v02", "c('SCREEN', 'BASELINE')",
  "GSKAVA105640_v02", "c('SCREEN', 'BASELINE')",
  "GSKB2C112060", "c('Screening')",
  "GSKDB2113360_v02", "c('Screening', 'Baseline')",
  "GSKHGS1006_C1056_v05", "c('Screening', 'Day 0')",
  "GSKHGS1006_C1057_v03", "c('Screening', 'Day 0')",
  "GSKMEA115588", "c('SCREENING', 'VISIT 2 (WEEK 0)')",
  "GSKFFA115285", "c('Visit 1 (Screening)')",
  "GSKFFR106080", "'VISIT 1'",
  "GSKAC4116135", "c('Screening', 'Baseline')"
)  # Missing "GSKDB2113374_v02", "c('Screening', 'Baseline')",

vits_visits <- by(vits_visits, vits_visits$trial, function(x) eval(parse(text = x$code)))
vits_visits <- stack(vits_visits) %>% 
  setNames(c("visit", "trial"))

n_distinct(vitals$trial) # 14, 1 trial has no vitals 
n_distinct(vits_visits$trial) # 14

vitals <- vitals %>% 
  semi_join(vits_visits)

n_distinct(vitals$trial) # 14

# Select only randmised participants @SCREEN
vitals <- vitals %>% 
  semi_join(demo)

n_distinct(vitals$trial)
setdiff(vitals$trial, vits_visits$trial)


## Smoking table ----
smoke <- all_trials$smoke
smoke <- BindRowsWLabels(smoke) %>% 
  mutate(subjid = as.character(subjid))

smoke_dem <- demo %>% 
  distinct(trial, subjid, susmhs)

smoke <- bind_rows(smoke, smoke_dem)

demo <- demo %>% 
  select(-susmhs)

## Re-organise tables so same as BI ----
bmi <- demo %>% 
  select(trial, subjid, ht, wt, bmi)
demo <- demo %>% 
  select(-ht, -wt, -bmi)
rand <- demo %>% 
  select(trial, subjid, trt, randdt)
demo <- demo %>% 
  select(-trt, -randdt)

## Add smoking data onto demo ----
demo <- demo %>% 
  left_join(smoke %>% 
              rename(smoke = susmhs) %>% 
              filter(!smoke == "") %>% 
              distinct(trial, subjid, .keep_all = TRUE))

## Convert data into a single object ----
gsk <- list(conmed = conmed, demo = demo, labs = labs, medhist = medhist,
            bp = vitals, bmi = bmi, rand = rand)
## rename and delete irrelevant variables so easier to match across companies ----
gsk$bp$visit <- NULL
# gsk$labs$newlab <- NULL

## 
gsk <- map(gsk, ~ .x %>%  rename(id = subjid))

# gather vitals to narrow
gsk$bmi <- gsk$bmi %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)

gsk$bp <- gsk$bp %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)

## Save gsk data ----
saveRDS(gsk, file = "GSK_processed_data/gsk.Rds")
write_csv(non_meddra_xmn, "Outputs/GSK_classes.csv")

rm(gsk)
gsk <- readRDS("GSK_processed_data/gsk.Rds")

# Check how many trials, should = 15
n_distinct(gsk$conmed$trial)  # 15
n_distinct(gsk$demo$trial)    # 15
n_distinct(gsk$labs$trial)    # 11 (4 trials have no labs)
n_distinct(gsk$medhist$trial) # 15
n_distinct(gsk$bp$trial)      # 14 (1 trial has no vitals table)
n_distinct(gsk$bmi$trial)     # 15
n_distinct(gsk$rand$trial)    # 15

gsk$medhist %>% 
  group_by(mhterm) %>% 
  summarise(n = n())
