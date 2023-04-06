## 09 UCB trials

### I have selected only baseline conmeds and BMI data
### I have also extracted outcome data
### Also excluded screen failures at this stage
### All are MEDDRA
### THe lab, conmed and adverse event data have been redacted
### BP has been redacted

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

UcbMakeName <- function(foldername){
  fileoutname <- str_locate(stringi::stri_reverse(foldername), fixed("/"))
  fileoutname <- fileoutname[1 , "start"]
  paste0("Data/UCB/", str_sub(foldername, str_length(foldername) - fileoutname + 2, -6),
         ".Rds")
}

MakeNewLillyFolderName <- function(studyname) {
  ## WARNING, I AM USING SUPERASSINGMENT FOR CONVENIENCE
  folderout <<- paste0("D:/", str_replace_all(studyname, "-", "_"), "/")
  if(!dir.exists(folderout)) dir.create(folderout)
  
  foldername <<- paste0("C:/Working/5483 - CSDR 1732/files_", studyname, ".zip")
  
  print("Created link to the following folders in the environement immediately above the function")
  list(paste0("Foldername:  ", foldername), paste0("Folderout:  ", folderout))
}


folderout <- paste0("D:/", foldername, "/")
ptrn <- str_sub(foldername, -5)
foldername <- FindFx()



## File locations
datastore <- "e:/Research Project 1732"
all_trials <- list.files(datastore, full.names = T)
ucb_trials <- all_trials[str_detect(all_trials, "UCB")]

# Extract all files 
foldername <- "files_UCB-RP1732-AS0001/Files/AS0001 ADaM" # ----
fileoutname <- UcbMakeName(foldername)
foldername <- paste0(datastore, "/",foldername, "/")
list.files(foldername, recursive = TRUE)

all_base <-  read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()

# Randomisation
rand <- all_base %>% 
  select(usubjid, arm, trtsdt)  %>% 
  filter(arm != "SCREEN FAILURE")

# Demographics
demo <- all_base %>% 
  select(usubjid, ageband, sex) %>% 
  semi_join(rand)

# Medical history
medhist <- read_sas(paste0(foldername, "admh.sas7bdat")) %>% names_to_lower()

medhist_meddra <- medhist %>%
  filter(mhcat %in% c("GENERAL", "PROCEDURE")) %>%
  select(usubjid, mhdecod, mhllt, mhdtc, mhsdt) 
# no missing start dates any(is.na(medhist_meddra$mhsdt))

medhist_meddra <- medhist_meddra %>% 
  inner_join(rand %>%  select(-arm)) %>%
  filter(mhsdt <= trtsdt) %>% 
  select(usubjid, mhdecod, mhllt)

## Long format medical history with yes/no; 4 conditions only - IBD, psoriasis, reactive arthritis, uveitis
medhist_yn <- medhist %>% 
  filter(!mhcat %in% c("GENERAL", "PROCEDURE"), mhoccur == "Y") %>%
  select(usubjid, mhcat, mhdtc, mhsdt)  %>% 
  inner_join(rand %>%  select(-arm)) %>%
  filter(mhsdt <= trtsdt) %>% 
  select(usubjid, mhcat) %>% 
  distinct()

# All medhist terms appear in meddra codes too
xmn <- medhist_yn %>% 
  inner_join(medhist_meddra)
rm(medhist_yn, xmn)

medhist <- medhist_meddra %>% 
  semi_join(rand)
rm(medhist_meddra)

# Conmed
conmed <- NULL

# Vitals
bmi <- all_base %>% 
  select(usubjid, height, weight, bmi) %>% 
  semi_join(rand)

# Smoking, all are visit 1
smoke <- all_base %>% 
  select(usubjid, tobacco) %>% 
  semi_join(rand)

# Adverse not included in dataset
# adverse <- read_sas(paste0(foldername, "adea.sas7bdat")) %>% names_to_lower()

## Various datasets
# # MRI data
# # admri    <-  read_sas(paste0(foldername, "admri.sas7bdat")) %>% names_to_lower()
# # Visits table
# adsv     <-   read_sas(paste0(foldername, "adsv.sas7bdat")) %>% names_to_lower()
# # Outcome data  for selected outcome (ASDAS)
adasas   <- read_sas(paste0(foldername, "adasas.sas7bdat")) %>% names_to_lower()
outcome <- adasas %>% 
  select(usubjid, visit, visitnum, adt, param, paramcd, aval, avalc)
## Other outcome data
## Outcome data 2
# adbasdai   <- read_sas(paste0(foldername, "adbasdai.sas7bdat")) %>% names_to_lower()
## Outcome data 3
# adbasfi  <- read_sas(paste0(foldername, "adbasfi.sas7bdat")) %>% names_to_lower()
# adbasdai   <- read_sas(paste0(foldername, "adbasdai.sas7bdat")) %>% names_to_lower()
## Outcome data 4
# adbasmi  <- read_sas(paste0(foldername, "adbasfi.sas7bdat")) %>% names_to_lower()


saveRDS(list(conmed = NULL, demo = demo, labs = NULL, medhist = medhist,
     bp = NULL, bmi = bmi, rand = rand, smoke = smoke, outcome = outcome, ae = NULL),
     fileoutname)

foldername <- "files_UCB-RP1732-C87085/Files/C87085 ADaM" # ----
fileoutname <- UcbMakeName(foldername)
foldername <- paste0(datastore, "/",foldername, "/")

list.files(foldername)

analysis_df <-  read_sas(paste0(foldername, "effcdai.sas7bdat")) %>% names_to_lower()
baseline <-  read_sas(paste0(foldername, "effhbi.sas7bdat")) %>% names_to_lower()

# Randomisation, note no screen failures in this set
rand <- baseline %>% 
  filter(visitl == "Week 0") %>% 
  select(usubjid, treatl, treatdt = visdt)

# Demographics
demo <- baseline %>% 
  filter(visitl == "Week 0") %>% 
  select(usubjid, ageband, sex, race, racel) %>% 
  semi_join(rand)

# Medical history
# Is meddra
# all start dates are before the first injection - all(range(medhist$finjdt - medhist$strtdt, na.rm = T) >= 0)
# So is a "baseline" table
medhist <- read_sas(paste0(foldername, "hismed.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  distinct(usubjid, mdptnm, mdlltnm) %>% 
  semi_join(rand)

# Conmed
conmed <- NULL

# Vitals
bmi <- baseline %>% 
  select(usubjid, hei, wei, bmi) %>% 
  semi_join(rand) %>% 
  group_by(usubjid) %>% 
  summarise_all(mean)


# Smoking, all are visit 1
smoke <- NULL

# Adverse events
ae <- NULL

# # Outcome data  for selected outcome (ASDAS), ibdq, clinical remission, corticosteroid free remission
# EFFIBDQ CIBTQ
# effcdai RED100, REM150
effcdai<- read_sas(paste0(foldername, "effcdai.sas7bdat")) %>% names_to_lower()
effcdai <- effcdai %>% 
  distinct(usubjid, red100, rem150, visdt)

effibdq <- read_sas(paste0(foldername, "effibdq.sas7bdat")) %>% names_to_lower()
effibdq <- effibdq %>% 
  select(usubjid, visdt, ibdq)

outcome <- list(effcdai, effibdq)
rm(effcdai, effibdq)


saveRDS(list(conmed = NULL, demo = demo, labs = NULL, medhist = medhist,
             bp = NULL, bmi = bmi, rand = rand, smoke = smoke, outcome = outcome, ae = NULL),
        fileoutname)

foldername <- "files_UCB-RP1732-N01252/Files/N01252 ADaM" # ----
fileoutname <- UcbMakeName(foldername)
foldername <- paste0(datastore, "/",foldername, "/")

list.files(foldername)

baseline <-  read_sas(paste0(foldername, "patpat.sas7bdat")) %>% names_to_lower()

# Randomisation, note no screen failures in this set
rand <- baseline %>% 
  filter(!seqtrtl == "") %>% 
  select(sbjnbr, seqtrtl, rnddte)

# Demographics
demo <- baseline %>% 
  select(sbjnbr, age, gdrl, rcel) %>% 
  semi_join(rand)

# Medical history
# Is meddra
# all start dates are before the first injection - all(range(medhist$finjdt - medhist$strtdt, na.rm = T) >= 0)
# So is a "baseline" table
medhist <- read_sas(paste0(foldername, "hisgmp.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  distinct(sbjnbr, gmppt, gmpllt, gmpbegdf) 
## Convert date of diagnosis to a date
medhist <- medhist %>% 
  mutate(gmpbegdf = if_else(gmpbegdf %in% c("UN", "", "00-FEP-2001", "UNK-2005"),
          "01JAN1990", gmpbegdf),
         yr = str_sub(gmpbegdf, -4),
         mnth = str_sub(gmpbegdf, -7, -5),
         dy = str_sub(gmpbegdf, -9, -8),
         mnth = if_else(mnth == "", "JUN", mnth),
         dy = if_else(dy == "", "15", dy),
         strt_dt = lubridate::ymd(paste(yr, mnth, dy, sep = "|"))) 
## Loses one diagnosis only
medhist <- medhist %>% 
  inner_join(rand %>%  select(sbjnbr, rnddte)) %>% 
  filter(strt_dt <= rnddte) %>% 
  select(sbjnbr, gmpllt, gmppt) %>% 
  distinct()

# Conmed
conmed <- NULL

# Vitals
bmi <- baseline %>% 
  select(sbjnbr, heibst, weibst, bmibst) %>% 
  semi_join(rand)


# Smoking, all are visit 1
smoke <- NULL

# Adverse events
ae <- NULL

# # Outcome data  for selected outcome seizures
# Crude approach simply counted seizures
effszdr <- read_sas(paste0(foldername, "effszdr.sas7bdat")) %>% names_to_lower()
outcome <- effszdr %>% 
  filter(szdrszpr == "Yes") %>% 
  group_by(sbjnbr) %>% 
  count()
  
rm(effszdr)


saveRDS(list(conmed = NULL, demo = demo, labs = NULL, medhist = medhist,
             bp = NULL, bmi = bmi, rand = rand, smoke = smoke, outcome = outcome, ae = NULL),
        fileoutname)


foldername <- "files_UCB-RP1732-N01253/Files/N01253 ADaM" # ----
fileoutname <- UcbMakeName(foldername)
foldername <- paste0(datastore, "/",foldername, "/")

list.files(foldername)

baseline <-  read_sas(paste0(foldername, "patpat.sas7bdat")) %>% names_to_lower()

# Randomisation, note no screen failures in this set
rand <- baseline %>% 
  filter(!seqtrtl == "") %>% 
  select(sbjnbr, seqtrtl, rnddte)

# Demographics
demo <- baseline %>% 
  select(sbjnbr, age, gdrl, rcel) %>% 
  semi_join(rand)

# Medical history
# Is meddra
# all start dates are before the first injection - all(range(medhist$finjdt - medhist$strtdt, na.rm = T) >= 0)
# So is a "baseline" table
medhist <- read_sas(paste0(foldername, "hisgmp.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  distinct(sbjnbr, gmppt, gmpllt, gmpbegdf) 

## Convert date of diagnosis to a date
medhist <- medhist %>% 
  mutate(gmpbegdf = if_else(gmpbegdf %in% c("UN", "", "00-FEP-2001", "UNK-2005"),
                            "01JAN1990", gmpbegdf),
         yr = str_sub(gmpbegdf, -4),
         mnth = str_sub(gmpbegdf, -7, -5),
         dy = str_sub(gmpbegdf, -9, -8),
         mnth = if_else(mnth == "", "JUN", mnth),
         dy = if_else(dy == "", "15", dy),
         strt_dt = lubridate::ymd(paste(yr, mnth, dy, sep = "|"))) 
## Loses one diagnosis only
medhist <- medhist %>% 
  inner_join(rand %>%  select(sbjnbr, rnddte)) %>% 
  filter(strt_dt <= rnddte) %>% 
  select(sbjnbr, gmpllt, gmppt) %>% 
  distinct()

# Conmed
conmed <- NULL

# Vitals
bmi <- baseline %>% 
  select(sbjnbr, heibst, weibst, bmibst) %>% 
  semi_join(rand)


# Smoking, all are visit 1
smoke <- NULL

# Adverse events
ae <- NULL

# # Outcome data  for selected outcome seizures
# Crude approach simply counted seizures
effszdr <- read_sas(paste0(foldername, "effszdr.sas7bdat")) %>% names_to_lower()
outcome <- effszdr %>% 
  filter(szdrszpr == "Yes") %>% 
  group_by(sbjnbr) %>% 
  count()

rm(effszdr)


saveRDS(list(conmed = NULL, demo = demo, labs = NULL, medhist = medhist,
             bp = NULL, bmi = bmi, rand = rand, smoke = smoke, outcome = outcome, ae = NULL),
        fileoutname)

foldername <- "files_UCB-RP1732-PSA001/Files/PSA001 ADaM" # ----
fileoutname <- UcbMakeName(foldername)
foldername <- paste0(datastore, "/",foldername, "/")

list.files(foldername)

all_base <-  read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()
# Randomisation
rand <- all_base %>% 
  select(usubjid, arm, trtsdt)  %>% 
  filter(!arm %in% c("SCREEN FAILURE", "Screen Failure"))

# Demographics
demo <- all_base %>% 
  select(usubjid, ageband, sex) %>% 
  semi_join(rand)

# Medical history
medhist <- read_sas(paste0(foldername, "admh.sas7bdat")) %>% names_to_lower()

medhist_meddra <- medhist %>%
  filter(mhcat %in% c("GENERAL", "PROCEDURE")) %>%
  select(usubjid, mhdecod, mhllt, mhdtc, mhsdt) 
# no missing start dates any(is.na(medhist_meddra$mhsdt))
medhist_meddra <- medhist_meddra %>% 
  inner_join(rand %>%  select(-arm)) %>%
  filter(mhsdt <= trtsdt) %>% 
  select(usubjid, mhdecod, mhllt)

medhist <- medhist_meddra %>% 
  semi_join(rand)
rm(medhist_meddra)

# Conmed
conmed <- NULL

# Vitals
bmi <- all_base %>% 
  select(usubjid, height, weight, bmi) %>% 
  semi_join(rand)

# Smoking, all are visit 1
smoke <- all_base %>% 
  select(usubjid, tobacco) %>% 
  semi_join(rand)

# Adverse not included in dataset
# adverse <- read_sas(paste0(foldername, "adea.sas7bdat")) %>% names_to_lower()

## Various datasets

## Outcome data , needed
# adacr - Analysis data for ACR [ie American College of Rheumatology] (ADACR)
# adpasi - Analysis data for PASI Dataset (ADPASI)
# adasf36 - HAS BEEN REDACTED
## Outcome data, not needed
# adhaqdi - Analysis data for HAQ-DI |
# admtss - Analysis data for mTSS Dataset (ADMTSS)



adacr   <- read_sas(paste0(foldername, "adacr.sas7bdat")) %>% names_to_lower()
adacr <- adacr %>% 
  select(usubjid, visit, visitnum, param, paramcd, aval, avalc)
adpasi   <- read_sas(paste0(foldername, "adpasi.sas7bdat")) %>% names_to_lower()
adpasi <- adpasi %>% 
  select(usubjid, visit, visitnum, param, paramcd, aval, avalc)

outcome <- list(adacr, adpasi)

saveRDS(list(conmed = NULL, demo = demo, labs = NULL, medhist = medhist,
             bp = NULL, bmi = bmi, rand = rand, smoke = smoke, outcome = outcome, ae = NULL),
        fileoutname)

# Consolidate all files ----
## The following consolidates the data and ensures all matches

## Combine all files into a single dataframe ----
# Read all data into a big list
all_trials <- map(list.files("Data/UCB", full.names = TRUE) , readRDS)
names(all_trials) <- list.files("Data/UCB")  %>%  str_sub(1, -5)

# Identify redacted data
redacted <- map(all_trials, ~ map_lgl(.x, is.null))
names(redacted) <- names(all_trials)
redacted <- do.call(cbind, redacted)
write.csv(redacted %>%  as.data.frame(), "Outputs/ucb_redacted_tables.csv")

# Drop adverse events and outcome tables for now (note AE has been redacted)
all_trials <- map(all_trials, ~ .x[!names(.x) %in% c("ae", "outcome")])

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)

# Remove redacted tables
all_trials <- all_trials[c("demo", "medhist", "bmi", "rand", "smoke")]

## Randomisation table ----
rand <- all_trials$rand
CreateTableNameLabels(rand)

rand <- map(rand, function(x) {
  names(x)[names(x) %in% c("sbjnbr", "usubjid")] <- "usubjid"
  names(x)[names(x) %in% c("arm", "seqtrtl", "seqtrtl", "treatl" )] <- "arm"
  names(x)[names(x) %in% c("rnddte", "treatdt", "trtsdt")] <- "trtsdt"
  x[ , names(x) %in% c("trial", "usubjid", "arm", "trtsdt")]
})
## Note one single row has a missing treatment start date, should just ignore this
rand <- bind_rows(rand, .id = "trial")

## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo)

# Rename age variable
demo <- map(demo, function(x) {
  names(x)[names(x) %in% c("sbjnbr", "usubjid")] <- "usubjid"
  names(x)[names(x) == "gdrl"] <- "sex"
  names(x)[names(x) %in% c("racel", "rcel")] <- "racel"
  x[ , names(x) %in% c("usubjid", "age","ageband", "sex", "racel")]
})
CreateTableNameLabels(demo)
demo[] <- map(demo, ~ map(.x, as.character))

CreateTableNameLabels(demo)
demo <- BindRowsWLabels(demo, 1)

# Convert age-bands to age
demo <- demo %>% 
  mutate(ageband = ConvertAge(ageband),
         age = if_else(is.na(age), ageband, as.integer(age))) %>% 
  select(-ageband)

tapply(demo$age, demo$trial, stem)

## Medical history table ----
medhist <- all_trials$medhist
CreateTableNameLabels(medhist)

medhist <- map(medhist, function(x) {
  names(x)[names(x) %in% c("sbjnbr", "usubjid")] <- "usubjid"
  names(x)[names(x) %in% c("gmpllt", "mdlltnm", "mhllt")] <- "mhllt"
  names(x)[names(x) %in% c("gmppt", "mdptnm", "mhdecod")] <- "mhdecod"
  x[ , names(x) %in% c("usubjid", "mhllt", "mhdecod")]
})

medhist <- BindRowsWLabels(medhist, 1)

## BMI ----
bmi <- all_trials$bmi
CreateTableNameLabels(bmi)

bmi <- map(bmi, function(x) {
  names(x)[names(x) %in% c("sbjnbr", "usubjid")] <- "usubjid"
  names(x)[names(x) %in% c("bmi", "bmibst")] <- "bmi"
  names(x)[names(x) %in% c("hei", "heibst", "height")] <- "height"
  names(x)[names(x) %in% c("wei", "weibst", "weight")] <- "weight"
    x[ , names(x) %in% c("usubjid", "height", "weight", "bmi")]
})
## Note height for C87085 is in m and rest are in cm, multiply the m one by 100
## All weights are in kg

bmi$C87085$height <- 100*bmi$C87085$height

bmi <- BindRowsWLabels(bmi, 1)

## Smoking ----
smoke <- all_trials$smoke
smoke <- smoke[!map_lgl(smoke, is.null)]
smoke <- BindRowsWLabels(smoke, 1)

## Note missing BMI values have a missing height or a missing weight or BOTH missing

## Create single object and consolidate ----
# Note all except medhist is a single row per person
ucb <- list(demo = demo, medhist = medhist, bmi = bmi, rand = rand, smoke = smoke)
ucb <- map(ucb, ~ .x %>% semi_join(rand))

saveRDS(ucb, file = "Processed_data/ucb.Rds")
