#03_extract_bi_mi.R ## I have selected only baseline characteristics for
#randomised participants at this stage ## These are the typical MI trial, little
#in the way of medical history other than the usual suspects in CV trials, and either no conmeds
# or highly restricted conmed
# lso ahve no lab data

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

MakeNewBiFolderName <- function(foldername) {
  ## WARNING, I AM USING SUPERASSINGMENT FOR CONVENIENCE
  folder_out <<- paste0("Data/", foldername)
  if(!dir.exists(folder_out)) dir.create(folder_out)
  
  foldername <- paste0("E:/Research Project 1732/files_",
                       str_replace(foldername, "BI", "BI_"),
                       "/Files/")
  folder1 <<- paste0(foldername, "raw_oc/")
  folder2 <<- paste0(foldername, "ads_ads/")
  print("Created the following folders in the environement immediately above the function")
  list(folder1, folder2, folder_out)
}

LabelandGather <- function(mydf, dropvalue = TRUE) {
  ## Take labels from variable names, arranges data to long format and changes name to label
  mydf_lbl <- mydf %>% ExtractLabel(return_object = T)
  mydf <- mydf %>% 
    gather("term", "value", -adp_pid) %>% 
    mutate(term = mydf_lbl[term]) %>% 
    filter(! value == 0) %>% 
    distinct()
  if(dropvalue == TRUE) mydf %>% select(-value) else mydf
}

# Extract all files 

foldername <- "BI1123_10" # @MISSING extract #----


ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

foldername <- FindFx(ptrn)
folderout <- "D:/BI1123_10/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))

### The following all come from the baseline table pat
### Demo
patd  <- read_sas(paste0(folder1, "pat.sas7bdat")) %>% names_to_lower()
demo <- patd %>% 
  select(adp_pid, age, sex, racec) %>% 
  mutate(sex = factor(sex, 1:2, c("male", "female")),
         racec = factor(racec, 1:4 , c("caucasian", "african descent", "asian", "other")))

# Smoking, all are visit 1
smoke <- patd %>%
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))

# vitals
bp <- patd %>% 
  select(adp_pid, sys, dia)

# Vitals, some missing height variables (<10%)
htwt <- patd %>% 
  select(adp_pid, wtstd, htstd)


## Randomisation table, for some reason treatment arm (tpatt) has been redacted from all the tables
rand <-  read_sas(paste0(folder1, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  # filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt)

## medhist, note is a baseline table
medhist <- read_sas(paste0(folder1, "medh.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, miprev:dmel)
medhist <- LabelandGather(medhist)

## Conmed - all are baseline
conmed <- read_sas(paste0(folder1, "med.sas7bdat")) %>% names_to_lower()
conmed <- conmed %>% 
  select(adp_pid, mdtk1:mdtk14) %>% 
  distinct()
conmed <- LabelandGather(conmed)

# adverse events, strange back and forward with names
ae1 <- read_sas(paste0(folder1, "aeaeb.sas7bdat")) %>% names_to_lower()
ae1 <- ae1  %>%
  select(adp_pid, actevent, visdt, aeptd, aesocd)
ae2 <- read_sas(paste0(folder1, "aeaec.sas7bdat")) %>% names_to_lower()
ae2 <- ae2  %>%
  select(adp_pid, actevent, visdt, aeptit, aesoc)
ae3 <- read_sas(paste0(folder1, "aeblaeb.sas7bdat")) %>% names_to_lower()
ae3 <- ae3  %>%
  select(adp_pid, actevent, visdt, aeptd, aesocd)
ae4 <- read_sas(paste0(folder1, "aeblaec.sas7bdat")) %>% names_to_lower()
ae4 <- ae4  %>%
  select(adp_pid, actevent, visdt, aeptit, aesoc)
## nil there all redacted
all_ae <- list(ae1, ae2, ae3, ae4)
rm(ae1, ae2, ae3, ae4)
all_ae[] <- map(all_ae, ~ set_names(.x, names(all_ae[[1]])))
all_ae <- bind_rows(all_ae)
all_ae <- all_ae %>% distinct()
ae <- all_ae

# labs
stat <- read_sas(paste0(folder2, "_stat.sas7bdat")) %>% names_to_lower()
labs <- tibble(note = "No lab results located. Sampling at least for haematology is mentioned in the CSR")


## save data
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folder_out, "/extract.Rdata"))
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)

foldername <- "BI1123_11" # extract #----


ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

foldername <- FindFx(ptrn)
folderout <- "D:/BI1123_11/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))

### The following all come from the baseline table pat
### Demo
patd  <- read_sas(unz(foldername, paste0("raw_oc/", "pat.sas7bdat"))) %>% names_to_lower()
demo <- patd %>% 
  select(adp_pid, age, sex, racec) %>% 
  mutate(sex = factor(sex, 1:2, c("male", "female")),
         racec = factor(racec, 1:4 , c("caucasian", "african descent", "asian", "other")))

# Smoking, all are visit 1
smoke <- patd %>%
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))

# vitals
bp <- patd %>% 
  select(adp_pid, sys, dia)

# Vitals, some missing height variables (<10%)
htwt <- patd %>% 
  select(adp_pid, wtstd, htstd)


## Randomisation table, for some reason treatment arm (tpatt) has been redacted from all the tables
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "rand.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  # filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt)

## medhist, note is a baseline table
medhist <- read_sas(unz(foldername, paste0("raw_oc/", "medh.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, miprev:dmel)
medhist <- LabelandGather(medhist)

## Conmed - all are baseline
conmed <- read_sas(unz(foldername, paste0("raw_oc/", "med.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>% 
  select(adp_pid, mdtk1:mdtk14) %>% 
  distinct()
conmed <- LabelandGather(conmed)

# adverse events, strange back and forward with names
ae1 <- read_sas(unz(foldername, paste0("raw_oc/", "aeaeb.sas7bdat"))) %>% names_to_lower()
ae1 <- ae1  %>%
  select(adp_pid, actevent, visdt, aeptd, aesocd)

ae2 <- read_sas(unz(foldername, paste0("raw_oc/", "aeaec.sas7bdat"))) %>% names_to_lower()
ae2 <- ae2  %>%
  select(adp_pid, actevent, visdt, aeptit, aesoc)

ae3 <- read_sas(unz(foldername, paste0("raw_oc/", "aeblaeb.sas7bdat"))) %>% names_to_lower()
ae3 <- ae3  %>%
  select(adp_pid, actevent, visdt, aeptd, aesocd)

ae4 <- read_sas(unz(foldername, paste0("raw_oc/", "aeblaec.sas7bdat"))) %>% names_to_lower()
ae4 <- ae4  %>%
  select(adp_pid, actevent, visdt, aeptit, aesoc)

## nil there all redacted
all_ae <- list(ae1, ae2, ae3, ae4)
rm(ae1, ae2, ae3, ae4)
all_ae[] <- map(all_ae, ~ set_names(.x, names(all_ae[[1]])))
all_ae <- bind_rows(all_ae)
all_ae <- all_ae %>% distinct()
ae <- all_ae

# labs
labs <- tibble(note = "No lab results located. Sampling at least for haematology is mentioned in the CSR")


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, patd, all_ae)
rm(folderout, foldername, ptrn)



foldername <- "BI1123_12" # @MISSING extract #----

# MakeNewBiFolderName(foldername)

ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

foldername <- FindFx(ptrn)
folderout <- "D:/BI1123_12/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))

### The following all come from the baseline table pat
### Demo
patd  <- read_sas(paste0(folder1, "pat.sas7bdat")) %>% names_to_lower()
demo <- patd %>% 
  select(adp_pid, age, sex, racec) %>% 
  mutate(sex = factor(sex, 1:2, c("male", "female")),
         racec = factor(racec, 1:4 , c("caucasian", "african descent", "asian", "other")))

# Smoking, all are visit 1
smoke <- patd %>%
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))

# vitals
bp <- patd %>% 
  select(adp_pid, sys, dia)

# Vitals, some missing height variables (<10%)
htwt <- patd %>% 
  select(adp_pid, wtstd, htstd)


## Randomisation table, for some reason treatment arm (tpatt) has been redacted from all the tables
rand <-  read_sas(paste0(folder1, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  # filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt)

## medhist, note is a baseline table
medhist <- read_sas(paste0(folder1, "medh.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, dmel, miprev:hypert, chf:pcip)
medhist <- LabelandGather(medhist)

## Conmed - all are baseline
conmed <- read_sas(paste0(folder1, "medh.sas7bdat")) %>% names_to_lower()
conmed <- conmed %>% 
  select(adp_pid, mdtk1:mdtk7) %>% 
  distinct()
conmed <- LabelandGather(conmed)

# adverse events, strange back and forward with names
ae <- read_sas(paste0(folder2, "_ae.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, visdt, mpt, msoc)

# labs
labs <- tibble(note = "No lab results located. Sampling at least for haematology is mentioned in the CSR")


## save data
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folder_out, "/extract.Rdata"))
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, patd, all_ae)

foldername <- "BI1123_28" # @MISSING extract #----
#MakeNewBiFolderName(foldername)

## reviewed all dataframes in folder 1 and folder 2 because documentation is inadequate
## All relevant baslein cahracteristics are in _stat in folder 1
## CAn get outcomes, bleeding and AE from there in due course

# folder1_lst <- setdiff(list.files(folder1), "r_processing")
# 
# folder1_vars <- map(paste0(folder1, folder1_lst), ~ .x %>% 
#       read_sas() %>%
#       names_to_lower() %>%
#       ExtractLabel(return_object = T)
#     )
# names(folder1_vars) <- folder1_lst
# a <- tibble(folder2_vars$`_stat.sas7bdat` %>%  names(),
#        folder2_vars$`_stat.sas7bdat`) 


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Take folder name and format for findfx function
ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

# Get file path for trial
foldername <- FindFx(ptrn)

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))


stat <- paste0(folder2, "_stat.sas7bdat") %>% 
  read_sas() %>%
  names_to_lower() %>% 
  select(adp_pid, sex, racec, age, randdt, sys, dia, 
         # miprevu, chfpu,  cabgu, pcipu, hypertu, dmelu, renalfpu, copdpu, ## these are the same as ones below
         miprev, chfp, cabg, pcip, hypert, dmel, renalfp, copdp, 
         htstd, weight, 
         creatstd, crcl, glucose, hgb1, plat1)

### The following all come from the baseline table pat
### Demo
demo <- stat %>% 
  select(adp_pid, age, sex, racec) %>% 
  mutate(sex = factor(sex, 1:2, c("male", "female")),
         racec = factor(racec, 1:4 , c("caucasian", "african descent", "asian", "other")))

# Smoking, all are visit 1
smoke <- tibble(note = "No smoking status data recorded")
  
# vitals
bp <- stat %>% 
  select(adp_pid, sys, dia)

# Vitals, some missing height variables (<10%)
htwt <- stat %>% 
  select(adp_pid, wtstd = weight, htstd)

## Randomisation table, for some reason treatment arm (tpatt) has been redacted from all the tables
rand <-  paste0(folder1, "pre_hosp1.sas7bdat") %>% 
  read_sas() %>%
  names_to_lower()
rand <- rand %>% 
  # filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, randtrtc, randdt) %>% 
  mutate(randtrtc = factor(randtrtc, 1:2, c("Tenecteplase", "pci")))

## medhist, note is a baseline table
medhist <- stat %>% 
  select(adp_pid, miprev, chfp, cabg, pcip, hypert, dmel, renalfp, copdp)
medhist <- LabelandGather(medhist)

## Conmed - all are baseline
conmed <- tibble(note = "No concomittant medication, acute treatment drugs only")

# adverse events, strange back and forward with names
ae <- read_sas(paste0(folder2, "aes.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, aeondt, mpt, msoc)

# labs
## note here are renaming creatinine so taking standard unit (mmol/L)
labs <- stat %>%
  select(adp_pid, creat = creatstd, crcl, glucose, hgb1, plat1)
labs <- LabelandGather(labs, dropvalue = FALSE)

## save data
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folder_out, "/extract.Rdata"))
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, stat)

## Combine all files into a single dataframe ----
# List folder names
foldernames <- list("BI1123_11") # MISSING TRIALS: "BI1123_10", "BI1123_12", "BI1123_28" 

# Check all have saveddata
a <- map(foldernames, function(foldername) list.files(paste0("D:/", foldername), patt = "extract"))
names(a) <- foldernames
a

# Read all data into a big list
all_trials <- map(foldernames, function(foldername){
  assign(foldername, new.env())
  load(paste0("D:/", foldername, "/extract.Rdata"), envir = get(foldername))
  as.list(get(foldername))
})
names(all_trials) <- foldernames

# Drop adverse events
all_trials <- map(all_trials, ~ .x[!names(.x) == "ae"])

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)

## Randomisation table ----
rand <- all_trials$rand
CreateTableNameLabels(rand)
names(rand)

rand <- map(rand, function(x) {
  names(x)[names(x) %in% c("randdt", "visdt")] <- "randdt"
  names(x)[names(x) %in% c("tpatt", "randtrtc")] <- "tpatt"
  x %>%  select("adp_pid", "randdt", "tpatt")
})
rand <- BindRowsWLabels(rand)

# Cannot use "" to limit to randomised patients, as this variable has been redacted
# rand <- rand %>% 
#   filter(tpatt != "")

## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo)

demo <- BindRowsWLabels(demo, 1)
# limit to randomised patients
demo <- demo %>% 
  semi_join(rand) %>% # @Screen
  rename(race = racec)

smoke <- all_trials$smoke
smoke$BI1123_28 <- NULL

smoke <- BindRowsWLabels(smoke) %>% 
  rename(smoke = smokcd)

demo <- demo %>% 
  left_join(smoke)

## Labs table ----
labs <- all_trials$labs["BI1123_28"]

## Join all into single table, drop first those without any data
labs <- BindRowsWLabels(labs)

## Limit to randomised patients
labs <- labs %>% 
  semi_join(rand) 

labs_lkp <- c(
  "Serum creatinine" = "CRE",
  "Serum creatinine [mcmol/L]" = "CRE",
  "Serum creatinine clearance [ml/min]" = "EGFR",
  "Blood glucose level" = "GLU",
  "Haemoglobin (first available value)" = "HGB", 
  "Platelets (first available value)" = "PLTCT")

labs <- labs %>% 
  rename(labnmx = term, labstd = value) %>% 
  mutate(labnm = labs_lkp[labnmx])

## Medical history table ----
medhist <- all_trials$medhist

medhist <- medhist %>% 
  BindRowsWLabels(1) %>% 
  mutate(meddra = "non-meddra")

## Concomittant medicines table ----
conmed <- all_trials$conmed
conmed$BI1123_28 <- NULL

CreateTableNameLabels(conmed)
conmed <- BindRowsWLabels(conmed)
conmed$term %>% unique()
## Drop conmed as it is clearly not going to be useful

## BMI ----
bmi <- all_trials$htwt
bmi <- BindRowsWLabels(bmi)
tapply(bmi$htstd, bmi$trial, mean, na.rm = TRUE)
tapply(bmi$wtstd, bmi$trial, mean, na.rm = TRUE)
bmi <- bmi %>% 
  mutate(bmi = wtstd/ (htstd/100)^2) %>% 
  rename(ht = htstd, wt = wtstd)

bmi <- bmi %>% 
  gather(key = "param", "value", -trial, -adp_pid)

## BP ----
bp <- all_trials$bp
bp <- BindRowsWLabels(bp)
bp <- bp %>% 
  semi_join(rand)

bp <- bp %>% 
  gather(key = "param", "value", -trial, -adp_pid)

## Convert data into a single object ----
bi <- list(demo = demo, labs = labs,
           medhist = medhist,
           bp = bp, bmi = bmi, rand = rand)
bi$labs <- NULL
bi <- map(bi, ~ .x %>% rename(id = adp_pid))


## Read in bi_new in order to simplify the inclusion of these 4 trials ----
bi_new <- readRDS(file = "Processed_data/bi_new_partial.Rds")

bi_new$demo <- bind_rows(bi_new$demo, bi$demo)
bi_new$medhist <- bind_rows(bi_new$medhist, bi$medhist)
bi_new$bp <- bind_rows(bi_new$bp, bi$bp)
bi_new$bmi <- bind_rows(bi_new$bmi, bi$bmi)
bi_new$rand <- bind_rows(bi_new$rand, bi$rand)
# Do not need to bind lab data as there is none for the single trial in this script

saveRDS(bi_new, file = "Processed_data/bi_new.Rds")
