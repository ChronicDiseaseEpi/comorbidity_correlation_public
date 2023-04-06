##01_extract_bi

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

#FolderRaw <- function(myfoldername, ads = TRUE){
#    if(ads == TRUE) {
#                str_replace("E:/Research Project 1732/files_BI_XXXXXXX/Files/ads_ads/",
#                          "XXXXXXX",
#                          str_sub(myfoldername, 3)) } else
#                str_replace("E:/Research Project 1732/files_BI_XXXXXXX/Files/raw_oc/",
#                            "XXXXXXX",
#                          str_sub(myfoldername, 3))      
#}

#foldername <- paste0("Data/", foldername, "/")
#foldername <- "V:/"
#folderout <- "D:/BI248_524/"


## Extract all files
foldername <- "BI1160_26"  # @MISSING extract # ----

ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

foldername <- FindFx(ptrn)
folderout <- "D:/BI1160_26/"

ct  <- read_sas(unz(foldername, "ct.sas7bdat")) %>% names_to_lower()
rand    <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
medhist <- read_sas(paste0(foldername, "medhist.sas7bdat")) %>% names_to_lower()
vitals  <- read_sas(paste0(foldername, "phys.sas7bdat")) %>% names_to_lower()
basco <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### Randomisation data
rand <- rand %>% 
  select(adp_pid, randdt, tpatt, study)

### conmed data
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt,adp_ctpn, adp_ctpncd, adp_wver)

### Medical history
medhist <- medhist %>% 
  select(adp_pid, stroke, tia, ncse, mi, hrtfail, diab,
         cad, hypt, aftype, otherca, pvd, 
         vhdaost, vhdmtst, vhdaorg, vhdmird, vhdoth,
         cancer, cpevent)

### Labdata
# labdata <- read_csv(paste0(foldername, "labdata.csv"))
# saveRDS(labdata, paste0(foldername, "labdata.Rds"))
# 
# labdata2 <- read_csv(paste0(foldername, "labdata2.csv"))
# saveRDS(labdata2, paste0(foldername, "labdata2.Rds"))
labs <- readRDS(paste0(foldername, "labdata.Rds"))
labs2 <- readRDS(paste0(foldername, "labdata2.Rds"))

labs <- labs %>% 
  as_tibble () %>% 
  setNames(tolower(names(.))) %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

labs2 <- labs2 %>% 
  as_tibble () %>% 
  setNames(tolower(names(.))) %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)
  
labs <- bind_rows(labs, labs2)
rm(labs2)


## Explored creatinine at this level Units are not compatible with reference ranges
## Reviewed these, modify units or drop if looks like must be something else
creat <- labs %>% 
  filter(labnm == "CRE") 
creat <- creat %>% 
  mutate(labstdu = case_when(llc <5 & ulc >= 50 ~ NA_character_,
                             llc > 1 & llc < 30 ~ NA_character_,
                             llc >=30 & ulc >= 70 ~ "mmol/l",
                             labstd > 50 ~ "mmol/l",
                             TRUE ~ labstdu)) %>% 
  filter(!is.na(labstdu)) %>% 
  mutate(labstd = if_else(labstdu == "mg/dL", labstd * 88.42, labstd))


labs <- labs %>% 
  filter(labnm != "CRE") %>% 
  bind_rows(creat)
rm(creat)


### vital signs, phys table is all follow-up data, but visit 2 is randomisation visit so can include
vitals2 <- vitals %>% 
  select(adp_pid, dia, sys, cpevent, actevent) %>% 
  filter(cpevent == "VISIT 2")
vitals1 <- basco %>% 
  select(adp_pid, dia = bdbp, sys = bsbp) %>% 
  mutate(cpevent = "VISIT 1",
         actevent = 1)
vitals <- bind_rows(vitals1, vitals2) %>% 
  arrange(adp_pid, cpevent)

### Demographics
demo <- basco %>% 
  select(adp_pid, age, sex, ht, wt, bmi, ethnicdc)

### Save and delete to prevent being read in next file
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))
rm(rand, demo, medhist, conmed, labs, vitals)



foldername <- "BI1160_46"  # @MISSING extract # ----
foldername <- paste0("Data/", foldername, "/")

basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()
ct <- read_sas(paste0(foldername, "ct.sas7bdat")) %>% names_to_lower()
labs <- read_sas(paste0(foldername, "lab.sas7bdat")) %>% names_to_lower()
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()

### Rand
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

### Demo
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokcddc)

### Medhist
medhist <-  read_sas(paste0(foldername, "medhcdb.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  filter(cdi ==1) %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

medhist2 <- basco %>% 
  select(adp_pid, bowel, coron, nohemtia, hypert, heartfai, diabete)

### Conmed, note no code and not all preferred terms for WHODD are the same as the WHO ATC preferred terms
### nor are they all on RXNORM
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn, adp_wver)
### Identify reported classes, note these are labelled with names stored as attributes
conmed_class <- ct %>% 
  select(adp_ctpn, antithr, dti, vka, ufh, lmwh, othhep, fond, oth, asa, plini, nsaid, cvmeds, diur, perv, 
         cart, antih, vaso, bba, ccb, aact, serum, pgp, pgpinh, verapa, quini, amioda, ketoco, ritona, saquin, 
         nelfi, tacro, cyclos, valsp, itraco, droned, pgpind, rifa, stjohn, carbam, corti, nsaid12, ticlo, thrombo, 
         glyco, clopi, dext) %>% 
  distinct()
### Identify drug names with at least one class
conmed_class <- conmed_class %>% 
  gather(key = "class_name", value = "y_n", -adp_ctpn, na.rm = TRUE) 
### Identify drug names without any class assigned
conmed_no_class <- conmed %>% 
  anti_join(conmed_class) %>% 
  distinct(adp_ctpn)

### Labs, note don't need labs 2
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### vitals, for this one the blood pressure from visit 1 is inlucded in vitals
vitals  <- read_sas(paste0(foldername, "phys.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent)



save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata")) 
saveRDS(medhist2, file = paste0(foldername, "extract_non_meddra_medhist.Rds"))
rm(rand, demo, medhist, medhist2, conmed, labs, vitals)


foldername <- "BI1160_47"  # @MISSING extract # ----
foldername <- paste0("Data/", foldername, "/")

basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### rand
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

### Demo
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokcddc)

### Medhist
medhist <-  read_sas(paste0(foldername, "medhcdb.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  filter(cdi ==1) %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

medhist2 <- basco %>% 
  select(adp_pid, diabet, heart, hyper, hstrok,
         hcad, hliver, hinflm, hgast)

### Conmed
ct <- read_sas(paste0(foldername, "ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn, adp_wver)

### Labs
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>% names_to_lower()
### Labs, note don't need labs 2
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### vitals, has plenty of vitals 1
vitals  <- read_sas(paste0(foldername, "phys.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))
saveRDS(medhist2, file = paste0(foldername, "extract_non_meddra_medhist.Rds"))
rm(rand, demo, medhist, medhist2, conmed, labs, vitals)

foldername <- "BI1160_53"  # @MISSING extract # ----
foldername <- paste0("Data/", foldername, "/")

basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### rand
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

### Demo
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokcddc)

### Medhist
# No medical history in basco
# "medhcda.sas7bdat" is completely redacted, "medh" and "medrec" dont have any information either
medhist <-  read_sas(paste0(foldername, "medhcdb.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  filter(cdi ==1) %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

### Conmed
ct <- read_sas(paste0(foldername, "ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt,adp_ctpn, adp_wver)

### Labs
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>% names_to_lower()
### Labs, note don't need labs 2
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### vitals 
vitals  <- read_sas(paste0(foldername, "phys.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent)

save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))
rm(rand, demo, medhist, conmed, labs, vitals)

foldername <- "BI1160_63"  # @MISSING extract # ----


ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

foldername <- FindFx(ptrn)
folderout <- "D:/BI1160_63/"




foldername <- paste0("Data/", foldername, "/")

basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### rand
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

### Demo
# racei = 
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racei, smokcd)

### Medhist
medhist <- basco %>%
  select(adp_pid, mh_can, mh_nst, mh_cad, mh_ld, mh_ibd, mh_gdu, mh_mcb,
  mh_rb, mh_fnb, mh_hmt, blc_ht, blc_dm, blc_hf)
# medhist <-  read_sas(paste0(foldername, "medhcdb.sas7bdat")) %>% names_to_lower()
# medhist <- medhist %>% 
#   select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd)

# medhist2 <- read_sas("Data/BI1160_63/medhcdb.sas7bdat") %>% names_to_lower()

### Conmed
ct <- read_sas(paste0(foldername, "ctt.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn)

### Labs
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>% names_to_lower()
### Labs, note don't need labs 2
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### vitals 
vitals  <- read_sas(paste0(foldername, "phys.sas7bdat")) %>% names_to_lower()
vitals2  <- read_sas(paste0(foldername, "vita.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent)
vitals2 <- vitals2 %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent)
vitals <- bind_rows(vitals, vitals2)

save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals)



# Extracted
foldername <- "BI1245_36"  # extract # ----

# @Missing lab data, missing liver function measures as not included in baseline file

# Set folder location to save extracted trial data  
folderout <- paste0("D:/", foldername, "/")

# Take folder name and format for findfx function
ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

# Get file path for trial
foldername <- FindFx(ptrn)

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|bcond"))

### rand
rand <- read_sas(unz(foldername, "raw_oc/rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

gentrt <- read_sas(unz(foldername, "ads_ads/gentrt.sas7bdat")) %>% names_to_lower()
gentrt <- gentrt %>% 
  distinct(adp_pid, atrlbl) %>% 
  filter(atrlbl %in% c("Empagliflozin 25mg", "Empagliflozin 10mg", "Placebo"))

gentrt <- gentrt %>% 
  inner_join(rand %>%  select(adp_pid, tpatt))

gentrt <- gentrt %>% 
  group_by(tpatt, atrlbl) %>% 
  count() %>% 
  group_by(tpatt) %>% 
  arrange(desc(n)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  distinct(tpatt, atrlbl) %>% 
  rename(tpattlbl = atrlbl)


### Demo
basco <- read_sas(unz(foldername, "ads_ads/bascodem.sas7bdat")) %>% names_to_lower() 
demo <- basco %>% 
  select(adp_pid, bage, sexdc, bhts, bwts, bbmi, raceidc, smokcddc)

### Medhist "bcond - This dataset contains information about baseline conditions"
bcond <- read_sas(unz(foldername, "ads_ads/bcond.sas7bdat")) %>% names_to_lower() 
medhist <- bcond %>% 
  select(adp_pid, actevent, mpt, mptcd, msoccd, msoc)

### Conmed
ct <- read_sas(unz(foldername, "ads_ads/ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegdt, ctpn, ctpncd)

### Labs
labs <- read_sas(unz(foldername, "raw_oc/labstds.sas7bdat")) %>% names_to_lower()
labsf <- read_sas(unz(foldername, "raw_oc/labstdsf.sas7bdat")) %>% names_to_lower()

### Labs, note lab variable listed in dataset is missing.
bascolab <- read_sas(unz(foldername, "ads_ads/bascolab.sas7bdat")) %>% names_to_lower() 

bascolab <- bascolab %>% 
  select(adp_pid, bhba1c, bfpg, begfr, bchol, bhdl, bldl, bcre, bhgb)

bilab2 <- read_sas(unz(foldername, "raw_oc/bilab2.sas7bdat")) %>% names_to_lower() 

#labs <- labs %>% 
 #  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx)
labs <- bascolab

### Vitals
vitals <- basco %>% 
  select(adp_pid, bsysse, bdiase)

### Save extracted files 
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, ct, labs, vitals, basco, bascolab, bcond, gentrt, labsf)
rm(folderout, foldername, ptrn)



foldername <- "BI1245_48"  # extract # ----

# Set folder location to save extracted trial data - make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Take folder name and format for findfx function
ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

# Get file path for trial
foldername <- FindFx(ptrn)

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|bcond|ptlvl"))

### rand
rand <- read_sas(unz(foldername, "raw_oc/rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

ptlvl <- read_sas(unz(foldername, "ads_ads/ptlvl.sas7bdat")) %>% names_to_lower() %>% 
  select(adp_pid, ranlbl1)

rand <- rand %>%
  select(-tpatt) %>% 
  inner_join(ptlvl) %>% 
  rename(tpatt = ranlbl1) %>% 
  filter(!tpatt == "Not randomized") # @SCREEN

### Demo
basco <- read_sas(unz(foldername, "ads_ads/bascodem.sas7bdat")) %>% names_to_lower() 
demo <- basco %>% 
  select(adp_pid, bage, sexdc, bhts, bwts, bbmi, raceidc, smokcddc)

### Medhist "bcond - This dataset contains information about baseline conditions"
bcond <- read_sas(unz(foldername, "ads_ads/bcond.sas7bdat")) %>% names_to_lower() 
medhist <- bcond %>% 
  select(adp_pid, actevent, mpt, mptcd, msoccd, msoc)

### Conmed
ct <- read_sas(unz(foldername, "ads_ads/ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>% 
  select(adp_pid, ctbegdt) # ctbegc, adp_ctpn, adp_wver do not exist, @MISSING / @ERROR 

### Labs
labs <- read_sas(unz(foldername, "ads_ads/lab.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
   select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### Vitals
vitals <- basco %>% 
  select(adp_pid, bsysse, bdiase)


### Save extracted files 
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals, basco, bcond, ct, ptlvl)
rm(folderout, foldername, ptrn)




foldername <- "BI248_524"  # extract # ----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|popu"))


### rand
rand <- read_sas(unz(foldername, "RAW_OC/rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

### Demo
basco  <- read_sas(unz(foldername, "ADS_ADS/basco.sas7bdat")) %>% names_to_lower()
demo <- basco %>% 
  select(adp_pid, age, sex, bmi, raceadc)

### Medhist
bconcd <- read_sas(unz(foldername, "RAW_OC/bconcd.sas7bdat")) %>% names_to_lower()
medhist <- bconcd %>% 
  select(adp_pid, actevent, adp_mpt, adp_mptcd, adp_msoccd, adp_msoc)

### Conmed
ct <- read_sas(unz(foldername, "ADS_ADS/ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  # select(adp_pid, ctbegc, ctbegdt, adp_ctpn)
  select(adp_pid, ctbegc, ctbegdt, subpn)
# subpn

### Labs, note don't need labs 2
labs <- read_sas(unz(foldername, "RAW_OC/labdata.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### vitals 
vitals  <- read_sas(unz(foldername, "RAW_OC/phy.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia1, sys1, dia2, sys2, htstd, wtstd, bmi, visdt, cpevent)

### Save extracted files 
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals)
rm(folderout, foldername, ptrn)



foldername <- "BI248_525"  # extract # ----

# Set folder location to save extracted trial data - Make sure to run new foldername 
folderout <- paste0("D:/", foldername, "/")

# Take folder name and format for findfx function
ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

# Get file path for trial
foldername <- FindFx(ptrn)

# See zipped files 
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "basco|bconcd|rand|ct|labdata"))


### rand
rand <- read_sas(unz(foldername, "RAW_OC/rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study)

### Demo
basco  <- read_sas(unz(foldername, "ADS_ADS/basco.sas7bdat")) %>% names_to_lower()
demo <- basco %>% 
  select(adp_pid, age, sex, bmi, raceadc)

### Medhist
bconcd <- read_sas(unz(foldername, "RAW_OC/bconcd.sas7bdat")) %>% names_to_lower()
medhist <- bconcd %>% 
  select(adp_pid, actevent, adp_mpt, adp_mptcd, adp_msoccd, adp_msoc)

### Conmed
ct <- read_sas(unz(foldername, "RAW_OC/ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn,  adp_wver)

### Labs, note don't need labs 2
labs <- read_sas(unz(foldername, "RAW_OC/labdata.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent)

### vitals 
vitals  <- read_sas(unz(foldername, "RAW_OC/phy.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia1, sys1, dia2, sys2,  htstd, wtstd, bmi, visdt, cpevent)

if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals)
rm(folderout, foldername, ptrn)



foldername <- "BI248_622"  # extract # ----

# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Take folder name and format for findfx function
ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

# Get file path for trial
foldername <- FindFx(ptrn)

# See unzipped files 
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "treatment|patd|basco|bconcd|rand|ct|labdata|vits|gentrt"))

### rand
rand <- read_sas(unz(foldername, "RAW_OC/e_trtexp.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, study, atrstdt) 

treatment <- read_sas(unz(foldername, "RAW_OC/treatment.sas7bdat")) %>% names_to_lower() %>% 
  distinct(adp_pid, atrlbl)

treatment <- treatment %>%
  inner_join(rand) %>%
  group_by(tpatt, atrlbl) %>% 
  count() %>% 
  ungroup() %>% 
  distinct(tpatt, atrlbl)

rand <- rand %>% 
  inner_join(treatment) %>% 
  select(-tpatt) %>% 
  rename(tpatt = atrlbl)

### Demo
patd  <- read_sas(unz(foldername, "RAW_OC/patd.sas7bdat")) %>% names_to_lower()
demo <- patd %>% 
  select(adp_pid, age, sex, racei, smokcd)

### Medhist
bconcd <- read_sas(unz(foldername, "RAW_OC/bconcd.sas7bdat")) %>% names_to_lower()
medhist <- bconcd %>% 
  select(adp_pid, adp_mpt, adp_mptcd, adp_msoccd, adp_msoc, actevent)

### Conmed
ct <- read_sas(unz(foldername, "RAW_OC/ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, adp_ctbegdt, adp_ctpn, adp_wver)

### Labs
labs <- tibble(note = "There was no lab data collected in this study")

### vitals 
vitals  <- read_sas(unz(foldername, "RAW_OC/vits.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia1, sys1, dia2, sys2, dia3, sys3, adp_visdt, htstd, wtstd, cpevent)


### SAVE
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals)
rm(folderout, foldername)









# Consolidate all files ----
## The following consolidates the data and ensures all matches

## Visits table, taken mainly from flowchart and schedule tables in study protocol ----
randomisation_schedule <- tribble(
  ~"trial", ~"pre", ~"rand", ~"post_rand", ~"special",
  "BI1245_36", "1:2", "3", "4:11", "",
  "BI1245_48", "c('1', '2', '2_1', '2_2')", "3", "4:6", "",
  "BI248_524", "1", "2", "3:12", "",
  "BI248_525", "1", "2", "3:13", "names TC 1:4 and V1:12",
  "BI248_622", "1", "2", "3:6", "names TC 1:4 and V1:12"
 )

# Add to above command if Missing trials added: 
#"BI1160_26", "1", "2", "3:14", "98 = 'last centre', 99 = 'last study'", 
#"BI1160_46", "1", "2", "3:10", "",
#"BI1160_47", "1", "2", "3:14", "",
#"BI1160_53", "1", "2:3", "4:10", "",
#"BI1160_63", "1", "2", "3:10", "",

## Combine all files into a single dataframe ----
#List folder names
foldernames <- list(
  "BI1245_36", "BI1245_48", "BI248_524", "BI248_525", "BI248_622")

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

# Check all table names the same
tbl_names <- map(all_trials, names) 
do.call(rbind, tbl_names)

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)

## Randomisation table ----
## All trials have randomisation data and identical variable names
## Except BI248_622 for which the randomisation date is missing so use atrstdt actual treatment date instead
rand <- all_trials$rand
CreateTableNameLabels(rand)
names(rand)
# treat actual treatment date as randomisation data for study BI248-622
rand$BI248_622$randdt <- rand$BI248_622$atrstdt
rand$BI248_622$atrstdt <- NULL

rand <- BindRowsWLabels(rand, 2)
tapply((rand$tpatt), rand$trial, unique)
tapply(is.na(rand$tpatt), rand$trial, unique)

# Limit to randomised patients
rand <- rand %>% # @SCREEN
  filter(tpatt != "")

# Take minimum randomisation date, does not lose any patients
rand <- rand %>% 
  group_by(trial, adp_pid, study, tpatt) %>% 
  summarise(randdt = min(randdt, na.rm = TRUE)) %>% 
  ungroup()

## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo)

# Rename age variable
demo <- map(demo, function(x) {
  names(x)[names(x) == "bage"] <- "age"
  names(x)[names(x) == "bbmi"] <- "bmi"
  names(x)[names(x) == "sexdc"] <- "sex"
  names(x)[names(x) %in% c("raceadc", "racedc", "racei", "raceidc", "ethnicdc")] <- "race"
  names(x)[names(x) %in% c("smokcd", "smokcddc")] <- "smoke"
  names(x)[names(x) %in% c("bhts", "ht", "htstd")] <- "ht"
  names(x)[names(x) %in% c("bwts", "wt", "wtstd")] <- "wt"
  x
  })

CreateTableNameLabels(demo)
demo <- map(demo, function(current_df) {
  x <- current_df
  x$race <- as.character(x$race)
  x$sex <- as.character(x$sex)
  if("smoke" %in% names(x)) x$smoke <-  as.character(x$smoke)
  x
  })

CreateTableNameLabels(demo)
demo <- BindRowsWLabels(demo, 2)

# limit to randomised patients. @SCREEN
demo <- demo %>% 
  semi_join(rand)


## Labs table ----
labs <- all_trials$labs
CreateTableNameLabels(labs)

# One with labs in baseline, one with no labs data
labs_stndrd <- labs[setdiff(names(labs), c("BI1245_36", "BI248_622"))]

# arrange so that one with correct labels is on top
CreateTableNameLabels(labs_stndrd)
labs_stndrd <- BindRowsWLabels(labs_stndrd, 2)
ExtractLabel(labs_stndrd)

## Not standard labs table -do not currently have these tables 
# labs_other <- labs[["BI1245_36"]]
# lbl_measure <- ExtractLabel(labs_other, return_object = T)
# labs_other <- gather(labs_other, key = "labnm", value = "labstd", -adp_pid )
# labs_other <- labs_other %>% 
#   mutate(trial = "BI1245_36",
#          labnmx = lbl_measure[labnm],
#          labnm = str_replace(labnm, "^b", ""),
#          labnm = if_else(labnm == "fpg", "GLU", labnm),
#          labnm = str_to_upper(labnm))

## Join all into single table
# setdiff(str_to_upper(labs_other$labnm), labs_stndrd$labnm)
labs2 <- bind_rows(labs_stndrd) #, labs_other)
a <- ExtractLabel(labs_stndrd, return_object = TRUE)
a <- a[names(labs2)]
labs2 <- MakeLabels(labs2, a)
labs <- labs2
rm(labs2, labs_stndrd)

## Limit to randomised patients, @SCREEN
labs <- labs %>% 
  semi_join(rand)

## Select wanted measures
labs_rv <- labs %>% distinct(labnm, labnmx) %>% 
  filter(labnm != "") %>% 
  group_by(labnm) %>% 
  summarise(labnmx = paste(labnmx, collapse = " | "))
# write_csv(labs_rv, "Scratch_data/reivew_bi_labs.csv")
# labs_slctd <- read_csv("Created_metadata/reivewED_bi_labs.csv") %>% 
#   filter(keep ==1) %>% 
#   select(-keep)
labs_slctd <- read_csv('labnm,labnmx
CRE,Creatinine |  | Baseline creatinine [umol/L]
GFRC,Creatinine clearance |
  GLU,Glucose | Baseline fasting plasma glucose [mg/dL]
HGB,Haemoglobin |  | Baseline haemoglobin [g/L]
PLTCT,Platelet count | Platelets |
  SGOT,"AST/GOT, SGOT |"
SGPT,"ALT/GPT, SGPT |"')


labs <- labs %>% 
  semi_join(labs_slctd %>% select(labnm))

# Select labs prior to treatment started
# For the following consulted actual labels as sometimes idiosyncratic, eg "baseline"
labs_visits <- tribble(
  ~"trial", ~ "code",
  "BI1245_36",  "'BASELINE'",           # all from baseline table, need to label below
  "BI1245_48", "paste('VISIT', c('1', '2_1', '2_2'))", # no 2_2 in labs
  "BI248_524", "paste('VISIT', 1:2)", # note no visit 2 in labs
  "BI248_525", "paste('VISIT', 1:2)", # note no visit 2 in labs
  "BI248_622",   NA)                  # no lab results in trial


# From missing trials, add to above command if found
#"BI1160_26", "paste('VISIT', 1:2)",
#"BI1160_46", "c(paste('VISIT', 1), 'Baseline')",
#"BI1160_47", "paste('VISIT', 1:2)", # note no visit 2 in labs
#"BI1160_53", "paste('VISIT', 1:3)", # note no visit 2 or 3 in labs
#"BI1160_63", "paste('VISIT', 1:2)", # note no visit 2 in labs


labs_visits <- labs_visits %>% 
  filter(!is.na(code))
labs_visits <- by(labs_visits, labs_visits$trial, function(x) eval(parse(text = x$code)))
labs_visits <- stack(labs_visits) %>% 
  setNames(c("cpevent", "trial"))
labs$cpevent[labs$trial == "BI1245_36"] <- "BASELINE"
labs <- labs %>% 
  semi_join(labs_visits)



## Medical history table ----
medhist <- all_trials$medhist

# most are MedDRA coded
meddra <- medhist [! names(medhist) %in% c("BI1160_26", "BI1160_63") ] # These trials are missing

CreateTableNameLabels(meddra)
meddra <- map(meddra, function(x) {
  names(x) <- str_replace(names(x), fixed("adp_"), "")
  names(x)[names(x) == "pid"] <- "adp_pid"
  x
})
CreateTableNameLabels(meddra)
meddra <- BindRowsWLabels(meddra, 1)

baseline_meddra_tables <- c("BI1245_36", "BI1245_48", "BI248_524", "BI248_525", "BI248_622")


# other tables are all visit 1
# so all meddra tables are baseline tables!
tapply(meddra$cpevent, meddra$trial, unique)

# Select only randomised patients
meddra <- meddra %>% 
  semi_join(rand)

## Three have only three different types of condition in the meddra table
tapply(meddra$msoc, meddra$trial, unique)


## Concomittant medicines table ----
conmed <- all_trials$conmed
CreateTableNameLabels(conmed)
conmed <- map(conmed, function(x) {
  names(x) <- str_replace(names(x), fixed("adp_"), "")
  names(x)[names(x) == "pid"] <- "adp_pid"
  names(x)[names(x) == "subpn"] <- "ctpn"
  x
})
CreateTableNameLabels(conmed)
conmed <- BindRowsWLabels(conmed, 1)

# Select only randomised patients
conmed <- conmed %>% # @SCREEN
  semi_join(rand)

# Select only patients with data for drugs
conmed <- conmed %>% 
  filter(!is.na(ctpn), ctpn != "")

# Examine conmed start dates
# |Very few missing values for start date where not also C (219 in total) assume these are not concommitant
table(conmed$ctbegc == "C", !is.na(conmed$ctbegdt))
range(conmed$ctbegdt, na.rm = TRUE)

conmed_noc <- conmed %>%
  filter(ctbegc != "C") 
round(100*tapply(conmed_noc$ctbegdt %>% is.na(), conmed_noc$trial, mean), 1)

# so define as pre-randomisation if is "C" or if date prior to randdt
conmed_dates <- conmed %>%
  filter(is.na(ctbegc) | ctbegc != "C", !is.na(ctbegdt)) %>% 
  inner_join(rand %>%  select(trial, adp_pid, randdt) %>%  distinct(trial, adp_pid, .keep_all = TRUE))
conmed_dates <- conmed_dates %>% 
  mutate(pre_exist = if_else(ctbegdt <= randdt, TRUE, FALSE))


conmed_pre <- bind_rows(conmed %>% filter(ctbegc == "C"),
                         conmed_dates %>% filter(pre_exist) %>% select(-randdt, -pre_exist))
conmed <- conmed_pre

rm(conmed_noc, conmed_pre, conmed_dates)


## Vitals table ----
vitals <- all_trials$vitals
CreateTableNameLabels(vitals)

# Used baseline table for two trials as no vitals table, assign these visits to zero
 vitals$BI1245_36$actevent <- 0L
 vitals$BI1245_48$actevent <- 0L
 vitals$BI1245_36$cpevent <- "BASELINE"
 vitals$BI1245_48$cpevent <- "BASELINE"

vitals <- map(vitals, function(x) {
  names(x)[names(x) %in% c("bdiase", "dia")] <- "dia1"
  names(x)[names(x) %in% c("bsysse", "sys")] <- "sys1"
  names(x)[names(x) %in% c("adp_visdt", "visdt")] <- "visdt"
  x
})
CreateTableNameLabels(vitals)
#vitals <- BindRowsWLabels(vitals, 10)
map(vitals, ~ min(.x$actevent, na.rm = TRUE))

# Select BMI and BP tables
bmi <- vitals[c("BI1245_36", "BI1245_48","BI248_524", "BI248_525", "BI248_622")]
bmi <- map(bmi, ~ .x[ ,names(.x) %in% c("adp_pid","cpevent", "visdt", "htstd", "wtstd", "bmi")])
bmi <- BindRowsWLabels(bmi, 1)

bp <- map(vitals, ~ .x[, names(.x) %in% c("adp_pid","cpevent", "visdt",
                                          "dia1", "dia2", "dia3", "sys1", "sys2", "sys3")])
bp <- BindRowsWLabels(bp, 10)
bp <- bp %>% 
  gather(key = "measure", value = "value", dia1:sys3)
bp <- bp %>% 
  separate(measure, into = c("param", "n_measure"), sep = c(3))

## Select pre/randomisation visits only
vitals_visits <- labs_visits %>% 
  # add_row(cpevent = "BASELINE", trial = "BI1245_48") %>% 
  add_row(cpevent = c("VISIT 1", "VISIT 2"), trial = "BI248_622")

bp <- bp %>% 
  semi_join(rand) %>% 
  semi_join(vitals_visits)

## Consolidate height and weight into a single table
## Set actevent to zero as is baseline
# bmi_demo <- demo %>% 
#   select(trial, adp_pid, htstd = ht, wtstd = wt, bmi) %>% 
#   mutate(visdt = as.Date("", "%m/%d/%y"),
#          cpevent = "BASELINE")

# a <- ExtractLabel(bmi, return_object = TRUE)
# a <- a[names(bmi_demo)]
# bmi2 <- bind_rows(bmi_demo, bmi)
# bmi2 <- MakeLabels(bmi2, a)
# bmi <-bmi2
# rm(bmi2, vitals)

# Select pre/randomisation visits only
vitals_visits <- bind_rows(vitals_visits,
                           vitals_visits %>%
                             distinct(trial) %>% 
                             mutate(cpevent = "BASELINE")) %>% 
  distinct()

bmi <- bmi %>% 
  semi_join(vitals_visits)

# demo <- demo %>% 
#   select(-ht, -wt, -bmi)

## Convert data into a single object ----

bi <- list(conmed = conmed, demo = demo, labs = labs, meddra = meddra, 
           # nonmeddra = nonmeddra,
           bp = bp, bmi = bmi, rand = rand)
# rename and delete irrelevant variables so match across companies ----
meddra_lkp <- bi$meddra %>% 
  distinct(mpt, mptcd, msoc, msoccd)
bi$meddra <- bi$meddra %>% 
  select(trial, adp_pid, term = mpt) %>% 
  mutate(meddra = "meddra")
# 
# bi$nonmeddra <- bi$nonmeddra %>% 
#   filter(present == 1) %>% 
#   select(trial, adp_pid, term = renamed) %>% 
#   mutate(meddra = "non-meddra")

bi$medhist <- bind_rows(bi$meddra)# , bi$nonmeddra)
bi$meddra <- NULL
bi$nonmeddra <- NULL

bi$labs <- bi$labs[, !names(bi$labs) %in% c("cpevent", "visno")]
bi$bp <- bi$bp[ , !names(bi$bp) %in% c("cpevent", "visdt", "n_measure")]
bi$bmi <- bi$bmi[ , !names(bi$bmi) %in% c("cpevent", "visdt")]
bi$bmi <- bi$bmi %>% 
  gather("param", "value", htstd, wtstd, bmi)
bi$conmed$ctbegc <- NULL
bi$conmed$ctbegdt <- NULL

# Rename all id variables
bi <- map(bi, ~ .x %>%  rename(id = adp_pid))

bi <- bi[c("conmed", "demo", "labs", "medhist", "bp", "bmi", "rand")]


# Identify treatment arm labels ----
ReadPopu <- function(BI, tblname, limitto = TRUE) {
  folder_pre  <- "E:/Research Project 1732/files_BI_"
  folder_post <- "/Files/ads_ads/"
  foldername <- str_sub(BI, 3) 
  foldername <- paste0(folder_pre, foldername, folder_post)
  mydf <- read_sas(paste0(foldername, tblname, ".sas7bdat")) %>%  names_to_lower()
  if (limitto == TRUE) mydf <- mydf %>%  distinct(tpatt, tpattlbl)
  mydf
}
# BI1160_46 <- ReadPopu("BI1160_46", tblname = "popu")
# BI1160_47 <- ReadPopu("BI1160_47", tblname = "gentrt")
# BI1160_53 <- ReadPopu("BI1160_53", tblname = "gentrt")
# BI1160_63 <- ReadPopu("BI1160_63", tblname = "gentrt")
# BI248_524 <- ReadPopu("BI248_524", tblname = "popu")
# BI248_525 <- ReadPopu("BI248_525", tblname = "popu")

## NOTE columns names are CAPITILISED
BI1245_36 <- read_sas(unz(FindFx("1245.36"), "ads_ads/gentrt.sas7bdat")) %>% distinct(TPATT) 
BI1245_48 <- read_sas(unz(FindFx("1245.48"), "ads_ads/gentrt.sas7bdat")) %>% distinct(TPATT) 
BI248_524 <- read_sas(unz(FindFx("248.524"), "ADS_ADS/popu.sas7bdat")) %>% distinct(TPATT, TPATTLBL)
BI248_525 <- read_sas(unz(FindFx("248.525"), "ADS_ADS/popu.sas7bdat")) %>% distinct(TPATT, TPATTLBL)
BI248_622 <- read_sas(unz(FindFx("248.622"), "ADS_ADS/gentrt.sas7bdat")) %>% distinct(TPATT, TPATTLBL)  

bi_lkp <- list(#BI1160_46, BI1160_47, BI1160_53, BI1160_63,
               BI1245_36, BI1245_48,
               BI248_524, BI248_525, BI248_622)

names(bi_lkp) <- c(
  # "BI1160_46", "BI1160_47", "BI1160_53", "BI1160_63",
  "BI1245_36", "BI1245_48", 
  "BI248_524", "BI248_525", "BI248_622")

bi_lkp <- bind_rows(bi_lkp, .id = "trial")
saveRDS(bi_lkp, "E:/C_analysis_code_meta/Extract_Data/Created_metadata/partial_set_arm_lookups_bi.Rds")

## Save all data ----
saveRDS(bi, file = "Processed_data/bi.Rds")




a <- read.csv("E:/C_analysis_code_meta/Extract_Data/Created_metadata/bi_trials_lookup_treatment_group_to_label.csv")
