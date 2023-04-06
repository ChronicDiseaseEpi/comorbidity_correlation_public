# 02_extract_bi_new.R
### I have selected only baseline characteristics for randomised participants at this stage
### All are MedDRA

rm(list = ls())

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


# Extract all files 
foldername <- "BI107_210" # extract #----

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
# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, "raw_oc/pat5.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))
  
## Analysis dataset
rand <-  read_sas(unz(foldername, "raw_oc/adm3.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, actevent, visdt, tpatt, atrstdt)

rand1 <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, atrstdt) %>% 
  arrange(adp_pid, atrstdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
#medhist_list_names <- map(medhist_list, names)
#medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                      n
# <chr>                 <int>
#   1 ""                     7227
# 2 HAEMORRHOIDS              1
# 3 HYPERTENSION NOS          1
# 4 SINUSITIS CHRONIC NOS     1

# All visits are visit 1, this is a screening diagnosis
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medicaiton
# Note that this excludes the index disease (osteoarthritis) therapy, which is in a separate table called ctoact
# All are visit one therefore all true concomitant medicaiton, not treatmetn of adverse events
conmed <-  read_sas(unz(foldername, "raw_oc/ctct.sas7bdat")) %>% names_to_lower()
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt, ctrte, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

#conmed <- conmed %>% 
 # filter(is.na(cti) | cti !=0) %>% 
  #select(adp_pid, ctpt, ctrte)
#rm(conmed2)

## Select only baseline concomitant medication by taking ones where start date or 
# Visit dates is before first randomised treatment dates
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, tx_start = atrstdt)) %>% 
  filter(ctbegdt <= tx_start | visdt <= tx_start) %>% 
  select(adp_pid, ctpt, ctrte)


# Vitals
vitals <- read_sas(unz(foldername, "raw_oc/phy3.sas7bdat")) %>% names_to_lower()
vitals2 <- vitals %>% 
  select(adp_pid, actevent, cpevent, visdt, sys, dia, wtstd, htstd2)
vitals <- vitals %>% 
  filter(cpevent == "VISIT 1") %>% 
  select(adp_pid, sys, dia, wtstd, htstd2)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/ri.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/aeaea.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, aeptit, aesoc)

# labs
labs <- tibble(note = "there is no lab data")

# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, smoke, vitals, rand, ae, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, smoke, vitals, rand, ae, conmed2, rand1, vitals2)
rm(folderout, foldername,ptrn)






foldername <- "BI248_629" # extract ----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd"))

### rand
rand <- read_sas(unz(foldername, "raw_oc/e_trtexp.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, study, atrstdt) %>% 
  filter(!is.na(tpatt), tpatt != "") %>% 
  arrange(adp_pid, atrstdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

### Demo, no smoke in this study
patd  <- read_sas(unz(foldername, "raw_oc/patd.sas7bdat")) %>% names_to_lower()
demo <- patd %>% 
  select(adp_pid, age, sex, racea)

### Medhist, all are visit 1
bconcd <- read_sas(unz(foldername, "raw_oc/bconcd.sas7bdat")) %>% names_to_lower()
medhist <- bconcd %>% 
  select(adp_pid, adp_mpt, adp_mptcd, adp_msoccd, adp_msoc)

### Conmed
ct <- read_sas(unz(foldername, "raw_oc/ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn, adp_wver, visdt) %>% 
  inner_join(rand %>%  select(adp_pid, atrstdt)) %>% 
  filter(ctbegc == "C" | visdt <= atrstdt | ctbegdt <= atrstdt) %>% 
  select(adp_pid, adp_ctpn)

### Labs
labs <- tibble(note = "There was no lab data collected in this study")

### vitals 
vitals  <- read_sas(unz(foldername, "raw_oc/phys.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia1, sys1, dia2, sys2, visdt, htstd, wtstd, cpevent, visdt) %>% 
  inner_join(rand %>%  select(adp_pid, atrstdt)) %>% 
  filter(visdt <= atrstdt) %>% 
  select(adp_pid, dia1, sys1, dia2, sys2, visdt, htstd, wtstd)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, rand, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals, rand, bconcd, ct, patd)
rm(folderout, foldername, ptrn)






foldername <- "BI502_327" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))


# Note patients randomised at visit 1

## demo variables
# all visit 1
# old
#demo <-  read_sas(paste0(folder1, "pat3.sas7bdat")) %>% names_to_lower()
# new
#myz <- "V:/502.327/502.327_VIVLI_v01.zip"

demo <- read_sas(unz(foldername, paste0("raw_oc/", "pat3.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Administration of drugs dataset, note will move NAs (ie drug not administered) to the end
# So will only be selected of never had drug administered
# use it to define pre-treatment BP, labs etc
# rand <-  read_sas(paste0(folder1, "adm1.sas7bdat")) %>% names_to_lower()
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "adm1.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  # drop the following line to include those nor randomised @RAND
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char


# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                         n
# <chr>                    <int>
#   1 ""                        6949
# 2 Retinopathy hypertensive     1


# All visits are visit 1, this is a screening diagnosis 
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, paste0("raw_oc/", "ctt.sas7bdat"))) %>% names_to_lower()
table(conmed$cpevent) # all visit 1
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt = adp_ctpn, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1" ) %>% 
  select(adp_pid, ctpt = adp_ctpn) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "phy2.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt = wtstd, ht = htstd)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "risk.sas7bdat"))) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "aee.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- read_sas(unz(foldername, paste0("raw_oc/", "labdata.sas7bdat"))) %>% names_to_lower()
#### This time labstd and labn are reported along with upper and lower limits
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  filter(labdttm <= randt) %>% 
  select(adp_pid, labnm, labstd, lab, labc, llc, ulc)

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "vit1.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)







foldername <- "BI502_254" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))
# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, "raw_oc/pat3.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Administration of trial medicaiton, take first one as date treatmetn first given for identifying labs conmeds etc
rand <-  read_sas(unz(foldername, "raw_oc/adm1.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                      n
# <chr>                 <int>
#   1 ""                     7227
# 2 HAEMORRHOIDS              1
# 3 HYPERTENSION NOS          1
# 4 SINUSITIS CHRONIC NOS     1

# All visits are visit 1, this is a screening diagnosis
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# Note that this excludes the index disease (steoarthritis) therapy, which is in a separate table called ctoact
# All are visit one therefore all true concomitant medicaiton, not treatmetn of adverse events
conmed <-  read_sas(unz(foldername, "raw_oc/ctct.sas7bdat")) %>% names_to_lower()
table(conmed$cpevent)
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt, ctrte, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  select(adp_pid, ctpt, ctrte)
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, "raw_oc/phy2.sas7bdat")) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/risk.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/aeaea.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, aeptit, aesoc)

# labs
labs <- read_sas(unz(foldername, "raw_oc/labblabb.sas7bdat")) %>% names_to_lower()
#### For some reason labstd and labn are all null, having to rely on lab character!!!
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, labnm, labc)
# labs$labc[is.na(as.double(labs$labc))] %>% unique() 
# [1] ""   "<6"

labs <- labs %>% 
  filter(!labc == "", !is.na(labc)) %>% 
  mutate(labstd = case_when(
    labc == "<6" ~  1,
    TRUE ~ as.double(labc)))

# vitals
bp <- read_sas(unz(foldername, "raw_oc/vit.sas7bdat")) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)

# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)






foldername <- "BI502_256" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))
# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, "raw_oc/pat3.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Adminsitration of drugs dataset, note will move NAs (ie drug not administrered) to the end
# So will only be slecetd of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, "raw_oc/adm1.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                      n
# <chr>                 <int>
#   1 ""                     7227
# 2 HAEMORRHOIDS              1
# 3 HYPERTENSION NOS          1
# 4 SINUSITIS CHRONIC NOS     1

# All visits are visit 1, this is a screening diagnosis
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, "raw_oc/ctct.sas7bdat")) %>% names_to_lower()
table(conmed$cpevent)
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt, ctrte, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt) %>% 
  select(adp_pid, ctpt, ctrte)
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, "raw_oc/phy2.sas7bdat")) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# For this trial only, height is in inches rather than cm and weight is in lbs rather than kg
htwt <- htwt %>% 
  mutate(ht = ht *2.54,
         wt = wt * 0.453592)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/risk.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/aeaea.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, aeptit, aesoc)

# labs
labs <- read_sas(unz(foldername, "raw_oc/labblabb.sas7bdat")) %>% names_to_lower()
#### This time abstd and labn are reported
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, labnm, labstd, lab, labc)

# vitals
bp <- read_sas(unz(foldername, "raw_oc/vit1.sas7bdat")) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)



# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)




foldername <- "BI502_316" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))
# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, "raw_oc/pat3.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Administration of drugs dataset, note will move NAs (ie drug not administrered) to the end
# So will only be selected if never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, "raw_oc/adm1.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                      n
# <chr>                 <int>
#   1 ""                     7227
# 2 HAEMORRHOIDS              1
# 3 HYPERTENSION NOS          1
# 4 SINUSITIS CHRONIC NOS     1

# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, "raw_oc/ctct.sas7bdat")) %>% names_to_lower()
table(conmed$cpevent)
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt, ctrte, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% # @SCREEN 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, ctpt, ctrte) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, "raw_oc/phys.sas7bdat")) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/risk.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/aeaea.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, aeptit, aesoc)

# labs
labs <- read_sas(unz(foldername, "raw_oc/labdata.sas7bdat")) %>% names_to_lower()
#### This time labstd and labn are reported along with upper and lower limits
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  filter(labdttm <= randt) %>% 
  select(adp_pid, labnm, labstd, lab, labc, llc, ulc)

# vitals
bp <- read_sas(unz(foldername, "raw_oc/vit1vit.sas7bdat")) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)



foldername <- "BI502_317" # extract #----

# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Take folder name and format for findfx function
ptrn <- foldername %>%
  sub("_", ".", .) %>%
  sub("BI", "", .)

# Get file path for trial
foldername <- FindFx(ptrn)
# Note patients randomised at visit 1

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))



## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, "raw_oc/pat3.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Adminsitration of drugs dataset, note will move NAs (ie drug not administrered) to the end
# So will only be slecetd of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, "raw_oc/adm1.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                      n
# <chr>                 <int>
#   1 ""                     7227
# 2 HAEMORRHOIDS              1
# 3 HYPERTENSION NOS          1
# 4 SINUSITIS CHRONIC NOS     1

# All visits are visit 1, this is a screening diagnosis
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, "raw_oc/ctct.sas7bdat")) %>% names_to_lower()
table(conmed$cpevent)
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt, ctrte, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, ctpt, ctrte) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, "raw_oc/phys.sas7bdat")) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/risk.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/aeaea.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, aeptit, aesoc)

# labs
labs <- read_sas(unz(foldername, "raw_oc/labdata.sas7bdat")) %>% names_to_lower()
#### This time labstd and labn are reported along with upper and lower limits
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  filter(labdttm <= randt) %>% 
  select(adp_pid, labnm, labstd, lab, labc, llc, ulc)

# vitals
bp <- read_sas(unz(foldername, "raw_oc/vit1vit.sas7bdat")) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)



foldername <- "BI502_391" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

### Demo
patd  <- read_sas(unz(foldername, "raw_oc/patd.sas7bdat")) %>% names_to_lower()
demo <- patd %>% 
  select(adp_pid, age, sex, racea)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/risk.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

## Randomisation table 
rand <-  read_sas(unz(foldername, "raw_oc/adm1.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

## Slightly different names to some others, some text is redacted, but not MEDDRA coded text
medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, adp_mhlt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                   n
# <chr>              <int>
#   1 ""                 14095
# 2 Depression             1
# 3 Infertility female     1
# 4 Insomnia               1
# 5 Osteoporosis           1
# 6 Tenosynovitis          1

# All visits are visit 1, this is a screening diagnosis
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

### Conmed
ct <- read_sas(unz(foldername, "raw_oc/ctt.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()

# Vitals
htwt <- read_sas(unz(foldername, "raw_oc/phys.sas7bdat")) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/aee.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- "No lab results located, may be present in data. Could not find reference to blood testing in CSR or CRF"

# vitals
bp <- read_sas(unz(foldername, "raw_oc/vit.sas7bdat")) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)

# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, ct, patd)
rm(folderout, foldername, ptrn)


foldername <- "BI502_392" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, "raw_oc/pat3.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Adminsitration of drugs dataset, note will move NAs (ie drug not administrered) to the end
# So will only be slecetd of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, "raw_oc/adm1.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                                        n
# <chr>                                   <int>
#   1 ""                                      14561
# 2 Arthroscopy                                 1
# 3 Bronchitis                                  1
# 4 Dandruff                                    1
# 5 Diabetes mellitus non-insulin-dependent     1
# 6 Drug hypersensitivity                       1
# 7 Dyspepsia                                   1
# 8 Hot flush                                   1
# 9 Menopause                                   1
# 10 Paraesthesia                                1
# 11 Postmenopause                               2
# 12 Spinal compression fracture                 1


# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, "raw_oc/ct.sas7bdat")) %>% names_to_lower()
table(conmed$cpevent) # all visit 1
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt = adp_ctpn, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1" ) %>% 
  select(adp_pid, ctpt = adp_ctpn) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, "raw_oc/phy2.sas7bdat")) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt = wtstd, ht = htstd)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, "raw_oc/risk.sas7bdat")) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, "raw_oc/ae.sas7bdat")) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- read_sas(unz(foldername, "raw_oc/labdata.sas7bdat")) %>% names_to_lower()
#### This time labstd and labn are reported along with upper and lower limits
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  filter(labdttm <= randt) %>% 
  select(adp_pid, labnm, labstd, lab, labc, llc, ulc)

# vitals
bp <- read_sas(unz(foldername, "raw_oc/vit1vit.sas7bdat")) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)



foldername <- "BI502_413" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

### Demo
# Race clearly miscoded in this data
demo <-  read_sas(unz(foldername, paste0("raw_oc/", "pat3.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex)

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "risk.sas7bdat"))) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

## Randomisation table 
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "adm1.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

## Slightly different names to some others, some text is redacted, but not MEDDRA coded text
medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, adp_mhlt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                             n
# <chr>                        <int>
#   1 ""                           13712
# 2 Benign prostatic hyperplasia     1
# 3 Diabetic neuropathy              1
# 4 Gastritis                        1
# 5 Obesity                          1

# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

### Conmed
ct <- read_sas(unz(foldername, paste0("raw_oc/", "ct.sas7bdat"))) %>% names_to_lower()
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()

# Vitals
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "phy2.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht)
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "ae.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- read_sas(unz(foldername, paste0("raw_oc/", "labdata.sas7bdat"))) %>% names_to_lower()
labs <- labs  %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent, labdttm) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt = visdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx)

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "vit1.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, ct)
rm(folderout, foldername, ptrn)



foldername <- "BI502_550" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

### Demo
# Race clearly miscoded in this data
demo <-  read_sas(unz(foldername, paste0("raw_oc/", "patd.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racei) %>% 
  mutate(racei = factor(racei, levels = 1:5, labels = c("American Indian/Alaskan Native",
                                                        "Asian",
                                                        "Black/African American",
                                                        "Hawain/Pacific Islander",
                                                        "White")) )

# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "patd.sas7bdat"))) %>% names_to_lower()
smoke <- smoke %>%
  select(adp_pid,  smokcd)  %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))

## Randomisation table 
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "adm.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

### Medhist, all are visit 1
medhist <-  read_sas(unz(foldername, paste0("raw_oc/", "bconcd.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist$cpevent)
# Some variables 
medhist <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, adp_msoc)

### Conmed
ct <- read_sas(unz(foldername, paste0("raw_oc/", "ctt.sas7bdat"))) %>% names_to_lower()
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()

# Vitals, no data in vitals at all
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "patd.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht, htu, wtu) %>% 
  mutate(htu = factor(htu, 41:42, c("in", "cm")),
         wtu = factor(wtu, c(21, 23), c("lb", "kg"))) %>% 
  mutate(ht = if_else(htu == "in", ht*2.54, ht),
         wt = if_else(wtu == "lb", wt/2.205, wt))
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt = wt)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "ae.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- read_sas(unz(foldername, paste0("raw_oc/", "labdata.sas7bdat"))) %>% names_to_lower()
labs <- labs  %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent, labdttm) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt = visdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx)

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "vit.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, ct)
rm(folderout, foldername, ptrn)



foldername <- "BI1276_1"  # extract ----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

### rand
rand <- read_sas(unz(foldername, paste0("raw_oc/", "rand.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study) %>% 
  filter(!is.na(tpatt), tpatt != "ZZZZZ")

### Demo
basco <- read_sas(unz(foldername, paste0("ads_ads/", "bascodem.sas7bdat"))) %>% names_to_lower() 
demo <- basco %>% 
  select(adp_pid, bage, sexdc, bhts, bwts, bbmi, raceidc, smokcddc)

### Medhist "bcond - This dataset contains information about baseline conditions"
bcond <- read_sas(unz(foldername, paste0("ads_ads/", "bcond.sas7bdat"))) %>% names_to_lower() 
medhist <- bcond %>% 
  select(adp_pid, mpt, mptcd, msoccd, msoc) 

### Conmed
ct <- read_sas(unz(foldername, paste0("raw_oc/", "ct.sas7bdat"))) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn, adp_wver, visdt)  %>% 
  inner_join(rand %>%  select(adp_pid, randt = randdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()

### Labs
labs <- read_sas(unz(foldername, paste0("ads_ads/", "lab.sas7bdat"))) %>% names_to_lower()

### Labs
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdt <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx)  

fpg <- read_sas(unz(foldername, paste0("raw_oc/", "fpg.sas7bdat"))) %>% names_to_lower()
fpg <- fpg %>% 
  select(adp_pid, sp, spu, blddt) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(blddt <= randdt) %>% 
  select(adp_pid, lab = sp) %>% 
  mutate(labnm = "FPG")

labs <- bind_rows(labs, fpg)

### Vitals
vitals <- basco %>% 
  select(adp_pid, bsysse, bdiase)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals, basco, bcond, ct, fpg)
rm(folderout, foldername, ptrn)



foldername <- "BI502_376" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, paste0("raw_oc/", "pat3.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Adminsitration of drugs dataset, note will move NAs (ie drug not administrered) to the end
# So will only be slecetd of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "adm1.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                    n
# <chr>               <int>
#   1 ""                   7592
# 2 Spinal disorder NOS     1


# All visits are visit 1, this is a screening  @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, paste0("raw_oc/", "ctct.sas7bdat"))) %>% names_to_lower()
table(conmed$cpevent) # all visit 1
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt = adp_ctpn, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1" ) %>% 
  select(adp_pid, ctpt = adp_ctpn) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "phy2.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht, htu, wtu) %>% 
  mutate(htu = factor(htu, 41:42, c("in", "cm")),
         wtu = factor(wtu, c(21, 23), c("lb", "kg"))) %>% 
  mutate(ht = if_else(htu == "in", ht*2.54, ht),
         wt = if_else(wtu == "lb", wt/2.205, wt))
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>% # @SCREEN
  select(adp_pid, ht, wt)


# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "risk.sas7bdat"))) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "aeaea.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- tibble(note = "No lab data in the dictionary or in the folder")
#### This time labstd and labn are reported along with upper and lower limits

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "vit1.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)



foldername <- "BI502_396" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, paste0("raw_oc/", "pat.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Administration of drugs dataset, note will move NAs (ie drug not administered) to the end
# So will only be selected of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "adm1.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# cdpt                         n
# <chr>                    <int>
#   1 ""                        9312
# 2 Anaemia                      1
# 3 Cataract                     2
# 4 Coronary artery disease      1
# 5 Diabetic retinopathy         2
# 6 Hepatic cirrhosis            1
# 7 Myopia                       1
# 8 Obesity                      1
# 9 Presbyopia                   1
# 10 Retinopathy hypertensive     1


# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, paste0("raw_oc/", "ct.sas7bdat"))) %>% names_to_lower()
table(conmed$cpevent) # Not all are  VISIT 1, so select visit 1 @SCREEN
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt = adp_ctpn, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% # @screen
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1" ) %>% 
  select(adp_pid, ctpt = adp_ctpn) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "phy2.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht, htu, wtu) %>% 
  mutate(htu = factor(htu, 41:42, c("in", "cm")),
         wtu = factor(wtu, c(21, 23), c("lb", "kg"))) %>% 
  mutate(ht = if_else(htu == "in", ht*2.54, ht),
         wt = if_else(wtu == "lb", wt/2.205, wt))
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)


# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "risk.sas7bdat"))) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "ae.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
### Labs
labs <- read_sas(unz(foldername, paste0("raw_oc/", "labdata.sas7bdat"))) %>% names_to_lower()
### Labs
labs <- labs %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt = admdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx) 

#### This time labstd and labn are reported along with upper and lower limits

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "vit1.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)



foldername <- "BI1199_32" # extract ----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

### rand
rand <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "rand.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study) %>% 
  filter(!is.na(tpatt), tpatt != "")

### Demo
basco  <- read_sas(unz(foldername, paste0(ptrn, "/ads_ads/", "basco.sas7bdat"))) %>% names_to_lower()
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokdc)

### Medhist, all are visit 1
medhist <-  read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "bconcd.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

### Conmed
ct <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "ct.sas7bdat"))) %>% names_to_lower()
ct <- ct %>%
  semi_join(rand %>%  select(adp_pid))
## 498 of 514 participants have one or more row in ct, looking at protocol, defines begin data as either "C"
### in ctbegc, or else supplies date
conmed <- ct %>% # @SCREEN @ERROR
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = randdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" |ctbegc == "C") %>% 
  select(adp_pid) %>% # adp_ctpn removed from select as var does not exist 
  distinct()
### 451 of 498 participants have a drug defined as on or before randomisation
ct2 <- ct %>%
  anti_join(conmed)

### Labs
labs <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "bilab2.sas7bdat"))) %>% names_to_lower()
### Labs
labs <- labs %>% 
  mutate(labdt %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdt <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx) 

### vitals 
vitals  <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "phys.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(cpevent == "VISIT 1") %>% 
  select(adp_pid, dia, sys)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals, basco, ct, ct2)
rm(folderout, foldername, ptrn)



foldername <- "BI1199_34" # extract ----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))


### rand
rand <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "rand.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study) %>% 
  filter(!is.na(tpatt), tpatt != "")

### Demo
basco  <- read_sas(unz(foldername, paste0(ptrn, "/ads_ads/","basco.sas7bdat"))) %>% names_to_lower()
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokdc)

### Medhist, all are visit 1
medhist <-  read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "bconcd.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

### Conmed
ct <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "ct.sas7bdat"))) %>% names_to_lower()
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = randdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" |ctbegc == "C") %>% 
  select(adp_pid) %>% # , adp_ctpn removed from this select call 
  distinct()

### Labs
labs <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "bilab2.sas7bdat"))) %>% names_to_lower()
### Labs
labs <- labs %>% 
  mutate(labdt %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdt <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx) 

### vitals 
vitals  <- read_sas(unz(foldername, paste0(ptrn, "/raw_raw/", "phys.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(cpevent == "VISIT 1") %>% 
  select(adp_pid, dia, sys)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals, basco, ct)
rm(folderout, foldername, ptrn)



foldername <- "BI244_2484"# extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

# Note patients randomised at visit 1

## demo variables  -- HELP, STILL DONT HAVE 
# all visit 1
demo <-  read_sas(unz(foldername, paste0("raw_oc/", "pt1a.sas7bdat"))) %>% names_to_lower()
demo <- demo %>%
  select(adp_pid, age, sex, racea) %>%
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Administration of drugs dataset, note will move NAs (ie drug not administered) to the end
# So will only be selected of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, paste0("ads_ads/", "ept.sas7bdat"))) %>% names_to_lower()
rand <- rand %>%
  select(tpatt = name, adp_pid) %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  distinct()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt, cdsys, cdi, visdt, cpevent, actevent)

# Note no 0 indicators, all are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()


# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 11 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, paste0("raw_oc/", "ctct.sas7bdat"))) %>% names_to_lower()
table(conmed$cpevent) # Not all are  VISIT 1, so select visit 1 @SCREEN
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, ctpt, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1" ) %>% # Select visit 1 here @SCREEN 
  select(adp_pid, ctpt, ctrte) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "phys.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>% # @SCREEN
  select(adp_pid, actevent, cpevent, wt, ht, htu, wtu) %>% 
  mutate(htu = factor(htu, 41:42, c("in", "cm")),
         wtu = factor(wtu, c(21, 23), c("lb", "kg"))) %>% 
  mutate(ht = if_else(htu == "in", ht*2.54, ht),
         wt = if_else(wtu == "lb", wt/2.205, wt))
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>%
  select(adp_pid, ht, wt)


# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "pt1a.sas7bdat"))) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, cpevent, smokcd)
smoke <- smoke %>% 
  filter(cpevent == "VISIT 1") %>% # @SCREEN
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "aeaea.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, adp_mpt = aeptit, adp_msoc = aesoc)

# labs
labs <- read_sas(unz(foldername, paste0("raw_oc/", "labblabb.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1") %>% # @SCREEN 
  select(adp_pid, labstd, labnm) 


#### This time labstd and labn are reported along with upper and lower limits

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "phys.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1") %>% # @SCREEN 
  select(adp_pid, sys, dia)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)



foldername <- "BI502_397" # extract #----

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
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

# Note patients randomised at visit 1

## demo variables
# all visit 1
demo <-  read_sas(unz(foldername, paste0("raw_oc/", "pat.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(adp_pid, actevent, cpevent, visdt, age, sex, racea) %>% 
  mutate(racea = factor(racea, 1:3, c("white", "black", "other")))

## Administration of drugs dataset, note will move NAs (ie drug not administered) to the end
# So will only be selected of never had drug administered
# use it to define pre-treatment BP, labs etc
rand <-  read_sas(unz(foldername, paste0("raw_oc/", "adm1.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  filter(tpatt != "", !is.na(tpatt)) %>% 
  select(adp_pid, actevent, visdt, tpatt, admdt) %>% 
  arrange(adp_pid, admdt) %>% 
  group_by(adp_pid) %>% 
  slice(1) %>% 
  ungroup()

# Medical history 
medhist_list_char <- c("cdallg.sas7bdat", "cdcard.sas7bdat", "cdgast.sas7bdat", "cdmetb.sas7bdat", 
                       "cdmusd.sas7bdat", "cdneur.sas7bdat", "cdoth.sas7bdat", "cdpsyc.sas7bdat", 
                       "cdpulm.sas7bdat", "cdrenl.sas7bdat", "cdrepd.sas7bdat")

medhist_list <- map(paste0("raw_oc/", medhist_list_char), ~ read_sas(unz(foldername, .x)) %>%  names_to_lower())
names(medhist_list) <- medhist_list_char

# Checked, names consistent across tables
# medhist_list_names <- map(medhist_list, names)
# medhist_list_names <- bind_rows(medhist_list_names)

medhist <- bind_rows(medhist_list)
medhist2 <- medhist %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys, cdi, visdt, cpevent, actevent)

# Note very few 0 indicators, vast majority most are for blank conditions, just remove
medhist2 %>% filter(cdi==0) %>% group_by(cdpt) %>%  count()
# A tibble: 16 x 2
# Groups:   cdpt [16]
# cdpt                                 n
# <chr>                            <int>
#   1 ""                               10122
# 2 Anaemia                              2
# 3 Asthma                               1
# 4 Depression                           1
# 5 Diabetic neuropathy                  1
# 6 Drug hypersensitivity                1
# 7 Gastrooesophageal reflux disease     1
# 8 Goitre                               1
# 9 Hypercholesterolaemia                1
# 10 Hypertension                         1
# 11 Hysterectomy                         2
# 12 Oedema peripheral                    1
# 13 Postmenopause                        1
# 14 Retinopathy                          1
# 15 Seasonal allergy                     1
# 16 Ventricular hypertrophy              1


# All visits are visit 1, this is a screening diagnosis @SCREEN
table(medhist2$cpevent)
# Some variables 
medhist <- medhist %>% 
  filter(is.na(cdi) | cdi != 0) %>% 
  select(adp_pid, cdpt = adp_mpt, cdsys)
rm(medhist2, medhist_list_char, medhist_list)

# Concomittant medication
# For some reason conmed includes visit 12, 14 and 7, use dates to identify in pre-existing medication
conmed <-  read_sas(unz(foldername, paste0("raw_oc/", "ct.sas7bdat"))) %>% names_to_lower()
table(conmed$cpevent) 
conmed2 <- conmed %>% 
  select(adp_pid, actevent, cpevent, visdt, ctpt = adp_ctpn, ctbegdt, cti)
conmed2 %>% 
  filter(cti ==0) %>% 
  group_by(ctpt) %>% 
  count()

## Select where visit date or drug begin date is before the first drug administration
conmed <- conmed %>% 
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid)) %>% 
  filter(cpevent == "VISIT 1" ) %>% # @SCREEN
  select(adp_pid, ctpt = adp_ctpn) %>% 
  distinct()
rm(conmed2)

# Vitals
htwt <- read_sas(unz(foldername, paste0("raw_oc/", "phy2.sas7bdat"))) %>% names_to_lower()
htwt <- htwt %>% 
  select(adp_pid, actevent, cpevent, visdt, wt, ht, htu, wtu) %>% 
  mutate(htu = factor(htu, 41:42, c("in", "cm")),
         wtu = factor(wtu, c(21, 23), c("lb", "kg"))) %>% 
  mutate(ht = if_else(htu == "in", ht*2.54, ht),
         wt = if_else(wtu == "lb", wt/2.205, wt))
htwt <- htwt %>% 
  filter(cpevent == "VISIT 1") %>% # @SCREEN
  select(adp_pid, ht, wt)


# Smoking, all are visit 1
smoke <- read_sas(unz(foldername, paste0("raw_oc/", "risk.sas7bdat"))) %>% names_to_lower()
smoke2 <- smoke %>%
  select(adp_pid, actevent, cpevent, visdt, smokcd)
smoke <- smoke %>% 
  select(adp_pid, smokcd) %>% 
  mutate(smokcd = factor(smokcd, 0:2, c("never", "ex", "current")))
rm(smoke2)

# adverse
ae <- read_sas(unz(foldername, paste0("raw_oc/", "ae.sas7bdat"))) %>% names_to_lower()
ae <- ae  %>%
  select(adp_pid, actevent, cpevent, visdt, adp_mpt, adp_msoc)

# labs
labs <- read_sas(unz(foldername, paste0("raw_oc/", "labblabb.sas7bdat"))) %>% names_to_lower()

#### For some reason labstd and labn are all null, having to rely on lab character!!!
## THe following selects the pre randomisation lab results
labs <- labs %>% 
  inner_join(rand %>%  select(adp_pid, randt = admdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, labnm, labc)
labs$labc[is.na(as.double(labs$labc))] %>% unique() # ERROR - NAs
# [1] ""   "<6" 

labs <- labs %>% 
  filter(!labc == "", !is.na(labc)) %>% 
  mutate(labstd = case_when(
    labc == ">9999" ~  9999,
    labc == "<10" ~ 10,
    labc == "<0.10" ~ 0.10,
    labc == ">9.0" ~ 9.0,
    labc == "NA" ~ NA_real_,
    labc == "<1.1" ~ 1.1,
    labc == "<20" ~ 1.1,
    TRUE ~ as.double(labc)))

# Count number of NAs
labs %>% summarise_all(~sum(is.na(.))) # 2 NAs in labstd, 0 in other 3 columns


#### This time labstd and labn are reported along with upper and lower limits

# vitals
bp <- read_sas(unz(foldername, paste0("raw_oc/", "vit1.sas7bdat"))) %>% names_to_lower()
bp <- bp %>% 
  inner_join(rand %>%  select(adp_pid, randt = visdt)) %>% 
  filter(visdt <= randt) %>% 
  select(adp_pid, sys1, sys2, sys3, dia1, dia2, dia3)


# Save data  
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, htwt, bp, rand, ae, smoke)
rm(folderout, foldername, ptrn)





foldername <- "BI248_543" # @MISSING extract #----

foldername <- "V:/248.543/248.5430_VIVLI_v01.zip"
folderout <- "D:/BI248_543/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))


basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### rand
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study) %>% 
  filter(!is.na(tpatt), tpatt != "")

### Demo
demo <- basco %>% 
  select(adp_pid, age, sex, bmi, racea)

### Medhist
bconcd <- read_sas(paste0(foldername, "bconcd.sas7bdat")) %>% names_to_lower()
medhist <- bconcd %>% 
  select(adp_pid, actevent, adp_mpt, adp_mptcd, adp_msoccd, adp_msoc)

### Conmed
ct <- read_sas(paste0(foldername, "ct.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  select(adp_pid, ctbegc, ctbegdt, adp_ctpn,  adp_wver, visdt) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(ctbegc == "C" | visdt <= randdt | ctbegdt <= randdt) %>% 
  select(adp_pid, adp_ctpn)

### Labs
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>% names_to_lower()
### Labs, note don't need labs 2
labs <- labs %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent, labdttm)  %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx)

### vitals 
vitals  <- read_sas(paste0(foldername, "phys.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia1, sys1, dia2, sys2,  htstd, wtstd, bmi, visdt, cpevent) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(visdt <= randdt | cpevent == "VISIT 1") %>% 
  select(adp_pid, dia1, sys1, dia2, sys2,  htstd, wtstd, bmi)

## Adverse events, dont save as very similar to BI248_525 so can hopefully add in there without too much difficulty
ae <- read_sas(paste0(foldername, "ae.sas7bdat")) %>% names_to_lower()
ae <- ae %>% 
  select(adp_pid, actevent, adp_mpt, adp_msoc, adp_mhlt, adp_mhlgt)

# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labs, vitals)
rm(folderout, foldername)


foldername <- "BI1160_24" # @MISSING extract #----


foldername <- paste0("", FindFx("1160.24"))
folderout <- "D:/BI1160_24/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

ct  <- read_sas(paste0(foldername, "ctt.sas7bdat")) %>% names_to_lower()
rand    <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
medhist <- read_sas(paste0(foldername, "bconcd.sas7bdat")) %>% names_to_lower()
vitals  <- read_sas(paste0(foldername, "bcon.sas7bdat")) %>% names_to_lower()
basco <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>%  names_to_lower()

### Randomisation data
rand <- rand %>% 
  select(adp_pid, randdt, tpatt, study) %>% 
  filter(!is.na(tpatt), tpatt != "")

### conmed data
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = randdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()

### Medical history
medhist <- medhist %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

### Labdata
labs <- labs  %>% 
  select(adp_pid, visno, labstd, labstdu, llc, ulc, labnm, labnmx, cpevent, labdttm) %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx)

### vital signs
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(visdt <= randdt) %>% 
  select(adp_pid, dia, sys)

### Demographics
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc)

### Save and delete to prevent being read in next file
save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))
rm(rand, demo, medhist, conmed, labs, vitals)

foldername <- "BI1160_48" # @MISSING extract ----


foldername <- paste0("", FindFx("1160.48"))
folderout <- "D:/BI1160_48/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))


basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### rand
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study) %>% 
  filter(!is.na(tpatt), tpatt != "")

### Demo
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokcddc)

### Medhist, all are visit 1
medhist <-  read_sas(paste0(foldername, "bconcd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

### Conmed
ct <- read_sas(paste0(foldername, "ctt.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = randdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()
### Labs
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>% names_to_lower()
### Labs
labs <- labs %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx) 


### vitals 
vitals  <- read_sas(paste0(foldername, "bcon.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(visdt <= randdt) %>% 
  select(adp_pid, dia, sys)


save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))
rm(rand, demo, medhist, conmed, labs, vitals)


foldername <- "BI1160_64" # @MISSING extract ----


foldername <- paste0("", FindFx("1160.64"))
folderout <- "D:/BI1160_64/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|vitals|labs||patd|pat3|adm1"))

basco  <- read_sas(paste0(foldername, "basco.sas7bdat")) %>% names_to_lower()

### rand
rand <- read_sas(paste0(foldername, "rand.sas7bdat")) %>% names_to_lower()
rand <- rand %>% 
  select(adp_pid, tpatt, randdt, study) %>% 
  filter(!is.na(tpatt), tpatt != "")

### Demo
demo <- basco %>% 
  select(adp_pid, age, sex, htstd, wtstd, bmi, racedc, smokcddc)

### Medhist, all are visit 1
medhist <-  read_sas(paste0(foldername, "bconcd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(adp_pid, adp_mpt, actevent, adp_mptcd, adp_msoc, adp_msoccd, cpevent)

### Conmed
ct <- read_sas(paste0(foldername, "ctt.sas7bdat")) %>% names_to_lower()
conmed <- ct %>%
  filter(is.na(cti) | cti !=0) %>% 
  inner_join(rand %>%  select(adp_pid, randt = randdt)) %>% 
  filter(ctbegdt <= randt | visdt <= randt | cpevent == "VISIT 1" ) %>% 
  select(adp_pid, adp_ctpn) %>% 
  distinct()
### Labs
labs <- read_sas(paste0(foldername, "labdata.sas7bdat")) %>% names_to_lower()
### Labs
labs <- labs %>% 
  mutate(labdttm = str_sub(labdttm, 1, 8) %>%  lubridate::ymd()) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(labdttm <= randdt) %>% 
  select(adp_pid, labstd, labstdu, llc, ulc, labnm, labnmx) 

### vitals 
vitals  <- read_sas(paste0(foldername, "bcon.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(adp_pid, actevent, dia, sys, visdt, cpevent) %>% 
  inner_join(rand %>%  select(adp_pid, randdt)) %>% 
  filter(visdt <= randdt) %>% 
  select(adp_pid, dia, sys)


save(rand, demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))
rm(rand, demo, medhist, conmed, labs, vitals)




# Consolidate all files ----
## The following consolidates the data and ensures all matches

## Combine all files into a single dataframe ----
#List folder names
foldernames <- list("BI107_210",
                    "BI248_629", 
                    "BI502_254", "BI502_256", "BI502_316", "BI502_317", 
                    "BI502_391", "BI502_392", "BI502_413", "BI502_550",
                    "BI502_376", "BI502_396", "BI502_327", "BI502_397",
                    "BI1276_1",
                    "BI1199_32", "BI1199_34", 
                    "BI244_2484")

# Missing - ADD TO FOLDERNAMES ABOVE when we get them
# "BI248_543", "BI1160_24", "BI1160_48", "BI1160_64", 
                    
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

# Seperate out vitals, bp and htwt
vitals     <- map(all_trials, ~ .x[ names(.x) %in% c("vitals", "bp", "htwt")])
all_trials <- map(all_trials, ~ .x[!names(.x) %in% c("vitals", "bp", "htwt")])
tbl_names <- map(all_trials, names) 
do.call(rbind, tbl_names)

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)

# Add back in vitals, note that this still needs transposed
all_trials$vitals <- vitals
rm(vitals)

## Randomisation table ----
rand <- all_trials$rand
CreateTableNameLabels(rand)
names(rand)

# treat actual treatment date, randomisation date and visit date as the same
rand$BI107_210$visdt <- NULL
rand <- map(rand, function(x) {
  names(x)[names(x) %in% c("atrstdt", "randdt", "visdt")] <- "randdt"
  x[ , names(x) %in% c("adp_pid", "randdt", "tpatt")]
})
rand <- BindRowsWLabels(rand)

# Limit to randomised patients, note only two here as excluded this
rand <- rand %>% # @SCREEN
  filter(tpatt != "")

# Take minimum randomisation date, does not lose any patients
rand <- rand %>% 
  group_by(trial, adp_pid, tpatt) %>% 
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
  names(x)[names(x) %in% c("race", "racea","raceadc", "racedc", "racei", "raceidc", "ethnicdc")] <- "race"
  names(x)[names(x) %in% c("smokcd", "smokcddc", "smokdc")] <- "smoke"
  names(x)[names(x) %in% c("bhts", "ht", "htstd")] <- "ht"
  names(x)[names(x) %in% c("bwts", "wt", "wtstd")] <- "wt"
  x
})
CreateTableNameLabels(demo)
demo <- map(demo, function(current_df) {
  x <- current_df
  if("race" %in% names(x))   x$race <- as.character(x$race)
  x$sex <- as.character(x$sex)
  if("smoke" %in% names(x)) x$smoke <-  as.character(x$smoke)
  x}
)
CreateTableNameLabels(demo)
demo <- BindRowsWLabels(demo, 3)
# limit to randomised patients
demo <- demo %>% # @SCREEN
  semi_join(rand)

## Labs table ----
labs <- all_trials$labs
# Next line is to convert note saying no lab data so matches other tables
labs$BI502_391 <- tibble(note = labs$BI502_391)
CreateTableNameLabels(labs)

## Join all into single table, drop first those without any data
labs <- BindRowsWLabels(labs[!names(labs) %in% c("BI107_210", "BI248_629", "BI502_376","BI502_391")], 1)

## Limit to randomised patients
labs <- labs %>% 
  semi_join(rand) 

## Select wanted measures
labs_rv <- labs %>% distinct(labnm, labnmx) %>% 
  filter(labnm != "") %>% 
  group_by(labnm) %>% 
  summarise(labnmx = paste(labnmx, collapse = " | "))
labs_slctd_prev <- read_csv("Created_metadata/reivewED_bi_labs.csv") %>% 
   filter(keep ==1) %>% 
   select(-keep)
# write_csv(labs_rv, "Scratch_data/reivew_bi_labs.csv")

labs_slctd <- read_csv('labnm,labnmx
CRE,Creatinine |  | Baseline creatinine [umol/L]
GFRC,Creatinine clearance |
  GLU,Glucose | Baseline fasting plasma glucose [mg/dL]
FPG,Glucose | Baseline fasting plasma glucose [mg/dL]
HGB,Haemoglobin |  | Baseline haemoglobin [g/L]
PLTCT,Platelet count | Platelets |
  SGOT,"AST/GOT, SGOT |"
SGPT,"ALT/GPT, SGPT |"')

labs <- labs %>% 
  semi_join(labs_slctd %>% select(labnm))

## Examine labs
labs_check <- labs %>% 
  group_by(labnm, trial) %>% 
  count() %>% 
  spread(key = labnm, value = n)
## Double checked - three trials have no glucose measures in the labs table.
labs_check %>% filter(is.na(FPG) & is.na(GLU)) %>%  pull(trial) %>% dput()
c("BI502_254") # Missing trials "BI1160_48", "BI1160_64" add back into brackets  

## Medical history table ----
medhist <- all_trials$medhist
# rename BI502_550
medhist$BI502_550  <- medhist$BI502_550 %>% 
                        rename(cdsys = adp_msoc)

medhist1 <- medhist [c("BI107_210", "BI502_254", "BI502_256", "BI502_316", "BI502_317",
                       "BI502_391", "BI502_392", "BI502_413", "BI502_550",
                       "BI502_376", "BI502_396","BI244_2484", "BI502_327", "BI502_397")]
CreateTableNameLabels(medhist1)
medhist1 <- BindRowsWLabels(medhist1, chosen_row = 12)

medhist2 <- medhist[c("BI248_629", "BI1199_32",  "BI1199_34")] # Missing trials "BI1160_24", "BI1160_48",  "BI248_543", "BI1160_64"
medhist2 <- BindRowsWLabels(medhist2)
medhist3 <- BindRowsWLabels(medhist["BI1276_1"])
                            
setdiff(names(medhist), unique(c(medhist1$trial, medhist2$trial, medhist3$trial)))

medhist1 <- medhist1 %>% 
  rename(mpt = cdpt)
names(medhist2) <- c("trial", "actevent", "mpt", "mptcd", "msoc", "msoccd", "adp_pid", 
                     "cpevent")

meddra <- bind_rows(medhist1 %>%  select(trial, adp_pid, mpt),
                          medhist2 %>%  select(trial, adp_pid, mpt),
                          medhist3 %>%  select(trial, adp_pid, mpt)) %>% 
  distinct()

meddra_lkp <- bind_rows(medhist2, medhist3) %>% 
  distinct(mpt, mptcd, msoc, msoccd)
# 50% of terms in medhist 1 are in the "meddra LKP". CHecked, and the 50% not present ARE, verbatim, in the meddra browser
setdiff(str_to_lower(medhist1$mpt), str_to_lower(meddra_lkp$mpt))

## Concomittant medicines table ----
conmed <- all_trials$conmed
CreateTableNameLabels(conmed)
conmed <- map(conmed, function(x) {
  names(x)[names(x) == "adp_ctpn"] <- "ctpt"
  x
})
conmed <- BindRowsWLabels(conmed)
conmed %>%
  group_by(trial) %>% 
  summarise_at(vars(ctpt, ctrte), function(x) mean(!is.na(x)))

## Vitals table ----
vitals <- all_trials$vitals

vitals1 <- vitals[c("BI107_210", 
                    "BI1276_1", "BI248_629", "BI1199_32", "BI1199_34")] # Missing trials "BI1160_24", "BI1160_48",  "BI1160_64","BI248_543"
vitals1 <- map(vitals1, ~ .x$vitals)
CreateTableNameLabels(vitals1)
## NOTE NO BMI WHERE DO NOT HAVE HEIGHT, AND NO BMI WHERE DO NOT HAVE WEIGHT
vitals1 <- map(vitals1, function(x) {
  names(x)[names(x) %in% c("htstd", "htstd2")] <- "ht"
  names(x)[names(x) %in% c("wtstd")] <- "wt"
  names(x)[names(x) %in% c("bdiase", "dia")] <- "dia1"
  names(x)[names(x) %in% c("bsysse", "sys")] <- "sys1"
  x
})
CreateTableNameLabels(vitals1)
vitals1 <- BindRowsWLabels(vitals1)

htwt1 <- vitals1 %>% 
  select(trial, adp_pid, ht, wt)

bp1 <- vitals1 %>% 
  select(trial, adp_pid, sys1, dia1)

vitals2 <- vitals[c("BI502_254", "BI502_256", "BI502_316", "BI502_317",
                    "BI502_391", "BI502_392", "BI502_413", "BI502_550",
                    "BI502_376", "BI502_396", "BI244_2484", "BI502_327", "BI502_397")]
vitals2 <- transpose(vitals2)

bp2 <- vitals2$bp
htwt2 <- vitals2$htwt
bp2 <- BindRowsWLabels(bp2)
htwt2 <- BindRowsWLabels(htwt2)

bp <- bind_rows(bp1, bp2)
htwt <- bind_rows(htwt1, htwt2)

bmi <- htwt %>%
  mutate(bmi = wt/(ht/100)^2) %>% 
  gather("param", "value", ht, wt, bmi, na.rm = TRUE) 

bmi_demo <- demo %>% 
  select(trial, adp_pid, ht, wt, bmi) %>% 
  # mutate(bmi = if_else(is.na(bmi), wt/(ht/100)^2, bmi)) %>% 
  gather("param", "value", ht, wt, bmi, na.rm = TRUE)

bmi <- bind_rows(bmi, bmi_demo)

demo <- demo %>% 
  select(-actevent, -cpevent, -bmi, -ht, -wt, -visdt)

## Convert data into a single object ----
bi <- list(conmed = conmed, demo = demo, labs = labs, medhist = meddra,
     bp = bp, bmi = bmi, rand = rand)
# rename and delete irrelevant variables so match across companies ----
bi$medhist <- bi$medhist %>% 
  select(trial, adp_pid, term = mpt) %>% 
  mutate(meddra = "meddra")

# Rename all id variables
bi <- map(bi, ~ .x %>%  rename(id = adp_pid))

bi$bp <- bi$bp %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)


## Identify arm labels ----
#ReadPopu <- function(BI, tblname, limitto = TRUE) {
 # folder_pre  <- "E:/Research Project 1732/files_BI_"
#  folder_post <- "/Files/raw_oc/"
#  foldername <- str_sub(BI, 3) 
#  foldername <- paste0(folder_pre, foldername, folder_post)
 # mydf <- read_sas(paste0(foldername, tblname, ".sas7bdat")) %>%  names_to_lower()
#  if (limitto == TRUE) mydf <- mydf %>%  distinct(tpatt, tpattlbl)
 # mydf
#}

## Identify arm labels(edited 30/03/22) ----
ReadPopu_new <- function(BI, tblname, limitto = TRUE) {
  ptrn <- sub("_", ".", BI)
  ptrn <- sub("BI", "", ptrn)
  foldername <- FindFx(ptrn)
  if (ptrn == 1199.32 | ptrn == 1199.34) {
    mydf <- read_sas(unz(foldername, paste0(str_sub(foldername, start = -15, end = -9), "/raw_raw/", tblname, ".sas7bdat"))) %>% names_to_lower()
  } else {
    mydf <- read_sas(unz(foldername, paste0("raw_oc/", tblname, ".sas7bdat"))) %>% names_to_lower()
  }
  if (limitto == TRUE) mydf <- mydf %>%  distinct(tpatt, tpattlbl)
  mydf
}

#demo <-  read_sas(unz(foldername, "raw_oc/pat5.sas7bdat")) 

## All have same table name inside same folder
## One has a different label name

bi_new_lkp <- c(
  "BI107_210",
  "BI248_629",
  "BI502_254", "BI502_256", "BI502_316", "BI502_317", 
  "BI502_391", "BI502_392", "BI502_413", "BI502_550",
  "BI502_376", "BI502_396", "BI1199_32", "BI1199_34", "BI244_2484",
  "BI502_327", "BI502_397") # Missing trials "BI1160_24", "BI1160_48", "BI1160_64",  "BI248_543", 

# new_new <- c("BI502_376", "BI502_396", "BI1199_32", "BI1199_34", "BI244_2484")
#new_new_new <- c("BI502_327", "BI502_397")
names(bi_new_lkp) <- bi_new_lkp

bi_new_lkp[] <- map(bi_new_lkp, ~
                      ReadPopu_new(.x, tblname = "e_tpatt", limitto = FALSE) %>% 
                      distinct(tpatt, name))

bi_new_lkp$BI1276_1 <- ReadPopu_new("BI1276_1", tblname = "e_tpatt", limitto = FALSE) %>% 
  select(tpatt, name = lname) %>% 
  distinct()

bi_new_lkp <- bind_rows(bi_new_lkp, .id = "trial")

# Read in partial lkp and rename columns to match then bind with new lkp
bi_lkp <- readRDS("E:/C_analysis_code_meta/Extract_Data/Created_metadata/partial_set_arm_lookups_bi.Rds")
bi_lkp <- bi_lkp %>% rename(tpatt = TPATT, 
                            tpattlbl = TPATTLBL)

# Check trials in partial BI lkp, someimtes all trials = 1

bi_lkp <- bind_rows(bi_lkp, bi_new_lkp %>%  rename(tpattlbl = name)) %>% 
  distinct()


BI1160_26 <- c(A = 'Dabigatran 110 mg bid', B = 'Dabigatran 110 mg bid', C = 'Warfarin')
BI1160_26 <- tibble(trial = "BI1160_26",
                    tpatt = names(BI1160_26),
                    tpattlbl = BI1160_26)
bi_lkp <- bi_lkp %>% 
  bind_rows(BI1160_26)

## review. The following seem fine
#allbi <- c("BI107_210", "BI248_629", "BI502_254", "BI502_256", "BI502_316", 
#           "BI502_317", "BI502_391", "BI502_392", "BI502_413", "BI502_550", 
#           "BI502_376", "BI502_396", "BI502_327", "BI502_397", "BI1276_1", 
 ##          "BI1199_32", "BI1199_34", "BI244_2484")
#lkpok <- c("BI107_210", "BI1160_26", "BI1199_32", "BI1199_34", "BI1276_1", 
#  "BI244_2484", "BI248_629", "BI502_254", "BI502_256", "BI502_316", 
#  "BI502_317", "BI502_327", "BI502_376", "BI502_391", "BI502_392", 
#  "BI502_396", "BI502_397", "BI502_413", "BI502_550")
#all(allbi %in% lkpok)


write_csv(bi_lkp, "Created_metadata/bi_trials_lookup_treatment_group_to_label.csv")

## Save all data ----
## Note need to run "01c_extract_bi_mi.R" to finish this off
saveRDS(bi, file = "Processed_data/bi_new_partial.Rds")
