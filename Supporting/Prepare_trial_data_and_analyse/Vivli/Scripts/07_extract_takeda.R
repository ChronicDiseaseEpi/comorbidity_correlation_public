# 07_extract_takeda
## Note that this was extracted after performing the consilidation for the others, so selected
# visits and measures during each trial extraction rahter than in consolidation phase

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

foldername <- "TKAMLN0002_C13006" # @MISSING ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername, -5)
foldername <- FindFx(ptrn)


demo  <- read_sas(paste0(foldername, "dm.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, visit, visitid, visindex, rfstdtc,
         sex_, age, ageu, race, oarm_)

conmed <- read_sas(paste0(foldername, "cm.sas7bdat")) %>% names_to_lower()
conmed <- conmed %>%
  select(usubjid, visitref, visitid, visindex,
         cmpref, cmcat, cmwhover, cmroute,
         cmstdtc, cmatctxt, cmstrf) %>% 
  filter(cmstrf == "PRE-INDUCTION")

medhist <- read_sas(paste0(foldername, "mh.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, visitid, visindex, visit, visitnum,
         mhterm, mhyn, mhstdtc) %>% 
  filter(visit == "SCREENING") %>% 
  select(-visitid, -visindex, -visit, -visitnum)

labs <- read_sas(paste0(foldername, "lb1.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, lbstresn, lbstresc, lbstresu, lbstnrlo, lbstnrhi,
         visit, lbtestcd, lbtest, lbcat, lbscat) %>% 
  filter(visit %in% c("SCREENING", "week0")) %>% 
  select(-visit) %>% 
  filter(lbtestcd %in% c("ALT", "AST", "CREAT", "GLUC", "HGB", "PLAT"))

vitals <- read_sas(paste0(foldername, "vs.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(usubjid, vsstresn, vsstresc, vsstresu, 
         visit, vstestcd, vstest) %>% 
  filter(visit %in% c("SCREENING", "week0")) %>% 
  select(-visit)

if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata"))
rm(demo, medhist, conmed, labs, vitals, foldername, folderout)

foldername <- "TKAMLN0002_C13011" # @MISSING ----
# This trial has not been provided via vivli

foldername <- "TKASYR322_305" # ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- paste0(str_sub(foldername, -7), "_IPD")  
foldername <- FindFx(ptrn) # Check returns correct trial 

## Zipped folder within a zipped folder
# Unzip trial folder 
folder_unz <- unzip(foldername)

# Unzip trial analysis folder
analysis_unz <- unzip(folder_unz[1])


demo <- read_sas(analysis_unz[6]) %>% names_to_lower() 
demo <- demo %>% 
  select(subjid, d_trtrc,
         d_weight, d_age, sexc, visitc, smokerc, d_bmi)


conmed <- read_sas(analysis_unz[5]) %>% names_to_lower()
conmed <- conmed %>%
  select(subjid, prefnam, d_pcmc, route, medtypc, d_medsdy) %>% 
  filter(d_medsdy <= 1) %>% 
  select(-medtypc, -d_medsdy, -d_pcmc)
# review4ed days on treatment against "concomitant", "prior" labels
# All prior have a negative datae, no post-treatment has a negative date
# and most concomittant have a negative date

medhist <- read_sas(analysis_unz[4]) %>% names_to_lower()
# medhist2 <- read_sas(paste0(foldername, "d_medhis.sas7bdat")) %>% names_to_lower()
# I think the second table is for the conditions ending
# it certainly has no start dates

medhist <- medhist %>% 
  select(subjid, visitc, pt_name) %>% 
  filter(visitc %in% c("PRE-SCREENING B", "SCREENING A")) %>% # @SCREEN 
  select(-visitc) %>% 
  mutate(meddra = "meddra")


labs <- read_sas(analysis_unz[24]) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visitc, ltstnam, labcat,
         sirval, siunit, silln, siuln) %>% 
  filter(visitc %in% c("PRE-SCREENING B", "SCREENING A", # @SCREEN
                       "BASELINE/RAND", "STABILIZATION -1")) %>% 
  select(-visitc) %>% 
  filter(ltstnam %in% c("ALT (SGPT)", "AST (SGOT)", "Creatinine",
                        "Creatinine Clearance, Estimated",
                        "Hemoglobin", "Platelet Count")) %>% 
  select(-labcat)
# note no glucose


vitals <- read_sas(analysis_unz[33]) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visitc, d_parmc, d_value) %>% 
  filter(visitc %in% c("BASELINE/RAND", "PRE-SCREENING B", "SCREENING A", "SCREENING B" #@SCREEN
  )) %>% 
  filter(d_parmc %in% c("BMI", "BPDIA", "BPSYS", "WEIGHT")) %>% 
  select(-visitc)


## Save
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, file = paste0(folderout, "/extract.Rdata"))
rm(demo, medhist, conmed, labs, vitals)
rm(foldername, folderout, ptrn, folder_unz, analysis_unz)
gc()



foldername <- "TKASYR322_402" # ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- paste0(sub( "_", "", paste0(str_sub(foldername, -7))), "_DATA_ANALYSIS")  
foldername <- FindFx(ptrn) # Check returns correct trial 


demo  <- read_sas(unz(foldername, "d_demog.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(subjid, d_trtrc,
         d_weight, d_age, sexc, visitc, smokerc, d_bmi)


conmed <- read_sas(unz(foldername, "d_conmed.sas7bdat")) %>% names_to_lower()
conmed <- conmed %>%
  select(subjid, prefnam, d_pcmc, medtypc, d_medsdy) %>% 
  filter(d_medsdy <= 1 | (is.na(d_medsdy) & medtypc == "Previous Medication")) %>% 
  select(-medtypc, -d_medsdy, -d_pcmc)
# reviewed days on treatment against "concomitant medication", "previous medication" labels
# All previous medication days are missing, most most concomittant have a negative date


medhist <- read_sas(unz(foldername, "d_condis.sas7bdat")) %>% names_to_lower()

medhist_lkp <- medhist %>%
  distinct(hlt_code, hlt_name, hlgt_cod, hlgt_nam, llt_code, llt_name, 
           pt_code, pt_name, soc_code, soc_name)

medhist <- medhist %>% 
  select(subjid, visitc, pt_name) %>% 
  filter(visitc %in% c("SCREENING")) %>% #@SCREEN
  select(-visitc) %>% 
  mutate(meddra = "meddra")


labs <- read_sas(unz(foldername, "d_lab.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(subjid, visitc, ltstnam, labcat,
         sirval, siunit, silln, siuln) %>% 
  filter(visitc %in% c("SCREENING",
                       "BASELINE")) %>% 
  select(-visitc) %>% 
  filter(ltstnam %in% c("ALT (SGPT)", "AST (SGOT)", "Creatinine",
                        "Creatinine Clearance, Estimated",
                        "Hemoglobin", "Platelet Count")) %>% 
  select(-labcat)
# note no glucose


vitals <- read_sas(unz(foldername, "d_vital.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(subjid, visitc, d_parmc, d_value) %>% 
  filter(visitc %in% c("SCREENING",
                       "BASELINE")) %>% 
  filter(d_parmc %in% c("BMI", "Diastolic Blood Pressure", "Systolic Blood Pressure", "Weight")) %>% 
  select(-visitc)


## Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, file = paste0(folderout, "/extract.Rdata"))
rm(demo, medhist, conmed, labs, vitals, medhist_lkp)
rm(foldername, folderout, ptrn)
gc()



# Consolidate all files ----
# consolidate
## Combine all files into a single dataframe ----
#List folder names
# Take from TEMP folder
foldernames <- list(#"C13006", @MISSING
                    # "TKAMLN0002_C13011", @MISSING 
                    "TKASYR322_305", 
                     "TKASYR322_402")

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

# Check all table names the same, they are 
tbl_names <- map(all_trials, names) 
do.call(rbind, tbl_names)

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)

## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo[1:2])
#CreateTableNameLabels(demo[3:4])

#demo1 <- BindRowsWLabels(demo[c("TKAMLN0002_C13006", "TKAMLN0002_C13011")])
demo2 <- BindRowsWLabels(demo[c("TKASYR322_305", "TKASYR322_402")])

ExtractLabel(demo2)

names_lkp <- c(
  "trial" = "trial",
  "usubjid" = "subjid",
  "subjid" = "subjid",
  "d_trtrc" = "arm",
  "oarm_" = "arm",
  "d_age" = "age",
  "sexc" = "sex",
  "sex_" = "sex",
  "race" = "race",
  "d_bmi" = "bmi",
  "d_weight" = "weight",
  "age" = "age"
)

#demo1 <- demo1[, names(demo1) %in% c(names(names_lkp), names_lkp)]
demo2 <- demo2[, names(demo2) %in% c(names(names_lkp), names_lkp)]
#names(demo1) <- names_lkp[names(demo1)]
names(demo2) <- names_lkp[names(demo2)]
demo2$subjid <- as.character(demo2$subjid)
demo <- demo2 # Until missing files added demo2 = demo 
#demo <- bind_rows(demo1, demo2)
rm(demo1, demo2)



# Select only randomised
table(demo$arm %>% is.na())
table(demo$arm)
demo <- demo %>% 
  filter(arm != "Not Randomized") # @SCREEN

## Randomisation table ----
rand <- demo %>% 
  select(trial, subjid, arm)

bmi_demo <- demo %>% 
  select(trial, subjid, weight, bmi)

demo <- demo %>% 
  select(-arm, -weight, -bmi)

## Concomittant medicines table ----
## Takeda has suppressed ATC codes for all studies
conmed <- all_trials$conmed
#conmed1 <- BindRowsWLabels(conmed[c("TKAMLN0002_C13006", "TKAMLN0002_C13011")])
conmed2 <- BindRowsWLabels(conmed[c("TKASYR322_305", "TKASYR322_402")])

#conmed1 <- conmed1 %>% 
#  select(trial, subjid = usubjid, route = cmroute, prefnam = cmpref) 
conmed2$subjid <- as.character(conmed2$subjid)
#conmed <- bind_rows(conmed1, conmed2)
conmed <- conmed2 # Until missing trials added conmed 2 = conmed

conmed <- conmed %>%
  semi_join(rand)


## Medical history table ----
medhist <- all_trials$medhist
#medhist1 <- BindRowsWLabels(medhist[c("TKAMLN0002_C13006", "TKAMLN0002_C13011")])
medhist2 <- BindRowsWLabels(medhist[c("TKASYR322_305", "TKASYR322_402")])

medhist <- medhist2 # until missing files found medhist2 = medhist

# Selct only randomised pateints
medhist <- medhist %>% 
  mutate(subjid = as.character(subjid)) %>% # @SCREEN
  semi_join(rand)

## Vitals table ----
vitals <- all_trials$vitals
#vitals1 <- BindRowsWLabels(vitals[c("TKAMLN0002_C13006", "TKAMLN0002_C13011")])
vitals2 <- BindRowsWLabels(vitals[c("TKASYR322_305", "TKASYR322_402")])

names_lkp <- c(
  "trial" = "trial",
  "usubjid" = "subjid",
  "vstestcd" = "d_parmc",
  "vsstresn" = "d_value")

#vitals1 <- vitals1[, names(names_lkp)]
#names(vitals1) <- names_lkp
vitals2$subjid <- as.character(vitals2$subjid)

#vitals <- bind_rows(vitals1, vitals2) %>% 
#  filter(d_parmc %in% c("HEIGHT", "WEIGHT", "DIABP", "SYSBP", 
 #                       "BPSYS", "BPDIA", "BMI", 
#                        "Systolic Blood Pressure", 
#                        "Diastolic Blood Pressure", 
#                        "Weight"))

vitals <- vitals2 # Until missing files found vitals2 = vitals 
vitals <- vitals %>%
  filter(d_parmc %in% c("WEIGHT", "DIABP", "SYSBP", # Height or height missing, might be from missing trials 
                        "BPSYS", "BPDIA", "BMI", 
                        "Systolic Blood Pressure", 
                        "Diastolic Blood Pressure", 
                        "Weight"))



## labs table ----
labs <- all_trials$labs
#labs1 <- BindRowsWLabels(labs[c("TKAMLN0002_C13006", "TKAMLN0002_C13011")])
labs2 <- BindRowsWLabels(labs[c("TKASYR322_305", "TKASYR322_402")])

names_lkp <- c(
  "trial" = "trial",
  "usubjid" = "subjid",
  "lbtest" = "ltstnam",
  "lbstresn" = "sirval",
  "lbstnrlo" = "silln",
  "lbstnrhi" = "siuln",
  "lbstresu" = "siunit")

#labs1 <- labs1 [ , names(names_lkp)]
#names(labs1) <- names_lkp
labs2$subjid <- as.character(labs2$subjid)
#labs <- bind_rows(labs1, labs2)
labs <- labs2 # until missing trials found labs2 = labs

## Re-organise tables so same as BI ----
rm(a, all_trials, conmed1, conmed2, labs1, labs2, medhist1, medhist2, vitals1, vitals2,
   tbl_names)


bmi <- vitals %>% 
  mutate(d_parmc = str_to_lower(d_parmc)) %>% 
  filter(d_parmc %in% c("weight", "bmi")) %>% # "height", removed
  group_by(d_parmc) %>% 
  mutate(grp_id = row_number()) %>% 
  ungroup() %>% 
  spread(key = d_parmc, value = d_value) %>% 
  select(trial, subjid, wt = weight, bmi) # ht = height,  removed 

bmi_demo <- bmi_demo %>% # bmi not found 
  rename(wt = weight)

bmi <- bind_rows(bmi, bmi_demo)
bmi <- bmi %>% 
  arrange(trial, subjid, wt) # ht removed

## bp
bp <- vitals %>%
  mutate(d_parmc = str_to_lower(d_parmc)) %>% 
  filter(d_parmc %in% c("diabp", "sysbp", "bpsys", "bpdia",
                        "systolic blood pressure", "diastolic blood pressure"))

## Create a single object ----
tak <- list(conmed = conmed, demo = demo, labs = labs, medhist = medhist,
            bp = bp, bmi = bmi, rand = rand)

## rename id variable
tak <- map(tak, ~ .x %>%  rename(id = subjid))

# gather or rename vitals tables
tak$bmi <- tak$bmi  %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)
tak$bp <- tak$bp %>% 
  rename(param = d_parmc, value = d_value)

## Save data in a single object ----
saveRDS(tak, file = "Processed_data/tak.Rds")




## Outtakes ----

# Old script (can be deleted at very end, kept in case useful
#list.files("V:/C13006/")
# foldername <- paste0("Data/", foldername, "/")
#foldername <- "V:/C13006/"
#folderout  <- paste0("D", str_sub(foldername,2))

#list.files("V:/SYR-322_305/")
#folderout <- paste0("D:/", foldername)
#foldernamez <- "V:/SYR-322_305/SYR-322_305_IPD.zip"
#unzip(foldernamez, exdir = "temp_zip")
#dir.create("D:/SYR_322_305")
#a <- list.files("temp_zip", recursive = TRUE)
#map(a, ~ {
#  print(.x)
#  file.copy(paste0("temp_zip/", .x), paste0("D:/SYR_322_305/", .x))
#  file.remove(paste0("temp_zip/", .x))
#})
#file.copy("temp_zip/SYR322_305_Analysis.zip", "D:/SYR_322_305")
#file.remove("temp_zip/SYR322_305_Analysis.zip")
#unzip("D:/SYR_322_305/SYR322_305_Analysis.zip", list = TRUE)
#unzip("D:/SYR_322_305/SYR322_305_Raw.zip", list = TRUE)
#foldernamez <- "D:/SYR_322_305/SYR322_305_Analysis.zip"


#d <- unz(foldername, paste0(tr_analysis,".zip/"))
#dd <- read_sas(unz(d, "d_demog.sas7bdat"))
#demog <- read_sas(unz(paste0(foldername, "/", tr_analysis,".zip"), "d_demog.sas7bdat")) %>% names_to_lower()
#d1 <- unzip(foldername)
#d_anl <- read_sas(unz(d1[1], tr_analysis), "d_demog.sas7bdat")  
#d2 <- unzip(paste0(foldername, ))

# Paste objects for file read ins 
#trial <- sub("-", "", str_sub(foldername, -19, -9))
#tr_analysis <- paste0(trial, "_Analysis.zip")
#tr_raw <- paste0(trial, "_Raw.zip")
