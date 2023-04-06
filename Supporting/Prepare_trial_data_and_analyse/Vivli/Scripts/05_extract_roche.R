# 05_extract_roche

#  Note:
# RCHMRA012JP is completely missing 
# "RCHWA19926", "RCHNA22823", "RCHWA17823" Missing subject ID therefore removed

##01_extract_data
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")


foldername <- "RCHNA25220"  # ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(pt, age, sex, race, hgtcm, wgtkg, trt1dy, visitdy, tobastat, rndgrp = rnd, rnddy)

medhist <- read_sas(unz(foldername,  paste0("Raw Datasets/SAS_raw/", "diag.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(pt, dgtyp, dgpt, dgsct, dgllt, dglltcd, dgbegdy, dgcpe) %>% 
  filter(dgcpe == "Screening") %>% 
  select(-dgcpe)

labs  <- read_sas(unz(foldername,  paste0("Raw Datasets/SAS_raw/", "labp.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(pt, lbparm, lbval, lblow, lbhigh, lbunit, lbvaln, lbvalc, lbfastst, lbcpe)

conmed <- read_sas(unz(foldername,  paste0("Analysis Ready Datasets/SAS_analysis/", "smedo.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
  select(pt, trpt, trsct, trrout, trtirel, trgoal)

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "efex.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(pt, efparm, efvaln, efval, efdy, efcpe) 


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals)
rm(folderout, foldername, ptrn)




foldername <- "RCHQ4881G"   # ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "demog.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(patnum, age, race, rand, sex, trtc)

labs <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "labv.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(patnum, visitdy, visitn, visit, testnm, testdes,
         labustd, silorng, sihirng, resstdc, resstdn)

medhist <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "medhx.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(patnum, visitdy, hxdy, hxraw, hxrec, hxtyp, forml) 
## 99% of history days are 0 or negarve, all visitdays are negative
## THis is clearly a baseline table

conmed <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "meds.sas7bdat"))) %>% names_to_lower()
# only 5 missing for basmed
conmed <- conmed %>% 
  select(patnum, mdc, mdrte, mdtyp, mdg, othsp, basmed, mdind)

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "vital.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(patnum, visitn, visit, visitdy, bpd, bps, ht, htu, wt, wtu)

smoking <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "tohx.sas7bdat"))) %>% names_to_lower()
smoking <- smoking %>% 
  select(patnum, visitdy, tobhx)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, smoking, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals, smoking)
rm(folderout, foldername, ptrn)


foldername <- "RCHQ4882G"   # ----


folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "demog.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(patnum, age, race, rand, sex, trtc)

labs <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "labv.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(patnum, visitdy, visitn, visit, testnm, testdes,
         labustd, silorng, sihirng, resstdc, resstdn)

medhist <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "medhx.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(patnum, visitdy, hxdy, hxraw, hxrec, hxtyp, forml) 

conmed <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "meds.sas7bdat"))) %>% names_to_lower()
# only 12 missing for basmed
conmed <- conmed %>% 
  select(patnum, mdc, mdrte, mdtyp, mdg, othsp, basmed, mdind)

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "vital.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(patnum, visitn, visit, visitdy, bpd, bps, ht, htu, wt, wtu)

smoking <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "tohx.sas7bdat"))) %>% names_to_lower()
smoking <- smoking %>% 
  select(patnum, visitdy, tobhx)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, smoking, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals, smoking)
rm(folderout, foldername, ptrn)


foldername <- "RCHQ4883G"   # ----


folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "demog.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(patnum, age, race, rand, sex, trtc)

labs <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "labv.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(patnum, visitdy, visitn, visit, testnm, testdes,
         labustd, silorng, sihirng, resstdc, resstdn)

medhist <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "medhx.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(patnum, visitdy, hxdy, hxraw, hxrec, hxtyp, forml) 

conmed <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "meds.sas7bdat"))) %>% names_to_lower()
# only 12 @MISSING for basmed
conmed <- conmed %>% 
  select(patnum, mdc, mdrte, mdtyp, mdg, othsp, basmed, mdind)

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "vital.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(patnum, visitn, visit, visitdy, bpd, bps, ht, htu, wt, wtu)

smoking <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "tohx.sas7bdat"))) %>% names_to_lower()
smoking <- smoking %>% 
  select(patnum, visitdy, tobhx)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, smoking, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals, smoking)
rm(folderout, foldername, ptrn)


foldername <- "RCHWA19924"  # ----


folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(pt, age, sex, race, hgtcm, wgtkg, trt1tc, smokstat)

medhist <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/SAS_analysis/", "sdiag.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(pt, dgtyp, dgpt, dgsct, dgmedv, dgbegdy, dgbegdp, dgcpe) %>% 
  filter(dgcpe == "SCREENING") %>%
  select(-dgcpe)

labs  <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "labp.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(pt, lbparm, lbval, lbunit, lblow, lbhigh, lbcpe)

conmed <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/SAS_analysis/", "smedo.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
  select(pt, trpt, trsct, trrout, trtirel, trreas)

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/SAS_raw/", "efex.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(pt, efparm, efval, efdy, efcpe) 

rand <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/SAS_analysis/", "demox.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  select(pt, rnd)


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, rand, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals, rand)
rm(folderout, foldername, ptrn)


foldername <- "RCHMRA012JP" # @MISSING ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
foldername <- FindFx(foldername)


demo  <- read_sas(paste0(foldername, "demo.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(pt, age, sex, race, hgtcm, wgtkg, bdsurf, rnd, trt1tc, tobayn)

medhist <- read_sas(paste0(foldername, "diag.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(pt, dgtyp, dgpt, dgsct, dgtirel, dg_lltc, dgbegsd)

labs  <- read_sas(paste0(foldername, "labp.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(pt, lbparm, lb_valns, lb_units, lbcpe)

conmed <- read_sas(paste0(foldername, "medo.sas7bdat")) %>% names_to_lower()
conmed <- conmed %>%
  select(pt, trpt, trsct, trrout, trtirel)

vitals <- read_sas(paste0(foldername, "efex.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(pt, efsd, efparm, efvaln, efval, efcpe) 

save(demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))




foldername <- "RCHWA19926"  # Missing subject ID ----


folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/", "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(age, sex, race, hgtcm, wgtkg) # @MISSING : pt, , trt1tc

medhist <- read_sas(unz(foldername,  paste0("Raw Datasets/", "diag.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(dgtyp, dgpt, dgsct, dgmedv, dgbegdp, dgcpe) %>% # @MISSING : pt, dgbegdy,
  filter(dgcpe == "SCREENING") %>% 
  select(-dgcpe)

labs  <- read_sas(unz(foldername, paste0("Raw Datasets/", "labp.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(lbparm, lbval, lbunit, lblow, lbhigh, lbcpe) # @MISSING : pt, lbdy,  

conmed <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/",  "smedo.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
  select(trpt, trsct, trrout, trtirel)  # @MISSING : pt, trreas 

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/",  "efex.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(efparm, efval, efcpe) # @MISSING : pt, efdy

rand <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/", "demox.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
  select(rnd) # @MISSING pt


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, rand, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals, rand)
rm(folderout, foldername, ptrn)


foldername <- "RCHNA22823"  # Missing subject ID ----

# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))


demo <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/", "demox.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(age, sex, race, hgtcm, wgtkg,  smokhis) # @MISSING: pt, trt1dy, visitdy, rndgrp = rnd 

medhist <- read_sas(unz(foldername, paste0("Raw Datasets/", "diag.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(dgtyp, dgpt, dgsct, dgbegsd, dgcpe) %>% # @MISSING: pt  
  filter(dgcpe == "SCREENING") %>% 
  select(-dgcpe)

labs  <- read_sas(unz(foldername, paste0("Raw Datasets/",  "labp.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(lbparm, lbval, lblow, lbhigh, lbunit, lbsd, lbcpe) # @MISSING: pt  

conmed <- read_sas(unz(foldername, paste0("Analysis Ready Datasets/", "smedo.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
  select(trpt, trsct, trrout, trtirel, trreas) # @MISSING: pt

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/",  "efex.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(efsd, efparm, efvaln, efval, efcpe) # @Missing: pt 


# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals)
rm(folderout, foldername, ptrn)



foldername <- "RCHWA17823"  # Missing subject ID ----


folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
ptrn <- str_sub(foldername,-5)
foldername <- FindFx(ptrn)

demo  <- read_sas(unz(foldername, paste0("Raw Datasets/", "demo.sas7bdat"))) %>% names_to_lower()
demo <- demo %>% 
  select(age, sex, race, hgtcm, wgtkg, bdsurf, rnd, tobayn) # @MISSING : pt, trt1tc,  

medhist <- read_sas(unz(foldername, paste0("Raw Datasets/", "diag.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(dgtyp, dgpt, dgsct, dgtirel, dgmedv) # @MISSING : pt, 

labs  <- read_sas(unz(foldername, paste0("Raw Datasets/", "labp.sas7bdat"))) %>% names_to_lower()
labs <- labs %>% 
  select(lbparm, lbval, lbunit, lbvaln, lbvalc, lbcpe) # @MISSING :pt, 

conmed <- read_sas(unz(foldername, paste0("Raw Datasets/", "medo.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
  select(trpt, trsct, trrout, trtirel, trreas) # @MISSING : pt, 

vitals <- read_sas(unz(foldername, paste0("Raw Datasets/",  "efex.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
  select(efsd, efparm, efvaln, efval, efcpe) # @MiSSING : pt, 

# Looks like @MISSING smoking history 

# Save 
if(!dir.exists(folderout)) dir.create(folderout)
save(demo, medhist, conmed, labs, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(demo, medhist, conmed, labs, vitals)
rm(folderout, foldername, ptrn)


# Consolidate all files ----
# consolidate
## Combine all files into a single dataframe ----

#List folder names
foldernames <- list("RCHNA25220", "RCHQ4881G",   "RCHQ4882G",  "RCHQ4883G", "RCHWA19924")

# Removed trials:
# @MISSING trial "RCHMRA012JP"
# Missing subject ID: "RCHWA19926", "RCHNA22823", "RCHWA17823" 

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

# Check all table names the same, they are not, some have a 
# smoking table and some a rand table
smoke <- map(all_trials, ~ .x$smoking)
smoke <- smoke[!map_lgl(smoke, is.null)]
rand <- map(all_trials, ~ .x$rand)
rand <- rand[!map_lgl(rand, is.null)]

all_trials <- map(all_trials, ~ .x[!names(.x) %in% c("smoking", "rand")])

tbl_names <- map(all_trials, names)
do.call(rbind, tbl_names)

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)
all_trials$smoke <- smoke # 3 trials 
all_trials$rand <- rand   # 2 trials 
rm(smoke, rand)

## Demographics table ----
demo <- all_trials$demo
CreateTableNameLabels(demo) 

# Rename age variable
demo <- map(demo, function(x) {
  names(x)[names(x) %in% c("patnum", "pt")] <- "pt"
  names(x)[names(x) %in% c("rand", "rnd", "rndgrp")] <- "rand"
  names(x)[names(x) %in% c("smokhis", "smokstat", "tobastat", "tobayn")] <- 
    "smokhis"
  x <- x[, ! names(x) %in% "trt1tc"]
    x
})

CreateTableNameLabels(demo) 

demo <- map(demo, function(current_df) {
  x <- current_df
  x$race <- as.character(x$race)
  x$sex <- as.character(x$sex)
  x}
)

CreateTableNameLabels(demo)
demo <- BindRowsWLabels(demo) 


## Randomisation table ----
rand <- all_trials$rand
CreateTableNameLabels(rand)
rand <- BindRowsWLabels(rand)
unique(rand$trial)

rand_demo <- demo %>% 
  select(trial, pt, rand, rnddy, trtc) %>% 
  group_by(trial) %>% 
  mutate(all_rand_mis = all(is.na(rand)),
         rand = if_else(!is.na(trtc), trtc, rand)) %>% 
  ungroup() 

msng <- rand_demo %>% 
  filter(all_rand_mis)

rand_demo <- rand_demo %>% 
  filter(!all_rand_mis) %>% 
  select(-all_rand_mis) %>% 
  rename(rnd = rand)

rand <- bind_rows(rand, rand_demo)
unique(rand$trial) 

# Select randomised only
table(rand$rnd)
rand <- rand %>% 
  filter(!rnd %in% c("") )

demo <- demo %>%
  semi_join(rand)

## Concomittant medicines table ----
## Note that Roche has not provided WHO ATC codes
# From documentation
# trtirel - 
##  previous = started and stopped prior to trial treatment
## previous_concommitant = started prior to trial treatment adn contiued
## concomittant = started after trial treatment
# basmed
# "Yes" = started prior to trial treatment

conmed <- all_trials$conmed
conmed1 <- conmed[c("RCHNA25220", "RCHWA19924")] 

# Removed:  @MISSING trial "RCHMRA012JP" and "RCHWA19926", "RCHNA22823" , "RCHWA17823"

conmed1 <- map(conmed1, function(x) {
  names(x)[names(x) %in% c("trgoal", "trreas")] <- "trreas"
  x
}) 

conmed1 <- BindRowsWLabels(conmed1)
conmed1 <- conmed1 %>% 
  filter(trtirel %in% c("PREVIOUS_CONCOMITANT", "PREVIOUS")) %>% 
  select(trial, pt, trpt, trrout, trsct, trreas)
  
conmed2 <- BindRowsWLabels(conmed[c("RCHQ4881G", "RCHQ4882G", "RCHQ4883G")])
conmed2 <- conmed2 %>% 
  filter(basmed == "Yes") %>% 
  mutate(trreas = paste0(mdind, "|", othsp)) %>% 
  select(-basmed, -mdtyp, -mdind, -othsp) 

ExtractLabel(conmed1)
ExtractLabel(conmed2)

names_lkp <- c(
  "trial" = "trial",
  "patnum" = "pt",
  "mdg" = "trpt",
  "mdc" = "trsct",
  "mdrte", "trrout",
  "mdind" = "trreas"
)
names(conmed2) <- names_lkp[names(conmed2)]

conmed <- bind_rows(conmed1, conmed2)
conmed <- MakeLabels(conmed, ExtractLabel(conmed1, return_object = TRUE))

# Select only randomized patients
conmed <- conmed %>% 
  semi_join(rand)



## Medical history table ----
medhist <- all_trials$medhist
medhist1 <- medhist[c("RCHNA25220", "RCHWA19924")] 

CreateTableNameLabels(medhist1)
medhist1 <- BindRowsWLabels(medhist1)

meddra_lkp <- medhist1 %>% 
  distinct(dgpt, dgllt, dglltcd, dgsct)

# note checked RCHNA22823, RCHNA25220, RCHWA19924 and RCHWA19926 - all are exclusively screening visits
medhist1 <- medhist1 %>% 
  filter(trial %in% c("RCHNA25220", "RCHWA19924")) %>% # removed: dgtirel %in% c("PREVIOUS_CONCOMITANT", "PREVIOUS") |
  select(trial, pt, dgpt) %>% 
  mutate(meddra = "meddra")

medhist2 <- BindRowsWLabels(medhist[c("RCHQ4881G", "RCHQ4882G", "RCHQ4883G")])
medhist2 <- medhist2 %>% 
  filter(visitdy <0, hxrec == "Yes")  %>% 
  select(trial, pt = patnum, dgpt = hxraw) %>% 
  mutate(meddra = "non-meddra")

medhist <- bind_rows(medhist1, medhist2)
medhist <- MakeLabels(medhist, ExtractLabel(medhist1, return_object = TRUE))

# Select only randomised pateints
medhist <- medhist %>% # @SCREEN
  semi_join(rand)

## Vitals table ----
vitals <- all_trials$vitals
CreateTableNameLabels(vitals[c( "RCHNA25220", "RCHWA19924")]) 
CreateTableNameLabels(vitals[c("RCHQ4881G", "RCHQ4882G", "RCHQ4883G")])

# long format vitals
vitals1 <- BindRowsWLabels(vitals[c("RCHNA25220", "RCHWA19924")]) 
vitals1 <- vitals1 %>% 
  filter(str_to_upper(efcpe) %in% c("PRE 1ST INF", "BASELINE", "SCREENING", "SCREEN")) %>% 
  select(-efcpe, -efdy) %>% 
  filter(efparm %in% c("DBP", "SBP", "HGT", "WGT")) %>% 
  mutate(efvaln = if_else(is.na(efvaln), as.double(efval), 
                          efvaln)) %>% 
  select(trial, pt, efparm, efvaln) %>% 
  group_by(efparm) %>%
  mutate(grp_id = row_number()) %>%
  spread(efparm, efvaln) %>% 
  ungroup() %>% 
  select(-grp_id)

# Wide format vitals
# from documentaiton
# RCHQ4881G screening and first visit are Day -14 and Day 1
# RCHQ4882G as above
#  RCHQ4882G as above
vitals2 <- BindRowsWLabels(vitals[c("RCHQ4881G", "RCHQ4882G", "RCHQ4883G")])
vitals2 <- vitals2 %>% 
  filter(visit %in% c("Day -14", "Day 1")) %>% 
  select(DBP = bpd, SBP = bps, trial, pt = patnum, WGT = wt, HGT = ht) %>% 
  distinct()

vitals <- bind_rows(vitals1, vitals2)
vitals <- MakeLabels(vitals, ExtractLabel(vitals1, return_object = TRUE))



## Labs table ----
# 9 of 13 have a standard format except subject ID name
labs <- all_trials$labs
CreateTableNameLabels(all_trials$labs[c("RCHNA25220", "RCHWA19924")]) 
CreateTableNameLabels(all_trials$labs[c("RCHQ4881G", "RCHQ4882G", "RCHQ4883G")])


# labs1
labs1 <- BindRowsWLabels(all_trials$labs[c( "RCHNA25220", "RCHWA19924")])
labs1 <- labs1 %>% 
  select(trial, pt, lbparm, lbval, lblow, lbhigh, lbunit, lbfastst, lbcpe)

labs1 <- labs1 %>%
  filter(str_to_upper(lbcpe) %in% c("PRE 1ST INF", "BASELINE", "SCREENING", "SCREEN")) %>% 
  select(-lbcpe)
  
# labs2
labs2 <- BindRowsWLabels(all_trials$labs[c("RCHQ4881G", "RCHQ4882G", "RCHQ4883G")])
labs2 <- labs2 %>% 
  filter(visit %in% c("Day -14", "Day 1")) %>% 
  select(trial, pt = patnum, lbparm = testnm, 
         lbunit = labustd, testdes,
         lbval = resstdn,  lblow = silorng, lbhigh = sihirng) %>% 
  mutate(lbval = as.character(lbval))

labs <- bind_rows(labs1, labs2)
labs <- MakeLabels(labs, c("testdes" = "testdes",
                            ExtractLabel(labs1, return_object = TRUE)))


# Extract Chosen measures
# labs_rv <- labs %>%
#   distinct(lbparm, testdes, lbunit) %>%
#   group_by(lbparm) %>%
#   summarise_all(function(x) paste(x %>%  na.omit(), collapse = " | "))
# write_csv(labs_rv, "Scratch_data/review_rch_labs.csv")
labs_slctd <- read_csv("Created_metadata/reViewED_rch_labs.csv") %>%
  filter(keep ==1) %>%
  select(lbparm, new_lab)
write_csv(labs_slctd, "clipboard")

labs_slctd <- read_csv(
"lbparm,new_lab
ALT,ALT
SGPT,ALT
AST,AST
SGOT,AST
CREAT,CREAT
CPK,CREAT
GLU,GLU
GLUC,GLU
HGB,HGB
PLAT,PLT
PLATE,PLT
PLT,PLT
SCRT,CREAT
")
labs <- labs %>% 
  inner_join(labs_slctd)
tapply(labs$trial, list(labs$trial, labs$new_lab), length)
# creatinine issing for some of the trials, although protocol says is measured
# Look into later

labs <- labs %>% 
  semi_join(rand)



## Smoke ----
smoke <- all_trials$smoke
CreateTableNameLabels(smoke)
ExtractLabel(all_trials$smoke[[1]])
table(all_trials$smoke$RCHQ4881G$tobhx)

smoke <- BindRowsWLabels(smoke)
smoke <- smoke %>% 
  rename(pt = patnum) %>% 
  filter(visitdy <0) %>% 
  select(-visitdy)

# limit to randomised patients
smoke <- smoke %>%
  semi_join(rand)


## Re-organise tables so same as BI ----
bmi_demo <- demo %>% 
  select(trial, pt, ht = hgtcm, wt = wgtkg)
demo <- demo %>% 
  select(-hgtcm, -wgtkg)

bmi <- vitals %>% 
  select(trial, pt, ht = HGT, wt = WGT) 

bmi <- bind_rows(bmi, bmi_demo)
bmi <- bmi %>% 
  arrange(trial, pt, ht, wt) %>% 
  filter(!is.na(ht) | !is.na(wt))


bmi_agg <- bmi %>% 
  group_by(trial, pt) %>% 
  summarise_all(function(x) mean(x, na.rm = TRUE)) %>% 
  mutate(bmi = wt/(ht/100)^2)

## bp
bp <- vitals %>%
  select(trial, pt, DBP, SBP) %>% 
  filter(!(is.na(DBP) & is.na(SBP))) %>% 
  distinct()

## Convert data in a single object ----
rch <- list(conmed = conmed, demo = demo, labs = labs, medhist = medhist,
                bp = bp, bmi = bmi, rand = rand)
## Rename to make matching other companies data easier ----
# rch$labs$new_lab <- NULL
rch$demo <- rch$demo %>% 
  select(trial, pt, age, race, sex, smokhis)

rch <- map(rch, ~ .x %>%  rename(id = pt))

rch$bmi <- rch$bmi %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)
rch$bp <- rch$bp %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)


## Save data ----
saveRDS(rch, file = "Processed_data/rch.Rds")

# 12/10/22
# After finding duplicates in final df, came back to check trials
# Found 3 trials have no subject ID so removed them and associated code
