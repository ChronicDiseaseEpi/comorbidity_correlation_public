# 04_extract_novartis

# Load functions and packages 
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")


#list.files("V:/", patt = "2306")


# Read files and select variables ----
# read from eahc data folder
foldername <- "NVT_SA_AIN457A2302" # @MISSING ----

# Folder to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")


#foldername <- paste0("Data/", foldername, "/")


foldername <- FindFx("NVT")
folderout <- "D:/NVT_SA_AIN457A2302/"

## Dataframe of zipped file contents
a <- unzip(foldername, list = TRUE) %>% as_tibble()
a %>% filter(str_detect(Name, "bconcd|rand|ct|labdata|basco|gentr|
                        bcond|pat5|adm3"))

demo  <- read_sas(unz(foldername, "dm.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age_cat, sex, race, arm, visit, visitnum) %>% 
  mutate(rand_visit = "Day 28")

adcm <-  read_sas(paste0(foldername, "adcm.sas7bdat")) %>% names_to_lower() 

## poor coding in this trial (lack of route, often codes mutually exclusive routes with same preferred number
## means exclude unless has term  (or has route)
conmed <- adcm %>% 
  filter(cmcat != "PSORIASIS THERAPY", !is.na(trtp), (cmwhsnn != "" | cmroute != "")) %>% 
  select(usubjid, cmver, anl01fl, cmatc4, cmatccd4, cmroute, cmstdy, cmwhsnn) %>% 
  distinct()

medhist <- read_sas(paste0(foldername, "mh.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhdecod, mhbodsys, mhoccur, mhstdtc, epoch, mhdy, mhpresp) %>% 
  filter(epoch %in% "SCREENING") %>% 
  # Exclude where states doesn't have specific diagnoses
  filter(!(mhpresp == "Y" & mhoccur %in% c("N", "U") )) %>% 
  select(-mhpresp, -mhoccur)

labs <- read_sas(paste0(foldername, "lb.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, lbspec, lbstresn, lbstresc, lbstresu, lbtest, lbtestcd, lbstnrhi, lbstnrlo, lbfast, lbdy, epoch) %>% 
  filter(epoch == "SCREENING")

vitals <- read_sas(paste0(foldername, "vs.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(usubjid, visit, vstestcd, vsstresn, epoch) %>% 
  filter(vstestcd %in% c("DIABP",  "SYSBP", "HEIGHT", "WEIGHT")
  )

smoking <- read_sas(paste0(foldername, "su.sas7bdat")) %>% names_to_lower() %>% 
  select(usubjid, suoccur, suendy)

save(demo, medhist, conmed, labs, vitals, smoking, file = paste0(foldername, "extract.Rdata"))


foldername <- "NVT_SA_AIN457A2303" # @MISSING ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
foldername <- FindFx("457A2303")





foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "dm.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(usubjid, age_cat, sex, race, arm, visit, visitnum) %>% 
  mutate(rand_visit = "Day 28")

adcm <-  read_sas(paste0(foldername, "adcm.sas7bdat")) %>% names_to_lower()
## poor coding in this trial (lack of route, often codes mutually exclusive routes with same preferred number
## means exclude unless has term  (or has route)
conmed <- adcm %>% 
  filter(cmcat != "PSORIASIS THERAPY", !is.na(trtp), (cmwhsnn != "" | cmroute != "")) %>% 
  select(usubjid, cmver, anl01fl, cmatc4, cmatccd4, cmroute, cmstdy, cmwhsnn) %>% 
  distinct()

medhist <- read_sas(paste0(foldername, "mh.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, mhdecod, mhbodsys, mhoccur, mhstdtc, epoch, mhdy, mhpresp) %>% 
  filter(epoch %in% "SCREENING") %>% 
  # Exclude where states doesn't have specific diagnoses
  filter(!(mhpresp == "Y" & mhoccur %in% c("N", "U") )) %>% 
  select(-mhpresp, -mhoccur)

labs <- read_sas(paste0(foldername, "lb.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(usubjid, visit, lbspec, lbstresn, lbstresc, lbstresu, lbtest, lbtestcd, lbstnrhi, lbstnrlo, lbfast, lbdy, epoch) %>% 
  filter(epoch == "SCREENING")

vitals <- read_sas(paste0(foldername, "vs.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(usubjid, visit, vstestcd, vsstresn, epoch) %>% 
  filter(vstestcd %in% c("DIABP",  "SYSBP", "HEIGHT", "WEIGHT")
  )

smoking <- read_sas(paste0(foldername, "su.sas7bdat")) %>% names_to_lower() %>% 
  select(usubjid, suoccur, suendy)

save(demo, medhist, conmed, labs, vitals, smoking, file = paste0(foldername, "extract.Rdata"))

foldername <- "NVT_SA_ENA713D1301" # @MISSING ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
foldername <- FindFx("1301")


foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "a_dmg.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(stysid1a, age_1n_cat, sex1c, rce1c, hgt_1n, wgt_1n, vis1d, vis777)

rand <- read_sas(paste0(foldername, "a_trt.sas7bdat")) %>% names_to_lower() %>% 
  select(stysid1a, tgpdsc1a, trtstt1d)

atc <-  read_sas(paste0(foldername, "a_cmdatc.sas7bdat")) %>% names_to_lower()
conmed <- atc %>% 
  select(stysid1a, cmdstt1o, atccode, atc_txt, pt_txt, cmdrte1c, cm_pre ) %>% 
  arrange(stysid1a, atccode)

medhist <- read_sas(paste0(foldername, "a_cnd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(stysid1a, llit_txt, pt_txt, hlt_txt,  soc_txt, vis1d, dgnsrg1o, day_1n)

labs <- read_sas(paste0(foldername, "a_lrs.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(stysid1a, visnam1a,
         parnam1c, fst1c, labcat1c, cvunt_1c, cvrsl_1n, cvlln_1c, cvuln_1c) 

vsn <- read_sas(paste0(foldername, "a_vsn.sas7bdat")) %>% names_to_lower()
vitals <- vsn %>% 
  select(stysid1a, visnam1a, hgt_1n, wgt_1n, bmi_1n, stnsbp1n, stndbp1n)

save(demo, rand, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))

foldername <- "NVT_SA_ZOL446H2310" # @MISSING ----
foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "a_dmg.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(stysid1a, age_1n_cat, sex1c, rce1c, hgt_1n, wgt_1n, vis1d)

rand <- read_sas(paste0(foldername, "a_trt.sas7bdat")) %>% names_to_lower() %>% 
  select(stysid1a, tgpdsc1a, trtstt1d)

atc <-  read_sas(paste0(foldername, "a_cmdatc.sas7bdat")) %>% names_to_lower()
conmed <- atc %>% 
  select(stysid1a, atccode, atc_txt, pt_txt, cmdstt1f, visnam1a, cmdtyp1c) %>% 
  arrange(stysid1a, atccode)
# cmdtyp1c - 1 if prior medication

medhist <- read_sas(paste0(foldername, "a_cnd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(stysid1a, llit_txt, pt_txt, hlt_txt,  soc_txt, vis1d, dgnsrg1o, day_1n, mlltcode, visnam1a)

labs <- read_sas(paste0(foldername, "a_lrs.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(stysid1a, visnam1a,
         parnam1c, fst1c, labcat1c, cvunt_1c, cvrsl_1n, cvlln_1c, cvuln_1c) 

vsn <- read_sas(paste0(foldername, "a_vsn.sas7bdat")) %>% names_to_lower()
vitals <- vsn %>% 
  select(stysid1a, visnam1a, hgt_1n, wgt_1n, bmi_1n, stnsbp1n, stndbp1n)

save(demo, rand, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))

foldername <- "NVT_SA_ZOL446H2409" # @MISSING ----
foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "a_dmg.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(stysid1a, age_1n_cat, sex1c, rce5c, hgt_1n, wgt_1n, vis1d)

rand <- read_sas(paste0(foldername, "a_trt.sas7bdat")) %>% names_to_lower() %>% 
  select(stysid1a, tgpdsc1a, trtstt1d)

atc <-  read_sas(paste0(foldername, "a_cmdatc.sas7bdat")) %>% names_to_lower()
conmed <- atc %>% 
  select(stysid1a, atccode, atc_txt, pt_txt, cmdstt1o, cmdstt1d, cmdstt1f, visnam1a,
         cmdtyp1c) %>% 
  arrange(stysid1a, atccode)

medhist <- read_sas(paste0(foldername, "a_cnd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(stysid1a, llit_txt, pt_txt, hlt_txt,  soc_txt, vis1d, dgnsrg1o, day_1n, mlltcode, visnam1a)

labs <- read_sas(paste0(foldername, "a_lrs.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(stysid1a, visnam1a,
         parnam1c, fst1c, labcat1c, cvunt_1c, cvrsl_1n, cvlln_1c, cvuln_1c) 

vsn <- read_sas(paste0(foldername, "a_vsn.sas7bdat")) %>% names_to_lower()
vitals <- vsn %>% 
  select(stysid1a, visnam1a, hgt_1n, wgt_1n, bmi_1n, stnsbp1n, stndbp1n)

smok <- read_sas(paste0(foldername, "a_base.sas7bdat")) %>% names_to_lower()
smoking <- smok %>%
  select(stysid1a, cursmk1c)

save(demo, rand, medhist, conmed, labs, vitals, smoking, file = paste0(foldername, "extract.Rdata"))

foldername <- "NVT_SA_ZOL446M2309" # @MISSING ----
foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "admg.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(stysid1a, agedrv1n_cat, sex1c, rce5c, bght1n, basewgt, vis1d)

rand <- read_sas(paste0(foldername, "atrt.sas7bdat")) %>% names_to_lower() %>% 
  select(stysid1a, tgpdsc1a, trtstt1d)

atc <-  read_sas(paste0(foldername, "acmdatc.sas7bdat")) %>% names_to_lower()
conmed <- atc %>% 
  select(stysid1a, atccode, atc_txt, pt_txt, cmdstt1o, imcmdstd, cmdtyp1c) %>% 
  arrange(stysid1a, atccode)

medhist <- read_sas(paste0(foldername, "acnd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(stysid1a, llit_txt, pt_txt, hlt_txt,  soc_txt, dgnsrg1o, mlltcode)

labs <- read_sas(paste0(foldername, "alrs.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(stysid1a, visnam1a,
         parnam1c, fst1c, labcat1c, cvunt_1c, cvrsl_1n, cvlln_1c, cvuln_1c) 

vsn <- read_sas(paste0(foldername, "avsn.sas7bdat")) %>% names_to_lower()
vitals <- vsn %>% 
  select(stysid1a, visnam1a, stnsbp1n, stndbp1n)

smok <- read_sas(paste0(foldername, "abase.sas7bdat")) %>% names_to_lower()
smoking <- smok %>%
  select(stysid1a, cursmk1c)

save(demo, rand, medhist, conmed, labs, vitals, smoking, file = paste0(foldername, "extract.Rdata"))

foldername <- "NVT_SA_ZOL446O2306" # @MISSING ----
foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "a_dmg.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(stysid1a, age_1n_cat, rce4c, sex1c, hgt_1n, wgt_1n, bmi_1n, vis_1n)

rand <- read_sas(paste0(foldername, "a_trt.sas7bdat")) %>% names_to_lower() %>% 
  select(stysid1a, tgpdsc1a, trtstt1d)

atc <-  read_sas(paste0(foldername, "a_cmdatc.sas7bdat")) %>% names_to_lower()
conmed <- atc %>% 
  select(stysid1a, atccode, atc_txt, pt_txt, cmdstt1o, cmdstt1d, cmdstt1f, visnam1a,
         cmdtyp1c) %>% 
  arrange(stysid1a, atccode)

medhist <- read_sas(paste0(foldername, "a_cnd.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(stysid1a, llit_txt, pt_txt, hlt_txt,  soc_txt, vis1d, dgnsrg1o, day_1n, mlltcode, visnam1a)

labs <- read_sas(paste0(foldername, "a_lrs.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(stysid1a, visnam1a, vis_1n,
         parnam1c, fst1c, labcat1c, cvunt_1c, cvrsl_1n, cvlln_1c, cvuln_1c) 

vsn <- read_sas(paste0(foldername, "a_vsn.sas7bdat")) %>% names_to_lower()
vitals <- vsn %>% 
  select(stysid1a, visnam1a, hgt_1n, wgt_1n, bmi_1n, stnsbp1n, stndbp1n)

smok <- read_sas(paste0(foldername, "a_base.sas7bdat")) %>% names_to_lower()
smoking <- smok %>%
  select(stysid1a, cursmk1c)

save(demo, rand, medhist, conmed, labs, vitals, smoking, file = paste0(foldername, "extract.Rdata"))


foldername <- "NVT_SA_ZOL446H2301" # @MISSING ----
foldername <- paste0("Data/", foldername, "/")

demo  <- read_sas(paste0(foldername, "a_dmg_mse.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(stysid1a, agedrv1n, rce2c, sex1c, hgt_1n, wgt_1n, bmi_1n, vis_1n)

rand <- read_sas(paste0(foldername, "a_trt_mse.sas7bdat")) %>% names_to_lower() %>% 
  select(stysid1a, tgpdsc1a, trtstt1d)

atc <-  read_sas(paste0(foldername, "a_cmdatc_mse.sas7bdat")) %>% names_to_lower()
conmed <- atc %>% 
  select(stysid1a, atccode, atc_txt, pt_txt, cmdstt1o, cmdstt1d, cmdstt1f, visnam1a, vis1n,
         cmdtyp1c) 

medhist <- read_sas(paste0(foldername, "a_cnd_mse.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(stysid1a, llit_txt, pt_txt, hlt_txt,  soc_txt, vis1d, dgnsrg1o, day_1n, mlltcode, visnam1a)

labs <- read_sas(paste0(foldername, "a_lrs_mse.sas7bdat")) %>% names_to_lower()
labs <- labs %>% 
  select(stysid1a, visnam1a, vis_1n,
         parnam1c, fst1c, labcat1c, cvunt_1c, cvrsl_1n, cvlln_1c, cvuln_1c) 

vsn <- read_sas(paste0(foldername, "a_vsn_mse.sas7bdat")) %>% names_to_lower()
vitals <- vsn %>% 
  select(stysid1a, visnam1a, hgt_1n, wgt_1n, bmi_1n, stnsbp1n, stndbp1n)

smok <- read_sas(paste0(foldername, "a_base_mse.sas7bdat")) %>% names_to_lower()
smoking <- smok %>%
  select(stysid1a, cursmk1c)

save(demo, rand, medhist, conmed, labs, vitals, smoking, file = paste0(foldername, "extract.Rdata"))





# 

# Consolidate all files ----
# consolidate
## Combine all files into a single dataframe ----
#List folder names
foldernames <- list("NVT_SA_AIN457A2302", "NVT_SA_AIN457A2303", "NVT_SA_ENA713D1301", 
                      "NVT_SA_ZOL446H2301", "NVT_SA_ZOL446H2310", "NVT_SA_ZOL446H2409", 
                      "NVT_SA_ZOL446M2309", "NVT_SA_ZOL446O2306")
# Check all have saveddata
a <- map(foldernames, function(foldername) list.files(paste0("Data/", foldername), patt = "extract"))
names(a) <- foldernames
a
# Read all data into a big list
all_trials <- map(foldernames, function(foldername){
  assign(foldername, new.env())
  load(paste0("Data/", foldername, "/extract.Rdata"), envir = get(foldername))
  as.list(get(foldername))
})
names(all_trials) <- foldernames

# Check all table names the same, they are not, some have a smoke table and some a rand table
smoke <- map(all_trials, ~ .x$smoking)
smoke <- smoke[!map_lgl(smoke, is.null)]
rand <- map(all_trials, ~ .x$rand)
rand <- rand[!map_lgl(rand, is.null)]

all_trials <- map(all_trials, ~ .x[!names(.x) %in% c("smoking", "rand")])

tbl_names <- map(all_trials, names) 
do.call(rbind, tbl_names)

# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)
all_trials$smoke <- smoke
all_trials$rand <- rand
rm(smoke, rand)

## Demographics table ----
demo <- all_trials$demo
a <- CreateTableNameLabels(demo)
# Rename age variable
demo <- map(demo, function(x) {
  names(x)[names(x) %in% c("race", "rce1c", "rce2c", "rce4c", "rce5c")] <- "race"
  names(x)[names(x) %in% c("sex", "sex1c")] <- "sex"
  names(x)[names(x) %in% c("basewgt", "wgt_1n")] <- "wgt_1n"
  names(x)[names(x) %in% c("bght1n", "hgt1n")] <- "bght1n"
  names(x)[names(x) %in% c("stysid1a", "usubjid")] <- "usubjid"
  names(x)[names(x) %in% c("age_1n_cat", "age_cat", "agedrv1n", "agedrv1n_cat")] <- "age"
  x <- x[!names(x) %in% c("rand_visit", "vis_1n", "vis777", "visit", "visitnum")] 
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
demo$arm <- NULL

## Randomisation table ----
rand <- all_trials$rand
CreateTableNameLabels(rand)
rand <- BindRowsWLabels(rand)

rand_demo <- all_trials$demo[c("NVT_SA_AIN457A2302", "NVT_SA_AIN457A2303")]
rand_demo <- BindRowsWLabels(rand_demo)
rand_demo <- rand_demo %>% 
  select(trial, usubjid, arm)

names_lkp <- c(
  "trial" = "trial",
  "stysid1a" = "usubjid",
  "tgpdsc1a" = "arm",
  "trtstt1d" = "trtstt1d"
)
names(rand) <- names_lkp[names(rand)]
rand <- bind_rows(rand, rand_demo)

# Select randomised only
rand <- rand %>% 
  filter(!arm %in% c("", "Screen Failure") )

demo <- demo %>%
  semi_join(rand)

## Concomittant medicines table ----
conmed <- all_trials$conmed
# just value "1" for all of these in this trial
conmed$NVT_SA_ZOL446H2301$cmdstt1o <- NULL

CreateTableNameLabels(conmed[1:2])
CreateTableNameLabels(conmed[3:8])
CreateTableNameLabels(conmed)
ExtractLabel(conmed$NVT_SA_AIN457A2302)
ExtractLabel(conmed$NVT_SA_ENA713D1301)

conmed <- map(conmed, function(x) {
  names(x)[names(x) %in% c("stysid1a","usubjid")] <- "usubjid"
  names(x)[names(x) %in% c("cmatc4","atc_txt")] <- "atc_txt"
  names(x)[names(x) %in% c("cmatccd4","atccode")] <- "atccode"
  names(x)[names(x) %in% c("cmdrte1c","cmroute")] <- "cmroute"
  names(x)[names(x) %in% c("cmwhsnn","pt_txt")] <- "pt_txt"
  names(x)[names(x) %in% c("anl01fl", "cm_pre", "cmdtyp1c")] <- "cmdtyp1c"
  
  x <- x[, !names(x) %in% c("cmdstt1f", "imcmdstd", "cmdstt1d", "cmdstt1o")]
  if("cmroute"  %in% names(x)) x$cmroute <- as.character(x$cmroute)
  x$cmdtyp1c <- as.character(x$cmdtyp1c)
  
  x
})
CreateTableNameLabels(conmed)
conmed <- BindRowsWLabels(conmed)

# Identify concomittant medications begun before randomisation
tapply(conmed$cmdtyp1c, conmed$trial, unique)
tapply(conmed$cmdtyp1c, conmed$trial, table)
# review labels for indicator variables
# NVT_SA_AIN457A2302 NVT_SA_AIN457A2303 NVT_SA_ENA713D1301 NVT_SA_ZOL446H2301 
# "anl01fl"          "anl01fl"           "cm_pre"         "cmdtyp1c" 
# NVT_SA_ZOL446H2310 NVT_SA_ZOL446H2409 NVT_SA_ZOL446M2309 NVT_SA_ZOL446O2306 
# "cmdtyp1c"         "cmdtyp1c"         "cmdtyp1c"         "cmdtyp1c" 

# from - data dictionaries
## NVT_SA_AIN457A2302 
# If CMSDT and CMEDT are both nonmissing
# and before TR01SDT then
# ANL01FL =’Y’
## NVT_SA_AIN457A2303
# If TPPSTHY is missing and If
# CMSDT and CMEDT are both nonmissing
# and CMEDT<TR01SDT then
# ANL01FL =’Y’. IF TPPSTHY is
# nonmissing ,set ANL01FL eq ‘Y’
## NVT_SA_ENA713D1301
# 0=No
# 1=Yes
## NVT_SA_ZOL446H2301
# 1 = Prior medication, 2 = Concom med.
## NVT_SA_ZOL446H2310
# 1 = Prior medication
# 2 = Concom med.
## NVT_SA_ZOL446H2409
# 1 = Prior medication
# 2 = Concom med.
# 3 = Prior/concom med.
## NVT_SA_ZOL446M2309
# 1 = Prior medication
# 2 = Concom med.
# 3 = Prior/concom med.
## NVT_SA_ZOL446O2306
# 1 = Prior medication
# 2 = Concom med.
# 3 = Prior/concom med.

conmed <- conmed %>%
  filter(cmdtyp1c %in% c("Y", "1", "3"))
# NVT_SA_ZOL446H2301 looked strange as only 11% pre-existing, but is correct
# Is like this becuase has large number of "SUmmary" records

# Selct only randomised pateints
conmed <- conmed %>% 
  semi_join(rand)

## Medical history table ----
# Checked documentation, all are MEDDRA, including first two where not described as meddra
medhist <- all_trials$medhist
CreateTableNameLabels(medhist)
CreateTableNameLabels(all_trials$medhist[1:2])
CreateTableNameLabels(all_trials$medhist[3:8])

ExtractLabel(all_trials$medhist[[1]])
ExtractLabel(all_trials$medhist[[6]])

CreateTableNameLabels(medhist)
medhist <- map(medhist, function(x) {
  names(x)[names(x) %in% c("stysid1a", "usubjid")] <- "usubjid"
  names(x)[names(x) %in% c("llit_txt", "mhdecod")] <- "llit_txt"
  names(x)[names(x) %in% c("mhbodsys", "soc_txt")] <- "soc_txt"
  names(x)[names(x) %in% c("mhdy", "day_1n")] <- "day_1n"
  names(x)[names(x) %in% c("visnam1a", "epoch")] <- "visnam1a"
  
  x <- x[ , names(x) %in% c("usubjid", "llit_txt", "soc_txt", "day_1n", "visnam1a", "dgnsrg1o")]
  x
})
CreateTableNameLabels(medhist)
medhist <- BindRowsWLabels(medhist)

# Create lookup category tables for meddra and non meddra
meddra_lkp <- all_trials$medhist[3:8]
CreateTableNameLabels(meddra_lkp)
meddra_lkp <- map(meddra_lkp, function(x) {
  x <- x[ , names(x) %in% c("hlt_txt", "llit_txt", "mlltcode", "pt_txt", "soc_txt")]
  if("mlltcode" %in% names(x)) x$mlltcode <- as.character(x$mlltcode)
  x
})
meddra_lkp <- BindRowsWLabels(meddra_lkp) %>% 
  distinct()

# Assuming as in long format, and never encountered any Y/N status
# variable that presentce indicates has disease
# Double checked, appears to be the case

# Select only randomised patients
medhist <- medhist %>% 
  semi_join(rand)

# Select only conditions diagnosed on or before randomisation
# all are screening or visit 1 except two trials where the visit name
# is not included c("NVT_SA_ENA713D1301", "NVT_SA_ZOL446M2309")
# 95-98% of rows for other 6 trials have a negative days to diagnosis variable
# reasonable to assume all are existing (ie not adverse events)
medhist <- medhist %>% 
  select(trial, usubjid, llit_txt) %>% 
  distinct()

## Labs table ----
# 9 of 13 have a standard format except subject ID name
labs <- all_trials$labs
CreateTableNameLabels(all_trials$labs[1:2])
CreateTableNameLabels(all_trials$labs[3:8])
ExtractLabel(all_trials$labs[[1]])
ExtractLabel(all_trials$labs[[8]])

# One trial with missing visit name information (blank character vector)
all_trials$labs$NVT_SA_ZOL446O2306$visnam1a <- 
  as.character(all_trials$labs$NVT_SA_ZOL446O2306$vis_1n)
names_lkp <- c(
  "stysid1a" = "usubjid",
  "visnam1a" = "visit",
  "parnam1c" = "lbtestcd",
  "fst1c"    = "lbfast",
  "labcat1c" = "lbspec",
  "cvunt_1c" = "lbstresu",
  "cvrsl_1n" = "lbstresn",
  "cvlln_1c" = "lbstnrlo",
  "cvuln_1c" = "lbstnrhi",
  "trial" = "trial"
)

labs1 <- BindRowsWLabels(all_trials$labs[1:2])
labs2 <- BindRowsWLabels(all_trials$labs[3:8])
labs1 <- labs1[ , c(names_lkp, "lbtest")]
labs2 <- labs2[ , c(names(names_lkp))]
names(labs2) <- names_lkp[names(labs2)]
labs2$lbfast <- as.character(labs2$lbfast)
labs2$lbtest <- ""

labs <- bind_rows(labs1, labs2)
labs <- MakeLabels(labs, ExtractLabel(labs1, return_object = T))

# Extract Chosen measures
# labs_rv <- labs %>% 
#   distinct(lbtest, lbtestcd, lbspec) %>% 
#   group_by(lbtestcd) %>% 
#   summarise_all(function(x) paste(x, collapse = " | "))
# write_csv(labs_rv, "Scratch_data/review_nvt_labs.csv")
# labs_slctd <- read_csv("Created_metadata/reViewED_nvt_labs.csv") %>%
#   filter(keep ==1) %>%
#   select(lbtestcd, lbspec, newlab)
# write_csv(labs_slctd, "clipboard")
labs_slctd <- read_csv(
"lbtestcd,lbspec,newlab
ALT,SERUM,ALT
AST,SERUM,AST
CRCL,BIOCHEM,CREA
CREA,BIOCHEM,CREA
CREAT,PLASMA/SERUM,CREA
GLUC,BIOCHEM,GLU
HGB,HEMA,HGB
HGB,BLOOD,HGB
PLAT,BLOOD,PLAT
PLAT2,HEMA,PLAT2
SGLUC,BIOCHEM,GLU
SGOT,BIOCHEM,AST
SGPT,BIOCHEM,ALT
")
labs <- labs %>% 
  inner_join(labs_slctd)

# Select only randomised and pre-randomised visits
tapply(labs$visit, labs$trial, function (x) sum(is.na(x) | x == ""))
tapply(labs$visit, labs$trial, function(x) x %>% unique() )

# Comments are taken from reviewing assessment schedule in CSR
labs_visits <- tribble(
  ~"trial", ~"code",
  "NVT_SA_AIN457A2302", "'SCREENING'",
  "NVT_SA_AIN457A2303", "'SCREENING'",
  # NVT_SA_ENA713D1301 V1-V8 V1 and V2 pre-treatment, V3-V8 treatment
  "NVT_SA_ENA713D1301", "c('WEEKS_-4 TO_-1', 'WEEK_0')",
  # NVT_SA_ZOL446H2301 V1-V2 screening, V3 randomisation, no lab tests on V3
  "NVT_SA_ZOL446H2301", "c('VISIT 1', 'VISIT 2')",
  # NVT_SA_ZOL446H2310 - V1 screening V2 randomisation V3 - V6 to Visit 6 to "Visit X" subsequent
  # LOST OF VISIT 1.1, 1.2 ETC, WILL NEED TO SEE IF THESE ARE AN ISSUE
  "NVT_SA_ZOL446H2310", "c('VISIT 1', 'VISIT 1.0', 'VISIT 1.0.1', 
                           'VISIT 1.1', 'VISIT 1.2', 'VISIT 1.3', 'VISIT 1.4',
                           'VISIT 2', 'VISIT 2.1', 'VISIT 2.2', 'VISIT 2.3')",
  "NVT_SA_ZOL446H2409", "c('Screening', 'Randomization')",
  "NVT_SA_ZOL446M2309", "c('Screening', 'Randomization')",
  # Visitname not feature of NVT_SA_ZOL446O2306 From schedule, V1 and V2 screening, V3-V7 subsequent
  "NVT_SA_ZOL446O2306", "as.character(1:2)"
)
labs_visits <- by(labs_visits, labs_visits$trial, function(x) eval(parse(text = x$code)))
labs_visits <- stack(labs_visits) %>% 
  setNames(c("visit", "trial"))
  
labs <- labs %>% 
  semi_join(labs_visits)


## Vitals table ----
vitals <- all_trials$vitals
CreateTableNameLabels(vitals[1:2])
CreateTableNameLabels(vitals[3:8])

# wide versus long format vitals
vitals1 <- BindRowsWLabels(vitals[1:2])
vitals2 <- BindRowsWLabels(vitals[3:8])
ExtractLabel(vitals1)
ExtractLabel(vitals2)

# long format vitals
vitals1$epoch %>% is.na() %>% table()
vitals1 <- vitals1 %>% 
  filter(epoch == "SCREENING") %>% 
  select(-epoch, -visit) %>% 
  spread(key = vstestcd, value = vsstresn)

# Wide format vitals
vitals2$visnam1a %>%  tapply(vitals2$trial, unique)
vitals_visits <- labs_visits %>%
  filter(trial %in% vitals2$trial) 
vitals_visits$visit[vitals_visits$trial == "NVT_SA_ZOL446O2306"] <- 
  c("Screening", "Randomization")
vitals_visits <- vitals_visits %>% 
  add_row(trial = "NVT_SA_ZOL446O2306", visit = "Baseline")
vitals2 <- vitals2 %>% 
  semi_join(vitals_visits) %>% 
  select(-visnam1a)

names(vitals1) <- str_to_lower(names(vitals1)) 

names_lkp <- c(
  "stysid1a" = "usubjid",
  "trial" = "trial",
  "bmi_1n" = "bmi_1n",
  "hgt_1n" = "height",
  "stndbp1n" = "diabp",
  "stnsbp1n"= "sysbp",
  "wgt_1n"= "weight")
names(vitals2) <- names_lkp[names(vitals2)]
setdiff(names(vitals2), names(names_lkp))

vitals <- bind_rows(vitals1, vitals2)

## Smoke ----
smoke <- all_trials$smoke
CreateTableNameLabels(smoke)
ExtractLabel(all_trials$smoke[[1]])
table(all_trials$smoke$NVT_SA_AIN457A2302$suoccur)

smoke1 <- BindRowsWLabels(all_trials$smoke[1:2])
smoke2 <- BindRowsWLabels(all_trials$smoke[3:6])
# All end dates are negative, varies greatly
mean(smoke1$suendy >0, na.rm = TRUE)

smoke1 <- smoke1 %>% 
  mutate(smoking = case_when(
    suoccur == "N" ~ "never",
    suoccur == "Y" & (is.na(suendy) | suendy >=30) ~ "current",
    suoccur == "Y" & (!is.na(suendy) & suendy <30) ~ "former")) %>% 
  select(trial, usubjid, smoking)

smoke2 <- smoke2 %>% 
  mutate(smoking = if_else(cursmk1c ==1, "current", "former or never")) %>% 
  select(trial, usubjid = stysid1a, smoking)

smoke <- bind_rows(smoke1, smoke2)
table(smoke$smoking)

# limit to randomised patients
smoke <- smoke %>%
  semi_join(rand)

## Re-organise tables so same as BI ----
bmi_demo <- demo %>% 
  select(trial, usubjid, ht = hgt_1n, wt = wgt_1n, bmi = bmi_1n)
demo <- demo %>% 
  select(-hgt_1n, -wgt_1n, -bmi_1n, -bght1n)

# No height and weight for one trial NVT_SA_ZOL446M2309, in vitals, 
# No weight for two trials NVT_SA_AIN457A2302, NVT_SA_AIN457A2303 in demo
# No height for two trials NVT_SA_AIN457A2302, NVT_SA_AIN457A2303 in demo
# no BMI for 8  in demo    NVT_SA_AIN457A2302, NVT_SA_AIN457A2303, 
# NVT_SA_ENA713D1301, NVT_SA_ZOL446H2310, NVT_SA_ZOL446H2409, NVT_SA_ZOL446M2309

bmi_demo <- bmi_demo %>%
  filter(!trial %in% c("NVT_SA_AIN457A2302", "NVT_SA_AIN457A2303"))

bmi <- vitals %>% 
  filter(trial != "NVT_SA_ZOL446M2309") %>% 
  select(trial, usubjid, ht = height, wt = weight) %>% 
  mutate(bmi = wt / (ht/100)^2)

bmi <- bind_rows(bmi, bmi_demo)

bp <- vitals %>% 
  select(trial, usubjid, diabp, sysbp)

## Create a single object ----
nvt <- list(conmed = conmed, demo = demo, labs = labs, medhist = medhist,
            bp = bp, bmi = bmi, rand = rand)

## rename and delete irrlevant variables so easier to match across companies ----
nvt$demo$vis1d <- NULL
nvt$labs$lbspec <- NULL
# nvt$labs$newlab <- NULL
nvt$labs$visit <- NULL
nvt$conmed$cmdtyp1c <- NULL
nvt$conmed$cmstdy <- NULL
nvt$conmed$vis1n <- NULL
nvt$conmed$visnam1a <- NULL

# Rename all id variables
nvt <- map(nvt, ~ .x %>%  rename(id = usubjid))

# gather vitals to narrow
nvt$bmi <- nvt$bmi %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)

nvt$bp <- nvt$bp %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)

## Save data  ----
saveRDS(nvt, file = "Processed_data/nvt.Rds")
