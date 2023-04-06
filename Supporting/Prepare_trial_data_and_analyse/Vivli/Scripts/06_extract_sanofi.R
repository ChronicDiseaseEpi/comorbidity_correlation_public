# 06_extract_sanofi

##01_extract_data
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")


list.files("V:", pattern = "6521")

foldername <- "SANEFC6521" # @MISSING ----


# Set folder location to save extracted trial data - Make sure to run new foldername  
folderout <- paste0("D:/", foldername, "/")

# Get file path for trial
foldername <- FindFx(foldername)

demo  <- read_sas(paste0(foldername, "dm.sas7bdat")) %>% names_to_lower()
demo <- demo %>% 
  select(rusubjid, agec, ageu, arm, race, rfstdy, sex)

# These drugs all appear to be for the index condition
conmed  <- read_sas(paste0(foldername, "cm.sas7bdat")) %>% names_to_lower()
conmed <- conmed %>% 
  select(rusubjid, visitnum, visit, cmcat, cmdecod, cmindc, cmoccur, cmroute,
         cmscat, cmstdy)

labs <- read_sas(paste0(foldername, "lb.sas7bdat")) %>% names_to_lower()
labs <-labs %>% 
  select(rusubjid, visit, visitnum, lbcat, lbstnrlo, lbstnrhi,
         lbstresc, lbstresn, lbstresu,
         lbtest, lbtestcd)

medhist <- read_sas(paste0(foldername, "mh.sas7bdat")) %>% names_to_lower()
medhist <- medhist %>% 
  select(rusubjid, visit, visitnum,
         mhbodsys, mhcat, mhdecod, mhllt, mhlltcd, mhoccur, mhpresp,
         mhstdy)

vitals <- read_sas(paste0(foldername, "vs.sas7bdat")) %>% names_to_lower()
vitals <- vitals %>% 
  select(rusubjid, visit, visitnum,
         vsdy, vsstresc, vsstresn, vsstresu,
         vstest, vstestcd)

save(demo, medhist, conmed, labs, vitals, file = paste0(foldername, "extract.Rdata"))


## Combine all files into a single dataframe ----
#List folder names
foldernames <- list("SANEFC6521")
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

# Only one trial, so add foldername as trial to each element of list
trial1 <- all_trials$SANEFC6521
trial1 <- map(trial1, function(x) {
  x$trial <- "SANEFC6521"
  x
})

# DEMO ----
demo <- trial1$demo

demo <- demo %>% 
  filter(arm != "Screen Failure")

# RAND ----
rand <- demo %>% 
  select(trial, rusubjid, arm, rfstdy)

demo <- demo %>% 
  select(-arm, -rfstdy)

# COnmed ----
conmed <- trial1$conmed

conmed <- conmed %>% 
  filter(visit == "SCREENING DAY -21 UP TO DAY 1")  %>% 
  select(trial, rusubjid, cmdecod, cmindc, cmscat)

conmed <- conmed %>% 
  semi_join(rand)
  
## medhist - all pre-specifed, just 12 terms ----
medhist <- trial1$medhist

medhist <- medhist %>% 
  filter(visit == "SCREENING DAY -21 UP TO DAY 1",
         mhoccur == "Y")  %>% 
  select(trial, rusubjid, mhdecod, mhllt, mhlltcd)
medhist <- medhist %>% 
  semi_join(rand)

## labs ----
labs <- trial1$labs
labs <- labs %>% 
  filter(visit == "SCREENING DAY -21 UP TO DAY 1") %>% 
  select(-visit, -visitnum)

# labs_rv <- labs %>%
#   distinct(lbtestcd, lbtest, lbstresu) %>%
#   group_by(lbtestcd) %>%
#   summarise_all(function(x) paste(x %>%  na.omit(), collapse = " | "))
# write_csv(labs_rv, "Scratch_data/review_san_labs.csv")

# labs_slctd <- read_csv("Created_metadata/reViewED_san_labs.csv") %>%
#   filter(keep ==1) %>%
#   select(lbtestcd)
# write_csv(labs_slctd, "clipboard")

# Note no glucose measures
labs_slctd <- read_csv(
"lbtestcd
ALT
AST
CREAT
CREATCLR
HGB
PLAT
")

labs <- labs %>% 
  semi_join(labs_slctd)

## vitals ----
vitals <- trial1$vitals
vitals <- vitals %>% 
  filter(visit == "SCREENING DAY -21 UP TO DAY 1") %>% 
  select(-visit, -visitnum, -vsdy)
  
vitals <- vitals %>% 
  filter(vstestcd %in% c("DIABP", "HEIGHT", "SYSBP", "WEIGHT")) %>% 
  select(trial, rusubjid, vstestcd, vsstresn)

vitals <- vitals %>% 
  spread(vstestcd, vsstresn)

## Re-organise tables so same as BI ----
bmi <- vitals %>% 
  select(trial, rusubjid, ht = HEIGHT, wt = WEIGHT)  %>% 
  distinct()

## bp
bp <- vitals %>%
  select(trial, rusubjid, DBP = DIABP, SBP = SYSBP) %>% 
  distinct()

## Creata single object ----
san <- list(conmed = conmed, demo = demo, labs = labs, medhist = medhist,
            bp = bp, bmi = bmi, rand = rand)

## Drop variable names and rename to make combining across companies easier ----
san$medhist$mhdecod <- NULL
san$medhist$mhlltcd <- NULL
san$labs$lbcat <- NULL
san$labs$lbstresc <- NULL
san$demo$ageu <-  NULL

san <- map(san, ~ .x %>%  rename(id = rusubjid))


san$bmi <- san$bmi %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)
san$bp <- san$bp %>% 
  gather("param", "value", -trial, -id, na.rm = TRUE)
  

## Save data in a single object ----
saveRDS(san, 
        file = "Processed_data/san.Rds")
