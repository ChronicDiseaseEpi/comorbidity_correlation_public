##08_extract_lilly

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

MakeNewLillyFolderName <- function(studyname) {
  ## WARNING, I AM USING SUPERASSINGMENT FOR CONVENIENCE
  folderout <<- paste0("D:/", str_replace_all(studyname, "-", "_"), "/")
  if(!dir.exists(folderout)) dir.create(folderout)
  
  foldername <<- paste0("C:/Working/5483 - CSDR 1732/files_", studyname, ".zip")
                       
  print("Created link to the following folders in the environement immediately above the function")
  list(paste0("Foldername:  ", foldername), paste0("Folderout:  ", folderout))
}

path <- "Files/Analysis Datasets/SAS_analysis/"


studyname <- "LILLY-B3D-MC-GHAC" # ----

MakeNewLillyFolderName(studyname)

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
    select(patient, visit, therapy, visdate, randcdte )
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "concom.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
    select(patient, visit, visdate, c_route, dictnry,c_indic, c_drug, c_drugid, c_strcdt, lltcode)
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "hxdiag.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
    select(patient, visit, visdate, event, bodysyst, reslvc,  dictnry, lltcode,
           bodysysc)
  
# Labdata
labdata <- read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
labdata <- labdata %>% 
    select(patient, visit, visdate, analyte,analytcd,analyttx, 
           result,resultc, unit,  hilim, lolim,refrng, sampcdt)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
    select(patient, visit, visdate, diabp, sysbp, bmi, heightmm, weightkg, smokstat)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "patinfo.sas7bdat"))) %>% names_to_lower()
demo <- basco %>% 
    select(patient, visit,  age, gender, origgrp)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)



studyname <- "LILLY-B3D-US-GHBZ" # ----

MakeNewLillyFolderName(studyname) 
  
# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
    select(patient, visit, therapy, visdate, v_strcdt )
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "concom.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>%
    select(patient, visit, visdate, c_route, dictnry,c_indic, 
           c_drug, c_drugid, c_strcdt, lltcode)
  
# Medical history
medhist <- read_sas(unz(foldername,  paste0(path, "events.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
    select(patient, visit, visdate,randcdte, ptclast,lltcode, medbodyt, 
           stopcdt,  dictnry, lltcode, relonset)
  
# Labdata
labdata <-  read_sas(unz(foldername,paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
labdata <- labdata %>% 
    select(patient, visit, visdate, analyte,analytcd,analyttx, result,resultc, unit,  hilim, lolim,refrng, sampcdt)
  
# vital signs
vitals <- read_sas(unz(foldername,  paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
    select(patient, visit, visdate, diabp, sysbp, bmi, heightcm, weightkg,bsa)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "patinfo.sas7bdat"))) %>% names_to_lower()
demo <- basco %>% 
    select(patient, age, gender, origin)

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)

  

studyname <- "LILLY-H3S-MC-GGGK" # ----

MakeNewLillyFolderName(studyname)

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
    select(patient, visit, therapy, v_strcdt, v_strcdt )
  
# conmed data
conmed      <- read_sas(unz(foldername, paste0(path, "concom.sas7bdat"))) %>% names_to_lower()
conmedcodes <- read_sas(unz(foldername, paste0(path,"atccodes.sas7bdat"))) %>% names_to_lower()
conmed      <- conmedcodes %>%
  select(patient, visit,   c_drug, c_drugid, atccode, atctext) %>%
  inner_join(conmed %>% 
               select(patient, visit, c_drug, c_route, c_strcdt, c_stpcdt))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "events.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
    select(patient, visit, event, onsetcdt, stopcdt,evtyptxt,
           bodysysc, bodysyst, relonset)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
labdata <- labdata %>% 
    select(patient, visit,  analyte,analytcd,analyttx, result,resultc, unit,  hilim, lolim,refrng, sampcdt)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
    select(patient, visit,  diabp, sysbp, bmi, heightcm, weightkg)
  
# Demographic
basco <- read_sas(unz(foldername, paste0(path, "patinfo.sas7bdat"))) %>% names_to_lower()
demo <- basco %>% 
    select(patient, age, gender, origin, smokinge)

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labdata, vitals, basco, conmedcodes)
rm(folderout, foldername, studyname)
  


studyname <- "LILLY-H6D-MC-LVGY" # ----

MakeNewLillyFolderName(studyname) 
  

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand <- rand %>% 
    select(usubjid, crfvis, trt, sddspdtc, sdldsdtc )
  
# conmed data
conmed  <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed <- conmed %>% 
  select(usubjid, crfvis, dict, whorefid, cmterm, 
         cmname, cmifulnm, cmstdtc, cmendtc)
  
# Medical history
medhist <- read_sas(unz(foldername,  paste0(path, "mhdis.sas7bdat"))) %>% names_to_lower()
medhist <- medhist %>% 
  select(usubjid, crfvis,  lltcd, llterm, pterm, dict, socterm)
   
# Labdata
labdata <- read_sas(unz(foldername,  paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
labdata <- labdata %>% 
    select(usubjid, crfvis, visdt, lbastdtc, lbtest, lbrn,
           lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername,  paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
vitals <- vitals %>% 
    select(usubjid, crfvis, vsdtc, vstestln, vsstrn, vsstruln, vsblval)
  
# Vitals hieght, treat first height as if baseline
rhc <- read_sas(unz(foldername,  paste0(path, "rhc.sas7bdat"))) %>% names_to_lower()
rhc <- rhc %>% 
    select(usubjid, cctestln, cctestsn, ccstresn, ccstrsu) %>% 
    filter(cctestsn %in% c("HGT")) %>% 
    select(-cctestsn) %>% 
    group_by(usubjid) %>% 
    slice(1) %>% 
    ungroup()
  
rhc2 <- rhc %>% 
    rename(vstestln = cctestln,
           vsstrn = ccstresn,
           vsstruln = ccstrsu) %>% 
    mutate(vsdtc = "2232-10-22",
           crfvis = 1,
           vsblval = vsstrn)
  
vitals <- bind_rows(vitals, rhc2)
rm(rhc, rhc2)
  
# Demographics
basco <- read_sas(unz(foldername,  paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
demo <- basco %>% 
    select(usubjid, racecdl,crfvis, sexlnm, ageyr)
  

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)


  
studyname <- "LILLY-H6D-MC-LVHB" # ----

MakeNewLillyFolderName(studyname) 
  
#medhist2 <- read_sas(unz(foldername, paste0(path, "mhdis.sas7bdat"))) %>% names_to_lower()
#basco2 <- read_sas(paste0(foldername, "racet.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
  select(usubjid, visid, trt, rndmdtc ) %>%
  inner_join( rand2 %>% select(usubjid, visid,visdtc ))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% select(usubjid, visid, dict, whodrgid, cmterm,
                            cmname ,cmifulnm, cmstdtc, cmendtc)
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid, lltcd, hxstdtc, hxendtc, pterm, dict, socterm)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
  select(usubjid, visid,  lbacstdtc, lbtest, 
         lbrn, lbru, lbnrhi ,lbnrlo, lbrefnm )
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest )
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm, tbbl, bmibl, hgtcmbl, wgtkgbl)
  

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)



  
studyname <- "LILLY-H6D-MC-LVHG" # ----

MakeNewLillyFolderName(studyname) 

# For this trial there is another level of folder within the sas analysis
# folder I choose the ole folder as "sdytrt" was in it but not the other
# folder called acute, apart from tht the two folders look nearly identical.
  
  
 
# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "ole/", "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "ole/", "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, dispdtc ) %>%
    inner_join( rand2 %>% select(usubjid, visid,visdtc ))
  
# conmed data
conmed  <- read_sas(unz(foldername, paste0(path, "ole/", "cmtpy.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% select(usubjid, visid, dict, atc4, cmterm, 
                            cmname, cmindcl, cmstdtc, cmendtc)
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "ole/", "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid, visid, lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "ole/", "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbastdtc, lbtest, 
           lbrn, lbru, lbnrhi ,lbnrlo, lbrefnm )
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "ole/", "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid,vsrn, vsrulnm, vstest )
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "ole/", "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmiv1, hgtcmv1, wgtkgv1)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)


  
studyname <- "LILLY-H6D-MC-LVHJ" # ----

MakeNewLillyFolderName(studyname) 

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc ) %>%
    inner_join(rand2 %>% select(usubjid, visid, visdtc))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmifulnm, cmstdtc, cmendtc) %>%
  inner_join(conmed2 %>% 
               select(cmterm, whodrgid,atccd4,atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxstdtc, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest )
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm, bmibl, tbbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname)

  
  
studyname <- "LILLY-H6D-MC-LVHR" # ----

MakeNewLillyFolderName(studyname) 
  
# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, rndmdtc) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid, visdtc ))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmifulnm, cmstdtc, cmendtc)%>%
    inner_join(conmed2 %>% 
                 select(cmterm, whodrgid, atccd4, atctxt4 ))
  
# Medical histor
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxstdtc, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi,lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname)

  
  
studyname <- "LILLY-H6D-MC-LVHS" # ----

MakeNewLillyFolderName(studyname) 
  
  

#medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid,visdtc))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmifulnm, cmstdtc, cmendtc)
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxstdtc, hxendtc)
  
# Labdata
labdata <- read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)  

  
  
studyname <- "LILLY-H6D-MC-LVID" # ----

MakeNewLillyFolderName(studyname) 
  
  
  #conmed2  <- read_sas(paste0(foldername, "cmtpyatc.sas7bdat")) %>% names_to_lower()
  
  #medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc ) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid, visdtc ))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmifulnm, cmstdtc, cmendtc)
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid, lltcd, pterm, dict, socterm, hxstdtc, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid, lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, labdata, vitals, basco)
rm(folderout, foldername, studyname)  

  
  
  
studyname <- "LILLY-H9X-MC-GBCF" # ----

MakeNewLillyFolderName(studyname) 
  
  

  
  #medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()
 # medhist3 <- read_sas(paste0(foldername, "mhdisptq.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc) %>%
    inner_join(rand2 %>% 
                  select(usubjid, visid, visdtc))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid,dict,whodrgid,cmterm, cmstdtc, cmendtc) %>%
  inner_join(conmed2 %>% 
                select(cmterm, whodrgid, atccd4, atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <- read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid, lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname) 

  
  
  
studyname <- "LILLY-H9X-MC-GBDA" # ----

MakeNewLillyFolderName(studyname) 
  
  
  
  
 # medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc ) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid, visdtc ))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% select(usubjid, visid, dict, whodrgid, cmterm, cmstdtc, cmendtc) %>%
    inner_join(conmed2 %>% 
                  select(cmterm, whodrgid, atccd4, atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi,lbnrlo, lbrefnm)
  
# vital signs
vitals  <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest )
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname) 


  
  
studyname <- "LILLY-H9X-MC-GBDB" # ----

MakeNewLillyFolderName(studyname) 

 
  
  
  #medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc ) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid,visdtc ))
  
# conmed data
conmed  <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2  <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
    select(usubjid, visid, dict, whodrgid, cmterm, cmstdtc, cmendtc) %>%
    inner_join(conmed2 %>% 
                 select(cmterm, whodrgid, atccd4, atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxendtc)
  
# Labdat
labdata <- read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)

# vital signs
vitals  <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest )
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)


### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname) 

  
  
studyname <- "LILLY-H9X-MC-GBDC" # ----

MakeNewLillyFolderName(studyname) 
  
  
  
  
  #medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()
  

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid, visdtc))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmstdtc, cmendtc) %>%
    inner_join(conmed2 %>% 
                 select(cmterm, whodrgid, atccd4, atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm ,ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)
  

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname) 


  
studyname <- "LILLY-H9X-MC-GBDD" # ----

MakeNewLillyFolderName(studyname) 

#  medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc ) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid, visdtc))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% select(usubjid, visid, dict, whodrgid, cmterm, cmstdtc, cmendtc) %>%
    inner_join(conmed2 %>% 
                  select(cmterm, whodrgid, atccd4, atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid, lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals  <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest )
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)
  

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname) 
  
  
  
studyname <- "LILLY-H9X-MC-GBDE" # ----

MakeNewLillyFolderName(studyname) 

#medhist2 <- read_sas(paste0(foldername, "mhdis.sas7bdat")) %>% names_to_lower()

# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc) %>%
    inner_join(rand2 %>% 
                 select(usubjid, visid, visdtc))
  
# conmed data,
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmstdtc, cmendtc) %>%
  inner_join(conmed2 %>% 
               select(cmterm, whodrgid, atccd4, atctxt4 ))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <- read_sas(unz(foldername, paste0(path, "labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
  select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi,lbnrlo, lbrefnm)
  
# vital signs
vitals <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)
  

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname) 

  
  
studyname <- "LILLY-H9X-MC-GBDG" # ----

MakeNewLillyFolderName(studyname) 
  
# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "sdytrt.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas(unz(foldername, paste0(path, "visit.sas7bdat"))) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, visid, trt, sdytrtstdtc) %>%
    inner_join(rand2 %>% select(usubjid, visid, visdtc))
  
# conmed data
conmed <- read_sas(unz(foldername, paste0(path, "cmtpy.sas7bdat"))) %>% names_to_lower()
conmed2 <- read_sas(unz(foldername, paste0(path, "cmtpyatc.sas7bdat"))) %>% names_to_lower()
ExtractLabel(conmed)
conmed <- conmed %>% 
  select(usubjid, visid, dict, whodrgid, cmterm, cmstdtc, cmendtc) %>%
    inner_join(conmed2 %>% 
                 select(cmterm, whodrgid, atccd4, atctxt4))
  
# Medical history
medhist <- read_sas(unz(foldername, paste0(path, "history.sas7bdat"))) %>% names_to_lower()
ExtractLabel(medhist)
medhist <- medhist %>% 
    select(usubjid,  lltcd, pterm, dict, socterm, hxendtc)
  
# Labdata
labdata <-  read_sas(unz(foldername, paste0(path,"labs.sas7bdat"))) %>% names_to_lower()
ExtractLabel(labdata)
labdata <- labdata %>% 
    select(usubjid, visid,  lbacstdtc, lbtest, lbrn, lbru, lbnrhi, lbnrlo, lbrefnm)
  
# vital signs
vitals  <- read_sas(unz(foldername, paste0(path, "vitals.sas7bdat"))) %>% names_to_lower()
ExtractLabel(vitals)
vitals <- vitals %>% 
    select(usubjid, visid, vsrn, vsrulnm, vstest)
  
# Demographics
basco <- read_sas(unz(foldername, paste0(path, "subjinfo.sas7bdat"))) %>% names_to_lower()
ExtractLabel(basco)
demo <- basco %>% 
    select(usubjid, racelnm, ageyr, sexlnm,  bmibl, tbbl, wgtkgbl, hgtcmbl, rndmdtc)
  

### Save
if(!dir.exists(folderout)) dir.create(folderout)
save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(folderout, "extract.Rdata")) 
rm(rand, rand2, demo, medhist, conmed, conmed2, labdata, vitals, basco)
rm(folderout, foldername, studyname)

  
  
studyname <- "LILLY-I1F-MC-RHAZ" # @MISSING ----

MakeNewLillyFolderName(studyname) 

 
  
  conmed  <- read_sas(paste0(foldername, "adcm.sas7bdat")) %>% names_to_lower()
  
  medhist <- read_sas(paste0(foldername, "admh.sas7bdat")) %>% names_to_lower()
  vitals  <- read_sas(paste0(foldername, "advs.sas7bdat")) %>% names_to_lower()
  basco <- read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()
  labdata <-  read_sas(paste0(foldername, "adlboth.sas7bdat")) %>% names_to_lower()
  labdata2    <- read_sas( paste0(foldername,"lb.sas7bdat")) %>% names_to_lower()
  
# Randomisation data
rand <- read_sas(unz(foldername, paste0(path, "adsl.sas7bdat"))) %>% names_to_lower()
rand2 <- read_sas( paste0(foldername,"ts.sas7bdat")) %>% names_to_lower()
ExtractLabel(rand)
rand <- rand %>% 
    select(usubjid, arm, randdt,rranddt) 
  
  ### conmed data,
  #ExtractLabel(conmed)
  conmed <- conmed %>% select(usubjid, visitnum ,cmwhover,cmclascd,cmclas, cmdecod, cmroute, cmindc, cmstdtc,cmendtc) 
  
  ### Medical history
  #ExtractLabel(medhist)
  medhist <- medhist %>% 
    select(usubjid,  mhcat, mhoccur,mhterm,
           mhlltcd, mhdecod, mhptcd, mhbodsys, mhstdtc, visitnum, mhout, mhendtc)
  
  ### Labdata
  ExtractLabel(labdata2)
  labdata <- labdata2 %>% 
    select(usubjid, visitnum,  lbcat,lbstresn,lbstresu, lbstnrlo ,lbstnrhi, lbspec ,lbtest,lbtestcd,lbfast, lbdy)
  
  ### vital signs
  #ExtractLabel(vitals)
  vitals <- vitals %>% 
    select(usubjid, visitnum,param, aval )
  
  ### Demographics
  #ExtractLabel(basco)
  demo <- basco %>% 
    select(usubjid, race, age, sex,  basewt1, baseht, basebmi1,tbccgr1)
  
  save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(foldername, "extract.Rdata"))
  
  
  
  
  
studyname <- "LILLY-I1F-MC-RHBA" # @MISSING ----
  MakeNewLillyFolderName(studyname) 
  
  
  conmed  <- read_sas(paste0(foldername, "adcm.sas7bdat")) %>% names_to_lower()
  rand    <- read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()
  rand2    <- read_sas( paste0(foldername,"ts.sas7bdat")) %>% names_to_lower()
  medhist <- read_sas(paste0(foldername, "admh.sas7bdat")) %>% names_to_lower()
  vitals  <- read_sas(paste0(foldername, "advs.sas7bdat")) %>% names_to_lower()
  basco <- read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()
  labdata <-  read_sas(paste0(foldername, "adlboth.sas7bdat")) %>% names_to_lower()
  labdata2    <- read_sas( paste0(foldername,"lb.sas7bdat")) %>% names_to_lower()
  
  ### Randomisation data
  #ExtractLabel(rand)
  rand <- rand %>% 
    select(usubjid,  arm , randdt ,rranddt) 
  
  ### conmed data,
  #ExtractLabel(conmed)
  conmed <- conmed %>% select(usubjid, visitnum ,cmwhover,cmclascd,cmclas, cmdecod, cmroute, cmindc, cmstdtc,cmendtc) 
  
  ### Medical history
  #ExtractLabel(medhist)
  medhist <- medhist %>% 
    select(usubjid,  mhcat, mhoccur,mhterm,
           mhlltcd, mhdecod, mhptcd, mhbodsys, mhstdtc, visitnum, mhout, mhendtc)
  
  ### Labdata
  ExtractLabel(labdata2)
  labdata <- labdata2 %>% 
    select(usubjid, visitnum,  lbcat,lbstresn,lbstresu, lbstnrlo ,lbstnrhi, lbspec ,lbtest,lbtestcd,lbfast, lbdy)
  
  ### vital signs
  #ExtractLabel(vitals)
  vitals <- vitals %>% 
    select(usubjid, visitnum,param, aval )
  
  ### Demographics
  #ExtractLabel(basco)
  demo <- basco %>% 
    select(usubjid, race, age, sex,  basewt1, baseht, basebmi1,tbccgr1)
  
  save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(foldername, "extract.Rdata"))
  
  
studyname <- "LILLY-I1F-MC-RHBC" # @MISSING ----
  MakeNewLillyFolderName(studyname) 
  
  
  conmed  <- read_sas(paste0(foldername, "adcm.sas7bdat")) %>% names_to_lower()
  rand    <- read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()
  rand2    <- read_sas( paste0(foldername,"ts.sas7bdat")) %>% names_to_lower()
  medhist <- read_sas(paste0(foldername, "admh.sas7bdat")) %>% names_to_lower()
  vitals  <- read_sas(paste0(foldername, "advs.sas7bdat")) %>% names_to_lower()
  basco <- read_sas(paste0(foldername, "adsl.sas7bdat")) %>% names_to_lower()
  labdata <-  read_sas(paste0(foldername, "adlboth.sas7bdat")) %>% names_to_lower()
  labdata2    <- read_sas( paste0(foldername,"lb.sas7bdat")) %>% names_to_lower()
  
  ### Randomisation data
  ExtractLabel(rand)
  rand <- rand %>% 
    select(usubjid,  arm , randdt ) 
  
  ### conmed data,
  ExtractLabel(conmed)
  conmed <- conmed %>% select(usubjid, visitnum ,cmwhover,cmclascd,cmclas, cmdecod, cmroute, cmindc, cmstdtc,cmendtc) 
  
  ### Medical history
  ExtractLabel(medhist)
  medhist <- medhist %>% 
    select(usubjid,  mhcat, mhoccur,mhterm,
           mhlltcd, mhdecod, mhptcd, mhbodsys, mhstdtc, visitnum, mhout, mhendtc)
  
  ### Labdata
  ExtractLabel(labdata2)
  labdata <- labdata2 %>% 
    select(usubjid, visitnum,  lbcat,lbstresn,lbstresu, lbstnrlo ,lbstnrhi, lbspec ,lbtest,lbtestcd,lbfast, lbdy)
  
  ### vital signs
  ExtractLabel(vitals)
  vitals <- vitals %>% 
    select(usubjid, visitnum,param, aval )
  
  ### Demographics
  ExtractLabel(basco)
  demo <- basco %>% 
    select(usubjid, race, age, sex,  basewt1, baseht, basebmi1,tbccgr1)
  
  save(rand, demo, medhist, conmed, labdata, vitals, file = paste0(foldername, "extract.Rdata"))
  
  
  

# Consolidate all files ----
## The following consolidates the data and ensures all matches
  
## Combine all files into a single dataframe ----

#List folder names
foldernames <- list("LILLY_B3D_MC_GHAC", "LILLY_B3D_US_GHBZ", "LILLY_H3S_MC_GGGK", 
                    "LILLY_H6D_MC_LVGY", "LILLY_H6D_MC_LVHB", "LILLY_H6D_MC_LVHG", 
                    "LILLY_H6D_MC_LVHJ", "LILLY_H6D_MC_LVHR", "LILLY_H6D_MC_LVHS", 
                    "LILLY_H6D_MC_LVID", "LILLY_H9X_MC_GBCF", "LILLY_H9X_MC_GBDA", 
                    "LILLY_H9X_MC_GBDB", "LILLY_H9X_MC_GBDC", "LILLY_H9X_MC_GBDD",
                    "LILLY_H9X_MC_GBDE", "LILLY_H9X_MC_GBDG") 

# @MISSING : "LILLY_I1F_MC_RHAZ", "LILLY_I1F_MC_RHBA", "LILLY_I1F_MC_RHBC"

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
  
# Transpose list so that trial is nested within table name
all_trials <- transpose(all_trials)
  
save(all_trials, file = "all_lilly_extracted.Rdata")

load("E:/C_analysis_code_meta/Extract_Data/all_lilly_extracted.Rdata")

# Add in LVGY data as has height from another table
load("D:/LILLY_H6D_MC_LVGY/extract.Rdata")

all_trials$vitals$LILLY_H6D_MC_LVGY <- vitals
rm(conmed, demo, labdata, medhist, rand, vitals)

## Randomisation table ----
rand <- all_trials$rand
a <- CreateTableNameLabels(rand)

# Harmonise variable names
rand <- map(rand, function(x) {
  names(x)[names(x) %in% c("arm", "trt","therapy")] <- "trt"
  names(x)[names(x) %in% c("randcdte","rndmdtc", "randdt")] <- "randdt"
  names(x)[names(x) %in% c("usubjid","patient")] <- "subjid"
  x <- x[!names(x) %in% c("visit","crfvis","dispdtc","sddspdtc" ,
                            "sdldsdtc" ,"sdytrtstdtc","rranddt",
                            "v_strcdt", "visdate","visdtc","visid" )]
  
    x
})

# Make variables same format in all trials
rand <- map(rand, function(current_df) {
  x <- current_df
  x$subjid <- as.character(x$subjid)
  if("randdt" %in% names(x)) x$randdt <-  as.character(x$randdt)
  x})

CreateTableNameLabels(rand) #"trial" "randdt" "subjid" "trt"  
rand <- BindRowsWLabels(rand,2)

# limit to randomised patients @SCREEN
table(rand$trt == "")
table(is.na(rand$trt))
not_randomised <- c("","SCREEN FAILURE", "NOT ASSIGNED")
rand <- rand %>%
  filter(!trt %in% not_randomised)

rand <- distinct(rand)

## Randomisation schedule, taken mainly from flowchart and schedule tables in study protocol ----
randomisation_schedule <- tribble(
  ~"trial", ~"pre", ~"rand", ~"post_rand", ~"special",
  "LILLY-B3D-MC-GHAC", "0:3", "4", "5:21","",
  "LILLY-B3D-US-GHBZ", "1", "2","3:10","",
  "LILLY-H3S-MC-GGGK", "1","2","3:13", "99 = '??', 299 = '??', 399 = '??'", #Couldn't find ?? in CRF or DD
  "LILLY-H6D-MC-LVGY", "1:2","3","4:12","99 = '??', 999 = 'Early discontinuation visit'", 
  "LILLY-H6D-MC-LVHB", "1:2", "3", "4:7","",
  "LILLY-H6D-MC-LVHG", "1:2","3","4:12","",
  "LILLY-H6D-MC-LVHJ", "1:2","3","4:7","801 = '??' ",
  "LILLY-H6D-MC-LVHR", "1:2","3","4:7","801 = '??' ",
  "LILLY-H6D-MC-LVHS", "1:2","3","4:7","801 = '??' ",
  "LILLY-H6D-MC-LVID", "1:2","3","4:7","",
  "LILLY-H9X-MC-GBCF", "1:3","4","4:15", "801 = '??',802 = '??' ",
  "LILLY-H9X-MC-GBDA", "1:4","5","6:12","801 = '??',802 = '??' ",
  "LILLY-H9X-MC-GBDB", "1:4","5","6:17","801 = '??',802 = '??',803 = '??' ",
  "LILLY-H9X-MC-GBDC", "1","2","3:8","801 = '??',802 = '??' ",
  "LILLY-H9X-MC-GBDD", "1:3","4","4:13", "801 = '??',802 = '??' ",
  "LILLY-H9X-MC-GBDE", "1:2","3","4:10","801 = '??',802 = '??' ",
  "LILLY-H9X-MC-GBDG", "1:2","3","4:9","801 = '??',802 = '??' ")
  
# @MISSING:  
#  "LILLY-I1F-MC-RHAZ", "1","2","3:25", "801 = 'Post tx visit 1',802 = 'Post tx visit 2',803 = 'Post tx visit 3' ",
#  "LILLY-I1F-MC-RHBA", "1","2","3:25", "801 = 'Post tx visit 1',802 = 'Post tx visit 2',803 = 'Post tx visit 3' ",
#  "LILLY-I1F-MC-RHBC", "1","2","3:22", "801 = 'Post tx visit 1',802 = 'Post tx visit 2',803 = 'Post tx visit 3' ")

# Replace dash with underscore to match file names 
randomisation_schedule$trial <- randomisation_schedule$trial %>% str_replace_all(pattern = "-", replacement = "_")
  


############################ 
# 801/2/3 as post-treatment probably consistent throughout but I could not find confirmation of this for most
pre_rand <- map2(randomisation_schedule$pre, randomisation_schedule$rand, ~ c(eval(parse(text = .x)),
                                                                              eval(parse(text = .y))))
names(pre_rand) <- randomisation_schedule$trial
pre_rand <- stack(pre_rand)
names(pre_rand) <- c("visit", "trial")


## Demographics table ----
demo <- all_trials$demo
a <- CreateTableNameLabels(demo)

#Smoking variable from vitals for one study
demo$LILLY_B3D_MC_GHAC <- demo$LILLY_B3D_MC_GHAC %>% 
  inner_join(all_trials$vitals$LILLY_B3D_MC_GHAC %>% select(patient , smokstat))

# Harmonise variable names
demo <- map(demo, function(x) {
  names(x)[names(x) %in% c("age", "ageyr")] <- "age"
  names(x)[names(x) %in% c("sex", "gender","sexlnm")] <- "sex"
  names(x)[names(x) %in% c("origgrp", "origin","race", "racecdl", "racelnm")] <- "race"
  names(x)[names(x) %in% c("basewt1", "wgtkgbl","wgtkgv1")] <- "wt"
  names(x)[names(x) %in% c("baseht", "hgtcmbl", "hgtcmv1")] <- "ht"
  names(x)[names(x) %in% c("basebmi1", "bmibl", "bmiv1")] <- "bmi"
  names(x)[names(x) %in% c("smokinge", "tbbl", "tbccgr1", "smokstat")] <- "smoke"
  names(x)[names(x) %in% c("usubjid","patient")] <- "subjid"
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("visit", "crfvis")]
  x
})

CreateTableNameLabels(demo) # "trial" "age" "bmi""ht" "race" "sex" "smoke""subjid" "wt" 

# Make variables same format in all trials
demo <- map(demo, function(current_df) {
  x <- current_df
  x$subjid <- as.character(x$subjid)
  x$age <- as.character(x$age)
  if("smoke" %in% names(x)) x$smoke <-  as.character(x$smoke)
  x}
)

demo <- BindRowsWLabels(demo, 2)

# limit to randomised patients @SCREEN
demo <- demo %>% 
  semi_join(rand)

demo <- distinct(demo)


saveRDS(demo, "Processed_data/lilly_demo.Rds")


## Add dates from demo into randomisation table, 16 of the 20 trials
rand <- rand %>% 
  left_join(demo %>% distinct(trial, subjid, rndmdtc)) %>% 
  mutate(randdt = if_else(is.na(randdt), rndmdtc, randdt)) %>% 
  select(-rndmdtc)


## Labs table ----
labs <- all_trials$labdata
a <- CreateTableNameLabels(labs)

# Similarly formatted groups of trials:
labs1 <- all_trials$labdata[names(all_trials$labdata) %in% 
                           c("LILLY_B3D_MC_GHAC", "LILLY_B3D_US_GHBZ", "LILLY_H3S_MC_GGGK")]
CreateTableNameLabels(labs1)
labs1 <- map(labs1, function(x) {
  names(x)[names(x) %in% c("patient")] <- "subjid"
  names(x)[names(x) %in% c("analyttx")] <- "lbtest"
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("analytcd", "analyte", "refrng","result","visdate")]
  x
})
CreateTableNameLabels(labs1)
labs1 <- BindRowsWLabels(labs1, 1)

labs2 <- all_trials$labdata[names(all_trials$labdata) %in%
                           c("LILLY_H6D_MC_LVGY", "LILLY_H6D_MC_LVHB", "LILLY_H6D_MC_LVHG",
                            "LILLY_H6D_MC_LVHJ", "LILLY_H6D_MC_LVHR", "LILLY_H6D_MC_LVHS", 
                            "LILLY_H6D_MC_LVID", "LILLY_H9X_MC_GBCF", "LILLY_H9X_MC_GBDA", 
                            "LILLY_H9X_MC_GBDB","LILLY_H9X_MC_GBDC", "LILLY_H9X_MC_GBDD",
                            "LILLY_H9X_MC_GBDE","LILLY_H9X_MC_GBDG")]

names(labs2$LILLY_H6D_MC_LVGY)
CreateTableNameLabels(labs2)
labs2 <- map(labs2, function(x) {
  names(x)[names(x) %in% c("usubjid")] <- "subjid"
  names(x)[names(x) %in% c("crfvis","visid")] <- "visit"
  names(x)[names(x) %in% c("lbacstdtc","lbastdtc" )] <- "sampcdt"
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("lbrefnm", "crfvis", "visdt")]
  x
})
CreateTableNameLabels(labs2)
labs2 <- BindRowsWLabels(labs2, 1)

#labs3 <- all_trials$labdata[names(all_trials$labdata) %in%
  #                         c("LILLY_I1F_MC_RHAZ","LILLY_I1F_MC_RHBA","LILLY_I1F_MC_RHBC")] # @MISSING

#CreateTableNameLabels(labs3)
#labs3 <- map(labs3, function(x) {
#  names(x)[names(x) %in% c("usubjid")] <- "subjid"
#  names(x)[names(x) %in% c("visitnum")] <- "visit"
#  x$subjid <- as.character(x$subjid)
#  x <- x[!names(x) %in% c("lbcat", "lbmethod","lbfast")]
#  x
#})
#CreateTableNameLabels(labs3)
#labs3 <- BindRowsWLabels(labs3, 1)

# Join 1&2 together
names_lkp <- c("trial" = "trial",
               "subjid" = "subjid",
               "lbtest" = "lbtest",
               "hilim" = "lbnrhi",
               "lolim" = "lbnrlo",
               "resultc" = "lbrn",
               "unit" = "lbru",
               "sampcdt" = "sampcdt",
               "visit" = "visit")

names(labs1) <- names_lkp[names(labs1)]
labs1[c("lbnrhi", "lbnrlo", "lbrn")] <- map(labs1[c("lbnrhi", "lbnrlo", "lbrn")], as.double)
mean(is.na(labs1$lbrn))
# 0.4684763
#this introduces NAs - check that these do not exceed number of ""
#entries by many (i.e.,, are true NAs on the whole)
#re-create labs1 then:
xmn <- labs1 %>% filter(! lbrn %in% "")
1- nrow(xmn)/nrow(labs1)
#0.4423597 of total missing (vs 0.4684763 after coercion to double)
#This looks fine (some other entries improperly formatted as expected)
#NB NAs are so high in general because of the way most trials' lb tables
#are structured - i.e., with a 'spare' row for each entry 
labs1_2 <- bind_rows(labs1, labs2)
a <- ExtractLabel(labs2, return_object = TRUE)
setdiff(names(labs1_2), names(a))
labs1_2 <- MakeLabels(labs1_2, a)
labs <- labs1_2
rm(labs1, labs2, labs1_2)
head(labs, 6)

# Join all 3 together
#names_lkp <- c("trial" = "trial",
#               "subjid" = "subjid",
#               "lbtest" = "lbtest",
#               "lbtestcd" = "lbtestcd",
#               "lbstnrhi" = "lbnrhi",
#               "lbstnrlo" = "lbnrlo",
#               "lbstresn" = "lbrn",
#               "lbstresu" = "lbru",
#               "lbspec" = "lbspec",
#               "lbdy" = "sampcdt",
#               "visit" = "visit")
#names(labs3) <- names_lkp[names(labs3)]
#labs3[c("lbnrhi", "lbnrlo", "lbrn")] <-
#  map(labs3[c("lbnrhi", "lbnrlo", "lbrn")], as.double)
#labs3[c("sampcdt")] <-
#  map(labs3[c("sampcdt")], as.character)
#labs1_2_3 <- bind_rows(labs1_2, labs3)

#a <- ExtractLabel(labs2, return_object = TRUE)
#setdiff(names(labs1_2_3), names(a))
#a <- c(a, "lbspec" = "lab_specimen_type")
#labs1_2_3 <- MakeLabels(labs1_2_3, a)
#labs <- labs1_2_3
#rm(labs1, labs1_2, labs1_2_3, labs2, labs3)




## Limit to randomised patients
labs <- labs %>% 
  semi_join(rand)

## Limt to pre/randomisation visits
labs <- labs %>% 
  semi_join(pre_rand)

# Review lab names
labs_unq <- labs %>%
  distinct(lbtest) %>% # lbspec missing so removed from distinct  
  arrange(lbtest)

# write_csv(labs_unq, "Scratch_data/review_lilly_labs.csv")
labs_slct <- read_csv("Created_metadata/reviewED_lilly_labs.csv")

labs <- labs %>% 
  inner_join(labs_slct) %>% 
  mutate(lbtest = lbtest_rename) %>% 
  select(-lbtest_rename)

a <- tapply(labs$lbtest, labs$trial, function(x) sort(unique(x)))
# Got all measures in all trials except glucose, only in 
reduce(a, intersect)
reduce(a, union)
b <- map(a, ~ (c("GLUCOSE, FASTING", "GLUCOSE, NON-FASTING OR RANDOM", "GLUCOSE") %in% .x))
b <- do.call(rbind, b) %>% 
  as_tibble() %>% 
  set_names(c("fasting", "non_fasting", "not_specified"))

saveRDS(labs, "Processed_data/lilly_labs.Rds")
rm(labs, a, b, labs_slct, labs_unq)
gc()



## Concomitant medicines table ----

# Similarly formatted groups of trials:
conmed1 <- all_trials$conmed[names(all_trials$conmed) %in% 
                              c("LILLY_B3D_MC_GHAC", "LILLY_B3D_US_GHBZ", "LILLY_H3S_MC_GGGK")]
conmed2 <- all_trials$conmed[names(all_trials$conmed) %in%
                              c("LILLY_H6D_MC_LVGY", "LILLY_H6D_MC_LVHB", "LILLY_H6D_MC_LVHG",
                                "LILLY_H6D_MC_LVHJ", "LILLY_H6D_MC_LVHR", "LILLY_H6D_MC_LVHS", 
                                "LILLY_H6D_MC_LVID", "LILLY_H9X_MC_GBCF", "LILLY_H9X_MC_GBDA", 
                                "LILLY_H9X_MC_GBDB","LILLY_H9X_MC_GBDC","LILLY_H9X_MC_GBDD",
                                "LILLY_H9X_MC_GBDE","LILLY_H9X_MC_GBDG")]
#conmed3 <- all_trials$conmed[names(all_trials$conmed) %in%
#                              c("LILLY-I1F-MC-RHAZ","LILLY-I1F-MC-RHBA","LILLY-I1F-MC-RHBC")] # Trials missing

a <- CreateTableNameLabels(conmed1)
conmed1 <- map(conmed1, function(x) {
  names(x)[names(x) %in% c("patient")] <- "subjid"
  names(x)[names(x) %in% c("c_strcdt")] <- "cmstdt"
  x$cmstdt <- as.character(x$cmstdt)
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("c_stpcdt", "visdate","lbfast")]
  x
})
CreateTableNameLabels(conmed1)
conmed1 <- BindRowsWLabels(conmed1, 1)

a <- CreateTableNameLabels(conmed2)
conmed2$LILLY_H6D_MC_LVHG$whodrgid <- NA # need to set to NA to convert to character; but check really missing

conmed2 <- map(conmed2, function(x) {
  names(x)[names(x) %in% c("usubjid")] <- "subjid"
  names(x)[names(x) %in% c("c_strcdt")] <- "cmstdt"
  names(x)[names(x) %in% c("atc4","atccd4")] <- "atccd4"
  names(x)[names(x) %in% c("cmifulnm","cmindcl")] <- "cmindc"
  names(x)[names(x) %in% c("crfvis","visid")] <- "visit"
  names(x)[names(x) %in% c("whodrgid","whorefid")] <- "whodrgid"
  x <- x[!names(x) %in% c("c_stpcdt", "visdate","cmname")]
  x$whodrgid <- as.character(x$whodrgid)
  x
})
CreateTableNameLabels(conmed2)

conmed2 <- BindRowsWLabels(conmed2, 1)


#CreateTableNameLabels(conmed3)
#These are identically formatted with all variables present
#conmed3 <- BindRowsWLabels(conmed3, 1)


# Join all 1&2 together
names_lkp <- c("trial" = "trial",
               "subjid" = "subjid",
               "c_drug" = "cmterm",
               "c_indic" = "cmindc",
               "cmstdt" = "cmstdtc",
                "dictnry" = "dict",
               "atccode" = "atccd4",
               "atctext" = "atctxt4",
               "c_drugid" = "c_drugid",
               "c_route" = "c_route",
               "lltcode" = "lltcode",
               "visit" = "visit")

names(conmed1) <- names_lkp[names(conmed1)]
conmed1_2 <- bind_rows(conmed1, conmed2)
a <- ExtractLabel(conmed2, return_object = TRUE)
setdiff(names(conmed1_2), names(a))
a <- c(a, "c_drugid" = "Drug Code","c_route" = "Route","lltcode" = "LLT_CODE|MedDRA Dictionary code")
conmed1_2 <- MakeLabels(conmed1_2, a)
setdiff(names(conmed1_2), names(a))

# Join all 3 together
#names_lkp <- c("trial" = "trial",
#               "usubjid" = "subjid",
#               "cmdecod" = "cmterm",
#               "cmindc" = "cmindc",
#               "cmstdtc" = "cmstdtc",
#               "cmwhover" = "dict",
#               "cmclas" = "cmclas",
#               "cmclascd" = "cmclascd",
#               "cmroute" = "c_route",
#               "cmendtc" = "cmendtc",
#               "visitnum" = "visit")
#(names(conmed3) <- names_lkp[names(conmed3)])
#conmed1_2_3 <- bind_rows(conmed1_2, conmed3)
#a <- ExtractLabel(conmed1_2, return_object = TRUE)
#setdiff(names(conmed1_2_3), names(a))
#a <- c(a, "cmclas" = "Medication Class","cmclascd" = "Medication Class Code")
#conmed1_2_3 <- MakeLabels(conmed1_2_3, a)




# Select only randomised patients
conmed <- conmed1_2 %>% 
  semi_join(rand)

conmed <- conmed1_2
rm(conmed1, conmed2, conmed1_2)

# Note that for LILLY-H6D-MC-LVHG" the atc code is actually the text label for the class
# I checked, 216 of the 250 drugs are in the list of drug classes
# Most match automatically MAtch these manually to fix. Mostly due to use of abbreviations eg combin. for combinations
# No route data, so do automatic matches downstream (91_conmed_clean) when have scripts to cope with ambiguous atc class names
lvgh <- conmed %>%
  filter(trial == "LILLY_H6D_MC_LVHG") %>% 
  distinct(atccd4, atctxt4)

conmed2 <- conmed %>% 
  mutate(atctxt4 = if_else(trial == "LILLY_H6D_MC_LVHG", atccd4, atctxt4),
         atccd4 = if_else(trial == "LILLY_H6D_MC_LVHG", NA_character_, atccd4))
class_lilly_lkp <- read_csv("Created_metadata/reviewED_lilly_drug_Classes_not_atc.csv") %>% 
  na.omit()

conmed2 <- conmed2 %>% 
  left_join(class_lilly_lkp %>%  rename(atctxt4 = atccd4, atccd4b = str)) %>% 
  mutate(atccd4 = if_else(trial == "LILLY_H6D_MC_LVHG" & !is.na(atccd4b), atccd4b, atccd4)) %>% 
  select(-atccd4b)
conmed <- conmed2
rm(conmed2, class_lilly_lkp)

# Further harmonise atc codes, under two labels currently
#table(!is.na(conmed$atccd4) & !is.na(conmed$cmclascd)) # cmclasscd missing as from conmed3 (missing trials)
table(!is.na(conmed$atccd4))
 
#conmed <- conmed %>% 
#mutate(atccd4 = if_else(atccd4 %>%  is.na(), cmclascd, atccd4)) %>% 
#    select(-cmclascd)

## Select only pre-attendance drugs (ie not adverse event or post randomisation)
# Splits data into three 16 trials, with vists data, 3 with treatment start
# dates, but with missing data for drug category (LILLY-I1F-MC-RHAZ,
# LILLY-I1F-MC-RHBA, LILLY-I1F-MC-RHBC) and one without visits data but with ony
# 3 missing values for the drug category "LILLY-H6D-MC-LVGY"

# First 16
# Note have visit for all trials except   
# and LILLY-H6D-MC-LVGY. The latter has all visit dates set to "40" according to the documentation
conmed16 <- conmed %>% 
  semi_join(pre_rand) 
# Looked at "prophylaxis" but includes drugs like seretide and amlodipine

# Three trials, use cmindic alone
#conmed3 <- conmed %>% 
#  filter(trial %in% c("LILLY-I1F-MC-RHAZ", "LILLY-I1F-MC-RHBA", "LILLY-I1F-MC-RHBC"))
#setdiff(conmed$trial, union(conmed3$trial, conmed16$trial))
# All three have cmindic values of 
# [1] ""                                       
# [2] "PRIMARY STUDY CONDITION"                
# [3] "PROPHYLAXIS OR NON-THERAPEUTIC"         
# [4] "PRE-EXISTING CONDITION OR ADVERSE EVENT" 
# However there are many empty strings
# There are tiny numbers with completely missing start dates, some dates are just yyyy, or yyyy-mm, pad these
#conmed3 <- conmed3 %>%
#  mutate(stl = str_length(cmstdtc),
#         start_date = case_when(
#    stl == 0 ~ NA_character_,
#    stl == 4 ~ paste0(cmstdtc, "-01-01"),
#    stl == 7 ~ paste0(cmstdtc, "-01"),
#    stl == 10 ~ cmstdtc
#  )) %>% 
#  mutate(start_date = as.Date(start_date))

#rand3 <- rand %>% 
#  filter(trial %in% c("LILLY-I1F-MC-RHAZ", "LILLY-I1F-MC-RHBA", "LILLY-I1F-MC-RHBC")) %>% 
#  mutate(rand_date = as.Date(randdt))

#conmed3 <- conmed3 %>%
#  mutate(conmed3id = seq_along(trial))

#conmed3 <- conmed3 %>% 
#  filter(!cmindc %in% c("PRIMARY STUDY CONDITION")) %>% 
#  left_join(rand3 %>%  select(trial, subjid, rand_date)) %>% 
#  filter( (!is.na(start_date) & start_date <= rand_date) |
 #           cmindc == "PRE-EXISTING CONDITION OR ADVERSE EVENT") %>% 
 # select(-rand_date)

# for "LILLY-H6D-MC-LVGY" indication is defined as Primary Study Condition, Pre-existing Condition, 
# Prophylaxis or Non-Therapeutic Use, OR Adverse Event, 3 blank values only
lvgy <- conmed %>% 
  filter(trial == "LILLY_H6D_MC_LVGY", cmindc == "Pre-existing Condition")

conmed_all <- bind_rows(conmed16, lvgy) %>% 
  select(trial, id = subjid, atccd4, cmterm, c_route) %>% 
  distinct()

## SUmmarise number with each condition
conmed_all %>%
  group_by(trial) %>%
  summarise(terms = sum(!duplicated(cmterm)),
            codes = sum(!duplicated(atccd4)),
            participants = sum(!duplicated(id)))

saveRDS(conmed_all, "Processed_data/lilly_conmed.Rds")
rm(conmed, conmed_all, conmed16, lvgh, lvgy)




## Medical history table ----
medhist <- all_trials$medhist
a <- CreateTableNameLabels(medhist)

## Error in earlier code had full stop rather than comma, caused not to select any fields
medhist$LILLY_H6D_MC_LVHB <-  medhist$LILLY_H6D_MC_LVHB %>% 
  select(usubjid, lltcd, hxstdtc, hxendtc, pterm, dict,
         socterm)

#MedDRA coding appears quite common
# at least for LILLY-H3S-MC-GGGK, where there is no lltcode, the "EVENT" variable is a meddra term
medhist <- map(medhist, function(x) {
  names(x)[names(x) %in% c("patient", "usubjid")] <- "subjid"
  names(x)[names(x) %in% c("visit", "visitnum","crfvis", "visid")] <- "visit"
  names(x)[names(x) %in% c("lltcd","mhlltcd", "lltcode")] <- "lltcd"
  names(x)[names(x) %in% c("mhterm","pterm","ptclast", "event")] <- "mhterm"
  # names(x)[names(x) %in% c("event")] <- "mhterm_qnonmed"
  names(x)[names(x) %in% c("hxendtc","mhendtc","stopcdt","reslvc")] <- "mhendtc"
  names(x)[names(x) %in% c("hxstdtc","mhstdtc","onsetcdt")] <- "mhstdtc"
  names(x)[names(x) %in% c("mhbodsys", "medbodyt","bodysyst", "socterm")] <- "mhsoc"
# names(x)[names(x) %in% c("mhoccur", "mhstat")] <- "mhstat" # have not extracted this
  names(x)[names(x) %in% c("mhcat", "evtyptxt")] <- "mhcat" # nor this, for many
  names(x)[names(x) %in% c("relonset")] <- "relonset"
  
  x$subjid <- as.character(x$subjid)
  x <- x[names(x) %in% c("subjid", "visit", "lltcd", "mhterm", "mhstdtc", "mhterm_qnormed", "mhendtc", "mhsoc", "mhcat", "relonset")]
  x
})
CreateTableNameLabels(medhist)

medhist <- map(medhist, function (x) {
  x %>% mutate_all(as.character)
}) 

medhist <- BindRowsWLabels(medhist, 1)

# Select only randomised patients
medhist <- medhist %>% # @SCREEN
  semi_join(rand)

## Select only pre-randomisation visits
pre_rand <- pre_rand %>% 
  as_tibble() %>% 
  mutate(visit_n = visit,
         visit = as.character(visit))

## All 20 trials are in the visit data tablew, but only 8 have visit numbers all of which have some pre-rand visits
medhist_visit_chk <- medhist %>%
  semi_join(pre_rand %>%  select(trial))

medhist_visit <- medhist %>%
  semi_join(pre_rand)

## 12 trials have no visit data, use date to select pre-rand
medhist_no_visit <- medhist %>% 
  anti_join(medhist_visit %>%  select(trial)) 

## 12 of these have an "end date character", the first trial "LILLY-H6D-MC-LVHB" does not.
## 5 of these have a start date
## THe relative onset dates are all for the ones with visit numbers
medhist_no_visit2 <- medhist_no_visit %>% 
  inner_join(rand %>%  select(-trt))

medhist_no_visit2 <- medhist_no_visit2 %>% 
  mutate(start_date = str_replace_all(mhstdtc, "-{2,}", ""),
         stl = str_length(start_date),
         start_date = case_when(
           stl == 0 ~ NA_character_,
           stl == 4 ~ paste0(start_date, "-01-01"),
           stl == 7 ~ paste0(start_date, "-01"),
           stl == 10 ~ start_date
         ),
         end_date = str_replace_all(mhendtc, "-{2,}", ""),
         etl = str_length(end_date),
         end_date = case_when(
           etl == 0 ~ NA_character_,
           etl == 4 ~ paste0(end_date, "-01-01"),
           etl == 7 ~ paste0(end_date, "-01"),
           etl == 10 ~ end_date
         ),
         start_date = as.Date(start_date),
         end_date = as.Date(end_date),
         rand_date = as.Date(randdt))

end_before_rand <- by(medhist_no_visit2, medhist_no_visit2$trial, function (x) {
  mean(x$rand_date >= x$end_date, na.rm = TRUE)
})
end_before_rand <- stack(end_before_rand)

start_before_rand <- by(medhist_no_visit2, medhist_no_visit2$trial, function (x) {
  mean(x$rand_date >= x$start_date, na.rm = TRUE)
})

start_before_rand <- stack(start_before_rand)

## So all have randomisation dates OR  visits
## Amalgamate into new medhist dataset
medhist_visit <- medhist %>% 
  filter(!is.na(visit))

medhist_visit2 <- medhist_visit %>% 
  semi_join(pre_rand)

medhist_visit <- medhist_visit2

rm(medhist_visit2)

medhist_no_visit3 <- medhist_no_visit2 %>% 
  filter(rand_date >= start_date | rand_date>= end_date)

setdiff(medhist_no_visit2$trial, medhist_no_visit3$trial)

medhist_bline <- bind_rows(medhist_visit,
                           medhist_no_visit3)

setdiff(medhist$trial, medhist_bline$trial)

medhist_bline <- medhist_bline %>% 
  select(trial, subjid, lltcd, mhterm, mhcat, mhsoc, mhstdtc) %>% 
  distinct()

medhist_bline %>% 
  distinct(trial, lltcd) %>% 
  group_by(trial) %>% 
  count()

## For LILLY-H3S-MC-GGGK, there is no LLT code, for this one use the MHTERM
medhist_bline <- medhist_bline %>% 
  select(trial, subjid, lltcd, term_no_lltcd = mhterm, mhsoc) %>% 
  mutate(term_no_lltcd = if_else(is.na(lltcd), term_no_lltcd, NA_character_))

medhist <- medhist_bline
rm(medhist_bline, medhist_visit, medhist_no_visit, medhist_no_visit2, 
   medhist_no_visit3, medhist_visit, medhist_visit_chk)



## Vitals table ----

# First 3 trials have wide format; remainder long

# Aim is for separate tables bp & bmi; long format

vitals1 <- all_trials$vitals[names(all_trials$vitals) %in% 
                              c("LILLY_B3D_MC_GHAC", "LILLY_B3D_US_GHBZ", "LILLY_H3S_MC_GGGK")]

vitals1$LILLY_B3D_MC_GHAC$heightcm <- vitals1$LILLY_B3D_MC_GHAC$heightmm/10 

vitals1 <- map(vitals1, function(x) {
  names(x)[names(x) %in% c("patient")] <- "subjid"
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("heightmm", "visdate","smokstat","bsa")]
  x
})

a <- CreateTableNameLabels(vitals1)
vitals1 <- BindRowsWLabels(vitals1)

bmi <- vitals1 %>% 
  select(trial, subjid, visit, bmi, weightkg, heightcm)

bp <- vitals1 %>% 
  select(trial, subjid, visit, diabp, sysbp)

# gather vitals to long
bmi <- bmi %>% 
  gather("param", "value", -trial, -subjid,-visit , na.rm = TRUE)

bp <- bp %>% 
  gather("param", "value", -trial, -subjid,-visit, na.rm = TRUE)

# Now remaining trials

vitals2 <- all_trials$vitals[!(names(all_trials$vitals) %in% 
      c("LILLY_B3D_MC_GHAC", "LILLY_B3D_US_GHBZ", "LILLY_H3S_MC_GGGK"))] 
a <- CreateTableNameLabels(vitals2)

vitals2 <- map(vitals2, function(x) {
  names(x)[names(x) %in% c("usubjid")] <- "subjid"
  names(x)[names(x) %in% c("aval","vsrn","vsstrn")] <- "value"
  names(x)[names(x) %in% c("param","vstest","vstestln")] <- "param"
  names(x)[names(x) %in% c("vsrulnm","vsstruln")] <- "unit"
  names(x)[names(x) %in% c("crfvis","visid", "visitnum")] <- "visit"
  x$subjid <- as.character(x$subjid)
  x <- x[!names(x) %in% c("vsdtc", "vsblval")]
  x
})

a <- CreateTableNameLabels(vitals2)
vitals2 <- BindRowsWLabels(vitals2)

# Reduce down to bp and bmi measurements only; make names consistent

vitals2 <- vitals2 %>% 
  mutate(param = ifelse(param %in% c("Sitting Diastolic Blood Pressure",
                                     "Supine Diastolic Blood Pressure", # Do we need to distinguish these?
                                     "BP Diastolic Result",
                                     "Diastolic Blood Pressure",
                                     "BP Diastolic Result Avg",
                                     "BP AVG Diastolic Result",
                                     "Average BP Diastolic Item Result",
                                     "BP Diastolic Result Average",
                                     "BP Diastolic Average Result",
                                     "Diastolic Blood Pressure (mmHg)"),"diabp",param)) %>% 
  mutate(param = ifelse(param %in% c("Sitting Systolic Blood Pressure",
                                     "Supine Systolic Blood Pressure",
                                     "BP Systolic Result",
                                     "Systolic Blood Pressure",
                                     "BP Systolic Result Avg",
                                     "BP AVG Systolic Result",
                                     "Average BP Systolic Result",
                                     "BP Systolic Result Average",
                                     "BP Systolic Average Result",
                                     "Systolic Blood Pressure (mmHg)"),"sysbp",param)) %>% 
  mutate(param = ifelse(param %in% c("Body Mass Index","BMI","Body Mass Index (kg/m^2)" ),"bmi",param)) %>% 
  mutate(param = ifelse(param %in% c("Height (cm)","Height" ),"heightcm",param)) %>%  
  mutate(param = ifelse(param %in% c("Weight","Weight (kg)", "Weight"),"weightkg",param)) %>%
  mutate(value = ifelse(unit %in% c("inch"),value*2.54,value)) %>%
  mutate(value = ifelse(unit %in% c("pound"),value*0.454,value)) %>%
  filter(param  %in% c("weightkg","heightcm","bmi","sysbp","diabp"))

# Need to check if any have height & weight but not bmi  
## ONly one trial has this, LVGY. However, it has height for only 94 (of 406) patients
## Also one other trial has it, but only for ONE participant

check_bmi <- bmi %>% group_by(trial, param, subjid) %>% count() %>% spread(param, n) %>%  ungroup()
check_bmi <- check_bmi %>% 
  filter(is.na(bmi), !is.na(heightcm), !is.na(weightkg) )
check_bmi %>% distinct(trial)
check_bmi %>% filter(trial =="LILLY_H6D_MC_LVHG")

#Add to previous trials
bmi <- bmi %>% bind_rows(vitals2 %>%
                           filter( param %in% c("bmi","heightcm","weightkg")) %>%
                           select( -unit))

bp <- bp %>% bind_rows(vitals2 %>%
                           filter( param %in% c("sysbp","diabp")) %>%
                           select( -unit))

# Select only randomised patients, @SCREEN
bmi <- bmi %>% 
  semi_join(rand)
bp <- bp %>% 
  semi_join(rand)

## Select ponly pre-rand visits
bmi <- bmi %>%
  rename(visit_n = visit) %>% 
  semi_join(pre_rand) %>% 
  select(-visit_n)

bp <- bp %>%
  rename(visit_n = visit) %>% 
  semi_join(pre_rand) %>% 
  select(-visit_n)


## Convert data into a single object ----
demo <- readRDS("Processed_data/lilly_demo.Rds")
labs <- readRDS("Processed_data/lilly_labs.Rds")
conmed_all <- readRDS("Processed_data/lilly_conmed.Rds")

lilly <- list(conmed = conmed_all, demo = demo, labs = labs, medhist = medhist,
            bp = bp, bmi = bmi, rand = rand)


## Where has urine data in specimen list, drop ----
#lilly$labs <- lilly$labs %>% filter(is.na(lbspec) | lbspec != "URINE") # lbspec missing 

## Save lilly data ----
saveRDS(lilly, file = "Processed_data/lilly.Rds")


## Examine lily wiht only Tadalafil and no control
read_sas("../files_LILLY-H6D-MC-LVHG/Files/Analysis Datasets/SAS_analysis/acute/")




