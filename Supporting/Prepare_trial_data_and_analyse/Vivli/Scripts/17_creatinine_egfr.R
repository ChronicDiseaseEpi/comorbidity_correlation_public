# 17_creatinine_egfr
## Search through documentation for insights on types of creatinine measures
## Then apply eGFR calculations
## 



source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

SlowRunOnce <- function(){
  
  proto_dict1 <- readRDS("../Documentation_searcher/Scratch_data/all_readable_pdfs1.Rds")
  proto_dict2 <- readRDS("../Documentation_searcher/Scratch_data/all_readable_pdfs2.Rds") %>% 
    mutate(pos = seq_along(document))
  proto_dict  <- readRDS("../Documentation_searcher/Created_metadata/all_readable_pdfs.Rds")
  ## FOllowing 3 all negative for searches
  str_detect(proto_dict$text, "isotope dilution") %>% any()
  str_detect(proto_dict$text, "IDMS") %>% any()
  str_detect(proto_dict$text, "NIDDK") %>%  any()
  
  ## Following search yielded 19 hits on reviewing which these were useful
  res <- str_locate_df(proto_dict1, "GFR|\\begfr|\\b(E|e)GFR|\\b(E|e)stimated glomerular")
  res2 <- CombineTextFinds(res, proto_dict1, char_before = 300)
  egfr_search <- res2 %>%
    filter(pos %in% c(3803, 9875, 10292, 103625)) %>% 
    distinct(document_type, document, res2)# %>%  
    # write_csv(egfr_search, "Created_metadata/egfr_search.csv")
    
  res <- str_locate_df(proto_dict2, "GFR|\\begfr|\\b(E|e)GFR|\\b(E|e)stimated glomerular")
  res2 <- CombineTextFinds(res, proto_dict2, char_before = 300)
  egfr_search2 <- res2 %>%
    filter(pos %in% c(6318, 12955)) %>% 
    distinct(document_type, document, res2)
  
  ## Both new trials used MDRD equation (and MDRD Japanese modification for Japan)
  write_csv(bind_rows(egfr_search, egfr_search2), "Created_metadata/egfr_search.csv")
  
  ## Following search yielded 129 hits
  res <- str_locate_df(proto_dict1, "\\bMDRD\\b|Modification of Diet in Renal Disease")
  res2 <- CombineTextFinds(res, proto_dict1, char_before = 200)
  ## Exclude already identified from egfr search
  res2 <- res2 %>%
    filter(!str_detect(res2, "\\begfr|\\bEGFR|\\b(E|e)stimated glomerular"))
  mdrd_search <- res2 %>%
    filter(pos %in% 
             c(3765,  
               4022, 4105,
               4371, 
               4504, 
               9903, 
               10328,
               82086, 
               96359,
               103610)) %>% 
    distinct(document_type, document, res2) # %>%  
  # write_csv(mdrd_search, "Created_metadata/mdrd_search.csv")
  ## repeat for new trials
  res <- str_locate_df(proto_dict2, "\\bMDRD\\b|Modification of Diet in Renal Disease")
  res2 <- CombineTextFinds(res, proto_dict2, char_before = 200)
  ## Exclude already identified from egfr search
  res2 <- res2 %>%
    filter(!str_detect(res2, "\\begfr|\\bEGFR|\\b(E|e)stimated glomerular"))
  res2 %>% distinct(document)
  # already captured mdrd knowlege for these two trials
  
  
  ## Look for CKD-EPI equation, 8 hits
  res <- str_locate_df(proto_dict1, "\\bCKD\\-EPI\\b|Chronic Kidney Disease Epidemiology")
  ckd_epi <- CombineTextFinds(res, proto_dict1, char_before = 200)
  ## One useful hit (all 8 are for same trial)
  ckd_epi <- ckd_epi %>% 
    filter(pos == 8852) %>% 
    distinct(document_type, document, res2)
  # write_csv(ckd_epi, "Created_metadata/ckd_epi_search.csv")
  ## No hits for new data
  res <- str_locate_df(proto_dict2, "\\bCKD\\-EPI\\b|Chronic Kidney Disease Epidemiology")
  
  ## Look for any creatinine term where standardi(z|s)ation or dilu is within 1000 characters
  ## (and on the same page), there are none
  res <- str_locate_df(proto_dict1, "\\b(C|c)reatinine")
  creat <- CombineTextFinds(res, proto_dict1, char_before = 1000)
  creat_stnd <- creat %>% 
    filter(str_detect(res2, "(S|s)tandardi(s|z)ation"))
  creat_dilut <- creat %>% 
    filter(str_detect(res2 %>%  str_to_lower(), "dilution"))
}

## re-examine selected files
list.files("E:/C_analysis_code_meta/Count_comorbidities/Created_metadata/", patt = "search") %>%  dput()

slctd <- map(c("ckd_epi_search.csv", "egfr_search.csv", "mdrd_search.csv"), ~
             read_csv(paste0("E:/C_analysis_code_meta/Count_comorbidities/Created_metadata/", .x)))

names(slctd) <- c("ckd_epi", "egfr", "mdrd")

slctd <- bind_rows(slctd, .id = "searched_on")
slctd <- slctd %>% 
  arrange(document)

## for the documents where the MDRD equation is given c(1, 3, 4, 7, 15, 16, 17), it is the IDMS standardised one
## which had the coefficeint of 175 (reviewed these manually)
## for the last of the trials the Study sites in Japan used the Japanese Nephrology Society MDRD equation
slctd %>% 
  slice( c(1, 3, 4, 7, 15, 16, 17)) %>% 
  select(searched_on, document)

unslctt <- slctd[-c(1, 3, 4, 7, 15, 16, 17),]

mdrd175 <- slctd %>% 
  slice( c(1, 3, 4, 7, 15, 16, 17)) %>% 
  filter(str_detect(res2, "175")) 

# One instance of 186 and it does not pertain to the MDRD equation
mdrd186 <- slctd %>% 
  slice( c(1, 3, 4, 7, 15, 16, 17)) %>% 
  filter(str_detect(res2, "186")) 

# searched_on document                                                                                                 
# 1 egfr        files_BI_1245_36/Files/Documents/report/1245-0036--16101--protocol-or-amendment_Redacted.pdf             
# 2 mdrd        files_BI_1245_48/Files/Documents/report/1245-0048--16101--protocol-or-amendment_Redacted.pdf             
# 3 mdrd        files_BI_1276_1/Files/Documents/report/1276-0001--16101--protocol-or-amendment_Redacted.pdf              
# 4 egfr        files_GSK-FFA112059/Files/Documents/Clinical Study Report/gsk-112059-clinical-study-report-redact.pdf    
# 5 mdrd        files_SAN-EFC11628/Files/Documents/redact-efc11628-combined CSR protocol and amendments SAP CRF-final.pdf
# 6 egfr        files_TKA-SYR322_305/Files/Documents/SYR-322_305-Clinical Study Report-Redacted-2016-12-05.pdf       
# 7 egfr        files_TKA-SYR322_402/Files/Documents/SYR-322_402_Redacted_2016-03-04.pdf       

## I will assume that where MDRD is stated, regardless of the equation, or CKD-EPI is stated, that an IDMS method has been used
## that means all of the "slctd" trials I will use the MDRD equation
## Where eGFR is reported, I will assume that the correct method has been used.

## For trials which have already calculated eGFR I will assume that their calculation is the appropriate one, based on knowledge
## of their laboratories
# rm(proto_dict1, proto_dict2, proto_dict, ckd_epi, creat, creat_dilut, creat_stnd, mdrd_search, egfr_search, res, res2)
cmpnies <- readRDS("../Extract_Data/Processed_data/all_sponsors_not_conmed.Rds")
labs <- cmpnies$labs
rm(cmpnies)

## 27 trials with egfr
egfr <- labs %>%
  filter(name == "EGFR", !company == "lilly") %>% 
  distinct(trial)

# 65 without egfr, but with creatinine
creat_no_egfr_trial <- labs %>% 
  filter(name == "CREAT") %>% 
  anti_join(egfr) %>% 
  distinct(trial) 

#-----------------------------------------------------------------------------
## get trial name for slctd trials (12) which have MDRD equation
#trial_doc <- readRDS("../Documentation_searcher/Created_metadata/all_readable_pdfs.Rds") %>% 
 # distinct(trial, document)

## One trial excluded SANOFI EFC11628 which is a rejected trial
## 6 trials gave equation, 5 didn't give any equation
## Assume all of these 11 trials used the new MDRD equation
#mdrd_calculate <- slctd %>% 
 # distinct(document, res2) %>% 
#  mutate(got_eqn = document %in% mdrd175$document) %>% 
#  inner_join(trial_doc) 

#mdrd_calculate %>% 
#  group_by(trial) %>% 
 # summarise(got_eqn = mean(got_eqn))
# ------------------------------------------------------------------------------

mdrd_calculate <- read_csv("trial, got_eqn
                           BI1245_36, 1
                           BI1245_48, 1
                           BI1276_1, 1
                           BI248_524, 0
                           BI248_525, 0
                           LILLY-H9X-MC-GBDG, 0
                           LILLY-I1F-MC-RHAZ, 0
                           LILLY-I1F-MC-RHBC, 0
                           TKASYR322-305, 1
                           TKASYR322_402, 1")

creat_no_egfr_trial <- creat_no_egfr_trial %>% 
  mutate(equation_choose = if_else(trial %in% mdrd_calculate$trial, "MDRD", "Older"))

creat_no_egfr_trial <- creat_no_egfr_trial %>% 
  mutate(mdrd_creat = if_else(equation_choose == "Older", 186, 175))


## two new trials from update pdf search had eGFR anyway!!!
mdrd_calculate %>% 
  anti_join(creat_no_egfr_trial)

## Now ready to perform eGFR calculations ---
## first limit demo to where need to know race for calculation
cmpnies <- readRDS("../Extract_Data/Processed_data/all_sponsors_not_conmed.Rds")
demo <- cmpnies$demo
demo <- demo %>% 
  inner_join(creat_no_egfr_trial)

## Examine numeric ones
racenum <- demo %>% group_by(trial) %>% mutate(racenum = any(str_detect(race, "[0-9]"))) %>% 
  ungroup() %>% 
  filter(racenum) %>% 
  distinct(trial, race) %>% 
  mutate(value = "y") %>% 
  spread(race, value, fill = "")

## examine CRFs for numeric race values
race_lkp <- read_csv("E:/C_analysis_code_meta/Count_comorbidities/Created_metadata/for_egfr_race.csv")

demo <- demo %>% 
  left_join(race_lkp %>% mutate(value = as.character(value)) %>%
              rename(race = value, race_rename = label))
demo <- demo %>% 
  mutate(race = if_else(is.na(race_rename), race, race_rename)) %>% 
  select(-race_rename)

race <- demo$race %>% unique()
race[str_detect(race %>% str_to_lower(), "african|black")] %>% sort() %>%  dput()

## reviewed these manually and read in to link across
aa <- read_csv(file = "E:/C_analysis_code_meta/Count_comorbidities/Created_metadata/terms_used_aa.csv")

demo <- demo %>% 
  mutate(aa = race %in% aa$terms)
rm(aa)
demo <- demo %>% 
  mutate(female = sex == "female")


## MDRD equation, non-creatinine component (same for new and old)
MDRD_age_sex_aa <- function(age, female, aa) {  age^-0.203 *   0.742^female * 1.212^aa} 
mdrd_demo <- demo %>% 
  group_by(trial) %>% 
  mutate(age_calc = if_else(is.na(age), mean(age %>% as.double(), na.rm = T), age %>% as.double())) %>% 
  ungroup() %>% 
  mutate(mdrd_demo = MDRD_age_sex_aa(age_calc, female, aa)) %>% 
  select(trial, id, mdrd_demo) %>% 
  distinct(trial, id, .keep_all = T)

## Convert creatinine. ----
## Standardise creatinine where there is no EGFR measure
# Convert creatinine mg/dL into mmol/L by multiplying by 88.42
creat <- cmpnies$labs %>% 
  filter(name == "CREAT")
egfr <- cmpnies$labs %>% 
  filter(name == "EGFR")
## Drop ones with egfr from creatinine list except Lili trials, where eGFR plots
creat_alone <- creat %>% 
  filter(!(trial %in% egfr$trial) | company == "lilly")

## Needed to look-up units for the following trials
## Note that none of the trials with EGFR were ambiguous for creatinine

## Two were apparent from the label
# company trial      
# <chr>   <chr>     
# 1 bi_new  BI1123_28  ## Bimodal, Changed so took labstd, now all in mmol/L (checked against unstandardised)
# 2 bi_new  BI244_2484 ## unimodal, mean 73, most likley mmol/L
# 3 bi_new  BI502_254  ## unimodal, mean 69, most likley mmol/L

# 5 bi_new  BI502_316  ## unimodal, after multiply by 88.42 is 77, most likely mg/dL
# 6 bi_new  BI502_317  ## unimodal, after multiply by 88.42 is 82, most likely mg/dL
# 7 bi_new  BI502_327  ## unimodal, after multiply by 88.42 is 78, most likely mg/dL
# 8 bi_new  BI502_392  ## unimodal, after multiply by 88.42 is 90, most likely mg/dL 
# 4 bi_new  BI502_256  ## unimodal, mean 0.87, most likley mg/dL
# 10 rch     RCHMRA012JP ## unimodal, all values less than 1, assume mg/dL
# 9 bi_new  BI502_397  ## Bimodal, no data or metadata, no values lie between 10 and 20, despite there being 860 participants,
# use this property to conver those below 10 to mmol/l
mmoll <-
  c("BI1245_36", 
    "GSKMEA115588",
    "BI1123_28", "BI244_2484", "BI502_254")
mgdl <- c("BI502_256", "BI502_316", "BI502_317", "BI502_327", "BI502_392", "RCHMRA012JP")
mmol_mgdl <- "BI502_397"

## Standardise unit names first (as per labs dataframe)
labs_unit_rename <- read_tsv("E:/C_analysis_code_meta/Count_comorbidities/Created_metadata/unit_rename.tsv")
creat_alone <- creat_alone %>% 
  inner_join(labs_unit_rename) %>% 
  select(-unit) %>% 
  rename(unit = unit_rename)

creat <- creat_alone %>% 
  mutate(unit = case_when(trial %in% mmoll ~ "umol/L",
                          trial %in% mgdl ~ "mg/dL",
                          trial %in% mmol_mgdl ~ if_else(result <10, "mg/dL", "umol/L"),
                          TRUE ~ unit))

## Note highest recorded creatinine was 50 mg/dL in somone survived, so only convert if < 60
## assuming that the creatinine has been misclassified
## If creatinine is <15 assume that it is in mg/dL regardless of unit, provided that trial includes
## patients who have had it measured in mg/dL
## there are 196 mmol/L measures in LILLY-H3S-MC-GGGK, just drop
# creat %>% filter(trial == "LILLY-H3S-MC-GGGK") %>% group_by(trial, unit) %>% count() %>% spread(unit, n, fill = "")

xmn_unit <- creat %>% 
  group_by(trial, unit) %>% 
  count() %>% 
  ungroup() %>% 
  spread(unit, n, fill = 0L)

both_units <- xmn_unit %>% 
  filter(`mg/dL` !=0 & `umol/L` !=0) %>% 
  pull(trial)

# Drop mg/dL and mmol/L where have "umol/L" in same trial
creat <- creat %>% 
  filter( (!trial %in% both_units) | unit == "umol/L")

creat <- creat %>% 
  group_by(trial) %>% 
  mutate(anymg = any(unit == "mg/dL")) %>% 
  ungroup() %>% 
  mutate(result = case_when(
    unit == "mg/dL" & result < 60 ~ result * 88.42,
    result <10 & anymg ~ result *88.42,
    TRUE ~ result)) %>% 
  select(-anymg)

# trial %in% c("LILLY-I1F-MC-RHAZ", "LILLY-I1F-MC-RHBA", "LILLY-I1F-MC-RHBC") &
#   ll == 0 ~ value *88.42,

# MakeDensity(creat %>% rename(param = name, value = result), "CREAT", binwidth = 10, tolog = FALSE)
## All look good except BI1123_28 which has some strange very high outliers. Just delete any very high ones prior to
## modelling, after conversion to eGFR

# MakeDensity(creat %>% 
#               filter(trial == "BI1123_28", result < 1000) %>% 
#               rename(param = name, value = result), 
#             "CREAT", binwidth = 10, tolog = FALSE)


## MDRD equation, creatinine component
creat_no_egfr_trial <- creat_no_egfr_trial %>% 
  mutate(mdrd_creat = if_else(equation_choose == "Older", 186, 175))

## Calculate EGFR
creat <- creat %>% 
  inner_join(creat_no_egfr_trial) %>% 
  inner_join(mdrd_demo %>%  distinct()) %>% 
  mutate(result = mdrd_creat * (result/88.42)^-1.154 * mdrd_demo,
         name = "EGFR")
  
MakeDensity(creat %>% filter(company == "lilly") %>% rename(value = result, param = name), my_param = "EGFR", binwidth = 10)

## All egfrs look fine
egfr <- bind_rows(local  = creat %>% 
                    select(company, trial, id, name, result),
                  sponsor = egfr %>% 
                    select(company, trial, id, name, result) %>%
                    filter(!company == "lilly"),
                  .id = "calculated_by")

## Plot efr_from_sponsor
MakeDensity(egfr %>%
              rename(value = result, param = name), my_param = "EGFR", binwidth = 10)


saveRDS(egfr, "../Extract_Data/Processed_data/egfr.Rds")

