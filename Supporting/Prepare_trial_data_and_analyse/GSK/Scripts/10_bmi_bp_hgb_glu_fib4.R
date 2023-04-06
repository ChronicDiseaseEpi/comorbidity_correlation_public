# 10_bmi_bp_hgb_glu_fib4.r

tosave <- TRUE

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

## Read trial data ----
gsk <- readRDS("GSK_processed_data/GSK_not_conmed.Rds")


## BMI, BSA and BP ----
### 111 trials with BMI data. On reviewing histogram, one outlying value, BMI == 1.33, exclude
# MakeDensity(gsk$bmi, "bmi", binwidth = 1, tolog = FALSE)

### 106 trials with BP data. On reviewing histogram, few outlying values, 
# MakeDensity(gsk$bp, "dbp", binwidth = 5)
lowdbp <- gsk$bp %>% filter(param == "dbp", value <=20) %>% arrange(value) 

## Calcualte BSA, note cannot obtain height from BMI with these data as when no
## height is no BMI
## note all height are in cm and weight are in kg
bsa <- gsk$bmi %>% 
  spread(param, value)
bsa <- bsa %>% 
  mutate(bsa =  ( (1/3600) * ht*wt)^0.5) %>% 
  select(company, trial, id, bsa)

xmn <- tapply(bsa$bsa, bsa$trial, quantile, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)
xmn <- do.call(rbind, xmn)
## all bsa's look fine
n_distinct(bsa$trial)
if(tosave) saveRDS(bsa, "GSK_processed_data/bsa.Rds")



### Review BP data similary
# MakeDensity(gsk$bp, "sbp", binwidth = 10)

sysviadia <- gsk$bp %>% 
  spread(param, value)
# None with the dbp > sbp
sysviadia %>% semi_join(lowdbp %>% select(trial, id))%>% 
  select(-id)
# company trial             dbp   sbp
# <chr>   <chr>           <dbl> <dbl>
#  1 bi_new  BI1123_11          5.   92.
# 2 bi_new  BI1123_12         10.   18.
# 3 bi_new  BI1123_12          0.    0.
# 4 gsk_new GSKAR2103413V02   20.   70.
# 5 gsk_new GSKAR2103413V02   20.   60.
# 6 gsk_new GSKAR2103413V02    0.   50.
# 7 gsk_new GSKAR2103413V02    0.   70.
# 8 gsk_new GSKAR2103413V02    0.    0.
# 9 gsk_new GSKAR2103413V02   20.   40.
# 10 gsk_new GSKAR2103413V02    0.   60.

sysviadia %>% 
  anti_join(lowdbp %>% select(trial, id))%>% 
  filter(sbp < 50)

## None with sbp <= dbp
sysviadia %>% 
  filter(dbp >= sbp) 


## Labs more complex as have not standardised units ----
labs <- gsk$labs %>% 
  rename(param = name,
         value = result)

## Standardise unit names
labs %>% 
  distinct(unit) %>% mutate(unit_rename = "") %>% 
  write_tsv("clipboard")
labs_unit_rename <- read_tsv("Supporting/Prepare_trial_data/GSK/Created_metadata/unit_rename.tsv")

labs <- labs %>% 
  inner_join(labs_unit_rename) %>% 
  select(-unit) %>% 
  rename(unit = unit_rename)









## Convert HGB----
hgb <- labs %>%
  filter(param == "HGB")
# MakeDensity(hgb, "HGB", binwidth = 1, tolog = FALSE)

## Examine units
hgbu <- hgb %>%
  group_by(trial) %>% 
  count(unit) %>% 
  ungroup() %>% 
  mutate(unit = if_else(is.na(unit), "missing", unit)) %>% 
  spread(key = unit, value = n, fill = "")

# 16/06/22
# GSKMEA115588 has missing units
# GSKAR1103420 and GSKAR2103413_v02 are g/dl convert to g/l

## Examine missing ones, 12 of these
missing <- hgbu %>% 
  filter(missing >=1)

# GSKMEA115588 missing 578 
msnghgb <- hgb %>% 
  filter(trial %in% missing$trial)

##  # GSKMEA115588 has g/L in label
msnglbl <- msnghgb %>% distinct(trial, label)
msnglbl <- msnglbl %>% 
  filter(label %in% "Hemoglobin (g/L)")
hgb <- hgb %>% 
  mutate(unit = if_else(trial %in% msnglbl$trial, "g/L", unit))

## Examine units
hgbu <- hgb %>%
  group_by(trial) %>% 
  count(unit) %>% 
  ungroup() %>% 
  mutate(unit = if_else(is.na(unit), "missing", unit)) %>% 
  spread(key = unit, value = n, fill = "")
View(hgbu) # No missing now 
unique(hgb$unit) # "g/L"  "g/dl"

# Change value and unti to g/L where unit = g/gl
hgb2 <- hgb %>% 
  mutate(value = if_else(unit %in% "g/dl", value * 10, value),
         unit = if_else(unit %in% "g/dl", "g/L", unit))

## Examine units
hgbu2 <- hgb2 %>%
  group_by(trial) %>% 
  count(unit) %>% 
  ungroup() %>% 
  mutate(unit = if_else(is.na(unit), "missing", unit)) %>% 
  spread(key = unit, value = n, fill = "")
View(hgbu2)
unique(hgb2$unit)

## reviewed these plots and can cut this way to get on same scale
hgb3 <- hgb2 %>% 
  mutate(value = case_when(
    trial == "GSKAR2103413_v02" & value >25 ~ NA_real_,
    value <= 30 ~ value * 10,
    value >30 ~ value))

hgb3 <- hgb3 %>% filter(!is.na(value))

## Examine units
hgbu3 <- hgb3 %>%
  group_by(trial) %>% 
  count(unit) %>% 
  ungroup() %>% 
  mutate(unit = if_else(is.na(unit), "missing", unit)) %>% 
  spread(key = unit, value = n, fill = "")
View(hgbu3)
unique(hgb3$unit)

hgb <- hgb3

rm(hgb2, hgb3, hgbu, hgbu2, hgbu3, msnglbl)
if(tosave) saveRDS(hgb, "GSK_processed_data/hgb.Rds")






## COnvert gluocse ----
glu <- labs %>% 
  filter(param == "GLU")

unique(glu$unit) 
sum(is.na(glu$unit))

chk_glu <- glu %>% 
  group_by(ul,ll, value, unit) %>%
  summarise(n())

# Missing values should have same mmol, can tell as similar value, ll and ul to others

glu <- glu %>%
  mutate(unit = if_else(is.na(unit), "mmol/L", unit))
unique(glu$unit) # All same units "mmol/L"

# Save 
if(tosave) saveRDS(glu, "GSK_processed_data/glu.Rds")
rm(chk_glu)



## ALT and AST
alt <- labs %>% 
  filter(param == "ALT")

alt %>% group_by(unit) %>% 
  summarise_at(vars(ll, ul), function(x)  paste(range(x, na.rm = T), collapse = ", "))
alt_smry <- alt %>% 
  group_by(trial, unit) %>% 
  summarise(n = length(id)) %>% 
  ungroup() %>% 
  spread(unit, n, fill = "")


## All means and sds look plausible to come from the same range
alt_m <- alt %>% 
  group_by(trial, unit) %>% 
  summarise(value = mean(value, na.rm = T) %>% round()) %>% 
  ungroup() %>% 
  spread(unit, value, fill = "")
alt_s <- alt %>% 
  group_by(trial, unit) %>% 
  summarise(value = sd(value, na.rm = T) %>% round()) %>% 
  ungroup() %>% 
  spread(unit, value, fill = "")


## therefore for ALT and AST take first occurence
ast <- labs %>% 
  filter(param == "AST")

ast %>% group_by(unit) %>% 
  summarise_at(vars(ll, ul), function(x)  paste(range(x, na.rm = T), collapse = ", "))

ast_smry <- ast %>% 
  group_by(trial, unit) %>% 
  summarise(n = length(id)) %>% 
  ungroup() %>% 
  spread(unit, n, fill = "")

## All means and sds look plausible to come from the same range
ast_m <- ast %>% 
  group_by(trial, unit) %>% 
  summarise(value = mean(value, na.rm = T) %>% round()) %>% 
  ungroup() %>% 
  spread(unit, value, fill = "")
ast_s <- ast %>% 
  group_by(trial, unit) %>% 
  summarise(value = sd(value, na.rm = T) %>% round()) %>% 
  ungroup() %>% 
  spread(unit, value, fill = "")

## AST and ALT are similar.
liver <- labs %>% 
  filter(param %in% c("AST", "ALT"))

# Change NA and U/L to IU/L
liver <- liver %>% 
  mutate(unit = if_else(is.na(unit), "IU/L", unit),
         unit = if_else(unit == "U/L", "IU/L", unit))

  

## Platelets ----
plat <- labs %>% 
  filter(param == "PLAT")

plat_smry <- plat %>% 
  group_by(trial, unit) %>%
  summarise(n = length(id)) %>%
  ungroup() %>%
  spread(unit, n, fill = "") # 3 different units

plat_m <-  plat %>% 
  group_by(trial, unit) %>% 
  summarise(value = mean(value, na.rm = T) %>% round()) %>% 
  ungroup() %>% 
  spread(unit, value, fill = "")

plat_s <-  plat %>% 
  group_by(trial, unit) %>% 
  summarise(value = sd(value, na.rm = T) %>% round()) %>% 
  ungroup() %>% 
  spread(unit, value, fill = "")

# Examine 50 from each unit 
pl <- plat %>% 
  group_by(unit) %>%
  slice_head(n = 50)

sum(is.na(plat$unit)) 
sum(plat$label %in% "Platelets (GI/L)")
pl2 <- plat %>% filter(is.na(unit)) # All NAs should be "GI/L" as label gives units 

# Make units GI/L for rows where label tells units  
plat2 <- plat %>% 
  mutate(unit = if_else(label %in% "Platelets (GI/L)", "GI/L", unit))
unique(plat2$unit) # All NAs removed

# Exmine 50 of each remaining unit 
pl2 <- plat2 %>% 
  group_by(unit) %>%
  slice_head(n = 50)
# note 1e9 and g/L has a one to one conversion

# Set  "10E9/L" to GI/L
plat2 <- plat2 %>%
  mutate(unit = if_else(unit %in% "10E9/L", "GI/L", unit))
unique(plat2$unit) # ALL the same 

plat <- plat2
rm(plat2, plat_m, plat_s, plat_smry, pl, pl2)



## Calculate fib4
Fib4 <- function(age, ast, alt, plt) (age*ast) / (plt*alt^0.5)

demo <- gsk$demo
demo <- demo %>% 
  group_by(trial) %>% 
  mutate(age = as.double(age),
         age = if_else(age %>% is.na(), mean(age, na.rm = TRUE), age)) %>% 
  ungroup() %>% 
  select(company, trial, id, age)

## 87 of 111 trials with platelet measures
demo_plat <- demo %>% 
  inner_join(plat %>% 
               select(trial, id, plat = value) %>% 
               group_by(trial, id) %>% 
               summarise(plat = mean(plat)) %>%  
               ungroup()) 
liver2 <- liver %>% 
  group_by(trial, id, param) %>% 
  summarise(value = mean(value, na.rm = TRUE)) %>% 
  ungroup() %>% 
  spread(param, value)
demo_plat_liv <- demo_plat %>%
  inner_join(liver2)


names(demo_plat_liv) <- str_to_lower(names(demo_plat_liv))
demo_plat_liv <- demo_plat_liv %>% 
  mutate(fib4 = Fib4(age, ast, alt, plt = plat)) %>% 
  select(company, trial, id, fib4)

## Four strange values, all ahve very low platelet counts and all are in BI1160_24, drop these
## Also one in NVT_SA_ZOL446H2301 is dubious with a platelet score of 9, drop this too
demo_plat_liv %>% 
  filter(fib4 >50) %>% 
  as.data.frame()

demo_plat_liv <- demo_plat_liv %>% 
  filter(fib4<=50) %>% 
  select(company, trial, id, fib4)

# IMPORTANT: a few v.high fib4 values, either mistakes or result of inflammatory condition 
range(demo_plat_liv$fib4)
hist(demo_plat_liv$fib4)

## SAVE
if(tosave) saveRDS(demo_plat_liv, "GSK_processed_data/fib4.Rds")

