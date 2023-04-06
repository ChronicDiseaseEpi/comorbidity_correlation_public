# 16_bmi_bp_hgb_glu_fib4.r

tosave <- TRUE

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

## Read trial data ----

cmpnies <- readRDS("../Extract_Data/Processed_data/all_sponsors_not_conmed.Rds")


## BMI, BSA and BP ----
### 111 trials with BMI data. On reviewing histogram, one outlying value, BMI == 1.33, exclude
#MakeDensity(cmpnies$bmi, "bmi", binwidth = 1, tolog = FALSE)

### 106 trials with BP data. On reviewing histogram, few outlying values, 
# MakeDensity(cmpnies$bp, "dbp", binwidth = 5)
lowdbp <- cmpnies$bp %>% filter(param == "dbp", value <=20) %>% arrange(value) 

## Calcualte BSA, note cannot obtain height from BMI with these data as when no
## height is no BMI
## note all height are in cm and weight are in kg
bsa <- cmpnies$bmi %>% 
  spread(param, value)

bsa <- bsa %>% 
  mutate(bsa =  ( (1/3600) * ht*wt)^0.5) %>% 
  select(company, trial, id, bsa)

xmn <- tapply(bsa$bsa, bsa$trial, quantile, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = T)

xmn <- do.call(rbind, xmn)

## all bsa's look fine
if(tosave) saveRDS(bsa, "../Extract_Data/Processed_data/bsa.Rds")

### Review BP data similary
MakeDensity(cmpnies$bp, "sbp", binwidth = 10) ## is "sbp" MakeDensity(cmpnies$bp, "sbp", binwidth = 10) a typo? should it be bp?

sysviadia <- cmpnies$bp %>% 
  spread(param, value)
# None with the dbp > sbp
sysviadia %>% semi_join(lowdbp %>% select(trial, id))%>% 
  select(-id)
# company trial             dbp   sbp
# <chr>   <chr>           <dbl> <dbl>
#  1 bi_new  BI1123_11          5.   92.

sysviadia %>% 
  anti_join(lowdbp %>% select(trial, id))%>% 
  filter(sbp < 50)

## None with sbp <= dbp
sysviadia %>% 
  filter(dbp >= sbp) 


## Labs more complex as have not standardised units ----
labs <- cmpnies$labs %>% 
  rename(param = name,
         value = result)

## Standardise unit names
labs %>% 
  distinct(unit) %>% 
  mutate(unit_rename = "") %>% 
  write_tsv("clipboard")

labs_unit_rename <- read_tsv("../Count_comorbidities/Created_metadata/unit_rename.tsv")

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

## Idnetify where has g/L or g/dL wiht mmol/L, 15 trials
## and drop these rows from the data
gdlboth <- hgbu %>% 
  filter(`mmol/L` != "", `g/dl` == `mmol/L` | `g/L` == `mmol/L`)
hgb <- hgb %>% 
  filter(!(trial %in% gdlboth$trial & unit == "mmol/L" ))

## Examine units again
hgbu <- hgb %>%
  group_by(trial) %>% 
  count(unit) %>% 
  ungroup() %>% 
  mutate(unit = if_else(is.na(unit), "missing", unit)) %>% 
  spread(key = unit, value = n, fill = "")

## Examine missing ones, 12 of these
missing <- hgbu %>% 
  filter(missing >=1)
msnghgb <- hgb %>% 
  filter(trial %in% missing$trial)

## Two have g/L in label
msnglbl <- msnghgb %>% distinct(trial, label)
msnglbl <- msnglbl %>% 
  filter(label %in% c("Baseline haemoglobin [g/L]", "Hemoglobin (g/L)"))
hgb <- hgb %>% 
  mutate(unit = if_else(trial %in% msnglbl$trial, "g/L", unit))
msnghgb <- msnghgb %>% 
  anti_join(msnglbl)
missing <- missing %>%
  anti_join(msnglbl)

# BI1123_28 , from protocol just categories, will need to impute these, for now, drop
BI1123_28_lkp <- c(not_done = 0L,
  below_normal_range = 1L,
  within_normal_range = 2L,
  above_normal_range = 3L)
msnghgb <- msnghgb %>% 
  filter(! trial  == "BI1123_28")
missing <- missing %>% 
  filter(! trial  == "BI1123_28")

## Examine plots and ranges
msnghgb %>% 
  group_by(trial) %>% 
  summarise_at(vars(value), list(mu = mean, s = sd, low = min, hi = max))
# MakeDensity(msnghgb, "HGB", binwidth = 1, tolog = FALSE)
## All except BI502_397 are unimodal (note had very large SD and very alrge range)
msnghgb <- msnghgb %>% 
  mutate(unit = case_when(trial == "BI244_2484" ~ "g/L",
                          trial == "BI502_397" ~ if_else(value <50, "g/dl", "g/L"),
                          trial %in% c("BI502_254", "BI502_256", "BI502_316", "BI502_317", "BI502_327", 
                                       "BI502_392", "RCHMRA012JP") ~ "g/dl"))
msnghgb <- msnghgb %>% 
  distinct(trial, id, unit) 

## Join back on to HGB
hgb2 <- hgb %>%
  left_join(msnghgb %>% rename(unit_rename = unit)) %>% 
  mutate(unit_rename = if_else(is.na(unit_rename) | unit_rename == "", unit, unit_rename))

## Examine units again
hgbu <- hgb2 %>%
  group_by(trial) %>% 
  count(unit_rename) %>% 
  ungroup() %>% 
  spread(key = unit_rename, value = n, fill = "")


## Convert all to g/L
## Convert mmol/L (also mmol/L-fe into g/L as follows) x 16.114, checked this against
## trial LILLY-B3D-US-GHBZ prior to harmonising mmol/L and mml/l-Fe unit names and is correct
hgb2 <- hgb2 %>% 
  mutate(value_std = case_when(
    unit_rename == "g/dl" ~ value *10,
    unit_rename == "g/L" ~ value,
    unit_rename == "mmol/L" ~ value * 16.114,
    TRUE ~ value
  ))

trial_small <- hgb2 %>% group_by(trial) %>% 
  summarise_at(vars(value), list(min = min, max = max), na.rm = TRUE) %>% 
  ungroup() %>% 
  filter(min <50) %>% 
  pull(trial) %>% 
  unique()


# MakeDensity(hgb2 %>%
#               # filter(trial %in% c("BI502_254", "BI502_256", "BI502_316", "BI502_317", "BI502_327", 
#               #                     "BI502_392", "RCHMRA012JP")) %>% 
#               select(-value) %>%  
#               rename(value = value_std), 
#             "HGB", binwidth = 1, tolog = FALSE)
#MakeDensity(hgb2, "HGB", binwidth = 1, tolog = FALSE)

## reviewed these plots and can cut this way to get on same scale
hgb3 <- hgb2 %>% 
  mutate(value = case_when(
    trial == "BI1123_28" ~ NA_real_,
    trial %in% c("BI502_397", "LILLY-B3D-MC-GHAC") & value < 30 ~ value *10,
    trial %in% c("BI502_397", "LILLY-B3D-MC-GHAC") & value >=30 ~ value,
    trial == "TKAMLN0002_C13006" & value >200 ~ NA_real_,
    trial == "GSKAR2103413V02" & value >25 ~ NA_real_,
    value <= 30 ~ value * 10,
    value >30 ~ value))
## All haemoblobin look sounds
#MakeDensity(hgb3, "HGB", binwidth = 1, tolog = FALSE)

hgb <- hgb3 %>% 
  filter(!is.na(value))

hgbu <- hgb %>%
  group_by(trial) %>% 
  count(unit_rename) %>% 
  ungroup() %>% 
  spread(key = unit_rename, value = n, fill = "")

rm(hgb2, hgbu, msnghgb, missing, msnglbl, gdlboth)
if(tosave) saveRDS(hgb, "../Extract_Data/Processed_data/hgb.Rds")
## Some outlying values but looks correct






## COnvert gluocse ----
glu <- labs %>% 
  filter(param == "GLU")
# MakeDensity(glu, "GLU", binwidth = 1, tolog = FALSE)


# BI1123_28 has categories,
## Data was provided in categories, take midpoints
glu_lkp <- read_csv("value, lbl, my_impute
0, not done, 
1, <81 mg/dL or 4.5 mmol/L, 4
2, 81 to 99 mg/dL or 4.5-5.5 mmol/L, 5
3, 100 to 125 mg/dL or 5.6-6.9 mmol/L, 7.55
4, 126 to 149 mg/dL or 7.0-8.2 mmol/L, 7.6
5, 150 to 199 mg/dL or 8.3-11.0 mmol/L, 9.65
6, >=200 mg/dL or - 11.1 mmol/L, 12")
glu_lkp <- glu_lkp %>% 
  select(value, my_impute)
glu_lkp_cw <- glu_lkp %>% 
  mutate(cw = paste0("trial == 'BI1123_28' & value == ", value, " ~ ", my_impute, ", "))
# cat(glu_lkp_cw$cw %>%  paste(collapse = "\n"))
glu <- glu %>% 
  mutate(
    value = case_when(
    trial == 'BI1123_28' & value == 0 ~ NA_real_, 
    trial == 'BI1123_28' & value == 1 ~ 4, 
    trial == 'BI1123_28' & value == 2 ~ 5, 
    trial == 'BI1123_28' & value == 3 ~ 7.55, 
    trial == 'BI1123_28' & value == 4 ~ 7.6, 
    trial == 'BI1123_28' & value == 5 ~ 9.65, 
    trial == 'BI1123_28' & value == 6 ~ 12,
    TRUE ~ value),
    unit = if_else(trial == "BI1123_28", "mmol/L", unit))

## Lots of bimodal ones
# Majority of these have mmol/L as well as mg/dL.
## Search for ones which are bimodal, all the lilly ones have both. Drop mg/dL where the number of measures is the same
glu_smry <- glu %>% 
  filter(str_detect(trial, "^LILLY|502_397")) %>% 
  group_by(trial, unit) %>% 
  summarise(n = length(id)) %>% 
  ungroup() %>% 
  spread(unit, n, fill = "")
lilly_glu <- glu_smry %>% 
  filter(str_detect(trial, "^LILLY"), `mg/dL` == `mmol/L`) %>% 
  distinct(trial) %>% 
  pull(trial)

glu <- glu %>% 
  filter(!(trial %in% lilly_glu & unit == "mg/dL"))
## No patients lost from this filter, but need to deal with "LILLY-B3D-MC-GHAC" and "LILLY-H3S-MC-GGGK"
## Have the units so just convert
glu %>% 
  filter(trial %in% c("LILLY-B3D-MC-GHAC", "LILLY-H3S-MC-GGGK")) %>% 
  group_by(trial, param, original_name, fast, label, unit) %>% 
  count()

## Examine  "BI502_397", complete separate between 17.15 and 43.5
BI502_397 <- glu %>% 
  filter(trial %in% c("BI502_397")) %>% 
  pull(value)
stem(BI502_397)
max(BI502_397[BI502_397 <= 40] )
min(BI502_397[BI502_397 > 17.15] )

glu <- glu %>% 
  mutate(unit = case_when(
    trial == "BI502_397" & value < 18 ~ "mmol/L",
    trial == "BI502_397" & value >40 ~ "mg/dL",
    TRUE ~ unit))

## Convert all from mg/dL into mmol/L
glu <- glu %>% 
  mutate(value = if_else(unit == "mg/dL", value/18, value))
# MakeDensity(glu2, "GLU", binwidth = 1, tolog = FALSE)
# All glucose results look good
if(tosave) saveRDS(glu, "../Extract_Data/Processed_data/glu.Rds")



## ALT and AST
alt <- labs %>% 
  filter(param == "ALT")
# MakeDensity(alt, "ALT", binwidth = 1, tolog = FALSE)
# all are unimodal
alt %>% group_by(unit) %>% 
  summarise_at(vars(ll, ul), function(x)  paste(range(x, na.rm = T), collapse = ", "))
alt_smry <- alt %>% 
  group_by(trial, unit) %>% 
  summarise(n = length(id)) %>% 
  ungroup() %>% 
  spread(unit, n, fill = "")

## IN Lilly trials IU/L is a synonym for U/L
alt_cmpr <- alt %>%
  filter(trial %>% str_detect("^LILLY")) %>%
  select(trial, unit, value) %>% 
  distinct(trial, unit, .keep_all = T) %>% 
  spread(unit, value, fill = "")
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
# MakeDensity(ast, "AST", binwidth = 1, tolog = FALSE)
# all are unimodal
ast %>% group_by(unit) %>% 
  summarise_at(vars(ll, ul), function(x)  paste(range(x, na.rm = T), collapse = ", "))

ast_smry <- ast %>% 
  group_by(trial, unit) %>% 
  summarise(n = length(id)) %>% 
  ungroup() %>% 
  spread(unit, n, fill = "")

lilly <- ast_smry %>% 
  filter(`IU/L`  == `U/L`, `IU/L` != "") %>% 
  select(trial) %>% 
  pull()

## IN Lilly trials IU/L is a synonym for U/L
ast_cmpr <- ast %>%
  filter(trial %>% str_detect("^LILLY")) %>%
  select(trial, unit, value) %>% 
  distinct(trial, unit, .keep_all = T) %>% 
  spread(unit, value, fill = "")

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
## Drop one set of units for Lilly and otherwise no changes needed
liver <- labs %>% 
  filter(param %in% c("AST", "ALT"),
         !((trial %in% lilly) & unit == "U/L" ))
  


## Platelets ----
plat <- labs %>% 
  filter(param == "PLAT")
# MakeDensity(plat, "PLAT", binwidth = 10, tolog = FALSE)
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

#Several trials without units appear to be using two different units in the same
#data. Easy to separate however, as 1000-fold differences in units 
# note 1e9 and g/L has a one to one conversion
# BI1160_64 appears to ahve some very high values, but these are all listed 
plat2 <- plat %>%
  mutate(unit = "10E9/L",
         value = case_when(
           trial %in% c("BI244_2484") & value  ~ value /1000,
           trial == "RCHMRA012JP" ~ value *10,
           TRUE ~ value),
         value = if_else(trial %in% c("BI1160_64", "BI502_397") & 
                           (value >= 10000 | 
                              (!is.na(ll) & ll >= 1000)
                            ),
                         value/1000, value),
         value = if_else(trial %in% c("BI502_316", "BI502_317") & value <1, value*1000, value)
         )
## Note that BI1160_64 still has some outliers with high platelet counts, howver, these are possible
# MakeDensity(plat2 %>%  filter(trial %in% c("BI244_2484", "BI502_397", "RCHMRA012JP","BI1160_64",
#                                           "BI502_316", "BI502_317")), "PLAT", binwidth = 10, tolog = FALSE)
plat <- plat2
rm(plat2, plat_m, plat_s)



## Calculate fib4
Fib4 <- function(age, ast, alt, plt) (age*ast) / (plt*alt^0.5)

demo <- cmpnies$demo
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
# MakeDensity(demo_plat_liv %>% mutate(param = "fib4", value = fib4), "fib4", binwidth = 0.33, tolog = FALSE)

## Four strange values, all ahve very low platelet counts and all are in BI1160_24, drop these
## Also one in NVT_SA_ZOL446H2301 is dubious with a platelet score of 9, drop this too
demo_plat_liv %>% 
  filter(fib4 >50) %>% 
  as.data.frame()

demo_plat_liv <- demo_plat_liv %>% 
  filter(fib4<=50) %>% 
  select(company, trial, id, fib4)
if(tosave) saveRDS(demo_plat_liv, "../Extract_Data/Processed_data/fib4.Rds")


MakeDensity(hgb, "HGB", binwidth = 1, tolog = FALSE)
MakeDensity(demo_plat_liv %>% mutate(param = "fib4", value = fib4), "fib4", binwidth = 0.33, tolog = FALSE)

