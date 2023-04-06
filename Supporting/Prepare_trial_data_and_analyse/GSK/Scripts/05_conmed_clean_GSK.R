# 05_conmed_clean

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

gsk <- readRDS("GSK_processed_data/GSK_transposed.Rds")

## Consolidate drugs ----
## Extract conmed table and lookup table from RXNORM and BNF and table of ATC classes
conmed <- gsk$conmed
rm(gsk)

path <- ""
drg_lkp <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/bnf_rxnorm_atc_codes.csv")
class_names <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/class_names.csv")
cmbn_lkp <- c(drg_lkp$str, class_names$str)



## Consolidate route data ----
route <- conmed %>%
  mutate(route = str_to_lower(route)) %>% 
  distinct(route)

#write_csv(route, "GSK_scratch_data/review_route_values.csv")

route_lkp <- read_csv("Created_metadata/reviewED_route_values.csv")
route_lkp <- route_lkp %>% 
  filter(route_classify != "unknown") %>% 
  select(route_lower = route, route_classify = route_classify_broad) %>% 
  distinct()

conmed <- conmed %>%
  mutate(route_lower = str_to_lower(route)) %>% 
  left_join(route_lkp) %>% 
  select(-route_lower)

## Examine which have multiple routes for the same term
same_term_multi_route <- conmed %>%
  filter(!is.na(route_classify), !is.na(term)) %>% 
  group_by(trial, id, term) %>% 
  summarise(route_n = sum(!duplicated(route_classify)),
            route = paste(route[!is.na(route)], collapse = ", "),
            route_classify = paste(route_classify[!is.na(route_classify)], collapse = ", ")) %>% 
  ungroup() %>% 
  filter(route_n >=2)

same_term_multi_route_unq <- same_term_multi_route %>% 
  distinct(term, route_classify)

# write_csv(same_term_multi_route_unq, "Scratch_data/review_same_term_multi_routes.csv")
## Reviewed where same participant  (in same trial) had multiple routes for the same term
## Mostly these were trivial, just take the first (eg sublingual versus oral for GTN)
## For some it could affect interpretation, so  I assigned these to the most appropriate
same_term_multi_route_unq <- read_csv("Created_metadata/reviewED_same_term_multi_routes.csv") 
same_term_multi_route_unq <- same_term_multi_route_unq %>% 
  separate(route_classify, into = "route1", sep = ",", remove = FALSE, extra = "drop") %>% # removed fixed wrapper from (sep = fixed(","))
  mutate(route_classify_new = if_else(selection == "take_any", route1, selection)) %>% 
  select(-selection, -route1) 

same_term_multi_route_unq <- same_term_multi_route_unq %>% 
  inner_join(same_term_multi_route) %>% 
  select(-route_n, -route) %>% 
  distinct()

conmed <- conmed %>%
  left_join(same_term_multi_route_unq) %>% 
  mutate(route_classify = if_else(!is.na(route_classify_new), route_classify_new, route_classify)) %>% 
  select(-route_classify_new)

## Consolidate route terms within patients, terms and classes, then within patients and term
## This is slow so run it only for those trials with route data, and where that route data is incomplete
# count_not_same <- conmed %>% 
#   filter(!is.na(term), term != "") %>% 
#   group_by(trial, id, term) %>% 
#   mutate(all_route_missing = all(is.na(route_classify)),
#          some_route_missing = any(is.na(route_classify))) %>% 
#   ungroup() %>% 
#   filter(!all_route_missing, some_route_missing)
# 
# count_not_same_denom <- count_not_same %>% 
#   group_by(trial, id, term) %>% 
#   summarise(n = n()) %>% 
#   ungroup()
# 
# count_not_same_num <- conmed %>% 
#   filter(!is.na(route_classify)) %>% 
#   distinct(trial, id, term, route_classify) %>% 
#   group_by(trial, id, term) %>% 
#   summarise(n_unq = n()) %>% 
#   ungroup()
# 
# count_not_same_totl <- count_not_same_denom %>% 
#   left_join(count_not_same_num) %>% 
#   mutate(n_unq = if_else(is.na(n_unq), 0L, n_unq))
# sum(count_not_same_totl$n > count_not_same_totl$n_unq)
# 
# count_not_same_totl <- count_not_same_totl %>%
#   filter(n > n_unq) %>% 
#   distinct(trial, id, term)
# 
# xmn <- conmed %>% 
#   semi_join(count_not_same_totl)

## <2000 entries with multiple rows where one is NA for route, but the other contains information 
## Identifies trials with route data
## Then identify patients with incomplete route data
conmed_rte <- conmed %>%
  group_by(trial) %>%
  summarise(any_route = any(!is.na(route_classify))) %>%
  ungroup() %>%
  filter(any_route) %>%
  distinct(trial)

conmed_rte <- conmed %>%
  semi_join(conmed_rte)

conmed_rte <- conmed %>%
  group_by(trial, id) %>%
  summarise(any_route = any(!is.na(route_classify)),
            some_route = any(is.na(route_classify))) %>%
  ungroup() %>%
  filter(any_route, some_route) %>%
  distinct(trial, id)

conmed_rte <- conmed %>%
  semi_join(conmed_rte)

conmed_not_fix_rte <- conmed %>%
  anti_join(conmed_rte %>% distinct(trial, id))


conmed2 <- conmed_rte %>% 
  group_by(trial, id, term, atc_code) %>% 
  mutate(route_classify = if_else(is.na(route_classify) | route_classify == "",
                                 paste(route_classify[!is.na(route_classify)] %>% unique(),
                                       collapse = ""),
                                 route_classify))   %>% 
  group_by(trial, id, term) %>% 
  mutate(route_classify = if_else(is.na(route_classify),
                                 paste(route_classify[!is.na(route_classify)] %>% unique(),
                                       collapse = ""),
                                 route_classify)) %>% 
  ungroup()
conmed2 <- conmed2 %>% 
  mutate(route_classify = if_else(str_trim(route_classify) == "", NA_character_, route_classify))
sum(!is.na(conmed2$route_classify)) - sum(!is.na(conmed$route_classify))

conmed2 <- conmed2 %>% 
  distinct(trial, id, term, atc_code, route_classify, .keep_all = TRUE)
conmed <- bind_rows(conmed_not_fix_rte, conmed2) %>%  arrange(company, trial, id)
rm(conmed2)

## Where same term in same participant is associated with multiple classes, and route is null
## set ATC code to null as it is essentially unreliable
## Is really concerned with steroids based on file "reviewED...atc_different_body_systems_steroids_to_ctdt" 
conmed2 <- conmed %>% 
  mutate(atc3 = str_sub(atc_code, 1, 3),
         gi_cv_skin_nas_inh = as.integer(atc3 %in% c("A07", "C05", "D07", "R01", "R03")),
         eye_ear_eyeear = atc3 %in% c("S01", "S02", "S03")) %>% 
  group_by(trial, id, term) %>% 
  summarise(contradict = sum(gi_cv_skin_nas_inh) + as.integer(any(eye_ear_eyeear))) %>% 
  ungroup()

## Total number of terms and rows with contradictory atc codes
conmed2 %>%
  filter(!is.na(term), contradict >=2) %>% 
  count(contradict) %>% 
  arrange(desc(contradict)) %>% 
  summarise(terms = sum(n),
            atc_Codes = sum(n*contradict))

## Create dataset for excluding contradictory ATC codes
contradict <- conmed2 %>%
  filter(!is.na(term), !term == "", contradict >=2) %>% 
  select(trial, id, term) %>% 
  distinct() %>% 
  mutate(contradict = 1L)

conmed3 <- conmed %>%
  left_join(contradict) %>% 
  mutate(contradict = if_else(is.na(contradict), 0L, contradict),
         atc_code = if_else(contradict == 1L, NA_character_, atc_code)) %>% 
  select(-contradict) %>% 
  distinct()
nrow(conmed) - nrow(conmed3)  
conmed <- conmed3
rm(conmed2, conmed3)







###### BODY SYSTEM MISSING 


## where mismatch between route and code, assume route is correct and remove ATC code from selected ----
conmed$body_system <- NA
conmed$indication <- NA

conmed2a <- conmed %>% 
  filter(!( is.na(atc_code) | atc_code == "" )) %>%  
  select(company, trial, id, term, route, body_system, indication, atc_code, route_classify)

# read in ambiguous terms at drug level (eg beclometasone)
remove_ambig_terms <- read_csv("Created_metadata/drugs_in_multiple_3char_atc.csv")

remove_ambig_terms <- remove_ambig_terms %>%
  mutate(route_classify = case_when(
    route == "oral" ~ "oral/enteral",
    route == "topical" ~ "topical",
    route == "nasal" ~ "nasal",
    route == "inhaled" ~ "inhaled/nebulised",
    route == "eye" ~ "eye")
  )

# Remove contradictory terms
conmed2a <- conmed2a %>% 
  mutate(str = str_to_lower(term),
         selected_code = str_sub(atc_code, 1, 3)) %>% 
  mutate(contradict = 
      !is.na(route_classify) & (
        (selected_code == "S01" & route_classify != "eye") |
        (selected_code == "R03" & route_classify != "inhaled/nebulised") |
        (selected_code == "R01" & route_classify != "nasal") |
        (selected_code != "S01" & route_classify == "eye") |
        (selected_code != "R03" & route_classify == "inhaled/nebulised") |
        (selected_code != "R01" & route_classify == "nasal") | 
        (str_sub(selected_code,1,1) == "D" & route_classify != "topical"))
  ) %>% 
  filter(!contradict)

# remove all ambiguous terms
conmed2a2 <- conmed2a %>% 
  anti_join(remove_ambig_terms %>%  distinct(str))
# Add back in where the ambiguity can be resolved with route data
conmed2a_route <- conmed2a %>% 
  semi_join(remove_ambig_terms %>% select(-route) %>%  filter(decision == "by_route"))
# Add back in where the ambiguity CANNOT be resolved with route data, but the route data is missing
conmed2a_no_route <- conmed2a %>% 
  filter(is.na(route_classify)) %>% 
  semi_join(remove_ambig_terms %>% select(-route) %>%  filter(decision == "by_route") %>% select(str))
# Add back in where I resolved the ambiguity by picking the most commonly used drug
conmed2a_slct <- conmed2a %>%
  semi_join(remove_ambig_terms %>% filter(decision == "choose_one") %>%  distinct(str, selected_code))
# Join together 
conmed2a2 <- bind_rows(conmed2a2, conmed2a_route, conmed2a_no_route, conmed2a_slct) %>% 
  select(-selected_code)
excluded_drug_codes <- conmed2a %>% 
  anti_join(conmed2a2, by = c("trial", "id", "term")) %>% 
  distinct(term, atc_code, route_classify)
# rename as conmed2a
conmed2a <- conmed2a2
remove(conmed2a2)

name_class <- conmed2a %>%
  filter(atc_code != "", term != "", !is.na(atc_code), !is.na(term)) %>% 
  distinct(term, atc_code, route_classify) %>% 
  filter(!is.na(term) , !is.na(atc_code)) %>%
  mutate(has_route = !is.na(route_classify),
         has_route = if_else(has_route, "A", "B")) %>% 
  arrange(term, atc_code, route_classify) %>% 
  distinct(term, atc_code, .keep_all = TRUE) %>% 
  select(-has_route)

## remove class-level terms to examine separately ----
# note there is no evidence of class-level terms in drugs with an ATC code of 5
term_lvl_drug <- conmed %>% 
  filter(str_length(atc_code) ==5) %>% 
  count(term, sort = TRUE)
# Review each of the terms with an ATC code <5
term_lvl_class <- conmed %>% 
  filter(str_length(atc_code) < 5) %>% 
  count(term, sort = TRUE)
term_lvl_class_found <- term_lvl_class %>% 
  filter(str_to_lower(term) %in% c(class_names$str, str_to_lower(conmed$body_system )))
term_lvl_class_non_drug <- term_lvl_class %>% 
  anti_join(term_lvl_class_found, by = "term") %>% 
  filter(str_detect(term %>% str_to_lower(), "non-drug|non drug"))
term_lvl_class_remain <- term_lvl_class %>% 
  anti_join(term_lvl_class_found, by = "term") %>%
  anti_join(term_lvl_class_non_drug, by = "term")

# write_csv(term_lvl_class_remain %>% arrange(term), path = "Scratch_data/term_possible_class_name.csv")
term_lvl_class_remain <- read_csv("Created_metadata/reviewED_term_possible_class_name.csv", 
                                  col_types = "cc_")

term_lvl_class <- bind_rows(term_lvl_class_found %>%  select(term),
                            term_lvl_class_non_drug %>%  select(term),
                            term_lvl_class_remain %>%  select(term)) %>% 
  filter(!is.na(term), term != "") %>% 
  arrange(term)

## Identify ambigious classes and do not link these
# All are remove, so just remove these from the name_class lookup
ambiguous_classes <- read_csv("Created_metadata/reviewED_same_terms_atc_different_body_system.csv")
ambiguous_classes <- ambiguous_classes %>% 
  filter(str %in% str_to_lower(term_lvl_class$term))
name_class <- name_class %>% 
  filter(!str_to_lower(term) %in% ambiguous_classes$str)

# Apply restrictions to remove ambiguous terms again, but more strictly as applying this to terms alone
# Now if no route, and is "by_route" exclude
name_class <- name_class %>% 
  mutate(str = str_to_lower(term),
         selected_code = str_sub(atc_code, 1, 3))
name_class2 <- name_class %>% 
  anti_join(remove_ambig_terms %>%  distinct(str))
name_class_route <- name_class %>% 
  semi_join(remove_ambig_terms %>%  filter(decision == "by_route"))
name_class_slct <- name_class %>%
  semi_join(remove_ambig_terms %>% filter(decision == "choose_one") %>%  distinct(str, selected_code))
name_class <- bind_rows(name_class2, name_class_route, name_class_slct)
name_class <- name_class %>% 
  distinct(term, atc_code, route_classify)

conmed2b_route <- conmed  %>% 
  anti_join(conmed2a, by = c("trial", "id", "term")) %>%  
  select(company, trial, id, term, route, body_system, indication, route_classify) %>%
  filter(!is.na(route_classify)) %>% 
  inner_join(name_class %>%  filter(!is.na(route_classify)))

conmed2b_no_route <- conmed  %>% 
  anti_join(conmed2a, by = c("trial", "id", "term")) %>%  
  anti_join(conmed2b_route, by = c("trial", "id", "term")) %>% 
  select(company, trial, id, term, route, body_system, indication) %>%
  inner_join(name_class)

conmed2b_no_route <- conmed2b_no_route %>% 
  filter(!str_to_lower(term) %in% remove_ambig_terms$str[remove_ambig_terms$decision %in% c("by_route")])

conmed2ab_got <- bind_rows(original = conmed2a, new_route = conmed2b_route, 
                           new_no_route = conmed2b_no_route, .id = "prov") %>%  distinct()

## Drugs which have not been categorised ----
conmed2c <- conmed  %>% 
  filter(is.na(atc_code) | atc_code == "") %>%  
  select(company, trial, id, term, route, body_system, indication, atc_code) %>%
  anti_join(name_class)

## unique terms for drugs which have not been categorised
conmed2c_lkp <- conmed2c %>% 
  filter(term != "") %>% 
  count(term, sort = TRUE) %>% 
  mutate(term_lower = str_to_lower(term))

## unique lower-case terms for drugs which have not been categorised having separated the fields where drugs are listed
## in a single cell
conmed2c_lkp2 <- conmed2c_lkp %>% 
  distinct(term_lower) %>% 
  separate(term_lower, into = paste0("v", 1:100), remove = FALSE, sep = "\\/|\\\\|[0-9]", fill = "right") %>% 
  gather(key = "pos", value = "term_component", - term_lower, na.rm = TRUE) %>% 
  select(-pos) %>% 
  mutate(term_component_no_symb = str_replace_all(term_component, "\\*|[0-9]", "") %>%  str_trim())

conmed2c_lkp2$in_lkp <- (conmed2c_lkp2$term_lower %in% cmbn_lkp | conmed2c_lkp2$term_component %in% cmbn_lkp |
                           conmed2c_lkp2$term_component_no_symb %in% cmbn_lkp 
)

conmed2c_lkp2_found <- conmed2c_lkp2 %>% 
  group_by(term_lower) %>% 
  summarise(found = any(in_lkp))
mean(conmed2c_lkp2_found$found)

# Exclude ambigious terms from the drug to class lookup
ambiguous_classes <- read_csv("Created_metadata/reviewED_same_terms_atc_different_body_system.csv")
ambiguous_classes <- ambiguous_classes %>% 
  filter(str %in% class_names$str)
class_names_definite <- class_names %>% 
  filter(!str %in% ambiguous_classes$str)

# Exclude ambiguous terms from the drug lookup
drg_lkp2a <- drg_lkp %>% 
  filter(!str %in% remove_ambig_terms$str)
# add back in where select one of many
drg_lkp2b <- drg_lkp %>% 
  mutate(selected_code = str_sub(code, 1, 3)) %>% 
  semi_join(remove_ambig_terms %>%  filter(decision == "choose_one") %>%  distinct(str, selected_code))
drg_lkp2 <- bind_rows(drg_lkp2a, drg_lkp2b)
# seperately create lookup for when have route information
drg_lkp_route <- drg_lkp %>%
  mutate(selected_code = str_sub(code, 1, 3)) %>% 
  inner_join(remove_ambig_terms %>% distinct(selected_code, route_classify))

# Retrieve rxnorm and bnf names want
# First regardless of route, ie not with drugs which need a route to resoluve the class ambigious route lookup
conmed2c_lkp_rxnorm_bnf_exact <- conmed2c_lkp2 %>% 
  left_join(drg_lkp2 %>% select(term_lower = str, code1 = code)) %>%
  left_join(drg_lkp2 %>% select(term_component = str,  code2 = code)) %>%
  left_join(drg_lkp2 %>% select(term_component_no_symb = str,  code3 = code)) %>% 
  left_join(class_names_definite %>% select(code4 = code, term_lower = str))

# Next by route
conmed2c_lkp_rxnorm_bnf_exact_route <- conmed2c_lkp2 %>% 
  left_join(drg_lkp_route %>% select(term_lower = str, code1 = code, route_classify)) %>%
  left_join(drg_lkp_route %>% select(term_component = str,  code2 = code, route_classify)) %>%
  left_join(drg_lkp_route %>% select(term_component_no_symb = str,  code3 = code, route_classify))
# Join route and no route together
conmed2c_lkp_rxnorm_bnf_exact <- bind_rows(no_route = conmed2c_lkp_rxnorm_bnf_exact,
                                           route = conmed2c_lkp_rxnorm_bnf_exact_route,
                                           .id = "route_type")

# Simplify dataset by removing duplicate terms
conmed2c_lkp_rxnorm_bnf_exact <- conmed2c_lkp_rxnorm_bnf_exact %>% 
  select(-term_component, -term_component_no_symb, -in_lkp) %>%  
  gather(key = "code_num", value = "atc_code", -term_lower, -route_classify, -route_type, na.rm = TRUE) %>% 
  select(-code_num) %>% 
  distinct(term_lower, atc_code, route_classify, route_type)

# Add in company and trial data
conmed2c_lkp_rxnorm_bnf_exact_nr <- conmed2c_lkp_rxnorm_bnf_exact %>% 
  filter(route_type == "no_route") %>% 
  inner_join(conmed %>%
               select(company, trial, id, term, route, body_system, indication) %>% 
               mutate(term_lower = str_to_lower(term)))

conmed2c_lkp_rxnorm_bnf_exact_yr <- conmed2c_lkp_rxnorm_bnf_exact %>% 
  filter(route_type == "route") %>% 
  inner_join(conmed %>%
               select(company, trial, id, term, route, body_system, indication, route_classify) %>% 
               mutate(term_lower = str_to_lower(term)))

conmed2c_rxnorm_bnf_exact <- bind_rows(conmed2c_lkp_rxnorm_bnf_exact_nr, conmed2c_lkp_rxnorm_bnf_exact_yr)
rm(conmed2c_lkp_rxnorm_bnf_exact_nr, conmed2c_lkp_rxnorm_bnf_exact_yr, conmed2c_lkp_rxnorm_bnf_exact_route)

## Separately analyse ambigious class terms
ambiguous_classes <- ambiguous_classes %>% 
  filter(action %in% 
           c("as A10, use as antimalarial low probability", "as C, radiopharmaceuticals rare", 
             "as N05, since GA not used in trial", "as R", "assume genitourinary", 
             "leave as C04, nakes no difference to diagnosis", 
             "use route, if none, exclude"))

class_names_ambig <- class_names %>% 
  filter(str %in% ambiguous_classes$str)

conmed2c_lkp_rxnorm_bnf_exact_ambig <- conmed2c_lkp2 %>% 
  semi_join(class_names_ambig %>%  select(term_lower = str ))

## Looks like 5 classes that are of interest
# # A tibble: 5 x 4
# term_lower                       term_component        term_component_no_symb in_lkp
# <chr>                            <chr>                 <chr>                  <lgl> 
#   1 beta blocking agents             beta blocking agents  beta blocking agents   TRUE  
#   2 alpha-adrenoreceptor antagonists alpha-adrenoreceptor~ alpha-adrenoreceptor ~ TRUE  
#   3 anticholinergics                 anticholinergics      anticholinergics       TRUE  
#   4 biguanides                       biguanides            biguanides             TRUE  
#   5 parasympathomimetics             parasympathomimetics  parasympathomimetics   TRUE 
ambiguous_classes <- ambiguous_classes %>% 
  filter(str %in% conmed2c_lkp2$term_lower) 
## Note of the 4 trials with these ambiguous terms have route information, simplifies assessment
conmed2c_ambig <- conmed2c %>% 
  filter(str_to_lower(term) %in% ambiguous_classes$str[ambiguous_classes$action != "use route, if none, exclude"])
class_names_ambig %>% 
  filter(str %in% str_to_lower(conmed2c_ambig$term))

conmed2c_ambig <- conmed2c_ambig %>% 
  mutate(str = str_to_lower(term),
         atc_code = case_when(
           str == "alpha-adrenoreceptor antagonists" ~ "C02CA",
           str == "biguanides" ~ "A10BA"
         )) %>% 
  select(-str)
## ONly one trial with more than 0.1% of participants taking either of these drugs
# people taking ALPHA-ADRENORECEPTOR ANTAGOSIts. Of these, only 1% of people taking alpha blockers (none taking biguanides)
# were taking it as a sole cardiovacsular drug
# therefore, exclude all ambiguous drug class labels
BI1160_26 <- conmed %>% 
  filter(trial == "BI1160_26", term %in% c("ANTITHROMBOTIC AGENTS", "BETA BLOCKING AGENTS", "HYDROCHLOROTHIAZIDE", 
                                           "ACE INHIBITORS", "TIABENDAZOLE",  "CALCIUM CHANNEL BLOCKERS", 
                                           "DIGOXIN", "ANGIOTENSIN II ANTAGONISTS, PLAIN",  
                                           "ALPHA-ADRENORECEPTOR ANTAGONISTS", 
                                           "AMIODARONE",  "ANTIARRHYTHMICS, CLASS I AND III"
  ))  %>% 
  distinct(id, term) %>% 
  group_by(id) %>% 
  mutate(single_cv_drug = length(term) ==1) %>% 
  ungroup() %>% 
  filter(term == "ALPHA-ADRENORECEPTOR ANTAGONISTS") %>% 
  summarise(alpha_single = mean(single_cv_drug))

# Where drug not coded and cannot find from rxnorm, use "body_system" the category
# varialbe is not useful, but this is, and means dont need to manually review 1166 terms
check_body_system <- conmed %>% 
  distinct(term, body_system) %>% 
  filter(!is.na(body_system)) %>% 
  mutate(term_lower = str_to_lower(term)) %>% 
  inner_join(conmed2c_lkp2 %>% filter(in_lkp == FALSE))
check_body_system$body_system <- as.character(check_body_system$body_system )
check_body_system_smry <- check_body_system %>% 
  group_by(body_system) %>% 
  summarise(terms = term %>% unique() %>% sort() %>% head(10) %>% paste(collapse = "| "))

# write_csv(check_body_system_smry, "Scratch_data/examine_drug_body_system_categories.csv")
body_system <- read_csv("Created_metadata/reviewED_examine_drug_body_system_categories.csv")
# reviewed each body system class and assigned to a suitable ATC code. If was not a class of interest, was not assigned
# to an ATC class (eg antibiotics)
body_system_assigned <- check_body_system %>% 
  inner_join(body_system %>% filter(!is.na(atc_code))) %>%
  distinct(term_lower, atc_code)
# Review terms where the class was "too broad"
body_system_too_broad <- check_body_system %>% 
  semi_join(body_system %>% filter(is.na(atc_code), reason_not_classified == "too broad") %>%  select(body_system)) %>% 
  distinct(term_lower)
# write_csv(body_system_too_broad, "Scratch_data/body_system_too_broad.csv")
body_system_too_broad <- read_csv("Created_metadata/reviewED_body_system_too_broad.csv") %>% 
  filter(!is.na(atc_code)) %>% 
  distinct(term_lower, atc_code)
# Combine all body system terms with ATC codes
body_system <- bind_rows(body_system_assigned, body_system_too_broad) %>% 
  arrange(atc_code, term_lower)

conmed2c_lkp_body_system <- conmed2c_lkp %>%
  anti_join(conmed2ab_got) %>% 
  anti_join(conmed2c_rxnorm_bnf_exact) %>% 
  inner_join(body_system) %>% 
  inner_join(conmed %>%
               select(company, trial, id, term, route, indication)) %>%
  select(-n, -term_lower) %>% 
  distinct()

## Combine all body system and originally available ATC codes
conmed2abc_got <- bind_rows(conmed2ab_got, conmed2c_rxnorm_bnf_exact, conmed2c_lkp_body_system) %>% 
  distinct()

## Manually review each term where it did not have a body system and was not originally categorised
conmed2c_lkp2_no_system <- conmed2c_lkp2 %>% 
  filter(in_lkp == FALSE) %>% 
  anti_join(check_body_system, by = "term_lower")

## fuzzy match drug names to identify most likely candidates
# conmed_fuzzy <- conmed2c_lkp2_no_system$term_lower[conmed2c_lkp2_no_system$in_lkp == FALSE] %>% unique() 
# Commented out and saved as scratch data as is slow
# dist_mtrx <- stringdistmatrix(conmed_fuzzy, c(drg_lkp$str, class_names$str),
#                               method="jw", p= 0.1)
# saveRDS(dist_mtrx, "Scratch_data/string_distance_matrix.Rds")
# take top five matches 
# dist_mtrx <- readRDS("Scratch_data/string_distance_matrix.Rds")
# rownames(dist_mtrx) <- conmed_fuzzy
# colnames(dist_mtrx) <- c(drg_lkp$str, class_names$str)

## Loop through taking minimum position each time
# note which.min already removes NAs 
# mins <- vector(length = 5, mode = "list")
# for (i in 1:5){
#   a <- apply(dist_mtrx, 1, which.min)
#   mins[[i]] <- a
#   dist_mtrx [1:nrow(dist_mtrx) , a] <- NA
# }
# mins <- do.call(cbind, mins)
# mins <- as_tibble(mins)
# mins[] <- map(mins, ~ colnames(dist_mtrx)[.x])
# names(mins) <- paste0("match", 1:5)
# mins <- mins %>% 
#   mutate(term = row.names(dist_mtrx)) %>% 
#   select(term, everything())
# write_csv(mins, "Scratch_data/examine_no_drug_match.csv")

## Read in manually reviewed drug matches
manual <- read_csv("Created_metadata/reviewED_examine_no_drug_match.csv")
# 1805 unique terms USed the following approach If not the same drug from
# examining the 5 closest string matches, and the drug was not known to the
# reviewer (DM) the drug was set to non. Drugs were generally considered to be
# the same if the differences were due to abbreviations in one source generally
# Rxnorm and not the other (eg hcl) or if a more precise term was given in the
# trial than in rxnorm (eg abacavir sulfate in trial and abacavir in rxnorm) or
# if a code had been included in the text but not in rxnorm Some of the terms
# not assigned to an rxnorm (or BNF) term will have been drugs not available in
# the US or UK market Others will have been herbal medicines We did not try and
# infer the class from the name eg did not asume drugs ending in -opril were ACE
# inhibitors 1133 no matches
manual_non <- manual %>% 
  filter(slct == "Non")
manual <- manual %>% 
  filter(slct != "Non") %>% 
  gather(key = "pos", value = "slct", -term, -match1, -match2, -match3, -match4, -match5,
         na.rm = TRUE)
## 46 identified as a drug from the name
## Note none of these are ambiguous WRT ATC
manual_name <- manual %>%
  filter(str_detect(slct, "[a-z]"  ))
manual_names <- manual_name$slct %>%  unique() %>%  sort()
names(manual_names) <- manual_names
manual_names2 <- manual_names %in% c(drg_lkp$str, class_names$str) 
names(manual_names2) <- manual_names
manual_names <- manual_names2
rm(manual_names2)
manual_names[manual_names]

## remaining identified on basis of selection of one of the 5 columns
manual_numbers <- manual %>% 
  filter(str_detect(slct, "[1-5]")) 

manual_numbers_lkp <- manual_numbers %>%  select(match1:match5) %>%  transpose()
manual_numbers_assign <- map2(manual_numbers_lkp, manual_numbers$slct %>%  as.integer(),
                              ~ .x[.y])
map_int(manual_numbers_assign, length) %>%  unique()
manual_numbers_assign <- unlist(manual_numbers_assign)
manual_numbers$slct <- manual_numbers_assign
manual <- bind_rows(manual_name, manual_numbers) %>%  select(term_lower = term, slct)

# no terms without atc code
manual %>% 
  select(str = slct) %>% 
  anti_join(bind_rows(drg_lkp, class_names))

manual_atc <- manual %>% 
  rename(str = slct) %>% 
  inner_join(bind_rows(drg_lkp2, class_names))

setdiff(manual$term_lower, conmed2c_lkp2_no_system$term_lower)

# 4 ambiguous terms
ambiguous_classes <- read_csv("Created_metadata/reviewED_same_terms_atc_different_body_system.csv")
ambiguous_terms   <- read_csv("Created_metadata/drugs_in_multiple_3char_atc.csv")
intersect(manual$slct, ambiguous_classes$str)
#  "antibiotics"      "biguanides"       "enzymes"          "muscle relaxants"
intersect(manual$slct, ambiguous_terms$str)
# drop all ambiguous terms at this stage

# Remove enzymes and ambiguous terms
manual <- manual %>% 
  filter(!slct %in% c("enzymes", ambiguous_terms$str))

# No otheraction needed as P01, other codes for biguanides are not needed
conmed2d <- conmed %>% 
  select(company, trial, id, term, route, indication) %>% 
  anti_join(conmed2abc_got, by = "term") %>% 
  mutate(term_lower = str_to_lower(term)) %>% 
  inner_join(manual_atc %>%  select(term_lower, atc_code = code)) %>% 
  select(-term_lower)

conmed2abcd_got <- bind_rows(conmed2abc_got, conmed2d)
conmed2abcd_got <- distinct(conmed2abcd_got)

# Review where multiple atc codes in a single cell
table(str_detect(conmed2abcd_got$atc_code, "\\,"),
      str_length(conmed2abcd_got$atc_code) >= 8)

multiple_codes <- conmed2abcd_got %>% filter(str_detect(atc_code, "\\,")) %>%  
  distinct(term, route, indication, atc_code)
# write_csv(multiple_codes, "Scratch_data/review_multiple_codes_single_cell.csv")
# Reviewed each of these manually and selected definitive code based on route and indication data and reviewing drug information online
slctd_from_multiple_codes <- read_csv("Created_metadata/reviewED_multiple_codes_single_cell.csv")
multiple_codes2 <- conmed2abcd_got %>% 
  filter(str_detect(atc_code, "\\,")) %>% 
  select(-atc_code) %>% 
  inner_join(slctd_from_multiple_codes) %>% 
  distinct()

conmed2abcd_got <- conmed2abcd_got %>% 
  filter(!str_detect(atc_code, "\\,")) 

conmed2abcd_got <- bind_rows(conmed2abcd_got, multiple_codes2)
# Final not gots
conmed_not_got <- conmed %>% 
  filter(!term %in% conmed2abcd_got$term)
# Proportion of terms with ATC CODE, 95%
mean(conmed$term %in% conmed2abcd_got$term)
# Examine by trial
# Looks fine across trials, including lilly
xmn <- conmed %>%
  distinct(trial, term) %>% 
  mutate(term_in = term %in% conmed2abcd_got$term) %>% 
  group_by(trial) %>% 
  summarise(term_in = mean(term_in))

# Final gots
conmed <- conmed2abcd_got

## Add trial ids
trial_nctid <- read_csv(paste0(path, "Count_comorbidities_GSK/Created_metadata/GSK_trials_in_respository_sas_with_medicine_condition.csv")) %>% 
  distinct(nct_id, trial)
## One non-joining trials as expected as this is a "rejected" trial
setdiff(conmed$trial, trial_nctid$trial)
conmed <- conmed %>% 
  inner_join(trial_nctid)

n_distinct(conmed$trial)# 29, Expect 2 missing 

saveRDS(conmed, "GSK_processed_data/GSK_conmed.Rds")




# Check saved conmed # ---
rm(list = ls())
source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")
path <- ""
conmed <- readRDS("GSK_processed_data/GSK_conmed.Rds")
trial_nctid <- read_csv("Supporting/Prepare_trial_data/GSK/Created_metadata/GSK_trials_in_respository_sas_with_medicine_condition.csv")


# Number of unique ATC3 codes p/trial
atc3 <- conmed %>%
  mutate(atc3 = str_sub(atc_code, 1, 3)) %>%
  group_by(trial) %>%
  summarise(n_unique_atc = n_distinct(atc3)) %>%
  left_join(trial_nctid) %>%
  select(trial, nct_id, n_unique_atc, medicine, condition)

write.csv(atc3, "Outputs/GSK_num_unique_atc3_per_trial.csv")


# count the number of A01, A02 etc codes per trial then spread that count across 
# the cols so we have one row per trial and one col per 3-character ATC class.
n_atc3 <- conmed %>%
  mutate(atc3 = str_sub(atc_code, 1, 3)) %>%
  group_by(trial, atc3) %>%
  summarise(n_unique_atc = n_distinct(atc3))  %>% 
  spread(atc3, n_unique_atc) %>% 
  inner_join(atc3)

write.csv(n_atc3, "Outputs/GSK_atc3_spread_per_trial.csv")


