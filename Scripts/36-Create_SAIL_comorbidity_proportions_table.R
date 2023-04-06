#' ---
#' title: "36: Create SAIL comorbidity proportions dataframe"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script creates a dataframe of the proportion of each comorbidity for 
# each index condition that was kept and analysed in the SAIL secure environment



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
sail_prev <- read_csv("Created_metadata/sail_top6_cond_summary_n_each.csv")
totals    <- read_csv("Outputs/cmnty_trial_summary_table.csv")



# Prepare tables ---------------------------------------------------------------

# Harmonise condition names
sail_prev <- sail_prev %>% 
  rename(condition = index_condition) %>% 
  mutate(condition = case_when(condition == "Hypertension, Pulmonary" ~ "Pulmonary_Hypertension",
                               condition == "Parkinson's disease (all)" ~ "Parkinson_Disease",
                               condition == "Psoriatic arthropathy" ~ "Psoriatic_arthritis",
                               TRUE ~ as.character(condition)),
         condition = str_replace_all(condition, " ", "_"))

# Long format 
sail_prev_l <- sail_prev %>%
  pivot_longer("1st":"6th", names_to = "rank", values_to = "prevalence")
  
# Split prevalence into comorbidity
sail_prev_l <- sail_prev_l %>% 
  separate(prevalence, into = c("comorbidity", "comorbidity_n"), sep = " - ")

# add totals 
sail_prev_l <- sail_prev_l %>% 
  inner_join(totals %>% select(condition, cmnty_n)) %>%
  rename(condition_n = cmnty_n)



# Calculate proportions -------------------------------------------------------- 

sail_prop <- sail_prev_l %>% 
  mutate(comorbidity_n = as.numeric(comorbidity_n)) %>% 
  group_by(condition, comorbidity, condition_n, comorbidity_n) %>%
  summarise(como_prop = comorbidity_n/condition_n,
            prop_se = sqrt((como_prop*(1-como_prop))/condition_n)) %>%
    mutate(repo = "Age restricted community",
           como_prop = como_prop*100,
           se_min = como_prop - (1.96*prop_se),
           se_max = como_prop + (1.96*prop_se)) %>%
  ungroup()



# Harmonise comorbidities ------------------------------------------------------

lk_up <- data.frame(new_como = c("Cardiovascular", "Pain", "Arthritis", "Acid-related disease", "Asthma", 
                                 "Anxiety", "Thyroid disorders", "Inflammatory disorders", "Diabetes", "Osteoporosis"),
                    comorbidity = c("CV", "pain", "arthritis", "antacids", "asthma_COPD", 
                                    "anxiety", "thyroid", "inflammatory", "diabetes", "osteoporosis"))

sail_prop <- sail_prop %>%
  inner_join(lk_up)

sail_prop <- sail_prop %>%
  select(-comorbidity) %>% 
  rename(comorbidity = new_como)



# Save 
write_csv(sail_prop, "Outputs/SAIL_age_restricted_comorbidity_proportions_by_condition.csv")


