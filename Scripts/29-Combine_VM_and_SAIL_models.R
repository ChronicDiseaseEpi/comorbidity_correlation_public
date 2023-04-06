#' ---
#' title: "29: Combine non restricted and age restricted model outputs"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script joins omega summaries (unrestricted and age restricted) from the
# 11 models ran on the vm with the omega summaries from the 5 models ran in SAIL.
# One unrestricted model per index condition and one age restricted model per index condition 



# Set up -----------------------------------------------------------------------

source("Scripts/00-Functions_and_packages.R")

# Data 
vm_cmnty   <- readRDS("Outputs/VM_community_omega_summaries.RDS")
sail_cmnty <- read_csv("Outputs/model_summaries_from_SAIL.csv")
com_como   <- read.csv("Created_metadata/SAIL_common_6_comorbidities.csv", row.names = 1)



# Add comorbidities for 5 models ran in SAIL -----------------------------------

# Filter to 5 conditions in sail 
com_como <- com_como %>% 
  filter(condition %in% c("Psoriatic_arthritis", "Axial_Spondyloarthritis", "Pulmonary_Hypertension",
                          "Parkinson_Disease", "Systemic_Lupus_Erythematosus")) %>%
  mutate(common_n = as.character(common_n)) %>%
  rename(old_como = como)

# Look up to harmonise comorbidities 
lk_up <- data.frame(new_como = c("Cardiovascular", "Pain", "Arthritis", "Acid-related disease", 
                            "Asthma", "Anxiety", "Thyroid disorders", "Inflammatory disorders", 
                            "Diabetes", "Osteoporosis"),
                    old_como = c("CV", "pain", "arthritis", "antacids", "asthma_COPD", 
                            "anxiety", "thyroid", "inflammatory", "diabetes", "osteoporosis"))

# Harmonise comorbidities and add duplicate column of como
com_como <- com_como %>%
  inner_join(lk_up, by = "old_como") %>%
  select(-old_como) %>%
  mutate(new_como2 = new_como)

# Remove unmatched columns, rename columns and add repo column 
sail5 <- sail_cmnty %>% 
  select(-c("5%":"95%")) %>% 
  rename(CI2.5 = "2.5%",
         CI97.5 = "97.5%") %>%
  mutate(repo = if_else(model_type == "allages", "Community", "Age restricted community"),
         Omega = params %>% str_remove_all("Omega") %>% str_remove_all("\\[|\\]") %>% str_remove_all(","))

# Harmonise condition names
sail5 <- sail5 %>%
  mutate(condition = case_when(condition == "Hypertension, Pulmonary" ~ "Pulmonary_Hypertension",
                               condition == "Parkinson's disease (all)" ~ "Parkinson_Disease",
                               condition == "Psoriatic arthropathy" ~ "Psoriatic_arthritis",
                               TRUE ~ as.character(condition)),
         condition = str_replace_all(condition, " ", "_"))

# Split omega 
sail5_sep <- sail5 %>%
  separate(Omega, into = c("rows", "cols"), sep = 1) 

# Add comos 
sail5_como <- sail5_sep %>%
  left_join(com_como %>% select(condition, new_como, common_n), by = c("condition", "rows" = "common_n")) %>%
  left_join(com_como %>% select(condition, new_como2, common_n), by = c("condition", "cols" = "common_n")); warning("This step relies on como in com_como being ordered in descending prevalence")

# Create comorbidty column and remove uncessary columns 
sail5_como <- sail5_como %>%
  unite("comorbidity", c(new_como, new_como2), sep = " : ", remove = FALSE) %>%
  rename(Omega = params,
         Var1 = new_como,
         Var2 = new_como2) %>%
  select(-c("rows", "cols", "model_type", "Rhat", "n_eff", "se_mean", "Omega"))



# Clean vm model summaries and bind vm and sail models -------------------------

# Remove 1:1 correlations 
vm_cmnty <- vm_cmnty %>%
  filter(Var1 != Var2)

# Add vm models and 5 models ran in SAIL together 
cmnty <- vm_cmnty %>%
  rename(CI2.5 = "q2.5",
         CI97.5 = "q97.5") %>%
  select(names(sail5_como)) %>%
  bind_rows(sail5_como)

# Order Var 1 and 2 alphabetically 
cmnty <- cmnty %>%
  mutate(new_var1 = pmin(Var1, Var2),
         new_var2 = pmax(Var1, Var2))

# Make unique comorbidity column
cmnty <- cmnty %>% 
  unite("new_comorbidity", c(new_var1, new_var2), sep = " : ", remove = FALSE)

# Checks
# Expected length = 480 (16 conds * 2 models types(rstrct/non) * 15 omegas)
unique(cmnty$repo)     # 2
n_distinct(cmnty$Var1) # 10
n_distinct(cmnty$Var2) # 9
unique(cmnty$Var1)     # 10
unique(cmnty$Var2)     # 9
unique(cmnty$condition)      # 16 
unique(cmnty$comorbidity)    # 47
length(cmnty$condition)      # 480, expected length = 15*(16*2) = 480 
map(cmnty, ~ sum(is.na(.x))) # 0 

unique(cmnty$new_comorbidity) # 37
unique(cmnty$new_var1) # 8
unique(cmnty$new_var2) # 9
unique(cmnty$repo)

# Clean up columns 
cmnty <- cmnty %>%
  select(-Var1, -Var2, - comorbidity) %>%
  rename(Var1 = new_var1,
         Var2 = new_var2,
         comorbidity = new_comorbidity)

# Save
write_csv(cmnty, "Outputs/all_community_omega_summaries.csv")


