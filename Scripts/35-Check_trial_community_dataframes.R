#' ---
#' title: "35: Check trial and communtiy dataframes"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script checks the comorbidities and comorbidity combinations across trials, community models
# (sail, vm and combined) and trial meta analysis to make sure they match 



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data
# Omega summaries 
vm_sum    <- readRDS("Outputs/VM_community_omega_summaries.RDS")
sail_sum  <- read_csv("Outputs/model_summaries_from_SAIL.csv")
cmnty_sum <- read_csv("Outputs/all_community_omega_summaries.csv")
tr_sum    <- read_csv("Outputs/all_trials_omega_summaries.csv")  

# Omega draws
vm_draws  <- readRDS("Outputs/vm_community_omega_mcmc_draws.RDS")
tr_draws  <- readRDS("Outputs/all_trials_omega_draws.RDS")

# Meta 
meta_w    <- read_csv("Outputs/trial_meta_analysis_weighted_omega_summaries.csv")



# Check omega summaries --------------------------------------------------------

# vm community models 
unique(vm_sum$Var1)        # 10
unique(vm_sum$Var2)        # 10
unique(vm_sum$comorbidity) # 47

vm_sum_unq <- vm_sum %>% filter(Var1 != Var2)
unique(vm_sum_unq$Var1) # 9 
unique(vm_sum_unq$Var2) # 8
unique(vm_sum_unq$comorbidity) # 37

# sail community models 
unique(sail_sum$params) # 15

# All community (vm & sail combined)
unique(cmnty_sum$Var1)        # 8
unique(cmnty_sum$Var2)        # 9
unique(cmnty_sum$comorbidity) # 37

# Trail omega summaries 
unique(tr_sum$Var1) # 8
unique(tr_sum$Var2) # 9
unique(tr_sum$comorbidity) # 37

# Meta 
unique(meta_w$Var1)        # 8
unique(meta_w$Var2)        # 9
unique(meta_w$comorbidity) # 37

# Check same 
setdiff(unique(tr_sum$comorbidity), unique(cmnty_sum$comorbidity)) # same 
setdiff(unique(cmnty_sum$comorbidity), unique(meta_w$comorbidity)) # same 
setdiff(unique(meta_w$comorbidity), unique(tr_sum$comorbidity)) # same 






# Check omega draws ------------------------------------------------------------


# Compare before only unique comorbidities
setdiff(unique(tr_draws$comorbidity), unique(vm_draws$comorbidity)) # 0
unique(vm_draws$comorbidity) # 84 
unique(tr_draws$comorbidity) # 84 



## Community - REMEMBER don't have draws from models ran in SAIL (too large to export)
# Split tr draws 
vm_draws <- vm_draws %>% 
  select(-estimate, -chain, -iteration, -estimate, -repo) %>% 
  separate(comorbidity, c("Var1", "Var2"), " : ", remove = FALSE)

# Check
unique(vm_draws$Var1) # 10
unique(vm_draws$Var2) # 10

# Add shortened omega 
vm_draws_unq <- vm_draws %>%
  mutate(om_short = str_remove_all(Omega, "Omega") %>% str_trim() %>% str_sub(2) %>% str_remove_all("\\[|\\]") %>% str_remove_all(","))

unique(vm_draws_unq$om_short)

# Distinct combos 
a <- combn(1:6, 2)
a <- apply(a, 2, function(x) paste(x, collapse = ""))

# Filter for distinct combos 
vm_draws_unq <- vm_draws_unq %>% 
  ungroup() %>%
  filter(om_short %in% a)



## Trials 
# Split tr draws 
tr_draws <- tr_draws %>% 
  select(-estimate, -chain, -iteration, -nct_id, -estimate, -repo) %>% 
  separate(comorbidity, c("Var1", "Var2"), " : ", remove = FALSE)

# Check
unique(tr_draws$Var1) # 10
unique(tr_draws$Var2) # 10

# Add shortened omega 
tr_draws_unique <- tr_draws_unique %>%
  mutate(om_short = str_remove(Omega, "Omega") %>% str_trim() %>% str_sub(2) %>% str_remove("\\."))

unique(tr_draws_unique$om_short) # 15

# Distinct combos 
a <- combn(1:6, 2)
a <- apply(a, 2, function(x) paste(x, collapse = ""))

# Filter for distinct combos 
tr_draws_unique <- tr_draws_unique %>% 
  ungroup() %>%
  filter(om_short %in% a)


# Trial unique draws 
unique(tr_draws_unique$comorbidity) # 44
unique(tr_draws_unique$Var1) # 7
unique(tr_draws_unique$Var2) # 10 

# Vm unique draws (no draws from models ran on sail only have omega summary)
unique(vm_draws_unq$comorbidity) # 43
unique(vm_draws_unq$Var1) # 7
unique(vm_draws_unq$Var2) # 10

# Difference? 
setdiff(unique(tr_draws_unique$comorbidity), unique(vm_draws_unq$comorbidity)) #  "asthma_COPD : arthritis"






