#' ---
#' title: "33: Prepare omega correlation difference dataframe"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This code adds the weighted draws for trials to the community draws and takes the 
# difference between the 2 so we have a mean difference and associated error of that estimate
# for plotting the difference between cmnty-trial correlations. 



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
w_draws   <- readRDS("Outputs/trial_meta_analysis_weighted_draws.rds")
vm_draws  <- readRDS("Outputs/VM_community_omega_mcmc_draws.RDS")
sail_sum  <- read_csv("Outputs/model_summaries_from_SAIL.csv")
cmnty_sum <- read_csv("Outputs/all_community_omega_summaries.csv")
com_como  <- read.csv("Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv", row.names = 1)



# Simulate omega draw estimates from SAIL --------------------------------------

# Age restricted SAIL 
sail_sum <- sail_sum %>% 
  filter(model_type %in% "agerestricted") %>% 
  select(condition, model_type, params, mean, sd)

iter   <- 1000
chains <- 3
conds  <- as.numeric(n_distinct(sail_sum$condition))
omegas <- as.numeric(n_distinct(sail_sum$params))

# Expand dataframe to correct length and ARRANGE
sail_draws <- map_dfr(seq_len(chains*iter), ~ sail_sum)
sail_draws <- sail_draws %>% arrange(condition)

# Add chain and iteration cols
omgit <- omegas*iter
sail_draws <- sail_draws %>% 
  mutate(iteration = rep(1:1000, times = chains*conds, each = omegas),
         chain = rep(1:3, times = conds, each = omgit))

# Add simulate mean estimate from omega summary mean/sd 
set.seed(123)
sail_draws$estimate <- rtruncnorm(nrow(sail_draws), a = -1, b = 1, mean = sail_draws$mean, sd = sail_draws$sd)

# Harmonise df names 
sail_draws <- sail_draws %>%
  rename(Omega = params) %>%
  select(-model_type, -mean, -sd) 

# Harmonise conditions
unique(sail_draws$condition)

sail_draws <- sail_draws %>%
  mutate(condition = case_when(condition == "Hypertension, Pulmonary" ~ "Pulmonary_Hypertension",
                               condition == "Parkinson's disease (all)" ~ "Parkinson_Disease",
                               condition == "Psoriatic arthropathy" ~ "Psoriatic_arthritis",
                               TRUE ~ as.character(condition)),
         condition = str_replace_all(condition, " ", "_"))

# Filter como prop to 5 conditions in sail 
com_como <- com_como %>% 
  filter(condition %in% unique(sail_draws$condition)) %>%
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

# Omega short column 
sail_draws <- sail_draws %>% 
  mutate(om_short = Omega %>% str_remove_all("Omega") %>% str_remove_all("\\[|\\]") %>% str_remove_all(","))

# Split omega 
sail_draws <- sail_draws %>%
  separate(om_short, into = c("rows", "cols"), sep = 1) 

# Add comos 
sail_draws <- sail_draws %>%
  left_join(com_como %>% select(condition, new_como, common_n), by = c("condition", "rows" = "common_n")) %>%
  left_join(com_como %>% select(condition, new_como2, common_n), by = c("condition", "cols" = "common_n")); warning("This step relies on como in com_como being ordered in descending prevalence within each index condition")

# Create comorbidty column and remove unnecessary columns 
sail_draws <- sail_draws %>%
  unite("comorbidity", c(new_como, new_como2), sep = " : ", remove = FALSE) %>%
  select(-rows, -cols, -new_como, -new_como2)

# Save simulated SAIL draws
saveRDS(sail_draws, "Outputs/simulated_SAIL_omega_draws.rds")



# Clean VM draws ---------------------------------------------------------------

# Age restricted
vm_draws <- vm_draws %>% filter(repo %in% "Age restricted community")

# Correct como names 
vm_draws <- vm_draws %>%
  separate(comorbidity, into = c("old_como", "Var2"), sep = " : ", remove = FALSE)

vm_draws <- vm_draws %>% 
  inner_join(lk_up, by = "old_como")

vm_draws <- vm_draws %>%
  select(-old_como) %>%
  rename(old_como = Var2,
         Var1 = new_como) %>%
  inner_join(lk_up, by = "old_como") %>%
  select(-old_como) %>%
  rename(Var2 = new_como)

vm_draws <- vm_draws %>%
  select(-comorbidity) %>%
  unite("comorbidity", c(Var1, Var2), sep = " : ")

# Unique correlations only 
vm_draws <- vm_draws %>% 
  mutate(om_short = Omega %>% str_remove_all("Omega") %>% str_remove_all("\\[|\\]") %>% str_remove_all(","))

a <- combn(1:6, 2)
a <- apply(a, 2, function(x) paste(x, collapse = ""))

vm_draws <- vm_draws %>% 
  filter(om_short %in% a) %>% 
  select(-om_short, -repo)

# Join vm and sail draws 
cmnty_draws <- vm_draws %>% 
  bind_rows(sail_draws) %>% 
  arrange(condition, Omega)

# Harmonise omega 
cmnty_draws <- cmnty_draws %>% 
  mutate(Omega = Omega %>% str_remove_all("\\[|\\]") %>% str_remove_all(","))






# Clean trial weighted draws ---------------------------------------------------

# Unique correlations only 
w_draws <- w_draws %>%
  mutate(om_short = Omega %>% str_remove_all("Omega") %>% str_remove_all("\\."))

w_draws <- w_draws %>% filter(om_short %in% a)

# Harmonise comorbidities 
w_draws <- w_draws %>%
  separate(comorbidity, c("Var1", "Var2"), " : ")

# Use look up to add new comos and remove old 
w_draws <- w_draws %>% 
  inner_join(lk_up, by = c("Var1" = "old_como")) %>%
  select(-Var1) %>%
  rename(Var1 = new_como) %>%
  inner_join(lk_up, by = c("Var2" = "old_como")) %>%
  select(-Var2) %>%
  rename(Var2 = new_como)

# Unite Var1 and Var2 back into comorbidity combo
w_draws <- w_draws %>%
  unite("comorbidity", c(Var1, Var2), sep = " : ") 

# Harmonise mega 
w_draws <- w_draws %>% 
  mutate(Omega = Omega %>% str_remove_all("\\.")) %>% 
  select(-om_short)





# Join draws df and make difference table --------------------------------------

# Join trials and community 
d_draws <- w_draws %>% inner_join(cmnty_draws)

# Take absolute difference and difference 
d_draws <- d_draws %>% mutate(diff = estimate - w_estimate,
                              abs_diff = abs(estimate - w_estimate))

# Weighted estimate for omega parameters for each condition
d_draws_sum <- d_draws %>%
  group_by(condition, Omega, comorbidity) %>% 
  summarise(across(diff, list(diff_mean = mean, 
                              diff_sd = sd,
                              CI2.5 = ~ quantile2(.x, probs = 0.025),
                              CI97.5 = ~ quantile2(.x, probs = 0.975)), .names = "{.fn}"),
            across(abs_diff, list(abs_diff_mean = mean, 
                              abs_diff_sd = sd,
                              abs_CI2.5 = ~ quantile2(.x, probs = 0.025),
                              abs_CI97.5 = ~ quantile2(.x, probs = 0.975)), .names = "{.fn}"),
            tr_est = mean(w_estimate),
            cmnty_est = mean(estimate)) %>% 
  mutate(across(diff_mean:abs_CI97.5, ~ round(.x, 2))) %>%
  ungroup()

# Add column for flipped correlation
d_draws_sum <- d_draws_sum %>% 
  mutate(test_tr = tr_est/abs(tr_est),
         test_cmnty = cmnty_est/abs(cmnty_est),
         chk = test_tr + test_cmnty,
         cor_flip = if_else(chk == 0, "Flipped", "Same"))

# Remove unnecessary cols
d_draws_sum <- d_draws_sum %>% 
  select(-test_tr, -test_cmnty, -chk, -tr_est, -cmnty_est)

# Organise comorbidity combiantions alphabetically 
d_draws_sum <- d_draws_sum %>% 
  separate(comorbidity, c("Var1", "Var2"), sep = " : ", remove = FALSE)

d_draws_sum <- d_draws_sum %>% 
  mutate(new_var1 = pmin(Var1, Var2),
         new_var2 = pmax(Var1, Var2))

d_draws_sum <- d_draws_sum %>% 
  unite("new_comorbidity", c(new_var1, new_var2), sep = " : ", remove = FALSE)

# Checks
unique(d_draws_sum$condition)      # 16
unique(d_draws_sum$Var1)           # 7
unique(d_draws_sum$Var2)           # 10
unique(d_draws_sum$comorbidity)    # 44
length(d_draws_sum$condition)      # 240 (16*15)
map(d_draws_sum, ~ sum(is.na(.x))) # 0

unique(d_draws_sum$new_var1) # 8
unique(d_draws_sum$new_var2) # 9
unique(d_draws_sum$new_comorbidity) # 37

# Clean columns 
d_draws_sum <- d_draws_sum %>% 
  select(-Var1, - Var2, -comorbidity) %>%
  rename(Var1 = new_var1,
         Var2 = new_var2, 
         comorbidity = new_comorbidity)

# Save
write_csv(d_draws_sum, "Outputs/trial-community_correlation_differences.csv")

