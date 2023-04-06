# 16_Extract_GSK_omega_summaries_and_mcmc

# Extract omega summary for each GSK trial and combine into single dataframe



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

# Data
gsk_como   <- read.csv("GSK_processed_data/cleaned_gsk_conmed_defined_comorbidities.csv", row.names = 1)
cmnty_como <- read.csv("Supporting/Prepare_trial_data/GSK/Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv", row.names = 1)
fit_paths  <- list.files("Results/model_fits/", full.names = TRUE)#[Add indexes to remove non model fits] 



# Dataframe of only 1 corner of matrix -----------------------------------------


# Trials and conditions
trials     <- unique(gsk_como$trial)
conditions <- unique(gsk_como$condition)

# Look up table to correct comorbidity names
lk_up <- c("Cardiovascular" = "CV",
           "Pain" = "pain",
           "Arthritis" = "arthritis",
           "Acid-related disease" = "antacids",
           "Asthma" = "asthma_COPD",
           "Anxiety" = "anxiety", 
           "Schizophrenia" = "schizophrenia",
           "Thyroid disorders" = "thyroid",
           "Inflammatory disorders" = "inflammatory",
           "Diabetes" = "diabetes",
           "Osteoporosis" = "osteoporosis")



m_df <- data.frame()

for (i in seq_along(trials)) {

# Get trial and index condition from file path 
trial_fit_path  <- fit_paths[i]
tr              <- trials[str_detect(trial_fit_path, trials)]

# Get trial IPD then condition 
trial_data <- gsk_como %>% filter(trial %in% tr)
trial_condition <- unique(trial_data$condition)

# Get 6 most common comorbidities in community IPD for trial index condition
trial_common_como <- cmnty_como %>%
  filter(condition %in% unique(trial_data$condition)) %>%
  distinct(como) %>% 
  unlist() %>% 
  as.character() 

# Limit to 6 common comorbidities then rename to full label for plotting
trial_data <- trial_data %>%
  select(all_of(trial_common_como)) %>% 
  rename(any_of(lk_up))

# Extract names  
correct_como_names <- colnames(trial_data)

# Read model fit for trial 
fit <- readRDS(trial_fit_path)

# Omega summary
om_sum <- as.data.frame(fit$summary)

# Individually extract summary terms 
omega  <- round(om_sum[1:36, "mean"], 2)  # Extract omega correlation matrix
ci2.5  <- round(om_sum[1:36, "2.5%"], 3)
ci97.5 <- round(om_sum[1:36, "97.5%"], 3) # Extract omega 95% CI
sd     <- round(om_sum[1:36, "sd"], 3)    # Extract sd
n_eff  <- round(om_sum[1:36, "n_eff"], 2) # Extract n_eff
rhat   <- round(om_sum[1:36, "Rhat"], 3)  # Extract Rhat

# Turn vector into 6x6 matrix 
dim(omega)  <- c(6, 6)
dim(ci2.5)  <- c(6, 6)
dim(ci97.5) <- c(6, 6)
dim(sd)     <- c(6, 6)
dim(n_eff)  <- c(6, 6)
dim(rhat)   <- c(6, 6)

# Comorbidities as row/column names
dimnames(omega)  <- list(correct_como_names, correct_como_names)
dimnames(ci2.5)  <- list(correct_como_names, correct_como_names)
dimnames(ci97.5) <- list(correct_como_names, correct_como_names)
dimnames(sd)     <- list(correct_como_names, correct_como_names)
dimnames(n_eff)  <- list(correct_como_names, correct_como_names)
dimnames(rhat)   <- list(correct_como_names, correct_como_names)

# Melt
omega  <- melt(omega, value.name = "mean")
ci2.5  <- melt(ci2.5, value.name = "CI2.5")
ci97.5 <- melt(ci97.5, value.name = "CI97.5")
sd     <- melt(sd, value.name = "sd")
n_eff  <- melt(n_eff, value.name = "n_eff")
rhat   <- melt(rhat, value.name = "rhat")

# Make var 1 and 2 characters instead of factors 
omega$Var1 <- as.character(omega$Var1)
omega$Var2 <- as.character(omega$Var2)
omega      <- as_tibble(omega)

ci2.5$Var1 <- as.character(ci2.5$Var1)
ci2.5$Var2 <- as.character(ci2.5$Var2)
ci2.5      <- as_tibble(ci2.5)

ci97.5$Var1 <- as.character(ci97.5$Var1)
ci97.5$Var2 <- as.character(ci97.5$Var2)
ci97.5      <- as_tibble(ci97.5)

sd$Var1 <- as.character(sd$Var1)
sd$Var2 <- as.character(sd$Var2)
sd      <- as_tibble(sd)

n_eff$Var1 <- as.character(n_eff$Var1)
n_eff$Var2 <- as.character(n_eff$Var2)
n_eff      <- as_tibble(n_eff)

rhat$Var1 <- as.character(rhat$Var1)
rhat$Var2 <- as.character(rhat$Var2)
rhat      <- as_tibble(rhat)

# Reduce to unique combinations
omega_2 <- omega %>% 
  mutate(var1_new = pmax(Var1, Var2), 
         var2_new = pmin(Var1, Var2)) %>%
  select(-Var1, -Var2) %>% 
  rename(Var1 = var1_new, 
         Var2 = var2_new) %>%
  distinct()

ci2.5_2 <- ci2.5 %>% 
  mutate(var1_new = pmax(Var1, Var2), 
         var2_new = pmin(Var1, Var2)) %>%
  select(-Var1, -Var2) %>% 
  rename(Var1 = var1_new, 
         Var2 = var2_new) %>%
  distinct()

ci97.5_2 <- ci97.5 %>% 
  mutate(var1_new = pmax(Var1, Var2), 
         var2_new = pmin(Var1, Var2)) %>%
  select(-Var1, -Var2) %>% 
  rename(Var1 = var1_new, 
         Var2 = var2_new) %>%
  distinct()

sd_2 <- sd %>% 
  mutate(var1_new = pmax(Var1, Var2), 
         var2_new = pmin(Var1, Var2)) %>%
  select(-Var1, -Var2) %>% 
  rename(Var1 = var1_new, 
         Var2 = var2_new) %>%
  distinct()

n_eff_2 <- n_eff %>% 
  mutate(var1_new = pmax(Var1, Var2), 
         var2_new = pmin(Var1, Var2)) %>%
  select(-Var1, -Var2) %>% 
  rename(Var1 = var1_new, 
         Var2 = var2_new) %>%
  distinct()

rhat_2 <- rhat %>% 
  mutate(var1_new = pmax(Var1, Var2), 
         var2_new = pmin(Var1, Var2)) %>%
  select(-Var1, -Var2) %>% 
  rename(Var1 = var1_new, 
         Var2 = var2_new) %>%
  distinct()

# Add columns 
df <- omega_2 %>% 
  inner_join(ci2.5_2, by = c("Var1", "Var2")) %>% 
  inner_join(ci97.5_2, by = c("Var1", "Var2")) %>%
  inner_join(sd_2, by = c("Var1", "Var2")) %>%
  inner_join(n_eff_2, by = c("Var1", "Var2")) %>%
  inner_join(rhat_2, by = c("Var1", "Var2")) %>%
  mutate(trial = tr,
         condition = trial_condition,
         comorbidity =  paste0(Var1, " : ", Var2)) 

# Bind rows
m_df <- rbind(m_df, df)
}

# Add identifying repo column 
m_df <- m_df %>% mutate(repo = "GSK")

# Save 
write_csv(m_df, "Results/GSK_trial_omega_summaries.csv")




# ------------------------------------------------------------------------------




##  Extract omega MCMC samples and combine into 1 dataframe  ##

chains <- rep(1:3, each = 1000)
iter <- rep(1:1000, times = 3)
om_draws_df <- data.frame()

for (i in seq_along(trials)) {
  
  # Read in trial model fit 
  trial_fit <- readRDS(fit_paths[i])
  
  # Get trial name 
  trial_name <- trials[str_detect(fit_paths[i], trials)]
  
  # Get trial IPD then nct id and condiiton 
  trial_data <- gsk_como %>% filter(trial %in% trial_name)
  trial_id <- unique(trial_data$nct_id)
  condition_name <- unique(trial_data$condition)
  
  # Get 6 most common comorbidities in community IPD for trial index condition
  trial_common_como <- cmnty_como %>%
    filter(condition %in% condition_name) %>%
    distinct(como) %>% 
    unlist() %>% 
    as.character() 
  
  # Comorbidity combination
  como_combo <- expand.grid(trial_common_como, trial_common_como) %>% 
    mutate(combo = paste0(Var1, " : ", Var2))
 
  # Omega draws as a dataframe 
  om_draws <- as.data.frame(trial_fit$omega_draws)
  
  # Add chains, trial and condition 
  om_draws <- om_draws %>%
    mutate(chain = chains,
           iteration = iter,
           trial = trial_name,
           condition = condition_name,
           nct_id = trial_id)
  
  # Gather omega estimates into 1 column
  om_draws_long <- om_draws %>%
    pivot_longer(Omega.1.1:Omega.6.6, "Omega", values_to = "estimate") 
  
  # Add comorbidity combination 
  om_draws_long <- om_draws_long %>%
    mutate(comorbidity =  rep(como_combo$combo, length(om_draws_long$chain)/36))
  
  # Bind rows
  om_draws_df <- rbind(om_draws_df, om_draws_long)
}

# Add identifying column
om_draws_df <- om_draws_df %>% mutate(repo = "GSK")

# Save 
saveRDS(om_draws_df, "Results/GSK_omega_draws.RDS")
