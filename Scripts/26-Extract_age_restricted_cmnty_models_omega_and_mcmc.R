#' ---
#' title: "26-Extract age restricted omega and MCMC"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script extracts the omega summaries (correlations) and MCMC draws from the  
# age restricted community models and saves them all in a single dataframe 



# Set up -----------------------------------------------------------------------

source("Scripts/00-Functions_and_packages.R")

set_cmdstan_path("/opt/cmdstanr/cmdstan-2.29.2")

# Data
fits <- list.files("Outputs/age_restricted_model_fits", full.names = TRUE)
cmnty <- read.csv("Created_metadata/SAIL_common_6_comorbidities.csv")



# Dataframe of only 1 corner of matrix -----------------------------------------

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


for (i in seq_along(fits)) {
  
 print(i)
  
  # Get index condition from file path 
  fit_path  <- fits[i]
  cond <- str_match(fit_path, "IPD_(.*?)_2022")[, 2]
  
  # Read model fit for index condition 
  fit <- readRDS(fit_path)

  # Omega summary
  om_sum <- fit$summary("Omega")
  
  # Extract omega estimates, q5, q95 and rhat individually
  omega  <- om_sum %>% select(mean) %>% mutate(mean = round(mean, 2))
  rhat   <- om_sum %>% select(rhat) %>% mutate(rhat = round(rhat, 3))
  std    <- om_sum %>% select(sd) %>% mutate(sd = round(sd, 3))
  ess_b  <- om_sum %>% select(ess_bulk) %>% mutate(ess_bulk = round(ess_bulk, 2))
  ess_t  <- om_sum %>% select(ess_tail) %>% mutate(ess_tail = round(ess_tail, 2))
  
  # Recalculate credible intervals using Omega draws to match Rstan output 
  om_draws <- as_draws_df(fit$draws("Omega")) %>% select(- c(.draw, .iteration, .chain))
  ci       <- map_df(om_draws, ~ quantile2(.x, probs = c(0.025, 0.975)))
  ci2.5    <- ci %>% select(q2.5) %>% mutate(q2.5 = round(q2.5, 3))
  ci97.5   <- ci %>% select(q97.5) %>% mutate(q97.5 = round(q97.5, 3))
  
  # Turn vector into 6x6 matrix with comorbidities as row/column names
  omega  <- as.matrix(omega, nrow = 6, ncol = 6)
  ci2.5  <- as.matrix(ci2.5, nrow = 6, ncol = 6)
  ci97.5 <- as.matrix(ci97.5, nrow = 6, ncol = 6)
  rhat   <- as.matrix(rhat, nrow = 6, ncol = 6)
  std    <- as.matrix(std, nrow = 6, ncol = 6)
  ess_b  <- as.matrix(ess_b, nrow = 6, ncol = 6)
  ess_t  <- as.matrix(ess_t, nrow = 6, ncol = 6)
  
  dim(omega)  <- c(6, 6)
  dim(ci2.5)  <- c(6, 6)
  dim(ci97.5) <- c(6, 6)
  dim(rhat)   <- c(6, 6)
  dim(std)    <- c(6, 6)
  dim(ess_b)  <- c(6, 6)
  dim(ess_t)  <- c(6, 6)
  
  # Get como names for index condition
  como_names <- get_como_names(cond, lk_up)
  
  # Give matrix dimensions correct como names
  dimnames(omega)  <- list(como_names, como_names)
  dimnames(ci2.5)  <- list(como_names, como_names)
  dimnames(ci97.5) <- list(como_names, como_names)
  dimnames(rhat)   <- list(como_names, como_names)
  dimnames(std)    <- list(como_names, como_names)
  dimnames(ess_b)  <- list(como_names, como_names)
  dimnames(ess_t)  <- list(como_names, como_names)
  
  # Melt
  omega   <- melt(omega, value.name = "mean")
  ci2.5   <- melt(ci2.5, value.name = "q2.5")
  ci97.5  <- melt(ci97.5, value.name = "q97.5")
  rhat    <- melt(rhat, value.name = "rhat")
  std     <- melt(std, value.name = "sd")
  ess_b   <- melt(ess_b, value.name = "ess_bulk")
  ess_t   <- melt(ess_t, value.name = "ess_tail")
  
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
  
  rhat$Var1 <- as.character(rhat$Var1)
  rhat$Var2 <- as.character(rhat$Var2)
  rhat      <- as_tibble(rhat)
  
  std$Var1 <- as.character(std$Var1)
  std$Var2 <- as.character(std$Var2)
  std      <- as_tibble(std)
  
  ess_b$Var1 <- as.character(ess_b$Var1)
  ess_b$Var2 <- as.character(ess_b$Var2)
  ess_b      <- as_tibble(ess_b)
  
  ess_t$Var1 <- as.character(ess_t$Var1)
  ess_t$Var2 <- as.character(ess_t$Var2)
  ess_t      <- as_tibble(ess_t)
  
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
  
  rhat_2 <- rhat %>% 
    mutate(var1_new = pmax(Var1, Var2), 
           var2_new = pmin(Var1, Var2)) %>%
    select(-Var1, -Var2) %>% 
    rename(Var1 = var1_new, 
           Var2 = var2_new) %>%
    distinct()
  
  std_2 <- std %>% 
    mutate(var1_new = pmax(Var1, Var2), 
           var2_new = pmin(Var1, Var2)) %>%
    select(-Var1, -Var2) %>% 
    rename(Var1 = var1_new, 
           Var2 = var2_new) %>%
    distinct()
  
  ess_b_2 <- ess_b %>% 
    mutate(var1_new = pmax(Var1, Var2), 
           var2_new = pmin(Var1, Var2)) %>%
    select(-Var1, -Var2) %>% 
    rename(Var1 = var1_new, 
           Var2 = var2_new) %>%
    distinct()
  
  ess_t_2 <- ess_t %>% 
    mutate(var1_new = pmax(Var1, Var2), 
           var2_new = pmin(Var1, Var2)) %>%
    select(-Var1, -Var2) %>% 
    rename(Var1 = var1_new, 
           Var2 = var2_new) %>%
    distinct()
  
  # Join rows
  df <- omega_2 %>% 
    inner_join(ci2.5_2, by = c("Var1", "Var2")) %>% 
    inner_join(ci97.5_2, by = c("Var1", "Var2")) %>%
    inner_join(rhat_2, by = c("Var1", "Var2")) %>%
    inner_join(std_2, by = c("Var1", "Var2")) %>%
    inner_join(ess_b_2, by = c("Var1", "Var2")) %>%
    inner_join(ess_t_2, by = c("Var1", "Var2")) %>%
    mutate(condition = cond,
           comorbidity =  paste0(Var1, " : ", Var2)) 
  # Bind rows
  m_df <- rbind(m_df, df)
}

# Add id column 
m_df <- m_df %>% mutate(repo = "Age restricted community")

# Checks
unique(m_df$condition)

# Save 
write_csv(m_df, "Outputs/VM_age_restricted_community_omega_summaries.csv")



# Extract omega MCMC samples and combine into single dataframe -----------------

# Bind rows to this 
om_draws_df <- data.frame()


for (i in seq_along(fits)) {
  
  print(i)
  
  # Read in trial model fit 
  cond_fit <- readRDS(fits[i])
  
  # Get index condition name 
  condition_name <- str_match(fits[i], "IPD_(.*?)_2022")[, 2]
  
  # Index condition comorbidities
  ind_common_como <- cmnty %>%
    filter(condition %in% condition_name) %>% 
    distinct(como) %>% 
    unlist() %>% 
    as.character()
  
  # Comorbidity combination
  como_combo <- expand.grid(ind_common_como, ind_common_como) %>% 
    mutate(combo = paste0(Var1, " : ", Var2))
  
  # Omega draws as a dataframe 
  om_draws <- as_draws_df(cond_fit$draws("Omega"))
  
  # Into long format, one parameter per row 
  om_draws_long <- om_draws %>%
    pivot_longer("Omega[1,1]":"Omega[6,6]", "Omega", values_to = "estimate")
  
  om_draws_long <- om_draws_long %>% 
    select(-.draw) %>%
    rename(chain = .chain,
           iteration = .iteration) %>%
    mutate(condition = condition_name,
           comorbidity = rep(como_combo$combo, times = 3000))
  
  # Bind rows
  om_draws_df <- rbind(om_draws_df, om_draws_long)
}

# Add repo column
om_draws_df <- om_draws_df %>% mutate(repo = "Age restricted community")

unique(om_draws_df$condition)
11*108000
NROW(om_draws_df$chain)

# Save 
saveRDS(om_draws_df, "Outputs/VM_age_restricted_community_omega_draws.RDS")
