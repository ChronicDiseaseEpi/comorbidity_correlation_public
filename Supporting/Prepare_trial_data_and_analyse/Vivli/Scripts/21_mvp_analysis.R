# 21 mvp analysis

# run MVP model on each trial

# Packages/ functions
source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

# Data
como        <- read.csv("Processed_data/cleaned_conmed_defined_comorbidity.csv", row.names = 1)
common_como <- read.csv("Supporting/Prepare_trial_data/Vivli/Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv", row.names = 1)

# Restrict to community index conditions
como <- como %>% filter(condition %in% common_como$condition)

# Get trials with single sex only - analyse separately
xmn <- como %>% 
  group_by(condition, trial, sex) %>% 
  summarise(n = n())

dup <- duplicated(xmn$trial)
dup <- xmn[dup, "trial"]
both_sex <- dup$trial



# Run MVP model on trials with both sexes --------------------------------------

#for (i in seq_along(both_sex)) {
  
  print(i)
  
  # Select trial
  tr <- como %>% filter(trial %in% both_sex[i]) 
  
  # Remove NAs
  na <- is.na(tr$age)
  tr <- tr[!na, ]

  # Restrict comorbidities to 6 most common in community IPD for given index condition
  limit_comos <- common_como %>%
    filter(condition %in% unique(tr$condition)) %>%
    distinct(como) %>%
    unlist() %>%
    as.character()

  # Limit comorbidities then make binary
  response   <- tr[limit_comos]
  response[] <- if_else(response == TRUE, 1L, 0L)

  # Add intercept and make sex binary
  tr <- tr %>%
    mutate(interc = 1,
           sex    = if_else(sex == "male", 1L, 0L))

  # Stan formatted data 
  data_list <- list("D" = 6,         # Number of comorbidities
                    "K" = 3,         # Number of covariates (Intercept, sex, age)
                    "N" = nrow(tr),  # Number of trial participants
                    "y" = as.matrix(response), # Comorbidities
                    "x" = as.matrix(tr[, c("interc", "sex", "age")])) # Design matrix

  # Sample posterior 
  rstan_options(auto_write = TRUE)
  ctrl <- list(adapt_delta = 0.95)

  run_time <- system.time(stan_fit <- stan(file = "Supporting/Prepare_trial_data/Vivli/Scripts/MVP_IPD_Rstan.stan",
                   data    = data_list,
                   chains  = 3,
                   init_r  = 0.1,
                   control = ctrl,
                   cores   = 3,
                   seed    = 123))

  # Extract results
  omega_draws <- extract(stan_fit, pars = "Omega")
  beta_draws  <- extract(stan_fit, pars = "beta")
  summary     <- summary(stan_fit, pars = c("Omega", "beta"))$summary
  samp_params <- get_sampler_params(stan_fit)

  # Print diagnostics to console 
  check_hmc_diagnostics(stan_fit)

  # list of results
  fit_output <- list(omega_draws    = omega_draws, 
                     beta_draws     = beta_draws,
                     summary        = summary,
                     sampler_params = samp_params,
                     run_time       = run_time)

  # Save 
  fldrnm <- "Results/model_fits/"
  if(!dir.exists(fldrnm)) dir.create(fldrnm, recursive = TRUE)

  saveRDS(fit_output, file = paste0(fldrnm,
                                    unique(tr$trial),
                                    "_",
                                    unique(tr$condition),
                                    "_",
                                    stringr::str_remove_all(Sys.Date(), "-"),
                                    ".RDS"))

  print(paste0(unique(tr$trial),
               " ",
               unique(tr$condition), 
               " has finished sampling and output has been saved in ",
               fldrnm, 
               " on ", 
               Sys.time()))
}




# Run MVP model on trials with single sexes ------------------------------------

single_sex <- como %>% filter(!trial %in% both_sex) %>% distinct(trial)
single_sex <- c(single_sex$trial)

for (i in seq_along(single_sex)) {
  
  print(i)
  
  # Select trial
  tr <- como %>% filter(trial %in% single_sex[i]) 
  
  # Remove NAs
  na <- is.na(tr$age)
  tr <- tr[!na, ]
  
  # Restrict comorbidities to 6 most common in community IPD for given index condition
  limit_comos <- common_como %>%
    filter(condition %in% unique(tr$condition)) %>%
    distinct(como) %>%
    unlist() %>%
    as.character()
  
  # Limit comorbidities then make binary
  response   <- tr[limit_comos]
  response[] <- if_else(response == TRUE, 1L, 0L)
  
  # Add intercept
  tr <- tr %>% mutate(interc = 1)
  
  # Stan formatted data 
  data_list <- list("D" = 6,         # Number of comorbidities
                    "K" = 2,         # Number of covariates (Intercept, age) - NO SEX AS SINGLE SEX TRIALS
                    "N" = nrow(tr),  # Number of trial participants
                    "y" = as.matrix(response), # Comorbidities
                    "x" = as.matrix(tr[, c("interc", "age")])) # Design matrix
  
  # Sample posterior 
  rstan_options(auto_write = TRUE)
  ctrl <- list(adapt_delta = 0.95)
  
  run_time <- system.time(stan_fit <- stan(file = "Scripts/MVP_IPD_Rstan.stan",
                                           data    = data_list,
                                           chains  = 3,
                                           init_r  = 0.1,
                                           control = ctrl,
                                           cores   = 3,
                                           seed    = 123))
  
  # Extract results
  omega_draws <- extract(stan_fit, pars = "Omega")
  beta_draws  <- extract(stan_fit, pars = "beta")
  summary     <- summary(stan_fit, pars = c("Omega", "beta"))$summary
  samp_params <- get_sampler_params(stan_fit)
  
  # Print diagnostics to console 
  check_hmc_diagnostics(stan_fit)
  
  # list of results
  fit_output <- list(omega_draws    = omega_draws, 
                     beta_draws     = beta_draws,
                     summary        = summary,
                     sampler_params = samp_params,
                     run_time       = run_time)
  
  # Save 
  fldrnm <- "Results/model_fits/"
  if(!dir.exists(fldrnm)) dir.create(fldrnm, recursive = TRUE)
  
  saveRDS(fit_output, file = paste0(fldrnm,
                                    unique(tr$trial),
                                    "_",
                                    unique(tr$condition),
                                    "_",
                                    stringr::str_remove_all(Sys.Date(), "-"),
                                    ".RDS"))
  
  print(paste0(unique(tr$trial),
               " ",
               unique(tr$condition), 
               " has finished sampling and output has been saved in ",
               fldrnm, 
               " on ", 
               Sys.time()))
}






# ------------------------------------------------------------------------------

# Re run 2 models where diagnostics indicated potential issues 
# BI502_316 has 3% divergent transitions
# TKASYR322_402 has low bulk and tail ess and one parameter did have high rhat 


tr <- como %>%
  filter(trial %in% "BI502_316")

unique(tr$sex) # both sexes present

# Remove NAs
na <- is.na(tr$age)
tr <- tr[!na, ]

# Restrict comorbidities to 6 most common in community IPD for given index condition
limit_comos <- common_como %>%
  filter(condition %in% unique(tr$condition)) %>%
  distinct(como) %>%
  unlist() %>%
  as.character()

# Limit comorbidities then make binary
response   <- tr[limit_comos]
response[] <- if_else(response == TRUE, 1L, 0L)

# Add intercept and make sex binary
tr <- tr %>%
  mutate(interc = 1,
         sex    = if_else(sex == "male", 1L, 0L))

# Stan formatted data 
data_list <- list("D" = 6,         # Number of comorbidities
                  "K" = 3,         # Number of covariates (Intercept, sex, age)
                  "N" = nrow(tr),  # Number of trial participants
                  "y" = as.matrix(response), # Comorbidities
                  "x" = as.matrix(tr[, c("interc", "sex", "age")])) # Design matrix

# Sample posterior 
rstan_options(auto_write = TRUE)
ctrl <- list(adapt_delta = 0.99, max_treedepth = 12) # Increased adapt delta and treedepth

run_time <- system.time(stan_fit <- stan(file = "Scripts/MVP_IPD_Rstan.stan",
                                         data    = data_list,
                                         chains  = 3,
                                         init_r  = 0.1,
                                         control = ctrl,
                                         cores   = 3,
                                         seed    = 123))

# Extract results
omega_draws <- extract(stan_fit, pars = "Omega")
beta_draws  <- extract(stan_fit, pars = "beta")
summary     <- summary(stan_fit, pars = c("Omega", "beta"))$summary
samp_params <- get_sampler_params(stan_fit)

# Print diagnostics to console 
check_hmc_diagnostics(stan_fit)

# list of results
fit_output <- list(omega_draws    = omega_draws, 
                   beta_draws     = beta_draws,
                   summary        = summary,
                   sampler_params = samp_params,
                   run_time       = run_time)

# Save 
fldrnm <- "Results/model_fits/"
if(!dir.exists(fldrnm)) dir.create(fldrnm, recursive = TRUE)

saveRDS(fit_output, file = paste0(fldrnm,
                                  unique(tr$trial),
                                  "_",
                                  unique(tr$condition),
                                  "_run2_",
                                  stringr::str_remove_all(Sys.Date(), "-"),
                                  ".RDS"))

print(paste0(unique(tr$trial),
             " ",
             unique(tr$condition), 
             " has finished sampling and output has been saved in ",
             fldrnm, 
             " on ", 
             Sys.time()))

# Original BI502_316 fit deleted

# Could not get re-run of TKASYR322_402 to save (too large with increased number of sampling iterations), 
# hence original fit deleted and not part of community-trial comparison.

# Re requested export of omega draws and summary, age and trial distributions on 22/11/22

