# 15_Run_MVP_analysis

# This script runs the MVP model on each GSK trial whose index condition is
# matched in the community (SAIL) data. 



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

# Data 
gsk_como <- read.csv("Processed_data/cleaned_gsk_conmed_defined_comorbidities.csv", row.names = 1)
cmnty_como <- read.csv("Supporting/Prepare_trial_data/GSK/Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv", row.names = 1)



# Run MVP model on trials with both sexes --------------------------------------

trials <- unique(gsk_como$trial)

#for (i in seq_along(trials)) {
  
  # select trial
  tr <- gsk_como %>% filter(trial %in% trials[i])
  
  # Restrict comorbidities to 6 most common in community IPD for given index condition
  limit_gsk_como <- cmnty_como %>%
    filter(condition %in% unique(tr$condition)) %>%
    distinct(como) %>% 
    unlist() %>% 
    as.character()
  
  # Limit comorbidities then make binary 
  response   <- tr[limit_gsk_como]
  response[] <- if_else(response == TRUE, 1L, 0L)
  
  # Add intercept and make sex binary 
  tr <- tr %>%
    mutate(interc = 1,
           sex = if_else(sex == "male", 1L, 0L))
  
  # Stan formatted data 
  data_list <- list("D" = 6,
                    "K" = 3,
                    "N" = nrow(tr),
                    "y" = as.matrix(response),
                    "x" = as.matrix(tr[, c("interc", "sex", "age")]))
  
  # Sample posterior
  rstan_options(auto_write = TRUE)
  ctrl <- list(adapt_delta = 0.95)
  
  run_time <- system.time(stan_fit <- stan(file    = "Scripts/MVP_IPD_Rstan.stan",
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
  warn        <- warnings()
  
  # Print diagnostics to console 
  check_hmc_diagnostics(stan_fit)
  
  # Save list of results 
  fit_output <- list(omega_draws    = omega_draws,
                     beta_draws     = beta_draws,
                     summary        = summary,
                     sampler_params = samp_params,
                     run_time       = run_time,
                     warning        = warn)
  
  # Save 
  fldrnm <- "Results/"
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
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------



# RE-RUN MODELS AND SAVE FULL FIT OBJECT # 



trials <- unique(gsk_como$trial)

for (i in seq_along(trials)) {
  
  # select trial
  tr <- gsk_como %>% filter(trial %in% trials[i])
  
  # Restrict comorbidities to 6 most common in community IPD for given index condition
  limit_gsk_como <- cmnty_como %>%
    filter(condition %in% unique(tr$condition)) %>%
    distinct(como) %>% 
    unlist() %>% 
    as.character()
  
  # Limit comorbidities then make binary 
  response   <- tr[limit_gsk_como]
  response[] <- if_else(response == TRUE, 1L, 0L)
  
  # Add intercept and make sex binary 
  tr <- tr %>%
    mutate(interc = 1,
           sex = if_else(sex == "male", 1L, 0L))
  
  # Stan formatted data 
  data_list <- list("D" = 6,
                    "K" = 3,
                    "N" = nrow(tr),
                    "y" = as.matrix(response),
                    "x" = as.matrix(tr[, c("interc", "sex", "age")]))
  
  # Sample posterior
  rstan_options(auto_write = TRUE)
  ctrl <- list(adapt_delta = 0.95)
  
  run_time <- system.time(stan_fit <- stan(file    = "Scripts/MVP_IPD_Rstan.stan",
                                           data    = data_list,
                                           chains  = 3,
                                           init_r  = 0.1,
                                           control = ctrl,
                                           cores   = 3,
                                           seed    = 123))
  
  # Extract results 
  omega_draws <- extract(stan_fit, pars = "Omega")
  beta_draws <- extract(stan_fit, pars = "beta")
  summary <- summary(stan_fit, pars = c("Omega", "beta"))$summary
  sampler_params <- get_sampler_params(stan_fit)
  
  # List results 
  fit_output <- list(omega_draws, beta_draws, summary, sampler_params)
  
  # Print diagnostics to console 
  check_hmc_diagnostics(stan_fit)
  
  # Save 
  fldrnm <- "Results/"
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




