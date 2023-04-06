# 17_Model_diagnostics

# check diagnostics of GSK MVP model runs 



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

gsk_como <- read_csv("GSK_processed_data/cleaned_gsk_conmed_defined_comorbidities.csv")
om_sum   <- read_csv("Supporting/Prepare_trial_data/GSK/Results/GSK_trial_omega_summaries.csv")
om_draws <- readRDS("Supporting/Prepare_trial_data/GSK/Results/GSK_omega_draws.RDS")

# Full model fit for each trial 
fits   <- list.files("Results/model_fits/", full.names = TRUE)[-1]
trials <- unique(gsk_como$trial)
tr_names <- map(fits, ~ trials[str_detect(.x, trials)])
fits <- map(fits, ~ readRDS(.x))
names(fits) <- tr_names



# Examine rhat -----------------------------------------------------------------

# Rhat should be below 1.1 (extreme 1.05), stan uses split Rhat statistic 
bad_rhat <- om_sum %>% filter(rhat > 1.02) # No trials with rhat greater than 1.02

# Examine ratio of effective samples to total iterations, should be > 0.01
bad_neff <- om_sum %>% 
  mutate(n_eff_ratio = n_eff/3000) %>%
  filter(n_eff_ratio <= 0.1) # no trial fit < 0.1

# Examine sampler params 
fits$GSK101468_169$sampler_params


# Traceplots -------------------------------------------------------------------

tr1 <- om_draws %>% filter(trial %in% trials[1])

tr1c1 <- tr1 %>% filter(chain == 1)

mcmc_trace(tr1c1$Omega)

