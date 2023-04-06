# This script runs some model diagnostics on the MVP models ran on Yoda trials



# Set up -----------------------------------------------------------------------

source("Supporting/Prepare_trial_data/Yoda/Scripts/00-Functions_and_packages.R")

# Data
yoda_como <- readRDS("Processed_data/cleaned_yoda_conmed_defined_comorbidities.Rds")
om_sum    <- read_csv("Results/omega_correlations/yoda_trial_omega_summaries.csv")
om_draws  <- readRDS("Results/omega_correlations/yoda_omega_draws.RDS")

# Full model fit for each trial 
fits        <- list.files("Results/model_fits_run2/", full.names = TRUE)
trials      <- unique(om_sum$trial)
tr_names    <- map(fits, ~ trials[str_detect(.x, trials)])
fits        <- map(fits, ~ readRDS(.x))
names(fits) <- tr_names



# Check for divergent transitions (over 2% = bad) ------------------------------



tr_divergent <- sapply(fits, function(x) sapply(x$sampler_params, function(x) sum(x[1000:2000, "divergent__"])))
tr_div_df <- as.data.frame(tr_divergent)
tr_div_df <- tr_div_df %>% 
  gather("trial", "divergent transitions") %>%
  mutate(chain = rep(1:3, times = 25),
         per_div = (`divergent transitions`/1000)*100) %>%
  arrange(desc(per_div))

# 2 trials with high number of divergent transitions
bad_div <- tr_div_df %>% 
  filter(per_div > 2) %>% 
  distinct(trial) %>% 
  unlist() # "DIA3005", "28431754DIA3014"

tr1 <- fits[2]$`28431754DIA3011`
tr1_samp <- tr1$sampler_params
colnames(tr1_samp[[1]])
sapply(tr1_samp, function(x) sum(x[1000:2000, "divergent__"]))
sapply(tr1_samp, function(x) mean(x[1000:2000, "energy__"]))



# Examine rhat -----------------------------------------------------------------

# Rhat should be below 1.1 (extreme 1.05), stan uses split Rhat statistic 
bad_rhat <- om_sum %>% filter(rhat > 1.05) # 0 trials with rhat > 1.05
ok_rhat <- om_sum %>% filter(rhat > 1.02) # 1.02, 28431754DIA3008

# Examine ratio of effective samples to total iterations, should be > 0.01
bad_neff <- om_sum %>% 
  mutate(n_eff_ratio = n_eff/3000) %>%
  filter(n_eff_ratio <= 0.1) # 7 trial fits with ratio < 0.1 



# Traceplots -------------------------------------------------------------------

# Examine trials with rhat > 1.01
# "28431754DIA3008", "ART3001", "C0524T06", "C0743T09", "DIA3015" 



tr1 <- om_draws %>% 
  filter(trial %in% trials[1]) %>% 
  select(trial, chain, iteration, estimate, Omega) %>% 
  spread(Omega, estimate) %>% 
  select(-iteration, -trial)

mcmc_trace(tr1)


tr2 <- om_draws %>% 
  filter(trial %in% trials[2]) %>% 
  select(trial, chain, iteration, estimate, Omega) %>% 
  spread(Omega, estimate) %>% 
  select(-iteration, -trial)

mcmc_trace(tr2)




# ESS --------------------------------------------------------------------------

rstan::ess_bulk(om_chains[, 1]) 

om_names <- unique(om_draws$Omega)
om_tr <- unique(om_draws$trial)

ess <- function(tr, param, draws_df) {

om_n <- draws_df %>%
  filter(trial %in% om_tr[tr]) %>%
  filter(Omega == om_names[param]) %>% 
  pivot_wider(names_from = chain, values_from = estimate)

om_chains <- om_n %>%
  select(c("1", "2", "3"))

output <- data.frame(trial = tr,
                     omega = param,
                     ess_tail_ch1 = posterior::ess_tail(om_chains[, 1]),
                     ess_tail_ch2 = posterior::ess_tail(om_chains[, 2]),
                     ess_tail_ch3 = posterior::ess_tail(om_chains[, 3]),
                     ess_bulk_ch1 = posterior::ess_bulk(om_chains[, 1]),
                     ess_bulk_ch2 = posterior::ess_bulk(om_chains[, 2]),
                     ess_bulk_ch3 = posterior::ess_bulk(om_chains[, 3]))

output
}


ess_total <- function(tr, param, draws_df) {
  
  om_n <- draws_df %>%
    filter(trial %in% om_tr[tr]) %>%
    filter(Omega == om_names[param]) %>%
    pivot_wider(names_from = chain, values_from = estimate)
  
  om_chains <- om_n %>%
    select(c("1", "2", "3"))
  
  output <- data.frame(trial = tr,
                       omega = param,
                       ess_tail = posterior::ess_tail(om_chains),
                       ess_bulk = posterior::ess_bulk(om_chains))
  
  output
}


combos <- expand.grid(om_tr, om_names)

ess_sum <- map_dfr(1:nrow(combos), ~ ess(combos$Var1[.x], combos$Var2[.x], om_draws))
ess_sum_total <- map_dfr(1:nrow(combos), ~ ess_total(combos$Var1[.x], combos$Var2[.x], om_draws))

bad_ess <- ess_sum_total %>% 
  filter(ess_tail < 300 | ess_bulk < 300) # 2 parameter estimates from 28431754DIA3008

