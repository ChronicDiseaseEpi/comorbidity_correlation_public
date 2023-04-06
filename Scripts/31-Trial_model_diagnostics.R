#' ---
#' title: "31: Trial model diagnostics"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script carries out diagnostics for all 83 trial models 



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
tr_sum <- read_csv("Outputs/all_trials_omega_summaries.csv")
tr_draws <- readRDS("Outputs/all_trials_omega_draws.RDS")



# Divergent transitions --------------------------------------------------------

# GSK:   Divergences (<1.1%), Tree depth (<33%) and Energy (all fine) diagnostic checks were ok 
# Vivli: TKASYR322_402 had 3% divergent transitions so ran again 
# Yoda:  checks for divergent transitions found 2 models (DIA3005 & 28431754DIA3014) with > 2%, 
#        hence these models were ran with increased adapt delta and treedepth which successfully 
#        lowered them well below the 2% threshold



# Convergence ------------------------------------------------------------------

# Rhat 
bad_rhat <- tr_sum %>% 
  filter(rhat > 1.05) # 1 estimate rhat = 1.098

bad_rhat_tr <- bad_rhat %>% 
  distinct(trial) %>% 
  unlist()

# Examine traceplot for trial with bad rhat  
tr1 <- tr_draws %>% 
  filter(trial %in% bad_rhat_tr) %>% 
  select(trial, chain, iteration, estimate, Omega) %>% 
  spread(Omega, estimate) %>% 
  select(-iteration, -trial)

mcmc_trace(tr1) # All plots look fine 



# N_eff ------------------------------------------------------------------------

# Examine ratio of effective samples to total iterations, should be < 0.01
bad_neff <- tr_sum %>% 
  mutate(n_eff_ratio = n_eff/3000) %>%
  filter(n_eff_ratio < 0.1) 

# 51 estimates n_eff/n ratio < 0.1 (all from different models) 
# Checked other diagnostics, all fine can ignore 
# 18 of them are for 1:1 correlations

b_neff1_1 <- bad_neff %>% filter(!mean == 1)



# Bulk and tail ESS ------------------------------------------------------------

# Set up 
om_names  <- unique(tr_draws$Omega)
om_trials <- unique(tr_draws$trial)
combos    <- expand.grid(om_trials, om_names) %>% rename(trial = Var1, omega = Var2)
combos$trial <- as.character(combos$trial)
combos$omega <- as.character(combos$omega)

output <- matrix(ncol = 4, nrow = NROW(combos))

for (i in seq_along(combos$trial)) {
  
  print(i)
  
  tr <- combos$trial[i]
  om <- combos$omega[i]
  
  om_n <- tr_draws %>%
    filter(trial %in% tr) %>%
    filter(Omega %in% om) %>%
    pivot_wider(names_from = chain, values_from = estimate)
  
  om_chains <- om_n %>% select(c("1", "2", "3"))
  
  # Calculate ESS
  tail_ess = posterior::ess_tail(om_chains)
  bulk_ess = posterior::ess_bulk(om_chains)
  
  # Update output 
  output[i, 1] <- tr
  output[i, 2] <- om
  output[i, 3] <- tail_ess
  output[i, 4] <- bulk_ess

}

output <- data.frame(output)
output <- output %>% 
  rename(trial = X1,
  Omega = X2,
  tail_ess = X3,
  bulk_ess = X4)

# Save as loop very slow 
#write_csv(output, "Outputs/checked_ess_all_trials.csv")
ess <- read_csv("Outputs/checked_ess_all_trials.csv")

# Tail and bulk ESS should be 100* number of chains 
bad_ess <- ess %>% 
  filter(tail_ess < 300 | bulk_ess < 300) 

# Not going to worry about BI502_550 as everything else fine and ESS not much lower then 300

# Re run TKASYR322_402 as one paramater did have high rhat (traceplots were fine)

## NOTE ##

# Diagnostics were also checked during the model fitting process using check_hmc_diagnostics(), 
# See the model diagnostic text file in the Results folder for GSK/Vivli.
# in Prepare_trial_data_and_analyse folder in the supporting folder.
# Supporting -> Prepare_trial_data_and_analyse -> Vivli or GSK -> Results
# Only available for Vivli and GSK. Yoda did not save. 



