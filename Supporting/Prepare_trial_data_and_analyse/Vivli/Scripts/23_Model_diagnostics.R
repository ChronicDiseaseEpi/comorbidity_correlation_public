# 23_model_diagnostics
# check diagnostics of MVP GSK model runs 



# Set up -----------------------------------------------------------------------

source("Scripts/00-Functions_packages.R")

# Data
vivli_como <- read.csv("Processed_data/cleaned_conmed_defined_comorbidity.csv", row.names = 1)
om_sum <- read_csv("Supporting/Prepare_trial_data/Vivli/Results/vivli_omega_summaries.csv")
om_draws <- readRDS("Supporting/Prepare_trial_data/Vivli/Results/vivli_omega_draws.RDS")

# Full model fit for each trial 
fits        <- list.files("Results/model_fits/", full.names = TRUE)[-37]
trials      <- unique(vivli_como$trial)
tr_names    <- map(fits, ~ trials[str_detect(.x, trials)])
fits        <- map(fits, ~ readRDS(.x))
names(fits) <- tr_names



# Divergent transitions --------------------------------------------------------

tr_divergent <- sapply(fits, function(x) sapply(x$sampler_params, function(x) sum(x[1000:2000, "divergent__"])))

tr_div_df <- as.data.frame(tr_divergent)

tr_div_df <- tr_div_df %>% 
  gather("trial", "div_trans") %>%
  group_by(trial) %>%
  summarise(n_div = sum(div_trans)) %>%
  mutate(percent_div = (n_div/3000)*100)

bad_div <- tr_div_df %>%
  filter(percent_div > 2) %>%
  distinct(trial) %>%
  unlist() # Need to re run BI502_316

x <- readRDS("Results/model_fits/BI502_316_Hypertension_20221006.RDS")
x_df <- as.data.frame(x$sampler_params)
x_div <- x_df %>% 
  select(divergent__, divergent__.1, divergent__.2) 
x_div <- x_div[1001:2000, ] 
x_sum <- x_div %>% colSums() # 91 



# Rhat -------------------------------------------------------------------------

# should be below 1.1 (extreme 1.05), stan uses split Rhat statistic
bad_rhat <- om_sum %>% filter(rhat > 1.05) 

# 1 trial with quite high rhat but below 1.1 -> visually inspect 
bad_rhat_tr <- unique(bad_rhat$trial) # "TKASYR322_402"

# TKASYR322_402 was checked and all Omega parameters looked fine. 



# N_eff ------------------------------------------------------------------------

# Examine ratio of effective samples to total iterations, should be > 0.001
bad_neff <- om_sum %>% 
  mutate(n_eff_ratio = n_eff/3000) %>%
  filter(n_eff_ratio < 0.1)

unique(bad_neff$trial)



# ESS --------------------------------------------------------------------------


map_df(fits, function(x) map)

trials <- unique(om_draws$trial)
oms <- unique(x$Omega)

x1 <- om_draws %>% 
  filter(trial %in% "BI107_210") %>% 
  select(chain, iteration, trial, Omega, estimate)

x2 <- x1 %>%
  pivot_wider(names_from = Omega, values_from = estimate) 

x3 <- x2 %>% 
  select(chain, iteration, oms[2]) %>%
  pivot_wider(names_from = chain, values_from = oms[2])

x4 <- x3 %>%select("1","2","3")
ess_bulk(x4)



