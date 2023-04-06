#' ---
#' title: "28: Community model diagnostics"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# Model diagnostics for unrestricted and age restricted community models 



# Set up -----------------------------------------------------------------------

source("Scripts/00-Functions_and_packages.R")
set_cmdstan_path("/opt/cmdstanr/cmdstan-2.29.2")
color_scheme_set("viridis")

# Data
fits <- list.files("Outputs/model_fits", full.names = TRUE)
rstrct_fits <- list.files("Outputs/age_restricted_model_fits", full.names = TRUE)
all_om_sum <- readRDS("Outputs/VM_community_omega_summaries.RDS")



# Rhat -------------------------------------------------------------------------

# Rhat should be below 1.1, stan uses split Rhat statistic 
bad_rhat <- all_om_sum %>% filter(rhat > 1.01) # No fits with rhat < 1.01



# N_eff ------------------------------------------------------------------------

# Examine ratio of effective samples to total iterations, should be > 0.01
bad_neff <- all_om_sum %>% 
  mutate(n_eff_ratio = n_eff/3000) %>%
  filter(n_eff_ratio <= 0.1) # no trial fit < 0.1



# Divergent transitions --------------------------------------------------------

diag <- map(fits, function(.x) { 
  x <- readRDS(.x)
  x$diagnostic_summary()}) # No divergent transitions in unrestricted models

diag_rstrct <- map(fits, function(.x) { 
  x <- readRDS(.x)
  x$diagnostic_summary()})# No divergent transitions in restricted models



# ESS --------------------------------------------------------------------------

bad_ess <- all_om_sum %>% filter(ess_bulk < 300 | ess_tail < 300) # NO fits with low ESS
