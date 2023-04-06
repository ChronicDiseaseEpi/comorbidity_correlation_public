#' ---
#' title: "00-functions and packages"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---



# Packages ---------------------------------------------------------------------

library(reshape2)
library(ggplot2)
library(gridExtra)
library(tidybayes)
library(bayesplot)
library(stringr)
library(stringi)
library(posterior)
library(cmdstanr)
library(cowplot)
library(ggthemes)
library(truncnorm)
library(tidyverse)
library(XML)  



# Functions --------------------------------------------------------------------

# Generate IPD for each index condition per sex 
simulate_IPD <- function(condition, mydata) {
  
  # Extract df for condition
  condition_dat <- as.data.frame(mydata[condition])
  names(condition_dat) <- gsub(pattern = paste0(condition, "."), replacement = "", x = names(condition_dat))
  
  # Remove issue with N for the times argument in rep
  condition_dat$n[condition_dat$n == "<=5"] <- sample(1:5, sum(condition_dat$n == "<=5"), replace = TRUE) 
  
  # Correct class types
  condition_dat$age_m <- as.numeric(condition_dat$age_m)
  condition_dat$age_s <- as.numeric(condition_dat$age_s)
  condition_dat$n     <- as.integer(condition_dat$n)
  
  # Set up IPD df 
  condition_IPD <- condition_dat[rep(1:length(condition_dat$n), times = condition_dat$n),]
  condition_IPD$n <- NULL
  
  # Generate IPD - Normal distribution truncated at 0 to prevent -ve ages
  condition_IPD$age_sim <- rtruncnorm(nrow(condition_IPD), a = 0, b = Inf, mean = condition_IPD$age_m, sd = condition_IPD$age_s)
  
  # Save simulated ipd
  write_csv(condition_IPD, paste0("Data/", condition, "_ipd.csv"))
  
  # Return Simulated IPD
  condition_IPD
}



# Restrict IPD and save as new csv
restrict_ipd <- function(ipd_file_path) {
  
  # Read in IPD 
  ipd <- read_csv(ipd_file_path)
  
  # Get condition and sex
  cond <- unique(ipd$index_condition)
  sexx <- unique(ipd$sex)
  
  # Match ipd index condition to index ages conditions
  cond <- cond %>% 
    str_remove_all(",") %>%
    str_remove_all("[()]") %>%
    str_replace_all(" ", "_")
  
  # Progress
  print(paste0(cond, "-", sexx))
  
  # Get age dist based on sex and condition
  age_dist <- tr_cond_age %>% 
    filter(sex %in% sexx) %>%
    filter(condition %in% cond)
  
  # Filter IPD so matches age distribution
  restricted_ipd <- ipd %>% filter(between(age_sim, age_dist$min_age, age_dist$max_age)) 
  
  # Save restricted IPD
  write_csv(restricted_ipd, paste0("Data/", 
                                   cond,
                                   "_", 
                                   sexx, 
                                   "_age_restricted_IPD.csv"))
  
  rm(ipd, cond, sexx, age_dist, restricted_ipd)
}



# Run model 
run_mod <- function(run_info, data_list, stanmodel_file, save_path) {
  
  system.time({
    
    print(paste0(run_info, "_model_run_warning_messages_", Sys.Date()))
    
    # Compile model 
    mod <- cmdstan_model(stanmodel_file)
    
    # Sample from posterior
    fit <- mod$sample(
      data = data_list,
      seed = 123, 
      chains = 3,
      parallel_chains = 3,
      refresh = 100, # print update every 100 iters
      init = 0.1, # Restrict to prevent overflow in inits
      adapt_delta = 0.95)
    
  })
  
  fldrnm <- save_path
  if(!dir.exists(fldrnm)) dir.create(fldrnm, recursive = TRUE)
  
  fit$save_object(file = paste0(fldrnm,
                                run_info,
                                "_",
                                stringr::str_remove_all(Sys.Date(), "-"),
                                ".RDS"))
  
  print(paste0(run_info, " has finished runninng - check fit has been saved", Sys.Date(), "_", Sys.time()))  
}



# get comorbidity names and change to desired name 
get_como_names <- function(condition, look_up) {
  
  df <- read_csv(paste0("Data/", condition, "_Male_ipd.csv"))
  
  df <- df %>% 
    rename(any_of(look_up))
  
  names(df[3:8])
}



# Comorbidity names in heatmap 
rename_como <- function(df) {
  
  df <- df %>% rename("Cardiovascular" = CV,
                      "Pain" = pain,
                      "Arthritis" = arthritis,
                      "Acid-related disease" = antacids,
                      "Asthma" = asthma_COPD,
                      "Anxiety" = anxiety)
  df
}



# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
