#' ---
#' title: "34: Check SAIL MCMC draws simulation"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# Because the model MCMC draws and draws summaries are so large and SAIL has a 
# export file size limit, we had to simulate the estimated correlations at each
# iteration of the MCMC. This script sense checks this process. We found it worked
# well with high precision, no difference was greater than 0.008.



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

sail_sum  <- read_csv("Outputs/model_summaries_from_SAIL.csv")



# Simulate (with same seed) ----------------------------------------------------

# Age restricted SAIL 
sail_sum <- sail_sum %>% 
  filter(model_type %in% "agerestricted")

iter   <- 1000
chains <- 3
conds  <- as.numeric(n_distinct(sail_sum$condition))
omegas <- as.numeric(n_distinct(sail_sum$params))

# Expand dataframe to correct length and ARRANGE
sail_draws <- map_dfr(seq_len(chains*iter), ~ sail_sum)
sail_draws <- sail_draws %>% arrange(condition)

# Add chain and iteration cols
omgit <- omegas*iter
sail_draws <- sail_draws %>% 
  mutate(iteration = rep(1:1000, times = chains*conds, each = omegas),
         chain = rep(1:3, times = conds, each = omgit))

# Add simulate mean estimate from omega summary mean/sd 
set.seed(123)
sail_draws$estimate <- rtruncnorm(nrow(sail_draws), a = -1, b = 1, mean = sail_draws$mean, sd = sail_draws$sd)



# Check ------------------------------------------------------------------------

check <- sail_draws %>% 
  group_by(condition, params) %>% 
  summarise(mean  = mean(estimate),
            sd    = sd(estimate),
            q2.5  = quantile2(estimate, probs = 0.025),
            q5    = quantile2(estimate, probs = 0.05),
            q10   = quantile2(estimate, probs = 0.1),
            q20   = quantile2(estimate, probs = 0.2),
            q30   = quantile2(estimate, probs = 0.3),
            q40   = quantile2(estimate, probs = 0.4),
            q50   = quantile2(estimate, probs = 0.5),
            q60   = quantile2(estimate, probs = 0.6),
            q70   = quantile2(estimate, probs = 0.7),
            q80   = quantile2(estimate, probs = 0.8),
            q90   = quantile2(estimate, probs = 0.9),
            q95   = quantile2(estimate, probs = 0.95),
            q97.5 = quantile2(estimate, probs = 0.975)) %>%
  mutate(across(mean:q97.5, ~ round(.x, 2)))


check2 <- check %>% inner_join(sail_sum, by = c("condition", "params"))

check2 <- check2 %>% 
  mutate(mean = mean.y - mean.x,
         sd = sd.y - sd.x,
         Q2.5 = `2.5%` - q2.5,
         Q5 = `5%` - q5,
         Q10 = `10%` - q10,
         Q20 = `20%` - q20,
         Q30 = `30%` - q30,
         Q40 = `40%` - q40,
         Q50 = `50%` - q50,
         Q60 = `60%` - q60,
         Q70 = `70%` - q70,
         Q80 = `80%` - q80,
         Q90 = `90%` - q90,
         Q95 = `95%` - q95,
         Q97.5 = `97.5%` - q97.5) %>% 
  select(condition, mean, sd, Q2.5,Q5,Q10,Q20,Q30,Q40,Q50,Q60,Q70,Q80,Q90,Q95,Q97.5)

check3 <- check2 %>%
  summarise(across(mean:Q97.5, ~max(.x)))

check4 <- check2 %>%
  ungroup() %>%
  summarise(across(mean:Q97.5, ~max(.x)))


# ALL GOOD - No difference bigger than 0.008
