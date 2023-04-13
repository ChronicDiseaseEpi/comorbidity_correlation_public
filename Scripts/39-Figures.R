#' ---
#' title: "39: Figures"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---

# This script produces publication figures and svaes them all in a single pdf.
# Figures were saved individually as jpeg files from the print window to be inserted
# into paper manuscript



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
sail_commmon_como <- read_csv("Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv")

tr_sum    <- read_csv("Outputs/all_trials_omega_summaries.csv")
cmnty_sum <- read_csv("Outputs/all_community_omega_summaries.csv")
meta_sum  <- read_csv("Outputs/trial_meta_analysis_weighted_omega_summaries.csv")

tr_prop   <- read_csv("Outputs/trial_comorbidity_proportions_by_condition.csv")
vm_prop   <- read_csv("Outputs/vm_age_restricted_comorbidity_proportions_by_condition.csv")
sail_prop <- read_csv("Outputs/SAIL_age_restricted_comorbidity_proportions_by_condition.csv")

tr_n   <- read_csv("Created_metadata/trial_condition_summary.csv")
vm_n   <- read_csv("Created_metadata/vm_age_restricted_conditions_baseline_table.csv")
sail_n <- read_csv("Created_metadata/sail_age_sex_summary.csv")




# Table 1: Community - Trial comparison summary --------------------------------


# Clean total tables 
tr_n <- tr_n %>%
  rename(trial_n = n,
         trial_mean_age = age_mean,
         trial_age_sd = age_sd,
         trial_male_pcnt = male_pcnt) %>%
  select(-male_n, -repo)

vm_n <- vm_n %>%
  rename(cmnty_n = n,
         cmnty_mean_age = age_mean,
         cmnty_age_sd = age_sd,
         cmnty_male_pcnt = male_pcnt) %>%
  select(-male_n, -repo)

sail_n <- sail_n %>% 
  rename(condition = index_condition,
         cmnty_mean_age = "Mean age",
         cmnty_age_sd = "SD age",
         cmnty_n = Individuals,
         cmnty_male_pcnt = "Percent male")

sail_n <- sail_n %>% 
  mutate(condition = case_when(condition == "Hypertension, Pulmonary" ~ "Pulmonary_Hypertension",
                               condition == "Parkinson's disease (all)" ~ "Parkinson_Disease",
                               condition == "Psoriatic arthropathy" ~ "Psoriatic_arthritis",
                               TRUE ~ as.character(condition)),
         condition = str_replace_all(condition, " ", "_"))

# bind sail and vm tables
cmnty_n <- sail_n %>% bind_rows(vm_n)

# Join total tables
table_1 <- tr_n %>% 
  inner_join(cmnty_n) %>%
  select(condition, cmnty_n, trial_n, n_trials, cmnty_mean_age, trial_mean_age, cmnty_age_sd, trial_age_sd, cmnty_male_pcnt, trial_male_pcnt) %>% 
  mutate(cmnty_mean_age = round(cmnty_mean_age, 0),
         cmnty_age_sd = round(cmnty_age_sd, 0),
         cmnty_male_pcnt = round(cmnty_male_pcnt, 1))

# Get total number of patients in community and trials 
tots <- table_1 %>%
  summarise(cmnty_total = sum(cmnty_n),
            trial_total = sum(trial_n),
            total_num_trials = sum(n_trials))

# Get medians 
meds <- table_1 %>% 
  summarise(across(cmnty_n:trial_male_pcnt, ~ median(.x)))

# Save - Table 1
write_csv(table_1, "Outputs/cmnty_trial_summary_table.csv")



# Figure 1 ---------------------------------------------------------------------


# Combine cmnty and trial proportions
prop <- bind_rows(tr_prop, vm_prop, sail_prop)

# Save
write_csv(prop, "Outputs/cmnty_trial_comorbidity_proportions_table.csv")

# Prepare for plotting
prop <- prop %>% 
  mutate(condition = if_else(condition == "Dementia_any", "Dementia", condition),
         condition = str_replace_all(condition, "_", " "),
         repo = if_else(repo ==  "Age restricted community", "Cmnty", repo))

# Figure 1: Proportion of the top six comorbidities for the sixteen index conditions compared between the community and trials 
fig_1 <- ggplot(prop, aes(reorder(condition, desc(condition)), como_prop, group = repo, colour = repo)) + 
  geom_point() + 
  facet_wrap(~comorbidity, nrow = 2, ncol = 5) + 
  theme_classic() + 
  labs(colour = NULL) +
  scale_color_colorblind() + 
  ylab("Comorbidity proportion (%)") + 
  xlab(NULL) + 
  theme(strip.background = element_rect(color=" light grey", fill=" light grey"),
        strip.text.x = element_text(size = 12, face = "bold")) + 
  coord_flip()

tiff("Outputs/Figures/Figure_1-cmnty-trial_proportion.tif", width = 4000, height = 2000, res = 300, compression = "lzw")
print(fig_1)
dev.off()




# Prepare plotting dataframe for figures 2 & 3 ---------------------------------


# Combine trial, community and meta omega summaries 
all_sum <- tr_sum %>% bind_rows(cmnty_sum, meta_sum)

# Change demenita condition name
all_sum <- all_sum %>%
  mutate(condition = if_else(condition == "Dementia_any", "Dementia", condition))


##  Find conditions with matching comorbidities  ##
conds <- unique(all_sum$condition)

# Get index conditions with the same top 6 comos 
comos <- sail_commmon_como %>%
  filter(condition %in% conds) %>% 
  group_by(condition) %>%
  distinct(como)

# Collapse comos into a single row for each condition
comos2 <- comos %>%
  arrange(como) %>%
  group_by(condition) %>%
  summarise(top6 = toString(como)) %>%
  ungroup()

# Number of matches
cc <- comos2 %>%
  group_by(top6) %>%
  summarise(n = n())

# Get index conditions that share same 6 comorbidities 
m8 <- cc %>% filter(n == 8) %>% distinct(top6) %>% unlist()
match8 <- comos2 %>% filter(top6 %in% m8) %>% distinct(condition) %>% unlist()




# Figure 2 ---------------------------------------------------------------------


# NOTE:
# Age restricted community and meta weighted trial estimates are used

# Figure 2: Correlations between comorbidities in the community and trials for the 8 index conditions with same top 6 comorbidities
fig_2 <- ggplot(all_sum %>% 
                  filter(repo %in% c("Age restricted community", "Trial meta weighted")) %>% 
                  filter(condition %in% match8) %>%
                  arrange(condition) %>%
                  mutate(condition = str_replace_all(condition, "_", " "),
                         comorbidity = factor(comorbidity),
                         repo = if_else(repo == "Age restricted community", "Cmnty", "Trials"),
                         repo = factor(repo, levels = rev(c("Cmnty", "Trials")))), 
                aes(x = reorder(condition, desc(condition)), y = mean, colour = repo, shape = repo), ymin = CI2.5, ymax = CI97.5) +
  geom_point(position = position_dodge(0.9)) +
  facet_wrap(~comorbidity, ncol = 3, nrow = 5) + 
  coord_flip(ylim = c(-0.5, 0.5)) +
  geom_linerange(aes(ymin = CI2.5, ymax = CI97.5), position = position_dodge(0.9)) +
  scale_y_continuous("") +
  geom_hline(yintercept=0.0, colour = "black", linetype = "dashed") + 
  theme_classic() +
  scale_x_discrete("", expand = c(0.2, 0.2)) + 
  theme(strip.background = element_rect(color=" light grey", fill=" light grey"),
        strip.text.x = element_text(size = 12, face = "bold")) + 
  guides(shape = guide_legend(reverse = TRUE), colour = guide_legend(reverse = TRUE)) +
  labs(shape = NULL, colour = NULL) +
  scale_colour_tableau()

tiff("Outputs/Figures/Figure_2-cmnty-trial_correlations_same_top6.tif", width = 3700, height = 2600, res = 300, compression = "lzw")
print(fig_2)
dev.off()


# Supplementary Figure S1: Correlations between comorbidities in the community and trials for the remaining 8 index conditions 
sup_fig_1 <- ggplot(all_sum %>% 
                      filter(repo %in% c("Age restricted community", "Trial meta weighted")) %>% 
                      filter(!condition %in% match8) %>%
                      arrange(condition) %>%
                      mutate(condition = str_replace_all(condition, "_", " "),
                             comorbidity = factor(comorbidity),
                             repo = if_else(repo == "Age restricted community", "Cmnty", "Trials"),
                             repo = factor(repo, levels = rev(c("Cmnty", "Trials")))), 
                    aes(x = reorder(condition, desc(condition)), y = mean, colour = repo, shape = repo), ymin = CI2.5, ymax = CI97.5) +
  geom_point(position = position_dodge(0.9)) +
  facet_wrap(~comorbidity, ncol = 6, nrow = 7) + 
  coord_flip(ylim = c(-0.5, 0.5)) +
  geom_linerange(aes(ymin = CI2.5, ymax = CI97.5), position = position_dodge(0.9)) +
  scale_y_continuous("") +
  geom_hline(yintercept=0.0, colour = "black", linetype = "dashed") + 
  theme_classic() +
  guides(colour = guide_legend(reverse = TRUE), shape = guide_legend(reverse = TRUE)) +
  labs(colour = NULL, shape = NULL) + 
  scale_x_discrete("", expand = c(0.2, 0.2)) + 
  theme(strip.background = element_rect(color=" light grey", fill=" light grey"),
        strip.text.x = element_text(size = 7, face = "bold")) + 
  scale_colour_tableau()

tiff("Outputs/Figures/Sup_Figure_S1-cmnty-trial_correlations_nomatch.tif", width = 5000, height = 3000, res = 300, compression = "lzw")
print(sup_fig_1)
dev.off()




# Figure 3 ---------------------------------------------------------------------


# Read in difference between age restricted community and weighted trial estimate 
diff_sum <-  read_csv("Outputs/trial-community_correlation_differences.csv")

# Highlight differences that are not NULL
diff_sum <- diff_sum %>% 
  mutate(corr_col = if_else(CI2.5 <= 0.00 & CI97.5 >= 0.00, "No difference", "Different to zero"),
         diff_mean = if_else(comorbidity == "Acid-related disease : Arthritis", diff_mean*-1, diff_mean),
         CI2.5 = if_else(comorbidity == "Acid-related disease : Arthritis", CI2.5*-1, CI2.5),
         CI97.5 = if_else(comorbidity == "Acid-related disease : Arthritis", CI97.5*-1, CI97.5),
         condition = if_else(condition == "Dementia_any", "Dementia", condition))


# Figure 3: Difference between comorbidity correlations for the 8 index conditions which share same top 6 comorbidities
# NOTE: To make figure more readable the correlation between arthritis and acid related disease has been flipped
fig_3 <- ggplot(diff_sum %>% 
                  filter(condition %in% match8) %>% 
                  mutate(condition = str_replace_all(condition, "_", " ")),
                aes(x = diff_mean, y = reorder(condition, desc(condition)), xmin = CI2.5, xmax = CI97.5, colour = corr_col)) + 
  geom_point(position = position_dodge(0.5)) +
  geom_linerange(position = position_dodge(0.5)) +
  geom_vline(xintercept=0.0, colour = "black", linetype = "dashed") + 
  xlab("Difference in correlation (Community - Trial)") +
  ylab(NULL) + 
  labs(colour = NULL) +
  theme_classic() +
  facet_wrap(~ comorbidity, ncol = 5, nrow = 3) + 
  theme(strip.background = element_rect(color=" light grey", fill=" light grey"),
        strip.text.x = element_text(size = 8, face = "bold")) + 
  scale_colour_manual(values = c("No difference" = "black", 
                                 "Different to zero" = "orange")) + 
  guides(colour = "none")

tiff("Outputs/Figures/Figure_3-cmnty-trial_difference_same_top6.tif", width = 4100, height = 2500, res = 300, compression = "lzw")
print(fig_3)
dev.off()



# Supplementary Figure S2: Difference between comorbidity correlations for the remaining 8 index conditions 
# NOTE: To make figure more readable the correlation between arthritis and acid related disease has been flipped
sup_fig_2 <- ggplot(diff_sum %>% 
                      filter(!condition %in% match8) %>%
                      mutate(condition = str_replace_all(condition, "_", " ")), 
                    aes(x = diff_mean, y = reorder(condition, desc(condition)), xmin = CI2.5, xmax = CI97.5, colour = corr_col)) + 
  geom_point(position = position_dodge(0.5)) +
  geom_linerange(position = position_dodge(0.5)) +
  geom_vline(xintercept=0.0, colour = "black", linetype = "dashed") + 
  xlab("Difference in correlation (Community - Trial)") +
  ylab(NULL) + 
  labs(colour = NULL) +
  theme_classic() +
  facet_wrap(~ comorbidity, ncol = 6, nrow = 7) + 
  theme(strip.background = element_rect(color=" light grey", fill=" light grey"),
        strip.text.x = element_text(size = 7, face = "bold"))+ 
  scale_colour_manual(values = c("No difference" = "black", 
                                 "Different to zero" = "orange")) + 
  guides(colour = "none")

tiff("Outputs/Figures/Sup_Figure_S2-cmnty-trial_difference_nomatch.tif", width = 5050, height = 2750, res = 300, compression = "lzw")
print(sup_fig_2)
dev.off()




# Save Figures to pdf 
pdf("Outputs/Figures/comorbidity_paper_figures.pdf", width = 20, height = 12)
print(fig_1)
print(fig_2)
print(sup_fig_1)
print(fig_3)
print(sup_fig_2)
dev.off()




# Summaries --------------------------------------------------------------------


## Correlations ## 

# Classify correlations 
all_sum <- all_sum %>% 
  mutate(point_summary = case_when(mean <= -0.1 ~ "Negative",
                                   mean > -0.1 & mean < 0.1 ~ "Null",
                                   mean >= 0.1 & mean < 0.3 ~ "Weakly positive",
                                   mean >= 0.3 ~ "Strongly positive"))

# Summary for age restricted community and weighted trial
cor_sum <- all_sum %>% 
  filter(repo %in% c("Age restricted community", "Trial meta weighted")) %>% 
  group_by(repo) %>% 
  summarise(median = median(mean),
            q25 = quantile(mean, 0.25),
            q75 = quantile(mean, 0.75),
            total = n(),
            negative = sum(point_summary == "Negative"),
            null = sum(point_summary == "Null"),
            weakly_positive = sum(point_summary == "Weakly positive"),
            strongly_positive = sum(point_summary == "Strongly positive"),
            negative_pcnt = round(sum(point_summary == "Negative")/total*100, 1),
            null_pcnt = round(sum(point_summary == "Null")/total*100, 1),
            weakly_positive_pcnt = round(sum(point_summary == "Weakly positive")/total*100, 1),
            strongly_positive_pcnt = round(sum(point_summary == "Strongly positive")/total*100, 1))



## Difference in correlations between community and trial ##

# Stronger in trials or communtiy or null
# Make df of only community age restricted and weighted trial estimate 
age_res_cmnty <- all_sum %>% 
  filter(repo %in% "Age restricted community") %>% 
  rename(age_res_mean = mean,
         age_res_CI2.5 = CI2.5,
         age_res_CI97.5 = CI97.5)

wtd_trial <- all_sum %>% 
  filter(repo %in% "Trial meta weighted") %>% 
  rename(wtd_trial_mean = mean,
         wtd_trial_CI2.5 = CI2.5,
         wtd_trial_CI97.5 = CI97.5)

# Check conditions match 
unique(age_res_cmnty$condition)
unique(wtd_trial$condition)
unique(diff_sum$condition)

# Dementia not matching 
diff_sum <- diff_sum %>%
  mutate(condition = if_else(condition == "Dementia_any", "Dementia", condition))

# Join age restricted community and weighted trial estimate as separate columns 
diff_sum <- diff_sum %>%
  left_join(age_res_cmnty %>% select(condition, comorbidity, Var1, Var2, age_res_mean, age_res_CI2.5, age_res_CI97.5)) %>%
  left_join(wtd_trial %>% select(condition, comorbidity, Var1, Var2, wtd_trial_mean, wtd_trial_CI2.5, wtd_trial_CI97.5))

# Check for NAs
map(diff_sum, ~ sum(is.na(.x)))

# Absolute differences between correlations - To determine if correlation stronger in trials or community 
diff_sum <- diff_sum %>% 
  mutate(abs_age_res_mean = abs(age_res_mean),
         abs_wtd_trial_mean = abs(wtd_trial_mean),
         corr_col = case_when(abs_wtd_trial_mean > abs_age_res_mean & corr_col == "Different to zero" ~ "Trials",
                              abs_wtd_trial_mean < abs_age_res_mean & corr_col == "Different to zero" ~ "Cmnty",
                              TRUE ~ as.character(corr_col)))

# Summary of differences 
diff_summary <- diff_sum %>%
  summarise(null_diff = sum(corr_col == "No difference"),
            strong_trials = sum(corr_col == "Trials"),
            strong_cmnty = sum(corr_col == "Cmnty"),
            same_mag = sum(corr_col == "Different to zero"),
            total = sum(null_diff, strong_trials, strong_cmnty, same_mag),
            null_pcnt = null_diff/total*100,
            trials_pcnt = strong_trials/total*100,
            cmnty_pcnt = strong_cmnty/total*100,
            same_mag_pcnt = same_mag/total*100,
            min = min(diff_mean),
            max = max(diff_mean),
            mean = mean(diff_mean),
            median = median(diff_mean))


