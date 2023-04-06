#' ---
#' title: "38: Sensitivity analysis"
#' author: "Jamie Crowther"
#' date: '`r format(Sys.Date(), "%B %d %Y")`'
#' output: html_document
#' ---



# Set up -----------------------------------------------------------------------

source("Scripts/00-functions_and_packages.R")

# Data 
sail_commmon_como <- read_csv("Created_metadata/SAIL_common_comorbidity_proportion_harmonised.csv")
tr_sum            <- read_csv("Outputs/all_trials_omega_summaries.csv")
cmnty_sum         <- read_csv("Outputs/all_community_omega_summaries.csv")
meta_sum          <- read_csv("Outputs/trial_meta_analysis_weighted_omega_summaries.csv")



# Combine all omega summaries --------------------------------------------------

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




# Compare non restricted vs age restricted community ---------------------------

# Community only
cmnty <- all_sum %>% 
  filter(repo %in% "Community") %>% 
  select(mean, condition, comorbidity, Var1, Var2) %>% 
  rename(cmnty_mean = mean) 

# Age restricted community only 
age_res_cmnty <- all_sum %>% 
  filter(repo %in% "Age restricted community") %>% 
  select(mean, condition, comorbidity, Var1, Var2) %>% 
  rename(age_res_mean = mean)

# Join 
cmpr <- cmnty %>%
  inner_join(age_res_cmnty, by = c("condition", "comorbidity", "Var1", "Var2")) 

# Difference between mean correlations in non restricted and age restricted community 
cmpr <- cmpr %>% 
  mutate(diff = cmnty_mean - age_res_mean,
         abs_diff = abs(cmnty_mean - age_res_mean)) %>% 
  arrange(desc(diff))

summary(cmpr$diff)     # Mean = -0.0009083, Max = 0.147
summary(cmpr$abs_diff) # Mean = 0.01213, Max = 0.147

# Largest difference is 0.147 for one correlation between Arthritis and Cardiovascular 
# in pulmonary hypertension.

# Second largest difference is 0.04.


# Index conditions with same six most common comorbidities (based on communtiy prevelance)
cmnty_vs_ageres_match6 <- ggplot(all_sum %>% 
                                   filter(repo %in% c("Age restricted community", "Community")) %>% 
                                   filter(condition %in% match8) %>%
                                   arrange(condition) %>%
                                   mutate(condition = str_replace_all(condition, "_", " "),
                                          comorbidity = factor(comorbidity),
                                          repo = factor(repo, levels = rev(c("Community", "Age restricted community")))), 
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
  scale_colour_tableau() +
  ggtitle("Cmnty vs age restricted cmnty for index conditions with same 6 common comorbidities")
print(cmnty_vs_ageres_match6)


# Remaining index conditions (do not share same six mst common)
cmnty_vs_age_res_nomatch <- ggplot(all_sum %>% 
                                      filter(repo %in% c("Age restricted community", "Community")) %>% 
                                      filter(!condition %in% match8) %>%
                                      arrange(condition) %>%
                                      mutate(condition = str_replace_all(condition, "_", " "),
                                             comorbidity = factor(comorbidity),
                                             repo = factor(repo, levels = rev(c("Community", "Age restricted community")))), 
                                    aes(x = reorder(condition, desc(condition)), y = mean, colour = repo, shape = repo), ymin = CI2.5, ymax = CI97.5) +
  geom_point(position = position_dodge(0.9)) +
  facet_wrap(~comorbidity) + 
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
  scale_colour_tableau() + 
  ggtitle("Cmnty vs age restricted cmnty for index conditions that do not have the same 6 common comorbidities")
print(cmnty_vs_age_res_nomatch)




# Non restricted communtiy vs trials ------------------------------------------- 

# Index conditions with same six most common comorbidities (based on community prevelance)
cmnty_trials_match6 <- ggplot(all_sum %>% 
                                   filter(repo %in% c("Trial meta weighted", "Community")) %>% 
                                   filter(condition %in% match8) %>%
                                   arrange(condition) %>%
                                   mutate(condition = str_replace_all(condition, "_", " "),
                                          comorbidity = factor(comorbidity),
                                          repo = factor(repo, levels = rev(c("Community", "Trial meta weighted")))), 
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
  scale_colour_tableau() + 
  ggtitle("Cmnty vs trials for index conditions with same 6 common comorbidities")
print(cmnty_trials_match6)

# Remaining index conditions (do not share same six mst common)
cmnty_trials_nomatch <- ggplot(all_sum %>% 
                                filter(repo %in% c("Trial meta weighted", "Community")) %>% 
                                filter(!condition %in% match8) %>%
                                arrange(condition) %>%
                                mutate(condition = str_replace_all(condition, "_", " "),
                                       comorbidity = factor(comorbidity),
                                       repo = factor(repo, levels = rev(c("Community", "Trial meta weighted")))), 
                              aes(x = reorder(condition, desc(condition)), y = mean, colour = repo, shape = repo), ymin = CI2.5, ymax = CI97.5) +
  geom_point(position = position_dodge(0.9)) +
  facet_wrap(~comorbidity) + 
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
  scale_colour_tableau() + 
  ggtitle("Cmnty vs trials for index conditions that do not have the same 6 common comorbidities")
print(cmnty_trials_nomatch)




# Age restricted community vs trials -------------------------------------------

# Index conditions with same six most common comorbidities (based on community prevelance)
ageres_trials_match6 <- ggplot(all_sum %>% 
                                 filter(repo %in% c("Trial meta weighted", "Age restricted community")) %>% 
                                 filter(condition %in% match8) %>%
                                 arrange(condition) %>%
                                 mutate(condition = str_replace_all(condition, "_", " "),
                                        comorbidity = factor(comorbidity),
                                        repo = factor(repo, levels = rev(c("Age restricted community", "Trial meta weighted")))), 
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
  scale_colour_tableau() + 
  ggtitle("Age restricted cmnty vs trials for index conditions with same 6 common comorbidities")
print(ageres_trials_match6)

# Remaining index conditions (do not share same six mst common)
ageres_trials_nomatch <- ggplot(all_sum %>% 
                                  filter(repo %in% c("Trial meta weighted", "Age restricted community")) %>% 
                                  filter(!condition %in% match8) %>%
                                  arrange(condition) %>%
                                  mutate(condition = str_replace_all(condition, "_", " "),
                                         comorbidity = factor(comorbidity),
                                         repo = factor(repo, levels = rev(c("Age restricted community", "Trial meta weighted")))), 
                                aes(x = reorder(condition, desc(condition)), y = mean, colour = repo, shape = repo), ymin = CI2.5, ymax = CI97.5) +
  geom_point(position = position_dodge(0.9)) +
  facet_wrap(~comorbidity) + 
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
  scale_colour_tableau() + 
  ggtitle("Age restricted cmnty vs trials for index conditions that do not have the same 6 common comorbidities")
print(ageres_trials_nomatch)


pdf("Outputs/sensitivity_analysis_figures.pdf", width = 20, height = 12)
print(cmnty_vs_ageres_match6)
print(cmnty_vs_age_res_nomatch)
print(cmnty_trials_match6)
print(cmnty_trials_nomatch)
print(ageres_trials_match6)
print(ageres_trials_nomatch)
dev.off()


