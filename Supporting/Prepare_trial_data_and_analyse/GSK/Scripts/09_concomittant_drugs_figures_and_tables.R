# 09_concomittant_meds_figures_and_Tables
source("Supporting/Prepare_trial_data/GSK/Scripts/00_functions_and_packages.R")

conmed_all <- readRDS("GSK_processed_data/GSK_conmed_defined_comorbidity.Rds")



# Examine summary data for all components
conmed_all_trial_lvl <- conmed_all %>% 
  select(-id) %>% 
  group_by(company, trial, nct_id, medicine, condition) %>% 
  summarise_all(function(x) round(100*mean(x),1)) %>% 
  ungroup() %>% 
  mutate(num_of_como = rowSums(.[6:27] != 0.0)) %>%
  arrange(num_of_como)

###   IMPORTANT   ###
# 29 trials as expected (2 have no conmed tables), 16/06/22
# 2 trials only have 3 and 4 comos respectively = GSKAVA102672_v02, GSKAVA102670_v02
# KEEP AN EYE ON THESE 2 TRIALS



## Overall plot for each condtion by trial
conmed_all_trial_lvl_lng <- conmed_all_trial_lvl %>% 
  ungroup() %>% 
  select(nct_id, condition, antacids:skin) %>% 
  gather(key = "comorbidity", value = "Percentage", -nct_id, -condition) %>% 
  arrange(condition, nct_id)

plot_cond <- ggplot(conmed_all_trial_lvl_lng, aes(x = comorbidity, y = nct_id)) +
  geom_tile(mapping = aes(fill = Percentage), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_discrete()
ggsave(filename = "outputs/GSK_conditions_by_trial_plot.pdf", plot_cond, height = 12, width = 18)

## COnducted detailed manual review of comorbidites
write_csv(conmed_all_trial_lvl, "GSK_outputs/GSK_comorbidities_by_trial.csv")
# Londucted line by line review, identifung and checking for any imlausible findgs
conmed <- readRDS("GSK_processed_data/GSK_conmed.Rds")

## COunt conditions after excluded skin and avoided double counting of pain/arthritis
conmed_count <- conmed_all %>% 
  mutate(skin = FALSE,
         pain = if_else(migraine|arthritis, FALSE, pain),
         arthritis = if_else(inflammatory, FALSE, arthritis))

conmed_count <- conmed_count %>% 
  mutate(disease_count = NA) 
conmed_count$disease_count <- rowSums(conmed_count %>% ungroup() %>% select(antacids:urological))



######### Do counts for different definitions
## Total count across all trials
overall <- table(conmed_count$disease_count)

conmed_count_agg <- conmed_count %>% 
  group_by(nct_id, disease_count) %>% 
  summarise(x = length(disease_count)) %>% 
  group_by(nct_id) %>% 
  mutate(n = sum(x)) %>% 
  ungroup() %>% 
  mutate(prop = round(100*x/n,1))

conmed_count_agg <- conmed_count_agg %>% 
  select(nct_id, n, disease_count, prop) %>% 
  spread(disease_count, prop, fill = 0)

#### SAVE Comorbidities per trial 
write_csv(conmed_count_agg, "Results/GSK_comorbidity_per_trial_count.csv")



# Calculate proportion with each comorbidity
conmed_all_trial_lvl <- conmed_all %>% 
  select(-id) %>% 
  group_by(company, trial, nct_id, medicine, condition) %>% 
  summarise_all(function(x) round(100*mean(x),1)) 

# Calcualte median and 10th to 90th centile across trials
conmed_all_trial_lvl %>% 
  ungroup() %>% 
  summarise_at(vars(antacids:urological), function(x) paste0(median(x), " (", quantile(x, 0.1), "-",quantile(x, 0.9), ")")) %>% 
  t()

# Make as long dataframe to draw heat plot
conmed_all_trial_lvl_lng <- conmed_all_trial_lvl %>% 
  ungroup() %>% 
  select(nct_id, condition, antacids:urological) %>% 
  gather(key = "comorbidity", value = "Percentage", -nct_id, -condition) %>% 
  arrange(condition, nct_id)

# Plot heatmap with and without CV diseases
plot_cond <- ggplot(conmed_all_trial_lvl_lng, aes(x = comorbidity, y = nct_id)) +
  geom_tile(mapping = aes(fill = Percentage), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_x_discrete()
plot_cond_no_cv <- plot_cond %+% (conmed_all_trial_lvl_lng %>% filter(comorbidity != "CV"))
ggsave(filename = "Results/GSK_conditions_by_trial_collapse_plot.pdf", plot_cond, height = 12, width = 18)
ggsave(filename = "Results/GSK_conditions_by_trial_collapse_no_cv_plot.pdf", plot_cond_no_cv, height = 12, width = 18)

## SAVE 
write_csv(conmed_all_trial_lvl, "Results/GSK_comorbidity_per_trial.csv")





