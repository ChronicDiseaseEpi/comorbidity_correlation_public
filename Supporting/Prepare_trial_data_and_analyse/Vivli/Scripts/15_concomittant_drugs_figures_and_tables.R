# 15_concomittant_meds_figures_and_Tables

source("Supporting/Prepare_trial_data/Vivli/Scripts/00_functions_and_packages.R")

conmed_all <- readRDS("../Extract_Data/Processed_data/conmed_defined_comorbidity.Rds")


# Examine summary data for all components
conmed_all_trial_lvl <- conmed_all %>% 
  select(-id) %>% 
  group_by(company, trial, nct_id, medicine, condition) %>% 
  summarise_all(function(x) round(100*mean(x),1))

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
#ggsave(filename = "../Extract_Data/Outputs/conditions_by_trial.pdf", plot_cond, height = 12, width = 18)

## COnducted detailed manual review of comorbidites
write_csv(conmed_all_trial_lvl, "../Extract_Data/Outputs/comorbidities_by_trial.csv")
# Londucted line by line review, identifung and checking for any imlausible findgs
conmed <- readRDS("../Extract_Data/Processed_data/all_sponsors_conmed.Rds")

# Also identified where should have excluded comorbidities - eg replacement knee and arthritis
## NCT00152971 - very large estimate for epilepsy, reviewed large amoungs of lorazepam.
# Have now removed bzds from epilepsy classification as used so widely for anxiety
# and seems unlikely to be sole agent.
BI1160_24 <- conmed %>% filter(trial == "BI1160_24")
BI1160_24_epi <- BI1160_24 %>% 
  filter(str_sub(atc_code, 1, 3) == "N03") %>% 
  filter(!str_detect(str_to_lower(term), "gabapentin|pregabalin|valproate|valproic acid")) %>% 
  group_by(term, atc_code) %>% 
  count(sort = T)

## Count conditions after excluded skin and avoided double counting of pain/arthritis
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
# Note drop NCT00694382 as there is no comorbidity data - this trial is not here anymore

write_csv(conmed_count_agg, "E:/Results/comorbidity_per_trial_count.csv")

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
ggsave(filename = "E:/Results/conditions_by_trial_collapse.pdf", plot_cond, height = 12, width = 18)
ggsave(filename = "E:/Results/conditions_by_trial_collapse_no_cv.pdf", plot_cond_no_cv, height = 12, width = 18)

write_csv(conmed_all_trial_lvl, "E:/Results/comorbidity_per_trial.csv")

