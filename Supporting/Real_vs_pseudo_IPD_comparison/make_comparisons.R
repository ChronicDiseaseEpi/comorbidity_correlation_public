library(tidyverse)
cmpr <- read_csv("Supporting/Real_vs_pseudo_IPD_comparison/model_comparison_real_pseudo_ipd.csv")

cmpr <- cmpr %>% 
  mutate(cond_n = paste0(formatC(n, width = 4, flag = "0"), " ", index_condition),
         terms = paste0(term1,term2))

cmpr_mn <- ggplot(cmpr, aes(x = terms,
                            y = mean,
                            ymin = `2.5%`,
                            ymax = `97.5%`,
                            colour = data_type)) +
  geom_point(position = position_dodge(0.35)) +
  geom_linerange(position = position_dodge(0.35)) +
  facet_wrap(~cond_n) +
  scale_x_discrete("First comorbidity and second comorbidity") +
  scale_y_continuous("Correlation")
cmpr_mn


mydif <- cmpr %>% 
  select(cond_n, terms, data_type, mean) %>% 
  spread(data_type, mean) %>% 
  mutate(res = pseudo - actual)

diff_mn <- ggplot(mydif, aes(x = terms,
                 y = res)) +
  geom_point() +
  facet_wrap(~cond_n) +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -0.02, linetype = "dashed") +
  geom_hline(yintercept =  0.02, linetype = "dashed") +
  scale_x_discrete("First comorbidity and second comorbidity") +
  scale_y_continuous("Pseudo - actual data")

pdf("Supporting/Real_vs_pseudo_IPD_comparison/plot_compare_actual_pseudo.pdf", width = 12, height = 10)
cmpr_mn
diff_mn
dev.off()
