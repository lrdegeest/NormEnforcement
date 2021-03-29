# about -------------------------------------------------------------------
# this file produces Figure A2 in the manuscript (appendix)
# author: @lrdegeest


# set-up ------------------------------------------------------------------
library(tidyverse)
library(haven)

# data
df = read_dta("data/norms_data_estimation.dta")

custom_theme = theme_minimal() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text.x = element_text(face = "bold", family = "serif", size = 16),
        strip.text.y = element_text(face = "bold", family = "serif", size = 16), 
        panel.grid = element_blank(),
        text=element_text(family="serif", size = 14))

p = df %>% 
  mutate(treatment = ifelse(treatment == 1, "Unobserved", "Observed"),
         target_type = ifelse(target_type == 0, "Low", "High")) %>% 
  mutate(target_type = fct_relevel(target_type, "Low")) %>% 
  filter(target_sanction > 0) %>% 
  ggplot(aes(x=target_cont, y=target_sanction, fill = target_type)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  scale_fill_manual(values = c("blue", "orange"), guide = FALSE) + 
  facet_grid(treatment ~ target_type) + 
  labs(x = "Target Contribution", y = "Average Sanction") + 
  custom_theme 

ggsave("avg_pun.pdf", p, width = 9, height = 5)