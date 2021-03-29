# about -------------------------------------------------------------------
# this file produces Figure 2 in the manuscript
# author: @lrdegeest


# set-up ------------------------------------------------------------------
library(tidyverse)
library(haven)

# theme
custom_theme = theme_minimal() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text.x = element_text(face = "bold", family = "serif", size = 16),
        strip.text.y = element_text(face = "bold", family = "serif", size = 16), 
        panel.grid = element_blank(),
        text=element_text(family="serif", size = 14))

# data
df = read_dta("data/norms_data_estimation.dta")

# rename treatments/types
df = df %>% 
  mutate(treatment = ifelse(treatment == 1, "Unobserved", "Observed"),
         sender_type = factor(ifelse(sender_type == 0, "Low", "High")),
         sender_type = fct_relevel(sender_type, "Low"),
         target_type = factor(ifelse(target_type == 0, "Low", "High")),
         target_type = fct_relevel(target_type, "Low"))

# reveal
df = df %>% 
  mutate(reveal = ifelse(target_cont > 10, 1, 0)) %>% 
  group_by(groupid, period) %>% 
  mutate(sum_reveal = sum(reveal)) %>% 
  ungroup() %>% 
  mutate(sum_reveal_str = case_when(
    sum_reveal == 0 ~ "No High Reveal",
    sum_reveal == 3 ~ "One High Reveal",
    sum_reveal == 6 ~ "Both High Reveal")) %>% 
  mutate(sum_reveal_str = factor(sum_reveal_str)) %>% 
  mutate(sum_reveal_str = fct_relevel(sum_reveal_str, "No High Reveal", "One High Reveal", "Both High Reveal"))


# observed vs unobserved
p1 = df %>% 
  filter(target_sanction > 0) %>% 
  count(treatment, sender_type) %>% 
  group_by(treatment) %>% 
  mutate(n_total = sum(n), 
         pct = round(n / n_total,2)) %>% 
  ggplot(aes(x = treatment, y = n, fill = sender_type)) + 
  geom_col(position = "dodge") + 
  geom_text(aes(label =  scales::percent(pct,accuracy = 1L)), position = position_dodge(width = 0.9), vjust = -0.6, family = "serif") + 
  scale_fill_manual(values = c("blue", "orange"), name = "Sender") +
  labs(x = "", y = "Punishment sent  (cases > 0)", title = "Treatments") + 
  ylim(0, 520) + 
  custom_theme

# unobserved by info state
p2 = df %>% 
  filter(target_sanction > 0) %>% 
  filter(treatment == "Unobserved") %>% 
  count(sum_reveal_str, sender_type) %>% 
  group_by(sum_reveal_str) %>% 
  mutate(n_total = sum(n), 
         pct = round(n / n_total,2)) %>% 
  ggplot(aes(x = sum_reveal_str, y = n, fill = sender_type)) + 
  geom_col(position = "dodge") + 
  geom_text(aes(label =  scales::percent(pct,accuracy = 1L)), position = position_dodge(width = 0.9), vjust = -0.75, family = "serif") + 
  scale_fill_manual(values = c("blue", "orange"), name = "Sender") +
  labs(x = "", y = "Punishment sent  (cases > 0)", title = "Information states (Unobserved)") + 
  ylim(0, 520) + 
  custom_theme

p = ggpubr::ggarrange(p1, p2, common.legend = TRUE, legend = "bottom", labels = "AUTO")
ggsave("enforcement_contributions.pdf", p, width = 9, height = 4)

