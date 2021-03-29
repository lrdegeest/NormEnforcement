# about -------------------------------------------------------------------
# this file produces Figures 4 - 6 in the manuscript
# author: @lrdegeest


# set-up ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(ggpubr)

# update theme_minimal() for facetted plots (make it look like theme_classic but without the title boxes)
custom_theme = theme_minimal() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text.x = element_text(face = "bold", family = "serif", size = 15),
        strip.text.y = element_text(face = "bold", family = "serif", size = 15), 
        panel.grid = element_blank(),
        text=element_text(family="serif", size = 14))


do_recode = function(data){
  recoded_data = data %>% 
    mutate(sender_type = factor(ifelse(sender_type == 0, "Low", "High"))) %>% 
    mutate(sender_type = fct_relevel(sender_type, "Low")) %>% 
    mutate(model = ifelse(model == 1, "Extensive", "Intensive"))
  return(recoded_data)
}

# data --------------------------------------------------------------------
observed = read_dta("data/estimated_norms/norms_results_observed_all.dta")
unobserved_no_reveal = read_dta("data/estimated_norms/norms_results_unobserved_no_reveal.dta")
unobserved_one_reveal_unknown = read_dta("data/estimated_norms/norms_results_unobserved_one_reveal_unknown.dta")
unobserved_one_reveal_high = read_dta("data/estimated_norms/norms_results_unobserved_one_reveal_high.dta")
unobserved_both_reveal_low = read_dta("data/estimated_norms/norms_results_unobserved_both_reveal_low.dta")
unobserved_both_reveal_high = read_dta("data/estimated_norms/norms_results_unobserved_both_reveal_high.dta")


# colors for plots (low = blue, high = orange)
## requires low to be the first factor level
colors = c("blue", "orange")

# observed ----------------------------------------------------------------

# low (ext + int)
p_obs_low = observed %>% 
  do_recode() %>%
  filter(target_type == 0) %>% 
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Target: Low") + 
  scale_color_manual(values = colors, name = "Sender") + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  facet_wrap(~model) + 
  custom_theme  

# high (ext + int)
p_obs_high = observed %>% 
  do_recode() %>%
  filter(target_type == 1) %>% 
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Target: High") + 
  scale_color_manual(values = colors, name = "Sender") + 
  facet_wrap(~model) + 
  custom_theme  
  
# combine
p_observed = ggarrange(p_obs_low, p_obs_high, labels = "AUTO", common.legend = TRUE, legend = "bottom")
# save
ggsave("likelihood_observed.pdf", p_observed, width = 9, height = 4)




# unobserved: no high reveal ----------------------------------------------

# plot
p_no_reveal = unobserved_no_reveal %>% 
  do_recode() %>% 
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Unobserved: No High Reveal", subtitle = bquote("Target: "~x[j]~ " = [0,10]")) + 
  scale_color_manual(values = colors, name = "Sender") + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  facet_wrap(~model) + 
  custom_theme  +
  theme(legend.position="bottom")

# save
ggsave("likelihood_unobserved_no_reveal.pdf", p_no_reveal, width = 6, height = 4)



# unobserved: one/both high reveal ----------------------------------------

# ONE REVEAL
## unknown
p_one_reveal_unknown = unobserved_one_reveal_unknown %>% 
  do_recode() %>%
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Unobserved: One High Reveal", subtitle = bquote("Target: "~x[j]~ " = [0,10]")) + 
  scale_color_manual(values = colors, name = "Sender") + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  facet_wrap(~model) + 
  custom_theme

# ONE REVEAL
## High
p_one_reveal_high = unobserved_one_reveal_high %>% 
  filter(sender_type == 0) %>% 
  do_recode() %>% 
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Unobserved: One High Reveal", subtitle = "Target: High") + 
  scale_color_manual(values = colors, name = "Sender") + 
  scale_x_continuous(breaks=c(11, 15, 20, 25, 30)) + 
  facet_wrap(~model) + 
  custom_theme + 
  theme(legend.position = "None")

# combine
#p_unobserved_one_high = ggarrange(p_one_reveal_unknown, p_one_reveal_high, labels = "AUTO", common.legend = TRUE, legend = "bottom")
# save
#ggsave("likelihood_unobserved_one_high.pdf", p_unobserved_one_high, width = 9, height = 4)

# BOTH REVEAL
## Low
p_both_reveal_low = unobserved_both_reveal_low %>% 
  do_recode() %>%
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Unobserved: Both High Reveal", subtitle ="Target: Low") + 
  scale_color_manual(values = colors, name = "Sender") + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  facet_wrap(~model) + 
  custom_theme


# BOTH REVEAL
## High
p_both_reveal_high = unobserved_both_reveal_high %>% 
  do_recode() %>%
  ggplot(aes(x = norm, y = normal_ll, group = sender_type, color = sender_type)) + 
  geom_line(size = 1.25) + 
  labs(x = "Norm", y = "Normalized LL", title = "Unobserved: Both High Reveal", subtitle ="Target: High") + 
  scale_color_manual(values = colors, name = "Sender") + 
  scale_x_continuous(breaks=c(11, 15, 20, 25, 30)) + 
  facet_wrap(~model) + 
  custom_theme


# combine
#p_unobserved_both_high = ggarrange(p_both_reveal_low, p_both_reveal_high, labels = "AUTO", common.legend = TRUE, legend = "bottom")
# save
#ggsave("likelihood_unobserved_both_high.pdf", p_unobserved_both_high, width = 9, height = 4)


# combine one reveal/both reveal into one figure
p_reveal = ggarrange(p_one_reveal_unknown, p_one_reveal_high, p_both_reveal_low, p_both_reveal_high, labels = "AUTO", common.legend = TRUE, legend = "bottom")
ggsave("likelihood_unobserved_reveal.pdf", p_reveal, width = 9, height = 6)


