# about -------------------------------------------------------------------
# this file produces Figure 2 in the manuscript
# author: @lrdegeest


# set-up ------------------------------------------------------------------
library(tidyverse)
library(haven)

custom_theme = theme_minimal() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text.x = element_text(face = "bold", family = "serif", size = 15),
        strip.text.y = element_text(face = "bold", family = "serif", size = 15), 
        panel.grid = element_blank(),
        text=element_text(family="serif", size = 14))

# data
df = read_dta("data/data_labels.dta")

# average contributions over time -----------------------------------------

avg_contributions = df %>% 
  mutate(endowment = ifelse(endowment == 0, 10, 30)) %>% 
  mutate(pct_cont = contribute / endowment) %>% 
  group_by(period, endowment, treatment) %>% 
  summarise(avg_cont = mean(contribute), 
            avg_pct_cont = mean(pct_cont)) %>% 
  ungroup()


p_time_low = avg_contributions %>% 
  filter(endowment == 10) %>% 
  ggplot(aes(x = period, y = avg_cont, group = factor(treatment), linetype = factor(treatment))) + 
  geom_line(color = "blue") + 
  scale_linetype_manual(name = "",
                        values=c("dashed", "solid"),
                        labels = c("Unobserved", "Observed")) +
  custom_theme + 
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,12), breaks = c(0,5,10)) + 
  labs(x = "Period", y = "Average Contribution", title = "Low") +
  theme(text=element_text(family="serif", size = 14)) +
  geom_hline(yintercept = 10, color = "red", linetype = "dotted") + 
  annotate(geom = "text", x = 11, y = 9.25, label = "Observed", color = "black", family="serif") +
  annotate(geom = "text", x = 11, y = 5., label = "Unobserved", color = "black", family="serif") + 
  annotate(geom = "text", x = 40, y = 10.75, label = "Social optimum", color = "red", family="serif")

p_time_high = avg_contributions %>% 
  filter(endowment == 30) %>% 
  ggplot(aes(x = period, y = avg_cont, group = factor(treatment), linetype = factor(treatment))) + 
  geom_line(color = "orange") + 
  scale_linetype_manual(name = "",
                        values=c("dashed", "solid"),
                        labels = c("Unobserved", "Observed")) +
  custom_theme + 
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0,34), breaks = c(0,10,20,30)) + 
  labs(x = "Period", y = "Average Contribution", title = "High") +
  theme(text=element_text(family="serif", size = 14)) + 
  geom_hline(yintercept = 30, color = "red", linetype = "dotted") + 
  annotate(geom = "text", x = 25, y = 25, label = "Observed", color = "black", family="serif") +
  annotate(geom = "text", x = 25, y = 15, label = "Unobserved", color = "black", family="serif") +
  annotate(geom = "text", x = 40, y = 32, label = "Social optimum", color = "red", family="serif")



# distributions -----------------------------------------------------------


# define linetype by time 
lines = c("unobserved" = "dashed", "observed" = "solid")

# now make the plots

## low 
low = df %>% 
  ggplot() + 
  stat_ecdf(aes(x = contribute, linetype = "unobserved"), data = filter(df, endowment == 0 & treatment == 0), color = "blue") + 
  stat_ecdf(aes(x = contribute, linetype = "observed"), data = filter(df, endowment == 0 & treatment == 1), color = "blue") + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  labs(x = "Contribution", y = "Proportion (ECDF)", title = "Low") + 
  scale_linetype_manual(values = lines, 
                        name="", 
                        labels = c("Observed", "Unobserved")) + 
  custom_theme + 
  theme(legend.position = "none") +
  annotate(geom = "text", x = 7, y = 0.625, label = "Unobserved", family="serif") + 
  annotate(geom = "text", x = 7, y = 0.21, label = "Observed", family="serif")

## low unobserved
high = df %>% 
  ggplot() + 
  geom_vline(xintercept = 10, color = "gray", linetype = "solid") + 
  stat_ecdf(aes(x = contribute, linetype = "unobserved"), data = filter(df, endowment == 1 & treatment == 0), color = "orange") + 
  stat_ecdf(aes(x = contribute, linetype = "observed"), data = filter(df, endowment == 1 & treatment == 1), color = "orange") + 
  labs(x = "Contribution", y = "Proportion (ECDF)", title = "High") + 
  scale_linetype_manual(values = lines, 
                        name="", 
                        labels = c("Observed", "Unobserved")) + 
  custom_theme + 
  theme(legend.position = "off") +
  theme(text=element_text(family="serif", size = 14)) + 
  annotate(geom = "text", x = 15, y = 0.78, label = "Unobserved", family="serif") + 
  annotate(geom = "text", x = 15, y = 0.21, label = "Observed", family="serif")


# combine everything
p = ggpubr::ggarrange(p_time_low, low, p_time_high, high, labels = "AUTO")
ggsave("summary_cont.pdf", p, width = 9, height = 6)
