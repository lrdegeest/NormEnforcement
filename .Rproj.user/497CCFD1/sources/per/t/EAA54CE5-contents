# about -------------------------------------------------------------------
# this file produces Figure 7 in the manuscript
# author: @lrdegeest

# set-up ------------------------------------------------------------------
library(tidyverse)
library(haven)
library(ggpubr)

custom_theme = theme_minimal() + 
  theme(strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        strip.text.x = element_text(face = "bold", family = "serif", size = 15),
        strip.text.y = element_text(face = "bold", family = "serif", size = 15), 
        panel.grid = element_blank(),
        text=element_text(family="serif", size = 14),
        legend.key=element_blank())

# data --------------------------------------------------------------------
ec = read_dta("data/estimated_costs/ec_observed_2.dta")
ec_no_reveal = read_dta("data/estimated_costs/ec_no_reveal.dta")
ec_one_reveal = read_dta("data/estimated_costs/ec_one_reveal.dta")
ec_both_reveal = read_dta("data/estimated_costs/ec_both_reveal.dta")


# expected payoffs --------------------------------------------------------

# calculate expected payoffs for low and high

## high
high_df = data.frame("contribute" = 0:30, 
                     "observed" = ec$TotalHighObserved,
                     "no_reveal" = c(ec_no_reveal$TotalHigh, rep(NA, length = 20)),
                     "one_reveal" = c(ec_one_reveal$TotalHighUnobserved_Unknown[1:11], ec_one_reveal$TotalHighUnobserved_reveal), 
                     "both_reveal" = c(rep(NA, 11), ec_both_reveal$TotalHigh),
                     "theory" = (30 - 0:30) + 0.4*(10 + 10 + 30 + 0:30) - 32) %>% 
  pivot_longer(-contribute) %>% 
  mutate(pi = (30 - contribute) + 0.4*(contribute + 10 + 10 + 30) - value)


## low
low_df = data.frame("contribute" = 0:10, 
                    "observed" = ec$TotalLowObserved[1:11],
                    "no_reveal" = ec_no_reveal$TotalLow[1:11],
                    "one_reveal" = ec_one_reveal$TotalLowUnobserved[1:11],
                    "both_reveal" = ec_both_reveal$TotalLow[1:11],
                    "theory" = (10 - 0:10) + 0.4*(10 + 30 + 30 + 0:10) - 32) %>% 
  pivot_longer(-contribute) %>% 
  mutate(pi = (10 - contribute) + 0.4*(contribute + 10 + 30 + 30) - value)




# plot expected costs -----------------------------------------------------

p_high_cost = high_df %>% 
  filter(name != "observed") %>% 
  mutate(name = factor(name)) %>% 
  ggplot() + 
  geom_line(data = filter(high_df, name == "observed"), mapping = aes(x = contribute, y = value), size = 1.5, color = "orange", alpha = 0.75) + 
  geom_line(data = filter(high_df, name == "theory"), mapping = aes(x = contribute, y = value), size = 1.5, color = "black", alpha = 0.25) + 
  geom_line(data = filter(high_df, !(name %in% c("observed", "theory"))), mapping = aes(x = contribute, y = value, shape = name), color = "orange") +
  geom_point(data = filter(high_df, !(name %in% c("observed", "theory"))), mapping = aes(x = contribute, y = value, shape = name), size = 2, fill = "white", color = "orange") + 
  scale_shape_manual(name="Unobserved", values = c(24,22,21),
                     breaks = c("no_reveal", "one_reveal", "both_reveal"), 
                     labels = c("No High Reveal", "One High Reveal", "Both High Reveal")) + 
  labs(x = bquote("Contribution ("~x[i]~")"), y = "Expected punishment (EDs)", title = "High") + 
  custom_theme + 
  scale_y_continuous(limits = c(0,30), breaks = c(0, 6, 10, 18, 20, 30)) +
  theme(axis.text.y = element_text(color = c("grey20", "red", "grey20", "red", "grey20", "grey20")),
        axis.ticks.y = element_line(color = c("grey20", "red", "grey20", "red", "grey20", "grey20"))) + 
  theme(legend.position = c(0.8, 0.7)) + 
  annotate(
    geom = "curve", x = 10, y = 6, xend = 10, yend = 8, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    geom = "curve", x = 15, y = 9, xend = 19, yend = 9, 
    curvature = 0, arrow = arrow(length = unit(2, "mm")), color = "gray") +
  annotate(geom = "text", x = 10, y = 9, label = "Observed", family = "serif") + 
  annotate(geom = "text", x = 21, y = 9, label = "Theory", family = "serif", color = "gray") #+ 
  #annotate(geom = "text", x = 8, y = 28, label = bquote("Optimal sanction is 18 when "~x[i]~"= 0"), family = "serif", color = "red", size = 3.5)

p_low_cost = low_df %>% 
  filter(name != "observed") %>% 
  mutate(name = factor(name)) %>% 
  ggplot() + 
  geom_line(data = filter(low_df, name == "observed"), mapping = aes(x = contribute, y = value), size = 1.5, color = "blue", alpha = 0.75) + 
  geom_line(data = filter(low_df, name == "theory"), mapping = aes(x = contribute, y = value), size = 1.5, color = "black", alpha = 0.25) + 
  geom_line(data = filter(low_df, !(name %in% c("observed", "theory"))), mapping = aes(x = contribute, y = value, shape = name), color = "blue") +
  geom_point(data = filter(low_df, !(name %in% c("observed", "theory"))), mapping = aes(x = contribute, y = value, shape = name), size = 2, fill = "white", color = "blue") + 
  scale_shape_manual(name="Unobserved", values = c(24,22,21),
                     breaks = c("no_reveal", "one_reveal", "both_reveal"), 
                     labels = c("No High Reveal", "One High Reveal", "Both High Reveal")) + 
  labs(x = bquote("Contribution ("~x[i]~")"), y = "Expected punishment (EDs)", title = "Low") + 
  custom_theme  + 
  scale_y_continuous(limits = c(0,30), breaks = c(0, 6, 10, 18, 20, 30)) +
  theme(axis.text.y = element_text(color = c("grey20", "red", "grey20", "red", "grey20", "grey20")),
        axis.ticks.y = element_line(color = c("grey20", "red", "grey20", "red", "grey20", "grey20"))) + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  theme(legend.position = c(0.8, 0.7)) + 
  annotate(
    geom = "curve", x = 1.5, y = 5.5, xend = 1.5, yend = 7.5, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 1.5, y = 8.5, label = "Observed", family = "serif") #+ 
  #annotate(geom = "text", x = 2.5, y = 28, label =bquote("Optimal sanction is 6 when "~x[i]~"= 0"), family = "serif", color = "red", size = 3.5) 


# plot expected payoffs ---------------------------------------------------

p_high = high_df %>% 
  filter(name != "observed") %>%
  mutate(name = factor(name)) %>% 
  ggplot() + 
  geom_segment(x=0, xend = 30, y=32, yend=32, color = "gray", size = 1.5) + 
  geom_line(data = filter(high_df, name == "observed"), mapping = aes(x = contribute, y = pi), size = 1.5, color = "orange", alpha = 0.75) + 
  geom_line(data = filter(high_df, name %in% c("no_reveal", "one_reveal", "both_reveal")), mapping = aes(x = contribute, y = pi, group = name), color = "orange") +
  geom_point(data = filter(high_df, name %in% c("no_reveal", "one_reveal", "both_reveal")), mapping = aes(x = contribute, y = pi, shape = name), size = 2, fill = "white", color = "orange") +
  labs(x = bquote("Contribution ("~x[i]~")"), y = "Expected payoffs (EDs)", title = "High") + 
  custom_theme + 
  scale_shape_manual(name="Unobserved", values = c(24,22,21),
                     breaks = c("no_reveal", "one_reveal", "both_reveal"), 
                     labels = c("No High Reveal", "One High Reveal", "Both High Reveal")) + 
  scale_y_continuous(limits = c(10,44), breaks = c(10, 20, 30, 32, 40)) +
  theme(axis.text.y = element_text(color = c("grey20", "grey20", "grey20", "red", "grey20")),
        axis.ticks.y = element_line(color = c("grey20", "grey20", "grey20", "red", "grey20"))) + 
  theme(legend.position = "none") +
  annotate(
    geom = "curve", x = 8, y = 38, xend = 8, yend = 36, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 8, y = 35, label = "Observed", family = "serif") + 
  #annotate(geom = "text", x = 23, y = 15, label = "Social optimum = 32 EDs", family = "serif", size = 3.5, color = "red") + 
  annotate(
    geom = "curve", x = 15, y = 32, xend = 15, yend = 27, 
    curvature = 0, arrow = arrow(length = unit(2, "mm")), color = "gray") +
  annotate(geom = "text", x = 15, y = 26, label = "Theory", family = "serif", color = "gray") +
  theme(text=element_text(family="serif", size = 14))


p_low = low_df %>% 
  filter(name != "observed") %>%
  mutate(name = factor(name)) %>% 
  ggplot() + 
  geom_segment(x=0, xend = 30, y=32, yend=32, color = "gray", size = 1.5) + 
  geom_line(data = filter(low_df, name == "observed"), mapping = aes(x = contribute, y = pi), size = 1.5, color = "blue", alpha = 0.75) + 
  geom_line(data = filter(low_df, name %in% c("no_reveal", "one_reveal", "both_reveal")), mapping = aes(x = contribute, y = pi, group = name), color = "blue") +
  geom_point(data = filter(low_df, name %in% c("no_reveal", "one_reveal", "both_reveal")), mapping = aes(x = contribute, y = pi, shape = name), size = 2, fill = "white", color = "blue") +
  labs(x = bquote("Contribution ("~x[i]~")"), y = "Expected payoffs (EDs)", title = "Low") + 
  custom_theme + 
  scale_shape_manual(name="Unobserved", values = c(24,22,21)) + 
  scale_y_continuous(limits = c(10,44), breaks = c(10, 20, 30, 32, 40)) +
  theme(axis.text.y = element_text(color = c("grey20", "grey20", "grey20", "red", "grey20")),
        axis.ticks.y = element_line(color = c("grey20", "grey20", "grey20", "red", "grey20"))) + 
  scale_x_continuous(breaks=c(0,5,10)) + 
  theme(legend.position = "none") +
  annotate(
    geom = "curve", x = 1.5, y = 28, xend = 1.5, yend = 31, 
    curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 1.5, y = 27, label = "Observed", family = "serif") + 
  #annotate(geom = "text", x = 8, y = 15, label = "Social optimum = 32 EDs", family = "serif", size = 3.5, color = "red") + 
  theme(text=element_text(family="serif", size = 14))




# combine -----------------------------------------------------------------

p = ggarrange(p_low_cost, p_high_cost, p_low, p_high, labels = "AUTO")
ggsave("ec_epi_updated.pdf", p, width = 9, height = 6)



