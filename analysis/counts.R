# about -------------------------------------------------------------------
# counting instances of contributions by endowment/treatment
# author: @lrdegeest

# set-up ------------------------------------------------------------------
library(tidyverse)
library(haven)

# data
df = read_dta("data/data_labels.dta")

# counts ------------------------------------------------------------------
# proportion of contributions equal to endowments across treatments
df %>% 
  mutate(endowment = ifelse(endowment == 1, 30, 10)) %>% 
  mutate(full_contribute = ifelse(contribute == endowment, 1, 0)) %>% 
  group_by(treatment, endowment) %>% 
  summarise(mean(full_contribute))

# high contributing <=10 or >10
df %>% 
  filter(endowment == 1) %>% 
  mutate(cont10orless = ifelse(contribute <= 10, 1, 0), 
         cont11ormore = ifelse(contribute > 10, 1, 0)) %>% 
  group_by(treatment) %>% 
  summarise(mean(cont10orless), mean(cont11ormore))

# high contributing 30
df %>% 
  filter(endowment == 1) %>% 
  mutate(cont30 = ifelse(contribute == 30, 1, 0)) %>% 
  group_by(treatment) %>% 
  summarise(mean(cont30))


# low contributing 10
df %>% 
  filter(endowment == 0) %>% 
  mutate(cont10 = ifelse(contribute == 10, 1, 0)) %>% 
  group_by(treatment) %>% 
  summarise(mean(cont10))

