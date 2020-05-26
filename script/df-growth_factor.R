library(tidyverse)

load("data/total_cases.rda")
source("script/calc_growth_factor.R")


### STATES 
state_total_cases <- total_cases %>%
  select(-c(1, 2, 4)) %>%
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)


#CALCULATE GROWTH FACTOR BY COUNTY 
gf_state <- f_DataFrame(state_total_cases, f_GrowthFactor)
save(gf_state, file = "data/gf_states.rda")


#MAKE DATA TIDY AND CHOP INTO GROUPS 
gf_state_tidy <- gf_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 
save(gf_state_tidy, file = "data/gf_state_tidy.rda")


### COUNTIES 
county_total_cases <- total_cases %>%
  filter(countyFIPS != 0) %>%
  select(-c(2:4)) %>%
  group_by(countyFIPS) %>%
  summarise_at(vars(-group_cols()), sum)


#CALCULATE GROWTH FACTOR BY COUNTY 
gf_county <- f_DataFrame(county_total_cases, f_GrowthFactor)
save(gf_county, file = "data/gf_county.rda")


#MAKE DATA TIDY AND CHOP INTO GROUPS 
gf_county_tidy <- gf_county %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 
save(gf_county_tidy, file = "data/gf_county_tidy.rda")
