library(tidyverse)
load("data/total_cases.rda")
source("script/function/calc_growth_factor.R")
source("script/function/calc_new_cases.R")

### STATES 
#sum up by states 
state_total_cases <- total_cases %>%
  select(-c(1, 2, 4)) %>%
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)

#calculate gf and make tidy 
gf_state <- f_DataFrame(state_total_cases, f_GrowthFactor) %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 

#calculate new casaes and make tidy 
nc_state <- f_DataFrame(state_total_cases, f_NewCases) %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "nc") %>%
  mutate(date = as.Date(date)) 

#create one data set 
covid19_state <- inner_join(gf_state, nc_state, by = c("state" = "state", "date" = "date"))


save(covid19_state, file = "data/covid19_state.rda") # send to data folder 
save(covid19_state, file = "state_gf/covid19_state.rda") #send data to app
