library(tidyverse)

load("data/total_cases.rda")
source("script/function/calc_new_cases.R")


### STATES 
state_total_cases <- total_cases %>%
  select(-c(1, 2, 4)) %>%
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)


#CALCULATE GROWTH FACTOR BY COUNTY 
nc_state <- f_DataFrame(state_total_cases, f_NewCases)



#MAKE DATA TIDY AND CHOP INTO GROUPS 
nc_state_tidy <- nc_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "nc") %>%
  mutate(date = as.Date(date)) 
save(nc_state_tidy, file = "data/nc_state_tidy.rda")
save(nc_state_tidy, file = "state_gf/nc_state_tidy.rda") #send data to app

