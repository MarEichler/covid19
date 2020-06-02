library(tidyverse)
load("data/total_cases.rda")
source("script/function/calc_growth_factor.R")
source("script/function/calc_new_cases.R")
source("script/function/calc_apply_to_df.R")
source("script/function/calc_ma_gf.R")
source("script/variable/gf_cut_info.R")

### STATES 
#sum up by states 
state_total_cases <- total_cases %>%
  select(-c(1, 2, 4)) %>%
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)

#calculate gf and make tidy 
gf_state_wide <- f_DataFrame(state_total_cases, f_GrowthFactor) 

gf_state <- gf_state_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 

#calculate new casaes and make tidy 
nc_state_wide <- f_DataFrame(state_total_cases, f_NewCases) 

nc_state <- nc_state_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "nc") %>%
  mutate(date = as.Date(date)) 

#create one data set 
covid19_state <- inner_join(gf_state, nc_state, by = c("state" = "state", "date" = "date"))


save(covid19_state, file = "data/covid19_state.rda") # send to data folder 
save(covid19_state, file = "diy-covid19-plots/covid19_state.rda") #send data to app


#weekly gf avg 
state_rollmean <- f_DataFrame(as_tibble(gf_state_wide), f_ma7)

min_date <- min(colnames(state_rollmean)[-1])
max_date <- max(colnames(state_rollmean)[-1])

#create selected days to 'start' week 
select_days <- seq(as.Date(min_date), as.Date(max_date), "week") 

select_days

length(select_days)

weeks <- data.frame(
  "week_start_day" = select_days
  , "week" = seq(1, length(select_days), 1)
)


covid19_state_weekly <- state_rollmean %>%
  pivot_longer(c(-1), names_to = "date", values_to = "gf_ma") %>%
  mutate(date = as.Date(date)) %>%
  filter(date %in% select_days) %>%
  left_join(., weeks, by = c("date" = "week_start_day"))%>%
  mutate( growth_factor = cut(gf_ma, breaks = gf_breaks , labels = gf_labels , right = gf_right)) 

save(covid19_state_weekly, file = "data/covid19_state_weekly.rda") # send to data folder 



