library(tidyverse)
library(quantmod)
library(zoo)

#scripts 
source("script/function/calc_growth_factor.R")
source("script/function/calc_new_cases.R")
source("script/function/calc_apply_to_df.R")
source("script/function/calc_ma_7.R")
source("script/function/calc_perc_7.R")
source("script/function/calc_norm_to_0_to_1.R")

#data
load("data/total_cases.rda")
load("data/total_deaths.rda")

### STATES 
# SUM TOTAL CASES
state_total_cases <- total_cases %>%
  select(-c(1, 2, 4)) %>%
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)

#DAILY NEW CASES 
nc_state_wide <- f_DataFrame(state_total_cases, f_NewCases) %>%
  #if re-adjust total 'new cases' may be negative; set to NA 
  mutate_at(vars(contains("2020")), funs(ifelse(. <0, NA, .))) 
  

nc_state <- nc_state_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "nc") %>%
  mutate(date = as.Date(date)) 

# 7-DAY MA OF NEW CASES 
nc_state_ma7_wide<- f_DataFrame(as_tibble(nc_state_wide), f_ma7)

nc_state_ma7 <- nc_state_ma7_wide %>%
  pivot_longer(cols = c(-1), names_to = "date", values_to = "nc_ma7") %>%
  mutate(date = as.Date(date))

#DAILY GROWTH FACTOR 
gf_state_wide <- f_DataFrame(state_total_cases, f_GrowthFactor) 

gf_state <- gf_state_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 

# 7-DAY MA OF GROTH FACTOR
gf_state_ma7_wide<- f_DataFrame(as_tibble(gf_state_wide), f_ma7)

gf_state_ma7 <- gf_state_ma7_wide %>%
  pivot_longer(cols = c(-1), names_to = "date", values_to = "gf_ma7") %>%
  mutate(date = as.Date(date))

# 7-DAY PERC DIFF OF 7-DAY MA OF NEW CASES 
nc_state_ma7_perc_wide<- f_DataFrame(as_tibble(nc_state_ma7_wide), f_perc7)

nc_state_ma7_perc <- nc_state_ma7_perc_wide %>%
  pivot_longer(cols = c(-1), names_to = "date", values_to = "nc_ma7_perc") %>%
  mutate(date = as.Date(date))

# 7-DAY MA OF NEW CASES NORMALIZED TO 0-1 (i.e. where is the curve)
nc_state_ma7_norm_wide <- f_DataFrame(as_tibble(nc_state_ma7_wide), f_norm01)

nc_state_ma7_norm <- nc_state_ma7_norm_wide %>%
  pivot_longer(cols = c(-1), names_to = "date", values_to = "nc_ma7_norm") %>%
  mutate(date = as.Date(date))


# SUM TOTAL CASES
state_total_deaths <- total_deaths %>%
  select(-c(1, 2, 4)) %>%
  group_by(state) %>%
  summarise_at(vars(-group_cols()), sum)

#DAILY NEW CASES 
nd_state_wide <- f_DataFrame(state_total_deaths, f_NewCases) %>%
  #if re-adjust total 'new cases' may be negative; set to NA 
  mutate_at(vars(contains("2020")), funs(ifelse(. <0, NA, .))) 

nd_state <- nd_state_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "nd") %>%
  mutate(date = as.Date(date)) 



#put all the stuff together 
ma7_df_nc     <- left_join(nc_state_ma7, nc_state_ma7_perc, by = c("state"="state", "date" ="date"))
ma7_df        <- left_join(ma7_df_nc, gf_state_ma7,         by = c("state"="state", "date" ="date"))
raw_df        <- left_join(gf_state, nc_state,              by = c("state"="state", "date" ="date")) 
raw_df        <- left_join(nd_state, raw_df,                by = c("state"="state", "date" ="date"))
norm_df       <-  nc_state_ma7_norm 
ma7_raw_df    <- left_join(raw_df, ma7_df,                   by = c("state"="state", "date" ="date"))
covid19_state <- left_join(ma7_raw_df, norm_df,              by = c("state" = "state", "date" = "date"))

#save data frames 
save(covid19_state, file = "data/covid19_state.rda") # send to data folder 
save(covid19_state, file = "diy-covid19-plots/covid19_state.rda") #send data to app

########################################################


#WEEKLY DATA FRAME FOR GIFs
min_date <-min(covid19_state$date)
max_date <- max(covid19_state$date) - 7

#create selected days to 'start' week 
select_days <- seq(as.Date(min_date), as.Date(max_date), "week") 

length(select_days)

weeks <- data.frame(
  "end_of_week" = select_days
  , "week" = seq(1, length(select_days), 1)
)


covid19_state_weekly <- nc_state_ma7_perc %>%
  filter(date %in% select_days) %>%
  left_join(., weeks, by = c("date" = "end_of_week")) 

save(covid19_state_weekly, file = "data/covid19_state_weekly.rda") # send to data folder 

########################################################