#library(tidyverse)

#get total confirmed cases
#download data here: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

tc_link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
td_link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"

#total cases and new cases by day 
cases <- read.csv(tc_link) %>%
  select(-c(1:4)) %>% 
  summarize_all(sum) %>%
  pivot_longer(cols =  everything(), names_to = "date", values_to = "total_cases") %>%
  mutate(date = as.Date(date, format = "X%m.%d.%y")) %>%
  arrange(date)

n_days <- length(unique(cases$date))

tc_today <- cases[[2]]
tc_yesterday <- c(0, tc_today[1:n_days-1])
new_cases <- tc_today - tc_yesterday

cases <- cbind(cases, new_cases)
########################################


#total deaths and new deaths by day 
deaths <-  read.csv(td_link) %>%
  select(-c(1:4)) %>% 
  summarize_all(sum) %>%
  pivot_longer(cols =  everything(), names_to = "date", values_to = "total_deaths") %>%
  mutate(date = as.Date(date, format = "X%m.%d.%y")) %>%
  arrange(date)

n_days <- length(unique(deaths$date))

td_today <- deaths[[2]]
td_yesterday <- c(0, td_today[1:n_days-1])
new_deaths <- td_today - td_yesterday

deaths <- cbind(deaths, new_deaths)
########################################


#merge data together 
covid19_US <- left_join(cases, deaths, by = c("date"))
#########################################



save(covid19_US, file = "data/covid19_US.rda")
