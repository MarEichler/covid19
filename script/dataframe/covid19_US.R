library(tidyverse)

source("script/variable/parameters.R")

#get total confirmed cases
#download data here: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/

tc_link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv"
td_link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv"

#total cases and new cases by day 
cases <- read.csv(tc_link) %>%
  select(-c(1:4)) %>% 
  mutate(X7.22.20 = as.integer(X7.22.20)) %>% #this column has numbers as 'factors' rather than integers; need to fix 
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



#growth rate 
#number of rows; use in `for loop`  
R <- nrow(covid19_US)

#GROWTH FACTOR  
covid19_US$growth_factor <- NA

#calculate growth rate
#start with 2 because when r=1; 1-1= 0 (don't have a 0th row)
for (r in 2:R){
  covid19_US$growth_factor[r] <- 
    #Growth Factor = New Cases Today / New Cases Yesterday 
    covid19_US$new_cases[r] /  covid19_US$new_cases[r-1] 
}

covid19_US <- covid19_US %>%
  mutate(
    growth_factor = ifelse(growth_factor == Inf, new_cases, growth_factor)
  )
######################################

#DEATH PERCENTAGE  
covid19_US <- mutate(covid19_US, death_percentage = total_deaths / total_cases)

#calculate moving average 14
ma_k <- 7
covid19_US$MA_growth_factor <- c(rep(NA, ma_k - 1), zoo::rollmean(covid19_US$growth_factor, k=ma_k, na.rm=TRUE))
covid19_US$MA_new_cases <- c(rep(NA, ma_k - 1), zoo::rollmean(covid19_US$new_cases, k=ma_k, na.rm = TRUE))
covid19_US$MA_new_deaths <- c(rep(NA, ma_k - 1), zoo::rollmean(covid19_US$new_deaths, k=ma_k, na.rm = TRUE))


####################################


save(covid19_US, file = "data/covid19_US.rda")
save(covid19_US, file = "diy-covid19-plots/covid19_US.rda") #send data to app
