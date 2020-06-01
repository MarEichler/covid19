
load("data/covid19_state.rda")
load("data/covid19_county.rda")


max_date <- max(covid19_state$date)
ndays <- 14 #14 day average
min_date <- max_date - ndays +1


covid19_state_ndays <- covid19_state %>%
  filter(date >= min_date)

save(covid19_state_ndays, file = "data/covid19_state_ndays.rda")

covid19_county_ndays <- covid19_county %>%
  filter(date >= min_date)

save(covid19_county_ndays, file = "data/covid19_county_ndays.rda")