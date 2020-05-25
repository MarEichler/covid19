
load("data/gf_state_tidy.rda")
load("data/gf_county_tidy.rda")


max_date <- max(gf_state_tidy$date)
ndays <- 14 #14 day average
min_date <- max_date - ndays +1


gf_state_ndays <- gf_state_tidy %>%
  filter(date >= min_date)

save(gf_state_ndays, file = "data/gf_state_ndays.rda")

gf_county_ndays <- gf_county_tidy %>%
  filter(date >= min_date)

save(gf_state_ndays, file = "data/gf_county_ndays.rda")