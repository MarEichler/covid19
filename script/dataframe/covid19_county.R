library(tidyverse)
load("data/total_cases.rda")
source("script/function/calc_growth_factor.R")
source("script/function/calc_new_cases.R")
source("script/function/calc_apply_to_df.R")
source("script/function/calc_ma_gf.R")
source("script/variable/gf_cut_info.R")

### counties 
county_total_cases <- total_cases %>%
  filter(countyFIPS != 0) %>% #remove cases not assigned to counties 
  select(-c(2:4)) %>%
  group_by(countyFIPS) %>%
  summarise_at(vars(-group_cols()), sum)

#calculate gf and make tidy 
gf_counties_wide <- f_DataFrame(county_total_cases, f_GrowthFactor)

gf_counties <- gf_counties_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date)) 

#calculate new casaes and make tidy 
nc_counties_wide <- f_DataFrame(county_total_cases, f_NewCases) 

nc_counties <- nc_counties_wide %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "nc") %>%
  mutate(date = as.Date(date)) 

#create one data set of tidy data
covid19_county <- inner_join(gf_counties, nc_counties, by = c("countyFIPS" = "countyFIPS", "date" = "date"))
save(covid19_county, file = "data/covid19_county.rda") # send to data folder 

