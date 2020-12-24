
#SOURCE: https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/
usafacts_cnty_link <- "https://static.usafacts.org/public/data/covid-19/covid_confirmed_usafacts.csv"
usafacts_pop_link  <- "https://static.usafacts.org/public/data/covid-19/covid_county_population_usafacts.csv"


state_abb_name <- tibble(
      abb = state.abb
    , name = state.name
  ) %>% 
  rows_insert(tibble(abb = "DC", name = "District of Columbia")) %>%
  rows_insert(tibble(abb = "AS", name = "American Samoa")) %>%
  rows_insert(tibble(abb = "GU", name = "Guam")) %>%
  rows_insert(tibble(abb = "MP", name = "Nothern Mariana Islands")) %>%
  rows_insert(tibble(abb = "PR", name = "Puerto Rico")) %>%
  rows_insert(tibble(abb = "VI", name = "Virgin Islands")) %>%
  rows_insert(tibble(abb = "USA", name = "United States"))

##################################

 usafacts_cases <- read_csv(usafacts_cnty_link)   %>% 
  pivot_longer(cols = c(5:ncol(.)), names_to = "date", values_to = "case_total") %>%
  mutate(date = mdy(date))%>%
  #remove cases that havea not been alocated to a specific county 
  filter(countyFIPS != 0) 
  

#########################
#########population 

usafacts_pop <- read_csv(usafacts_pop_link) %>%
  #remove state-wide row 
  filter(countyFIPS != 0)



########################
######## combine to main df



covid19_county <- left_join(usafacts_cases, usafacts_pop, by = c("countyFIPS", "County Name", "State"))  %>%
  rename( #new = old
      county      = `County Name`
    , state_abbv  = State
    , county_fips = countyFIPS
    , state_fips  = stateFIPS
  ) %>%
  filter(date > max(date) - 8) %>% #subset data do it doesn't take so long
  #arrange by date 
  arrange(date, state_fips, county_fips) %>% 
  #have all of the calcs done by each county 
  group_by(county_fips) %>%
  mutate(
    #new cases for each day; take total from today minus total from yesterday
      case_new = case_total - lag(case_total)
    #growth factor = new cases today / new cases yesterday
    #if new cases yesterday = 0; set gf = cases new today (technically = inf)
   # , gf = ifelse(lag(case_new) == 0 | is.na(lag(case_new)), case_new , case_new / lag(case_new))
  )  %>% 
  #moving averages 
  mutate( #THIS STEP TAKES ABOUT 5 SECONDS TO RUN!! (~3-045 if do all dates)
      case_MA7  = c(rep(NA, 6), zoo::rollmean(case_new,  k = 7, na.rm = TRUE))
   # , gf_MA7    = c(rep(NA, 6), zoo::rollmean(gf,        k = 7, na.rm = TRUE))
  ) %>%
  #ungroup
  ungroup() %>%
  # per capita (prop of population)
  mutate(
      case_total_PC    = case_total    / population 
    , case_new_PC      = case_new      / population 
    , case_MA7_PC      = case_MA7      / population 
  ) %>%
  #create amounts per 100,000 people
  mutate_at(vars(contains("_PC")), funs("100k"= .*100000)) %>% 
  mutate(
    county_fips = as.character(county_fips)
    , state_fips = as.character(state_fips)
  ) %>%
  select(
      date
    , contains("county")
    , contains("state")
    , contains("case")
    , population
    ) 

