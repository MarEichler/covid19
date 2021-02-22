
#SOURCE: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
jh_link <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"

#SOURCE:https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
census_pop_cnty_link <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"


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
jh_cases <- read_csv(jh_link)  %>% 
  select(countyFIPS = FIPS, county = Admin2, state = Province_State, contains("/")) %>%
  pivot_longer(cols = c(4:ncol(.)), names_to = "date", values_to = "case_total") %>%
  mutate(date = mdy(date))%>%
  #remove cases that havea not been alocated to a specific county 
  filter(countyFIPS != 0) 


#########################
#########population 

cnty_population <- read_csv(census_pop_cnty_link) %>% 
  mutate(countyFIPS = paste0(STATE, COUNTY)) %>%
  select(countyFIPS, POPESTIMATE2019) %>%
  rename( #new = old
    population = POPESTIMATE2019
  ) %>%
  mutate(countyFIPS = as.numeric(countyFIPS))



########################
######## combine to main df

covid19_county <- left_join(jh_cases, cnty_population, by = c("countyFIPS"))  %>% 
  rename( #new = old
    county_fips = countyFIPS
  ) %>%
  filter(date > max(date) - 28) %>% #subset data do it doesn't take so long
  #arrange by date 
  arrange(state, county_fips, date) %>% 
  #have all of the calcs done by each county 
  group_by(county_fips) %>%
  mutate(
    #new cases for each day; take total from today minus total from yesterday
      case_new = case_total - lag(case_total)
    #if negative; total has been re-adjusted; set to 0 
    , case_new = ifelse(case_new <0, 0, case_new)
    #growth factor = new cases today / new cases yesterday
    #if new cases yesterday = 0; set gf = cases new today (technically = inf)
    # , gf = ifelse(lag(case_new) == 0 | is.na(lag(case_new)), case_new , case_new / lag(case_new))
  )  %>% 
  # if 2019-02-19 IA -- change case_new to NA; change in reporting; numbers very different than surrounding days
  mutate(
    case_new = ifelse(state == "Iowa" & date == as.Date("2021-02-19"), NA, case_new)
  ) %>%
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
  ) %>%
  select(
    date
    , contains("county")
    , contains("state")
    , contains("case")
    , population
  ) %>% 
  #remove rows that don't have MA
  drop_na(case_MA7)

