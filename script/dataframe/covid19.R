library(tidyverse)


#SOURCE: https://covidtracking.com/data/download
ctp_link_usa    <- "https://covidtracking.com/data/download/national-history.csv"
ctp_link_states <- "https://covidtracking.com/data/download/all-states-history.csv"

#SOURCE: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_1873399417
census_pop_link <- "http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"


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
#####pull out USA and state data 

us <- read_csv(ctp_link_usa) %>% 
  select(
      date
    , death
    , deathIncrease
    , positive
    , positiveIncrease
    , totalTestResults
    , totalTestResultsIncrease
  ) %>%
  group_by(date) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(geo = "USA")

state <- read_csv(ctp_link_states) %>% 
  select(
      date
    , state
    , death
    , deathIncrease
    , positive
    , positiveIncrease
    , totalTestResults
    , totalTestResultsIncrease
  ) %>%
  rename(geo = state) #new = old 

us_and_state <- rbind(state, us)

#########################
#########population 

usa_population <- read_csv(census_pop_link) %>% 
  filter(SUMLEV %in% c("010", "040")) %>% #filter if 010 = National or 040 = State (remove region rows)
  select(NAME, POPESTIMATE2019) %>%
  left_join(., state_abb_name, by = c("NAME" = "name")) %>%
  rename( #new = old
      geo = abb
    , population = POPESTIMATE2019
  ) %>%
  select(-NAME)


########################
######## combine to main df

covid19 <- left_join(us_and_state, usa_population, by = "geo") %>%
  rename( #new = old
      death_total = death
    , death_new   = deathIncrease
    , case_total  = positive
    , case_new    = positiveIncrease
    , test_total  = totalTestResults
    , test_new    = totalTestResultsIncrease
  ) %>%
  #arrange by date 
  arrange(date, geo) %>%
  # new columns 
  mutate(
    death_prop    = ifelse(case_total == 0 | is.na(case_total), NA, death_total/case_total)
  ) %>% 
  group_by(geo) %>%
  mutate(
    #growth factor = new cases today / new cases yesterday
    #if new cases yesterday = 0; set gf = cases new today (technically = inf)
    gf = ifelse(lag(case_new) == 0, case_new , case_new / lag(case_new))
  ) %>%
  #moving averages 
  mutate(
      case_MA7  = c(rep(NA, 6), zoo::rollmean(case_new,  k = 7, na.rm = TRUE))
    , death_MA7 = c(rep(NA, 6), zoo::rollmean(death_new, k = 7, na.rm = TRUE))
    , test_MA7  = c(rep(NA, 6), zoo::rollmean(test_new,  k = 7, na.rm = TRUE))
    , gf_MA7        = c(rep(NA, 6), zoo::rollmean(gf,        k = 7, na.rm = TRUE))
  ) %>%
  #ungroup
  ungroup() %>%
  # per capita (prop of population)
  mutate(
      death_total_PC   = death_total   / population
    , death_new_PC     = death_new     / population 
    , death_MA7_PC     = death_MA7     / population 
    , case_total_PC    = case_total    / population 
    , case_new_PC      = case_new      / population 
    , case_MA7_PC      = case_MA7      / population 
    , test_total_PC    = test_total    / population 
    , test_new_PC      = test_new      / population 
    , test_MA7_PC      = test_MA7      / population 
  ) %>%
  select(date, geo, contains("death"), contains("case"), contains("gf"), contains("test"), population) 

