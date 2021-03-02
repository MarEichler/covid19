
#SOURCE: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
jh_link_cases  <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
jh_link_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"

#SOURCE: https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_1873399417
census_pop_link <- "http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"


state_abb_name <- tibble(
    abb = state.abb
  , name = state.name
) %>% 
  rows_insert(tibble(abb = "DC", name = "District of Columbia")) %>%
  rows_insert(tibble(abb = "AS", name = "American Samoa")) %>%
  rows_insert(tibble(abb = "GU", name = "Guam")) %>%
  rows_insert(tibble(abb = "MP", name = "Northern Mariana Islands")) %>%
  rows_insert(tibble(abb = "PR", name = "Puerto Rico")) %>%
  rows_insert(tibble(abb = "VI", name = "Virgin Islands")) %>%
  rows_insert(tibble(abb = "USA", name = "United States"))

state_abb_name

##################################
jh_cases <- read_csv(jh_link_cases) %>% 
  select(state = Province_State, contains("/")) %>%
  group_by(state) %>%
  summarize_at(vars(contains("/")), funs(sum(., na.rm = TRUE))) %>%
  janitor::adorn_totals("row") %>%
  mutate(state = ifelse(state == "Total", "United States", state) ) %>%
  tibble() %>%
  pivot_longer(cols = c(contains("/")), names_to = "date", values_to = "case_total") %>%
  mutate(date = mdy(date))

jh_deaths <- read_csv(jh_link_deaths) %>% 
  select(state = Province_State, contains("/")) %>%
  group_by(state) %>%
  summarize_at(vars(contains("/")), funs(sum(., na.rm = TRUE))) %>%
  janitor::adorn_totals("row") %>%
  mutate(state = ifelse(state == "Total", "United States", state) ) %>%
  tibble() %>%
  pivot_longer(cols = c(contains("/")), names_to = "date", values_to = "death_total") %>%
  mutate(date = mdy(date))

jh_data <- full_join(jh_cases, jh_deaths, by = c("state", "date")) %>%
  left_join(., state_abb_name, by = c("state" = "name")) %>%
  rename(#new = old
    geo = abb
    )



#########################
#########population 

usa_population <- read_csv(census_pop_link) %>% 
  filter(SUMLEV %in% c("010", "040")) %>% #filter if 010 = National or 040 = State (remove region rows)
  select(NAME, POPESTIMATE2019) %>%
  left_join(., state_abb_name, by = c("NAME" = "name")) %>%
  rename( #new = old
      population = POPESTIMATE2019
  ) %>%
  select(-NAME)



########################
######## combine to main df


covid19 <- left_join(jh_data, usa_population, by = c("geo" = "abb")) %>%
  #arrange by date
  arrange(geo, date) %>%
  #have all of the calcs done by each county 
  group_by(geo) %>%
  mutate(
    #new cases for each day; take total from today minus total from yesterday
      case_new = case_total - lag(case_total)
    #if negative; total has been re-adjusted; set to 0 
    , case_new = ifelse(case_new <0, 0, case_new)
    #growth factor = new cases today / new cases yesterday
    #if new cases yesterday = 0; set gf = cases new today (technically = inf)
    , gf = ifelse(lag(case_new) == 0 | is.na(lag(case_new)), case_new , case_new / lag(case_new))
    #new cases for each day; take total from today minus total from yesterday
    , death_new = death_total - lag(death_total)
    #if negative; total has been re-adjusted; set to 0 
    , death_new = ifelse(death_new <0, 0, death_new)
    #death proportion 
    , death_prop    = ifelse(case_total == 0 | is.na(case_total), NA, death_total/case_total)
  )  %>% 
  # if 2019-02-19 IA -- change case_new to NA; change in reporting; numbers very different than surrounding days
  mutate(
    case_new = ifelse(geo == "IA" & date == as.Date("2021-02-19"), NA, case_new)
  ) %>%
 #moving averages 
  mutate(
      case_MA7  = c(rep(NA, 6), zoo::rollmean(case_new,  k = 7, na.rm = TRUE))
    , death_MA7 = c(rep(NA, 6), zoo::rollmean(death_new, k = 7, na.rm = TRUE))
    , gf_MA7    = c(rep(NA, 6), zoo::rollmean(gf,        k = 7, na.rm = TRUE))
  ) %>%
  #ungroup
  ungroup() %>%
  # per capita (prop of population)
  mutate_at(vars(contains("death_") | contains("case_")), funs("PC" = ./population)) %>%
  #create amounts per 100,000 people
  mutate_at(vars(contains("_PC")), funs("100k"= .*100000))%>% 
  select(date, geo, contains("death"), contains("case"), contains("gf"), population) 


########################
######## NATIONAL US

covid19_US <- covid19 %>% filter(geo == "USA")
