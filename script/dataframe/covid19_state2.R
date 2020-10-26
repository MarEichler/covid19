library(tidyverse)

source("script/dataframe/population.R")

ctp_link <- "https://covidtracking.com/data/download/all-states-history.csv"

raw <- read_csv(ctp_link)

us <- raw %>% 
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
  mutate(state = "USA")

state <- raw %>% 
  select(
      date
    , state
    , death
    , deathIncrease
    , positive
    , positiveIncrease
    , totalTestResults
    , totalTestResultsIncrease
  ) 

us_and_state <- rbind(state, us)

population_list <- state_population %>%
  add_row(State = "USA", population = sum(.$population)) 

pc_maxdate <- max(raw$date)
pc_mindate <- pc_maxdate - 7

covid19_state2 <- us_and_state %>%
  filter(date > pc_mindate) %>%
  group_by(state) %>%
  summarise(
    deaths_total = max(death)
    , deaths_7avg  = mean(deathIncrease)
    , cases_total  = max(positive)
    , cases_7avg   = mean(positiveIncrease)
    , tests_total  = max(totalTestResults)
    , tests_7avg   = mean(totalTestResultsIncrease)
  ) %>%
  left_join(., population_list, by = c("state" = "State")) %>%
  mutate_at(vars(contains(c("_total", "_7avg")))
            , funs(
              "prop" =  . / population
              , "100k" = (. / population)*100000
            )
  )


save(covid19_state2, file =    "data/covid19_state2.rda") # send to data folder 
save(covid19_state2, file = "hex-map/covid19_state2.rda") #send data to app




