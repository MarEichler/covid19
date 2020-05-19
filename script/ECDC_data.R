#download data directly from European CDC
#https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
raw_data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
                     , na.strings = "", fileEncoding = "UTF-8-BOM")


covid19_US <- raw_data %>% 
  #only look at USA 
  filter(countryterritoryCode  == "USA") %>%
  #read date character into date format
  mutate(date = as.Date(dateRep, format="%d/%m/%Y")) %>%
  #order by date
  arrange(date) %>%
  #calculate cumulative sums
  mutate(
    total_cases = cumsum(cases)
    , total_deaths = cumsum(deaths)
  ) %>%
  #only select date, new cases, new deaths
  select(
    date
    , new_cases = cases
    , new_deaths =deaths
    , total_cases
    , total_deaths
  )
