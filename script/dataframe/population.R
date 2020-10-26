library(tidyverse)

pop_link <- "https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_county_population_usafacts.csv"

county_population <- read_csv(pop_link)

state_population <- county_population %>%
  group_by(State) %>%
  summarise(population = sum(population))



#create one data set of tidy data
save(county_population, file = "data/county_population.rda") # send to data folder 
save(county_population, file = "hex-map/county_population.rda") # send to hex app

save(state_population, file = "data/state_population.rda") # send to data folder 
save(state_population, file = "hex-map/state_population.rda") # send to hex app