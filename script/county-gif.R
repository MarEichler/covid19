library(tidyverse)
library(gganimate)


#DATA AND COLOR FOR STATE 
load("data/gf_county.rda")
gf_county_tidy <- gf_county %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))


max_date <- max(gf_county_tidy$date)
n_days <- 14 #14 day average
min_date <- max_date - n_days  
gf_labels <- c("0", "0-1", "1-2", "2+")

#create grwothrate average for 14 days 
gf_bins_tidy <- gf_county_tidy %>%
  mutate( growth_factor = 
            cut(
                gf
              , breaks = c(-Inf, 0,1, 2, Inf)
              , labels = gf_labels 
              , right = TRUE
            )
  )

#set color palette

source("script/colors.R")

color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)


library(urbnmapr) # https://github.com/UrbanInstitute/urbnmapr

counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 

#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , gf_bins_tidy, by=c("county_fips" = "countyFIPS") )

ggplot(county_shp_gf) + 
  geom_sf(aes(fill = growth_factor))




library(maps)

data(county.fips)

county.fips <- county.fips %>%
  separate(polyname, into = c("state", "county"), sep = ",")

#the best state in the union 
county_map <- map_data("county") %>% 
  select(long, lat, group, state = region, county = subregion) %>%
  left_join(county.fips, by = c("state" = "state", "county" = "county")) 

county_map_gf <- county_map %>%
  left_join(., gf_bins_tidy, by = c("fips" = "countyFIPS"))


#county_map_gf %>%
#ggplot(aes(x = long, y = lat)) +
#  geom_polygon(aes(group = group, fill = growth_factor) , color = "grey35") +
#  coord_quickmap() +
#  theme_void() 
