library(tidyverse)
library(gganimate)
library(urbnmapr) # https://github.com/UrbanInstitute/urbnmapr


#outside scripts
source("script/variable/colors.R")

#data 
load("data/covid19_county_weekly.rda")

color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)


counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 

#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , covid19_county_weekly, by=c("county_fips" = "countyFIPS") )




gif_data <- county_shp_gf 

#county_week_gif <- 
ggplot(gif_data) + 
  geom_sf(aes(fill = growth_factor), color = NA) +
  geom_sf(
    data = state_sf
    , fill = NA
    , color = "white" #line_state
  ) +
  scale_fill_manual(
    name = "Growth Factor"
    , values = color_palette
    , guide = guide_legend(reverse = TRUE)
  ) +
  theme_void() +
  transition_time(week)

county_week_gif

county_gif <- animate(
  county_week_gif
#  , fps = 1
#  , nframes = length(unique(gif_data$week))
  , width = 7
  , height = 5
  , units = c("in")
  , res = 100
)

anim_save("img/county_week_gif.gif", county_gif)


anim <- ggplot(airquality, aes(Day, Temp)) +
  geom_point(aes(colour = factor(Month))) +
  transition_time(Day)

anim
