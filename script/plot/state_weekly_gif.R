# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(tidyr)
library(gganimate)
library(rgeos)

#outside scripts 
source("script/variable/colors.R")

#data
load("data/covid19_state_weekly.rda")

# Download the Hexagones boundaries at geojson format here: 
#   https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

spdf@data <-  spdf@data %>% 
  mutate(
      google_name = gsub(" \\(United States\\)", "", google_name)
    , state_pc = iso3166_2
  )
spdf_fortified <- tidy(spdf, region = "state_pc")

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))


gf_labels <- covid19_state_weekly %>% 
  select(-week, -growth_factor) %>%
  mutate(gf_ma = round(gf_ma, 2))

labels <- centers %>%
  left_join(., gf_labels, by = c("id" = "state")) 


shp_gf <- spdf_fortified %>%
  left_join(. , covid19_state_weekly, by=c("id" = "state") )


weekly_plot <- ggplot() +
  geom_polygon(
    data = shp_gf
    , aes(fill =  growth_factor, x = long, y = lat, group = group)
    , color = "white"
  ) +
  geom_text(
    data = centers
    , aes(x=x, y=y+0.5, label=id)
    , color = state_abbrv_color
    , fontface = "bold"
  )+
  geom_text(
    data = labels
    , aes(x=x, y=y-0.8, label=gf_ma)
    , color = state_abbrv_color
    , size = 2.5
  )+
  scale_fill_manual(
      name = "Growth Factor"
    , values = color_palette
    , guide = guide_legend(reverse = TRUE)
  ) +
  theme_void() +
  coord_map() +
  labs(
      title = "Average Growth Factor by Week of Pandemic"
    , subtitle = "Week {frame}\n Week starting on {frame_time}"
  ) +
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  transition_time(date) 

state_weekly_gif <- animate(
    weekly_plot
  , fps = 0.5
  , nframes = length(unique(shp_gf$week))
  , width = 7
  , height = 5
  , units = c("in")
  , res = 300
  )

anim_save("img/state_weekly_gif.gif", state_weekly_gif)
##########################################