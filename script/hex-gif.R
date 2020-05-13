# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(tidyr)
library(gganimate)

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
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))



load("data/gf_state.rda")
gf_state_tidy <- gf_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))

min_date <- min(gf_state_tidy$date)
max_date <- max(gf_state_tidy$date)
  
gf_labels <- c("0", "0-1", "1-2", "2+")

gf_bins_tidy <- gf_state_tidy %>%
  mutate( growth_factor = 
            cut(
                gf
              , breaks = c(-Inf, 0,1, 2, Inf)
              , labels = gf_labels
              , right = TRUE
            )
  )

source("script/colors.R")

color_palette <- c(gf0, gf1_0, gf1_2, gf2plus)


shp_gf <- spdf_fortified %>%
  left_join(. , gf_bins_tidy, by=c("id" = "state") )




goo <- ggplot() +
  geom_polygon(
    data = shp_gf
    , aes(fill =  growth_factor, x = long, y = lat, group = group)
    , color = "white"
  ) +
  geom_text(
      data = centers
    , aes(x=x, y=y, label=id)
    , color = state_abbrv_color
  ) +
  scale_fill_manual(
      name = "Growth Factor"
    , values = color_palette
  ) +
  theme_void() +
  coord_map() +
  transition_time(date)+
  labs(
      title = "Growth Factor on {frame_time}"
    , caption = "Data Source: usafacts.org"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

#takes 3-5 minutes to run, depending on my machine's mood
hex_map <- animate(
    goo
  , nframes=2*length(unique(shp_gf$date))
  , fps = 4
  , width = 7
  , height = 5
  , units = c("in")
  , res = 100
  )

anim_save("img/hex_gif.gif", hex_map)


