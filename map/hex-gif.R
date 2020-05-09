# library
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(tidyr)
library(gganimate)

# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("map/us_states_hexgrid.geojson",  what = "sp")

spdf@data <-  spdf@data %>% 
  mutate(
      google_name = gsub(" \\(United States\\)", "", google_name)
    , state_pc = iso3166_2
  )
spdf_fortified <- tidy(spdf, region = "state_pc")

# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))



load("gf_state.rda")
gf_state_tidy <- gf_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))
  
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

darkpink <- brewer.pal(11, "PiYG")[1]
lightpink <- brewer.pal(11, "PiYG")[2]
lightgreen <- brewer.pal(11, "PiYG")[10]
lightgrey <- "grey80"

color_palette <- c(lightgrey, lightgreen, lightpink, darkpink)



new <- spdf_fortified %>%
  left_join(. , gf_bins_tidy, by=c("id" = "state") )

goo <- ggplot() +
  geom_polygon(
    data = new
    , aes(fill =  growth_factor, x = long, y = lat, group = group)
    , color = "white"
  ) +
  geom_text(
      data = centers
    , aes(x=x, y=y, label=id)
    , color = "grey90"
  ) +
  scale_fill_manual(
      name = "Growth Factor"
    , values = color_palette
  ) +
  theme_void() +
  coord_map() +
  transition_states(date, transition_length = 0.1, state_length = 1)+
  ggtitle('{closest_state}')


hex_map <- animate(goo, nframes=2*length(unique(new$date)), fps=.5)

anim_save("hex_gif.gif", hex_map)

