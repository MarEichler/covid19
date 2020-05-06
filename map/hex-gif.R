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



load("gf_state.rda")
gf_state_tidy <- gf_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))
  

gf_bins_tidy <- gf_state_tidy %>%
  mutate( growth_factor = 
            cut(
                gf
              , breaks = c(-Inf, 0,1, 2, Inf)
              , labels = c("0", "0-1", "1-2", "2+")
              , right = TRUE
            )
  )


colors <- c("grey50", rev(brewer.pal(5, "PiYG")[c(1,2,4)]))


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
    , values = colors
  ) +
  theme_void() +
  coord_map() +
  transition_states(date, transition_length = 1, state_length = 4)+
  ggtitle('{closest_state}')


hex_map <- animate(goo, nframes=2*length(unique(new$date)))

anim_save("hex_map.gif", hex_map)

