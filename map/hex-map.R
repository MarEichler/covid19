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
  

gf_mean7 <- gf_state_tidy %>%
  filter(date > max(gf_state_tidy$date)-7) %>%
  group_by(state) %>%
  summarize(gf_mean7 = mean(gf)) %>%
  mutate( growth_factor = 
            cut(
                ifelse(gf_mean7 < 0, gf_mean, NA)
              , breaks = c(0,1, 2, Inf)
              , labels = c("0-1", "1-2", "2+")
              , right = TRUE
            )
  )

colors <- rev(brewer.pal(5, "PiYG")[c(1,2,4)])

shp_gf_avg7 <- spdf_fortified %>%
  left_join(. , gf_mean7, by=c("id" = "state") )


ggplot() +
  geom_polygon(
    data = shp_gf_avg7
    , aes(fill =  growth_factor, x = long, y = lat, group = group)
    , color = "white"
  ) +
  geom_text(
    data = centers
    , aes(x=x, y=y, label=id)
    , color = "grey20"
  )+
  scale_fill_manual(
      name = "Growth Factor"
    , values = colors
    , na.value = "grey80"
    , guide = guide_legend(reverse = TRUE)
  ) +
  theme_void() +
  coord_map() 


