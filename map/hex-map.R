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

max_date <- max(gf_state_tidy$date)
n_days <- 14 #14 day average
min_date <- max_date - n_days  
gf_labels <- c("0", "0-1", "1-2", "2+")

gf_mean7df <- gf_state_tidy %>%
  filter(date > min_date) %>%
  group_by(state) %>%
  summarize(gf_mean7 = mean(gf)) %>%
  mutate( growth_factor = 
            cut(
                gf_mean7 
              , breaks = c(-Inf, 0,1, 2, Inf)
              , labels = gf_labels 
              , right = TRUE
            )
  )



shp_gf_avg7 <- spdf_fortified %>%
  left_join(. , gf_mean7df, by=c("id" = "state") )

darkpink <- brewer.pal(11, "PiYG")[1]
lightpink <- brewer.pal(11, "PiYG")[2]
lightgreen <- brewer.pal(11, "PiYG")[10]
lightgrey <- "grey60"

if (gf_labels[1] %in% gf_mean7df$growth_factor) {
  color_palette <- c(lightgrey, lightgreen, lightpink, darkpink)
} else {
  color_palette <- c(lightgreen, lightpink, darkpink)
}
  
title <- paste(n_days, "Day Average of Growth Rate from", min_date, "to", max_date)


ggplot() +
  geom_polygon(
    data = shp_gf_avg7
    , aes(fill =  growth_factor, x = long, y = lat, group = group)
    , color = "white"
  ) +
  scale_fill_manual(
    name = "Growth Factor"
    , values = color_palette
    , guide = guide_legend(reverse = TRUE)
  ) +
  geom_text(
    data = centers
    , aes(x=x, y=y, label=id)
    , color = "grey80"
  )+
  theme_void() +
  coord_map() +
  labs(
      title = title
    , caption = "Data Source: usafacts.org"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

