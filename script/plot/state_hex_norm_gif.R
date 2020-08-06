library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(tidyr)
library(rgeos)
library(tidyr)
library(gganimate)

#outside scripts
source("script/variable/colors.R")

#data 
load("data/covid19_state.rda")


#HEX MAP SET UP

#INFO HERE: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html 

# Download the Hexagones boundaries at geojson format here:
#     https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load downloaded file 
spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

spdf@data <-  spdf@data %>% 
  mutate(
    google_name = gsub(" \\(United States\\)", "", google_name)
    , state_pc = iso3166_2
  )
spdf_fortified <- tidy(spdf, region = "state_pc")


df <- covid19_state #%>% filter(date >= max(date)-14)

max_date <- max(covid19_state$date)

hex_data<- spdf_fortified %>%
  left_join(. , df, by=c("id" = "state") )

#HEX MAP STATE LABELS 
# Calculate the centers of each hexagon to add the labels:
#requires rgeos package 
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))


labels <- centers %>%
  left_join(., hex_data, by = c("id" = "id")) %>%
  mutate( value = round(nc_ma7_norm, 2))


title <- paste("Percent of Curve")
subtitle <- paste("Where are states in their curve?\n 7-Day Moving Average on", 
                  max_date, "divided by max 7-Day Moving Average")



hex_gif <- ggplot(data = hex_data, aes(x = long, y = lat, group = group, fill = nc_ma7_norm)) + 
  geom_polygon() +
  geom_polygon( color = "white" , size = 1, show.legend = FALSE) +
  scale_fill_gradientn(
    name = NULL
    , colors = nc_perc_palette
    , values = c(0, .25, .45, .5, .7, 1)
    , limits = c(0, 1)
    , breaks = c(0, 0.25, .45, .55, .75, 1)
    , labels = scales:: percent
  ) +
  guides(fill = guide_legend(
    nrow = 1, 
    direction = "horizontal",
    label.position = "bottom",
    keywidth = 3, 
    keyheight = .5, 
    label.hjust = 0, 
  )
  )+
  geom_text(
    data = labels
    , aes(x=x, y=y, label=id)
    , color = "white"
    , fontface = "bold"
  )+
  theme_void() +
  coord_map()  +   transition_time(date)

#animate(hex_gif, fps = 1, nframes = length(unique(hex_data$date)))

state_hex_norm_gif <- animate(
  hex_gif
  , fps = 0.5
  , nframes = length(unique(hex_data$date))
  , width = 7
  , height = 5
  , units = c("in")
  , res = 300
)

anim_save("img/state_hex_norm_gif.gif", state_hex_norm_gif)
##########################################
