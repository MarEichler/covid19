library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(tidyr)
library(rgeos)

#outside scripts
source("script/variable/colors.R")
source("script/variable/gf_cut_info.R")


#data 
load("data/covid19_state_ndays.rda")


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



max_date <- max(covid19_state_ndays$date)
min_date <- min(covid19_state_ndays$date)
ndays <- length(unique(covid19_state_ndays$date))


#create growth factor  average for 14 days 
gf_mean_df <- covid19_state_ndays %>%
  group_by(state) %>%
  summarize(gf_mean = mean(gf)) %>%
  mutate( growth_factor = cut(gf_mean, breaks = gf_breaks , labels = gf_labels , right = gf_right))


#join growth rate data with shape file data
shp_gf_avg7 <- spdf_fortified %>%
  left_join(. , gf_mean_df, by=c("id" = "state") )

#HEX MAP STATE LABELS 
# Calculate the centers of each hexagon to add the labels:
#requires rgeos package 
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
labels <- centers %>%
  left_join(., gf_mean_df, by = c("id" = "state")) %>%
  mutate(
    gf_label = round(gf_mean, 2)
  )


title <- paste("Average Growth Factor over last", ndays, "days")
subtitle <- paste(min_date, "to", max_date)

hex_map <- ggplot() +
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
    data = labels
    , aes(x=x, y=y+0.5, label=id)
    , color = state_abbrv_color
    , fontface = "bold"
  )+
  geom_text(
    data = labels
    , aes(x=x, y=y-0.8, label=gf_label)
    , color = state_abbrv_color
    , size = 2.5
  )+
  theme_void() +
  coord_map() +
  labs(
    title = title
    , subtitle = subtitle
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  )


ggsave(
  "img/hex_map.png"
  , plot = hex_map
  , width = 7
  , height = 5
  , units = c("in")
  , dpi = 300
)
