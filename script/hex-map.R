library(tidyverse)
library(geojsonio)
library(rgdal)
library(broom)
library(tidyr)

#HEX MAP SET UP

#INFO HERE: https://www.r-graph-gallery.com/328-hexbin-map-of-the-usa.html 

# Download the Hexagones boundaries at geojson format here:
#     https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.

# Load this file. (Note: I stored in a folder called DATA)
spdf <- geojson_read("data/us_states_hexgrid.geojson",  what = "sp")

spdf@data <-  spdf@data %>% 
  mutate(
    google_name = gsub(" \\(United States\\)", "", google_name)
    , state_pc = iso3166_2
  )
spdf_fortified <- tidy(spdf, region = "state_pc")



#DATA AND COLOR FOR STATE 
load("data/gf_state_ndays.rda")

max_date <- max(gf_state_ndays$date)
min_date <- min(gf_state_ndays$date)
ndays <- length(unique(gf_state_ndays$date))

source("script/gf_cut_info.R")

#create grwothrate average for 14 days 
gf_mean_df <- gf_state_ndays %>%
  group_by(state) %>%
  summarize(gf_mean = mean(gf)) %>%
  mutate( growth_factor = cut(gf_mean, breaks = gf_breaks , labels = gf_labels , right = gf_right))


# gf_state_ndays %>% filter(state == "MN")
# gf_state_ndays %>% filter(state == "MN") %>% group_by(state) %>% summarize(mean(gf))

#join growth rate data with shape file data
shp_gf_avg7 <- spdf_fortified %>%
  left_join(. , gf_mean_df, by=c("id" = "state") )

#HEX MAP STATE LABELS 
# Calculate the centroid of each hexagon to add the label:
library(rgeos)
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))
labels <- centers %>%
  left_join(., gf_mean_df, by = c("id" = "state")) %>%
  mutate(
    gf_label = round(gf_mean, 2)
  )

#set color palette

source("script/colors.R")

#if "0" in set; start with grey; if not start with green 
if (gf_labels[1] %in% gf_mean_df$growth_factor) {
  color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)
} else {
  color_palette <- c(gf0_1, gf1_2, gf2plus)
}


title <- paste("Average Growth Rate over last", ndays, "days")
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
