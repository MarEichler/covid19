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

max_date <- max(covid19_state$date)
min_date <- max_date - 7


df <- covid19_state %>%
  filter(date == max_date) %>%
  mutate(
      perc_ch = cut(nc_ma7_perc, breaks =number_breaks, labels = chacter_breaks)
    , perc_numb = nc_ma7_perc
    , perc_adj = ifelse(nc_ma7_perc > 1, 1, ifelse(nc_ma7_perc < -1, -1, nc_ma7_perc))
  ) 



hex_data<- spdf_fortified %>%
  left_join(. , df, by=c("id" = "state") )



#HEX MAP STATE LABELS 
# Calculate the centers of each hexagon to add the labels:
#requires rgeos package 
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))


labels <- centers %>%
  left_join(., hex_data, by = c("id" = "id")) %>%
  mutate( value = round(perc_numb, 2))


title <- paste("Percentage Change in Average Cases")
subtitle <- paste("Last Week (", min_date, ") Compared to This Week (", max_date, ")", sep = "") 


state_hex <- ggplot(data = hex_data, aes(x = long, y = lat, group = group, fill = perc_ch)) + 
  geom_polygon() +
  geom_polygon( color = "white" , size = 1, show.legend = FALSE) +
  scale_fill_manual( name = NULL, values = nc_perc_palette , limits = chacter_breaks) +
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
    , aes(x=x, y=y+0.5, label=id)
    , color = "white"
    , fontface = "bold"
  )+
  geom_text(
    data = labels
    , aes(x=x, y=y-0.8, label=scales::percent(value, accuracy = 2))
    , color = "white"
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
    , legend.position = "top"
    , legend.spacing.x = unit(0, 'cm')
    , legend.margin=margin(t = .5, unit='cm')
  )


#scale_fill_distiller(
#  name = NULL
#  , palette = "RdYlGn"
#  , limits = c(-1, 1)
#  , breaks = c(-1, -.5, -.05, .05, .5, 1)
#  , labels = scales:: percent
#) +
state_hex

ggsave(
  "img/state_hex.png"
  , plot = state_hex
  , width = 7
  , height = 5
  , units = c("in")
  , dpi = 300
)
