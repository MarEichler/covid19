library(tidyverse)

library(forcats)
library(ggmap)
library(statebins)
library(sf)
library(lubridate)
library(geofacet)
library(moderndive)

# https://github.com/clauswilke/dataviz/blob/master/geospatial_data.Rmd

load("data/gf_state.rda")
gf_state_tidy <- gf_state %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))



adjust_labels <- as_labeller(
  function(x) {
    case_when(
      x == "New Hampshire" ~ "N. Hampshire",
      x == "District of Columbia" ~ "DC",
      TRUE ~ x
    )
  }
)

n_days <- 14
x_max <- max(gf_state_tidy$date)
x_min <- x_max - n_days

facet_data <- gf_state_tidy %>% 
  filter(date > x_min)

plus3 <- facet_data %>%
  filter(gf > 3) 


x_min <- min(facet_data$date)
x_max <- max(facet_data$date)
source("script/colors.R")

title <- paste("Daily Growth Rate over last", n_days, "days")
subtitle <- paste(x_min, "to", x_max)

facet_map <- ggplot(facet_data, aes(date, gf)) +
  annotate("rect", xmin = x_min, xmax = x_max, ymin =  0, ymax =  1,   fill = "white") +
  annotate("rect", xmin = x_min, xmax = x_max, ymin =  1, ymax =  Inf,   fill = gf2plus,   alpha = 0.45) +
  geom_line( color = "grey35") +
  geom_area(color = "grey35" , alpha = 0.6) +
  geom_point( data = plus3, aes(date, gf), color = "grey35", size = 1.5) + 
  scale_y_continuous(
      name = "Growth Factor"
    , limits = c(0, 3)
    , expand = c(0, 0)
    , breaks = c(0, 1, 2, 3)
    , labels = c("0", "1", "2", "3")
    , oob = scales::squish
  ) +
  scale_x_date(
      name = NULL
    , expand = c(0, 0)
    , breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  facet_geo(~state, grid = "us_state_grid1", labeller = adjust_labels) +
  labs(
    title = title
    , subtitle = subtitle
    , caption = "Data Source: usafacts.org"
  ) +
  theme_minimal() + 
  theme(
      strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
    , axis.line.x = element_blank()
    , panel.spacing.x = grid::unit(5, "pt")
    , panel.spacing.y = grid::unit(5, "pt")
    , panel.grid = element_blank()
    , axis.text = element_text(size = 7)
    , axis.ticks.length.y = unit(5, "pt")
    , axis.ticks = element_line(color = "grey75")
    , plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) 

facet_map

ggsave(
  "img/facet_map.png"
  , plot = facet_map
  , width = 7
  , height = 5
  , units = c("in")
  , dpi = 300
)
