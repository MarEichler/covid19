library(tidyverse)
library(forcats)
library(ggmap)
library(statebins)
library(sf)
library(lubridate)
library(geofacet) #https://hafen.github.io/geofacet/
library(moderndive)
library(cowplot) 
library(png)
library(tidyquant)

#outside scripts 
source("script/variable/colors.R")

#data
load("data/covid19_state.rda")


#use code from the fundamentals of data vizualization book: 
# https://github.com/clauswilke/dataviz/blob/master/geospatial_data.Rmd


adjust_labels <- as_labeller(
  function(x) {
    case_when(
      x == "New Hampshire" ~ "N. Hampshire",
      x == "District of Columbia" ~ "DC",
      TRUE ~ x
    )
  }
)

ma_k <- 7

min_date <- as.Date("2020-03-15")  #min(facet_data$date)

facet_data <- covid19_state %>%
  filter(date >= min(min_date - ma_k, as.Date("2020-01-22")))

max_date <- max(facet_data$date)


title <- paste("New Cases and", ma_k, "Day Moving Average of New Cases")
subtitle <- paste(min_date, "to", max_date)

facet_map <- ggplot(facet_data, aes(date, nc)) +
  geom_col(alpha = 0.3) +
  geom_ma(ma_fun = SMA, n = 7, color = blue_comp, linetype = "solid", size = 0.8) + #SMA = simple moving average
  scale_y_continuous(
      name = NULL
    , expand = c(0, 0)
  ) +
  scale_x_date(
      name = NULL
    , expand = c(0, 0)
    , breaks = NULL
  ) +
  facet_geo(~state, grid = "us_state_grid2" , labeller = adjust_labels, scales = "free") +
  labs(
    title = title
    , subtitle = subtitle
    , caption = "Scales are NOT FIXED\nThis plot is not to compare numbers state to state, but to look at curves and where cases are increasing or decreasing"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(min_date, NA)) +
  theme(
      strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
    , panel.spacing.x = grid::unit(5, "pt")
    , panel.spacing.y = grid::unit(5, "pt")
    , panel.grid = element_blank()
    , axis.text = element_blank() #element_text(size = 7)
    , axis.title.x = element_text(margin = margin(20, 0, 0, 0))
    #, axis.ticks.length.y = unit(5, "pt")
    , axis.ticks = element_blank() #element_line(color = "grey75")
    , plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) 


ggsave("img/state_facet_nc.png", plot = facet_map, width = 7, height = 5, units = c("in"), dpi = 300)
