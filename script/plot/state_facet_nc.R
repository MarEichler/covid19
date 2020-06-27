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


### 7-day average curve plot 

min_date <- as.Date("2020-03-15")  
facet_data <- covid19_state %>%
  filter(date >= min_date)
  
max_date <- max(facet_data$date)
title <- paste("New Cases and 7-Day Moving Average of New Cases")
subtitle <- paste(min_date, "to", max_date)
  
state_facet_0315 <- ggplot(facet_data, aes(date, nc)) + 
    geom_col(alpha = 0.3) +
    geom_line(aes(y = nc_ma7), color = blue_comp) + 
    facet_geo(~state, grid = "us_state_grid2" , labeller = adjust_labels, scales = "free") +
    scale_x_date(      name = NULL, breaks = NULL, expand = c(0, 0)) + 
    scale_y_continuous(name = NULL, breaks = NULL, expand = c(0, 0)) +
    theme_minimal() +
    labs(
      title = title
      , subtitle = subtitle
      , caption = paste("Scales are NOT FIXED\nThis plot is not to compare numbers state to state,",
      "but to look at curves and where cases are increasing or decreasing")
    ) +
    theme_minimal() +
    theme(
        strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
      , plot.title = element_text(face = "bold", hjust = 0.5)
      , plot.subtitle = element_text(hjust = 0.5, size = 12)
    ) 

ggsave("img/state_facet_0315.png", plot = state_facet_0315, width = 7, height = 5, units = c("in"), dpi = 300)

################################



####TREND PLOT   
max_date <- max(df$date)
min_date14 <- max(covid19_state$date) - 14

n_days <- max_date - min_date14

df_trend <-covid19_state %>%
  filter(date == max_date | date == min_date14) %>%
  select(state, date, nc_ma7) %>%
  pivot_wider(., names_from = date, values_from = nc_ma7) %>%
  select(state, min_date = 2, max_date = 3) %>%
  mutate(trend = ifelse(min_date <= max_date, "Increasing", "Decreasing")) 
  
trend_data <- covid19_state %>%
  filter(date >= min_date14) %>%
  left_join(., df_trend, by = "state")
  
title <- paste("Last", n_days, "days: New Cases and 7-Day Moving Average of New Cases")
subtitle <- paste(min_date, "to", max_date)

state_facet_trend14  <- ggplot(trend_data, aes(date, nc)) + 
  geom_col(alpha = 0.3) +
  geom_line(aes(y = nc_ma7, color = trend)) + 
  facet_geo(~state, grid = "us_state_grid2" , labeller = adjust_labels, scales = "free") +
  scale_x_date(      name = NULL, breaks = NULL, expand = c(0, 0)) + 
  scale_y_continuous(name = NULL, breaks = NULL, expand = c(0, 0)) +
  scale_color_manual(
      name = "Difference in 7-day moving average between endpoints"
    , values = c("forestgreen", "red")
  )+
  theme_minimal() +
  labs(
    title = title
    , subtitle = subtitle
    , caption = paste("Scales are NOT FIXED\nThis plot is not to compare numbers state to state,",
                      "but to look at curves and where cases are increasing or decreasing")
  ) +
  theme_minimal() +
  theme(
     strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
    , plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
    , legend.position = "top"
  ) 

ggsave("img/state_facet_trend14.png", plot = state_facet_trend14, width = 7, height = 5, units = c("in"), dpi = 300)
#####################33