library(tidyverse)
library(urbnmapr)
library(RColorBrewer)

#outside scripts
source("script/variable/colors.R")
source("script/variable/gf_cut_info.R")

#data
load("data/covid19_county_ndays.rda")

# MAPPING PACKAGE INFORMATION 
# https://github.com/UrbanInstitute/urbnmapr

counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 


max_date <- max(covid19_county_ndays$date)
min_date <- min(covid19_county_ndays$date)
ndays <- length(unique(covid19_county_ndays$date))


#create growth factor  average for 14 days 
gf_mean_df <- covid19_county_ndays %>%
  group_by(countyFIPS) %>%
  summarize(gf_mean = mean(gf)) %>%
  mutate( growth_factor = cut(gf_mean, breaks = gf_breaks , labels = gf_labels , right = gf_right))


#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , gf_mean_df, by=c("county_fips" = "countyFIPS") )


title <- paste("Average Growth Factor over last", ndays, "days")
subtitle <- paste(min_date, "to", max_date)

county_map <- ggplot(county_shp_gf) +
  geom_sf(
      aes(fill = growth_factor)
    , color = NA
  ) +
  geom_sf(
      data = state_sf
    , fill = NA
    , color = "white" #line_state
  ) +
  scale_fill_manual(
      name = "Growth Factor"
    , values = color_palette
    , guide = guide_legend(reverse = TRUE)
    ) +
  theme_void() +
  labs(
    title = title
    , subtitle = subtitle
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  )


ggsave(
  "img/county_map.png"
  , plot = county_map
  , width = 7
  , height = 5
  , units = c("in")
  , dpi = 300
)
