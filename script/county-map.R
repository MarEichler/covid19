library(tidyverse)
library(urbnmapr)
library(RColorBrewer)

# https://github.com/UrbanInstitute/urbnmapr


counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 


#DATA AND COLOR FOR STATE 
load("data/gf_county.rda")
gf_county_tidy <- gf_county %>%
  pivot_longer(cols = c(-1),  names_to = "date", values_to = "gf") %>%
  mutate(date = as.Date(date))


max_date <- max(gf_county_tidy$date)
n_days <- 14 #14 day average
min_date <- max_date - n_days  
gf_labels <- c("0", "0-1", "1-2", "2+")

#create grwothrate average for 14 days 
gf_mean_df <- gf_county_tidy %>%
  filter(date > min_date) %>%
  group_by(countyFIPS) %>%
  summarize(gf_mean = mean(gf)) %>%
  mutate( growth_factor = 
            cut(
              gf_mean
              , breaks = c(-Inf, 0,1, 2, Inf)
              , labels = gf_labels 
              , right = TRUE
            )
  )



#set color palette

source("script/colors.R")


#if "0" in set; start with grey
if (gf_labels[1] %in% gf_mean_df$growth_factor) {
  color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)
} else {
  color_palette <- c(gf0_1, gf1_2, gf2plus)
}

#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , gf_mean_df, by=c("county_fips" = "countyFIPS") )


title <- paste("Average Growth Rate over last", n_days, "days")
subtitle <- paste(min_date, "to", max_date)

county_map <- ggplot(county_shp_gf) +
  geom_sf(
      aes(fill = growth_factor)
    , color = NA
  ) +
  geom_sf(
      data = state_sf
    , fill = NA
    , color = line_state
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
    , caption = "Data Source: usafacts.org"
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



#small_state_names <- c("NY", "CT", "NJ", "RI", "DE", "VT","NH", "MA")

#small_county <- county_shp_gf %>%
#  filter(state_abbv %in% small_state_names)

#small_state <- state_sf %>%
#  filter(state_abbv %in% small_state_names)


#small_plot <- ggplot(small_county) +
#  geom_sf(
#    aes(fill = growth_factor)
#    , color = NA
#  ) +
#  geom_sf(
#    data = small_state
#    , fill = NA
#  ) +
#  scale_fill_manual(values = color_palette) 



