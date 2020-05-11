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

darkpink <- brewer.pal(11, "PiYG")[1]
lightpink <- brewer.pal(11, "PiYG")[2]
lightgreen <- brewer.pal(11, "PiYG")[10]
lightgrey <- "grey60"

#if "0" in set; start with grey; if not start with green 
if (gf_labels[1] %in% gf_mean_df$growth_factor) {
  color_palette <- c(lightgrey, lightgreen, lightpink, darkpink)
} else {
  color_palette <- c(lightgreen, lightpink, darkpink)
}

#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , gf_mean_df, by=c("county_fips" = "countyFIPS") )

ggplot(county_shp_gf) +
  geom_sf(
      aes(fill = growth_factor)
    , color = NA
  ) +
  geom_sf(
      data = state_sf
    , fill = NA
  ) +
  scale_fill_manual(values = color_palette)

#ggsave(
#  "img/county_map.png"
#  , width = 7
#  , height = 5
#  , units = c("in")
#  , dpi = 300
#)

head(county_shp_gf)

small_state_names <- c("NY", "CT", "NJ", "RI", "DE", "VT","NH", "MA")

small_county <- county_shp_gf %>%
  filter(state_abbv %in% small_state_names)

small_state <- state_sf %>%
  filter(state_abbv %in% small_state_names)


small_plot <- ggplot(small_county) +
  geom_sf(
    aes(fill = growth_factor)
    , color = NA
  ) +
  geom_sf(
    data = small_state
    , fill = NA
  ) +
  scale_fill_manual(values = color_palette) 



