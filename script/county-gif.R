library(tidyverse)
library(urbnmapr)
library(gganimate)

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
gf_bins_tidy <- gf_county_tidy %>%
  mutate( growth_factor = 
            cut(
                gf
              , breaks = c(-Inf, 0,1, 2, Inf)
              , labels = gf_labels 
              , right = TRUE
            )
  )





#set color palette

source("script/colors.R")

color_palette <- c(gf0, gf1_0, gf1_2, gf2plus)


#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , gf_bins_tidy, by=c("county_fips" = "countyFIPS") )


title <- paste(n_days, "Day Average of Growth Rate from", min_date, "to", max_date)


county_shp_gf %>%
  filter(date >= max_date) %>%
ggplot() +
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
#  transition_time(date)+
  labs(
    title = "Growth Factor on {frame_time}"
    , caption = "Data Source: usafacts.org"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


#takes 3-5 minutes to run, depending on my machine's mood
county_gif <- animate(
  goo
  , nframes=2*length(unique(county_shp_gf$date))
  , fps = 1
  , width = 7
  , height = 5
  , units = c("in")
  , res = 10
)

anim_save("img/county_gif.gif", county_gif)
