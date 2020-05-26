library(tidyverse)
library(gganimate)


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
              , right = FALSE
            )
  )

#set color palette

source("script/variable/colors.R")

color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)


library(urbnmapr) # https://github.com/UrbanInstitute/urbnmapr

counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 

#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , gf_bins_tidy, by=c("county_fips" = "countyFIPS") )

title <- paste("Average Growth Rate over last", n_days, "days")
subtitle <- paste(min_date, "to", max_date)

gif_data <- county_shp_gf %>%
  filter(date >= as.Date("2020-03-01"))



all_gif <- ggplot(gif_data) + 
  geom_sf(aes(fill = growth_factor), color = NA) +
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
    , caption = "Data Source: usafacts.org"
  ) +
  labs(
    title = "Growth Rate from January to Present"
    , subtitle = "{frame_time}"
    , caption = "Data Source: usafacts.org"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) +
  transition_time(date) 



county_gif <- animate(
  all_gif
  , fps = 1
  , nframes = length(unique(gif_data$date))
  , width = 7
  , height = 5
  , units = c("in")
  , res = 100
)

 anim_save("img/county_gif.gif", county_gif)



