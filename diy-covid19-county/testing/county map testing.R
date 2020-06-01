library(tidyverse)
library(urbnmapr)
library(RColorBrewer)

load("diy-covid19-county/county_week.rda") 


gf0     <- "grey80"
gf0_1   <- brewer.pal(5, "YlOrRd")[2]
gf1_2   <- brewer.pal(5, "YlOrRd")[3]
gf2plus <- brewer.pal(5, "YlOrRd")[4]

blue_comp <- "#2085f0"

# https://github.com/UrbanInstitute/urbnmapr


counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
  mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 



plot_week <- "1"

plot_data <- county_week %>%
  filter(week == plot_week)
  



color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)


#join growth rate data with shape file data
county_shp_gf <- counties_sf %>%
  left_join(. , plot_data, by=c("county_fips" = "countyFIPS") )



title <- paste("Average Growth Rate for Week", plot_week)

ggplot(county_shp_gf) +
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
  ggtitle(title) + 
  theme_void() 


