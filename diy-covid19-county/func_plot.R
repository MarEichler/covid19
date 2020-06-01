library(RColorBrewer)

gf0     <- "grey80"
gf0_1   <- brewer.pal(5, "YlOrRd")[2]
gf1_2   <- brewer.pal(5, "YlOrRd")[3]
gf2plus <- brewer.pal(5, "YlOrRd")[4]

blue_comp <- "#2085f0"

color_palette <- c(gf0, gf0_1, gf1_2, gf2plus)

state_abbrv_color <- "grey20"

line_state <- "grey20"


county_map <- function(plot_data){
  
  county_shp_gf <- counties_sf %>%
    left_join(. , plot_data, by=c("county_fips" = "countyFIPS") )
  
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
    theme_void() 
  
  
}


