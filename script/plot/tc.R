
# https://github.com/MarEichler/us_hex_map/tree/main/states_and_territories
centers   <- read_csv("data/usa_st_centers.csv")
spdf_fort <- read_csv("data/usa_st_fort.csv")


current_date <- max(covid19$date)

hex_items <- c(state.abb, "DC", "PR", "USA")

quant_breaks <- covid19_county %>% 
  pull(case_total_PC) %>%
  quantile(prob = c(0, quant_perc, 1), na.rm = TRUE)

quant_labels <- c(
  paste0("[", percent(quant_breaks[1], accuracy = 0.1), ", ", percent(quant_breaks[2], accuracy = 0.1), ")")
  , paste0("[", percent(quant_breaks[2], accuracy = 0.1), ", ", percent(quant_breaks[3], accuracy = 0.1), ")")
  , paste0("[", percent(quant_breaks[3], accuracy = 0.1), ", ", percent(quant_breaks[4], accuracy = 0.1), ")")
  , paste0("[", percent(quant_breaks[4], accuracy = 0.1), ", ", percent(quant_breaks[5], accuracy = 0.1), ")")
  , paste0("[", percent(quant_breaks[5], accuracy = 0.1), ", ", percent(quant_breaks[6], accuracy = 0.1), "]")
)

names(quant_colors) <-  quant_labels

df <- covid19 %>% 
  filter(geo %in% hex_items) %>%
  filter(date == current_date) %>%
  mutate(
    quantile = cut(
      round(case_total_PC, digits = 3)
      , breaks = c(quant_breaks[1:5], Inf)
      , labels = quant_labels
      , right = FALSE
    )
    , font_color = SET_FONT_COLOR_quant(quantile)
  ) 

max_date <- max(covid19$date)

 

hex_data<- spdf_fort %>%
  left_join(., df, by=c("id" = "geo")) %>%
  filter(id %in%  hex_items) 


labels <- centers %>%
  left_join(., hex_data, by = "id") %>%
  filter(id %in%  hex_items) 


USA_data <- df %>% filter(geo == "USA") %>% filter(date == max_date)
title <- paste("Total Cases as Percent of Population as of ", max_date)
USA_val_text <- paste0("National US value: ", percent(USA_data$case_total_PC, accuracy = 0.1))

USA_bg_color <- quant_colors[names(quant_colors) == USA_data$quantile]
USA_ft_color <- SET_FONT_COLOR_quant(USA_data$quantile)

hex_data$USA_val <- USA_val_text

PLOTtc_hex <- ggplot(data = hex_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = quantile)) +
  geom_polygon(color = "white", fill = NA, size = 1, show.legend = FALSE) +
  geom_polygon(data = subset(hex_data, font_color == "black"), color = "grey80", fill = NA, size = 0.25 ) +  
  facet_grid( . ~ USA_val) + 
  scale_fill_manual(
      name = NULL
    , values = quant_colors
    , drop = FALSE #show all categories
  ) + 
  guides(fill=guide_legend(title.position="top")) + 
  geom_text(
      data = labels
    , aes(x=x, y=y+5, label=id)
    , color = labels$font_color 
    , fontface = "bold"
  ) +
  geom_text(
      data = labels
    , aes(x=x, y=y-6, label=scales::percent(case_total_PC, accuracy = 0.1))
    , color = labels$font_color
    , size = 2.5
  ) +
  theme_void() +
  coord_fixed() +
  labs(title = title) +
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0))
    , plot.subtitle = element_text(hjust = 0.5, size = subtitle_size)
    , strip.text.x  = element_text(size = 12, color = USA_ft_color, face = "bold", margin = margin(3, 0, 3, 0))
    , strip.background.x = element_rect(fill = USA_bg_color, color = USA_bg_color,)
    , legend.spacing.x = unit(0, 'cm')
    , legend.margin=margin(t = .5, unit='cm')
    , legend.position = "bottom"
    , legend.text = element_text(margin = margin(0,12,0,3)) # t r b l
  )


state_sf  <- get_urbn_map("states", sf = TRUE)
counties_sf <- get_urbn_map("counties", sf = TRUE) %>% 
  #data import fips doesn't have 0's at begining of certain FIPS; so convert to numeric and then back to charcter to remove leading zeros 
  mutate(
    county_fips = as.numeric(county_fips)
    , county_fips = as.character(county_fips)
  )


county_map_data <- covid19_county  %>% 
  filter(date == current_date) %>%
  mutate(
    quantile = cut(
      round(case_total_PC, digits = 3)
      , breaks = c(quant_breaks[1:5], Inf)
      , labels = quant_labels
      , right = FALSE
    )
  ) %>% 
  select(date, county_fips, case_total_PC, quantile) %>%
  left_join(., counties_sf, by = "county_fips")

lower48_sf  <- get_urbn_map("states", sf = TRUE) %>% subset(!state_abbv %in% c("AK", "HI"))
lower48_border <- st_union(lower48_sf)

PLOTtc_cnty <- ggplot() +
  geom_sf(data = county_map_data, aes(geometry = geometry, fill = quantile), color = NA) + 
  geom_sf(data = state_sf,        aes(geometry = geometry), color = "white", fill = NA) + 
  geom_sf(data = lower48_border, aes(geometry = geometry), fill = NA, color = "grey50") + 
  geom_sf(data = subset(state_sf, state_abbv %in% c("AK", "HI")), aes(geometry = geometry), color = "grey50", fill = NA) + 
  #facet_grid( . ~ USA_val) + 
  scale_fill_manual(
      name = NULL
    , values = quant_colors
    , drop = FALSE #show all categories
    , na.value = "grey70"
  ) +
  theme_void() +
  coord_sf() +
  #labs(title = title ) +
  theme(
      plot.title = element_text(face = "bold", hjust = 0.5, margin = margin(0, 0, 10, 0))
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
    , strip.text.x  = element_text(size = 12, color = "white", face = "bold", margin = margin(3, 0, 3, 0))
    , strip.background.x = element_rect(fill = USA_bg_color, color = USA_bg_color,)
    , legend.spacing.x = unit(0, 'cm')
    , legend.margin=margin(t = .5, unit='cm')
    , legend.position = "none"
    , legend.text = element_text(margin = margin(0,12,0,3)) # t r b l
  )

PLOTtc <- cowplot::plot_grid(PLOTtc_hex, PLOTtc_cnty, ncol = 1)