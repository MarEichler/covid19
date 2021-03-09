
# https://github.com/MarEichler/us_hex_map/tree/main/states_and_territories
centers   <- read_csv("data/usa_st_centers.csv")
spdf_fort <- read_csv("data/usa_st_fort.csv")


current_date <- max(covid19$date)

hex_items <- c(state.abb, "DC", "PR", "USA")

df <- covid19 %>% 
  filter(geo %in%  hex_items) %>%
  filter(date == current_date) %>%
  mutate(
    risk_group = cut(
      round(case_MA7_PC_100k, digits = 0)
      , breaks = risk_group_breaks
      , labels = risk_group_labels
      , right = FALSE
    )
    , font_color = SET_FONT_COLOR(risk_group)
  )

max_date <- max(covid19$date)



hex_data<- spdf_fort %>%
  left_join(., df, by=c("id" = "geo")) %>%
  filter(id %in%  hex_items) 


labels <- centers %>%
  left_join(., hex_data, by = "id") %>%
  filter(id %in%  hex_items) 


USA_data <- df %>% filter(geo == "USA") %>% filter(date == max_date)
title <- paste("New Cases 7-Day Moving Average per 100,000 on", max_date)
USA_val_text <- paste0("National US value: ", round(USA_data$case_MA7_PC_100k, 0))

USA_bg_color <- risk_group_colors[names(risk_group_colors) == USA_data$risk_group]
USA_ft_color <- SET_FONT_COLOR(USA_data$risk_group)

hex_data$USA_val <- USA_val_text

PLOTnc_hex <- ggplot(data = hex_data, aes(x = long, y = lat, group = group, fill = risk_group)) + 
  geom_polygon() +
  geom_polygon( color = "white" , size = 1, show.legend = FALSE) +
  facet_grid( . ~ USA_val) + 
  scale_fill_manual(
      name = NULL
    , values = risk_group_colors
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
    , aes(x=x, y=y-6, label=round(case_MA7_PC_100k, 0))
    , color = labels$font_color
    , size = 2.5
  )+
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
    risk_group = cut(
      round(case_MA7_PC_100k, digits = 0)
      , breaks = risk_group_breaks
      , labels = risk_group_labels
      , right = FALSE
    )
  ) %>% 
  select(date, county_fips, case_MA7_PC_100k, risk_group) %>%
  left_join(., counties_sf, by = "county_fips")

lower48_sf  <- get_urbn_map("states", sf = TRUE) %>% subset(!state_abbv %in% c("AK", "HI"))
lower48_border <- st_union(lower48_sf)

PLOTnc_cnty <- ggplot(county_map_data, aes(geometry = geometry)) +
  geom_sf(aes(fill = risk_group), color = NA) + 
  geom_sf(data = state_sf, aes(geometry = geometry), color = "white", fill = NA) + 
  geom_sf(data = lower48_border, aes(geometry = geometry), fill = NA, color = "grey50") + 
  geom_sf(data = subset(state_sf, state_abbv %in% c("AK", "HI")), aes(geometry = geometry), color = "grey50", fill = NA) + 
  #facet_grid( . ~ USA_val) + 
  scale_fill_manual(
      name = NULL
    , values = risk_group_colors
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


PLOTnc <- cowplot::plot_grid(PLOTnc_hex, PLOTnc_cnty, ncol = 1)
