gf_facet_val <- covid19 %>%
  filter(geo %in% c(state.abb, "DC")) %>% 
  filter(date > max(date) - 14)  #x_min from parameters.R file 

gf_facet_max <- gf_facet_val %>%
  group_by(geo) %>%
  summarize(max_gf_from_one = max(gf_MA7)-1)

gf_facet <- left_join(gf_facet_val, gf_facet_max)



min_gf_val <- min(RoundTo(min(gf_facet$gf_MA7) - 1, multiple = 0.05), -0.1)

PLOTstate_twoweeksgf <- ggplot(gf_facet, aes(date, y = gf_MA7 - 1)) + 
  geom_area(aes(y = max_gf_from_one), fill = gf_red, alpha = 0.15) +
  geom_area(fill = blue_comp, alpha = 0.5) + 
  facet_geo(~geo, grid = "us_state_grid2" , scales = "free") +
  scale_x_date(      name = NULL, expand = c(0, 0), breaks = NULL) + 
  scale_y_continuous(
      name = NULL
    , expand = c(0, 0)
  ) +
  labs(
      title = "Individual State 7-Day Moving Average Growth Factor Last Two Weeks"
    , subtitle =  "Growth Factor Difference from One"
    , caption = paste0(
      "Dates range from ", min(gf_facet$date), " to ", max(gf_facet$date)
      , "\n"
      , "A growth factor less than one (difference less than zero) means new cases are decreasing"
    )
  ) + 
  theme_minimal() +
  theme(
      strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
    , plot.title = element_text(hjust = 0.5, size = title_size)
    , plot.subtitle = element_text(hjust = 0.5, size = subtitle_size)
    , axis.text.y  = element_text(size = 7)
    , panel.grid = element_blank()
    , panel.background = element_rect(color = "grey80")
    , legend.position = "top"
  ) 