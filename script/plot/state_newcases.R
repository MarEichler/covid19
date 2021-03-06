facet_data <- covid19 %>%
  filter(geo %in% c(state.abb, "DC")) %>% 
  filter(date >= x_min) #x_min from parameters.R file 


title <- paste("7-Day Moving Average of New Cases per 100,000 ")
subtitle <- paste(min(facet_data$date), "to", max(facet_data$date)) 

max_val <- RoundTo(max(facet_data$case_MA7_PC_100k), multiple = 5)


PLOTstate_newcases <- ggplot(facet_data, aes(date, case_MA7_PC_100k)) + 
  geom_area(fill = blue_comp, alpha = 0.2) + 
  geom_line(color = blue_comp) +
  facet_geo(~geo, grid = "us_state_grid2" , labeller = adjust_labels, scales = "fixed") +
  scale_x_date(      name = NULL, expand = c(0, 0), breaks = NULL) + 
  scale_y_continuous(
      name = NULL
    , expand = c(0, 0)
    , breaks = c(0, max_val)
    , limits = c(0, max_val)
  ) +
  theme_minimal() +
  labs(
      title = title
    , subtitle = subtitle
  ) +
  theme_minimal() +
  theme(
      strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
    , plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
    , axis.text.y  = element_text(size = 7)
    , panel.grid = element_blank()
    , panel.background = element_rect(color = "grey80")
    , legend.position = "top"
  ) 