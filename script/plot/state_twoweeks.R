facet_data <- covid19 %>%
  filter(geo %in% c(state.abb, "DC")) %>% 
  filter(date >= x_min) #x_min from parameters.R file 

last14days_facet_data <- facet_data %>%
  filter(date > max(date) -14)  %>%
  select(date, geo, case_new_PC_100k, case_MA7_PC_100k) 

last14days_facet_data <- last14days_facet_data %>%
  filter(date == min(date) | date == max(date)) %>% 
  select(date, geo, case_MA7_PC_100k) %>%
  pivot_wider(., names_from = date, values_from = case_MA7_PC_100k) %>%
  select(geo, min_date = 2, max_date = 3) %>%
  mutate(
    trend = case_when(
      abs(min_date - max_date) <= 2 ~ "Minimal change (<=2)"
      ,   min_date - max_date  < -2 ~ "Increasing"
      ,   min_date - max_date  >  2 ~ "Decreasing"
    )
  ) %>%
  #mutate(trend = ifelse(min_date <= max_date, "Increasing", "Decreasing")) %>%
  select(geo, trend) %>%
  left_join(last14days_facet_data, ., by = "geo")


title <- paste("New Cases per 100,000 last 2 Weeks")
subtitle <- paste(min(last14days_facet_data$date), "to", max(last14days_facet_data$date)) 

max_val <- RoundTo(max(last14days_facet_data$case_new_PC_100k, na.rm = TRUE), multiple = 5)


colors_indec <- c(
  "grey20"
  , brewer.pal(n=7, "RdYlGn")[1]
  , brewer.pal(n=7, "RdYlGn")[7]
)



names(colors_indec) <-  last14days_facet_data %>% distinct(trend) %>% pull(trend)

PLOTstate_twoweeks <- ggplot(last14days_facet_data, aes(date, case_MA7_PC_100k)) + 
  geom_col(aes(y = case_new_PC_100k), alpha=0.3) +
  #geom_area(alpha = 0.2, aes(fill = trend)) + 
  geom_line(aes(color = trend), alpha = 0.6) + 
  facet_geo(~geo, grid = "us_state_grid2" , labeller = adjust_labels, scales = "fixed") +
  scale_color_manual(name = "Trend of 7-Day Moving Average", values = colors_indec, drop = FALSE) + 
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