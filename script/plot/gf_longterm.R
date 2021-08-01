gf_data <- covid19 %>%
  filter(geo == "USA") %>%
  select(date, contains("gf")) %>%
  filter(date >= x_min)

gf_max <- gf_data %>% filter(date > as.Date("2020-05-01")) %>% pull(gf) %>% max(na.rm = TRUE)

labels_gf <- gf_data %>% 
  filter(date >= as.Date("2020-03-25"), date <= as.Date("2020-05-01")) %>%
  top_n(n=1, wt = gf_MA7)

firstmax <- gf_data%>%
  filter(date >= x_min, date <= as.Date("2020-05-01")) %>%
  top_n(n= 1, wt = gf) %>%
  pull(gf) 

nudge_gf_val <- ((firstmax-4.5)- (labels_gf$gf_MA7 - 1))*0.75

PLOTgf_longterm <- ggplot(gf_data, aes(x=date))+
  annotate("rect", xmin = min(gf_data$date), xmax = max(gf_data$date), ymin = 0, ymax =  Inf, fill = gf_red, alpha = 0.15) + 
  geom_col(aes(y = gf - 1), alpha=0.3) +
  geom_area(
    aes(y = gf_MA7 - 1)
    , fill = blue_comp
    , alpha = 0.5
  ) +
  geom_text_repel(
    seed = 100
    , data = labels_gf
    , aes(y = gf_MA7-1, label = "7-Day\nMoving Average")
    , color = blue_comp
    , nudge_y = nudge_gf_val
    , nudge_x = 60
  ) +
  date_scaling + 
  scale_y_continuous(minor_breaks = NULL) + 
  labs(
    title = "USA National Daily and 7-Day Moving Average Growth Factor"
    , subtitle =  "Growth Factor Difference from One"
    , caption = "A growth factor less than one (difference less than zero) means new cases are decreasing"
  ) +
  theme_minimal()+
  coord_cartesian(ylim = c(NA, gf_max)) + 
  theme(
      plot.title = element_text(hjust = 0.5, size = title_size)
    , plot.subtitle = element_text(hjust = 0.5, size = subtitle_size)
    , strip.text = element_text(size = subtitle_size, face = "bold")
    , axis.title.x=element_blank()
    , axis.title.y=element_blank()
    , plot.margin  = margin(t=0, r=15, b=0, l=0)
    , axis.ticks.length = unit(5, "pt")
    , panel.grid.major.y = element_blank()
  ) 
