
gf_data4wk <- covid19 %>%
  filter(geo == "USA") %>%
  select(date, contains("gf")) %>%
  filter(date > max(date) - 14)

labels_gf <- gf_data4wk %>% 
  filter(date == max(date)-10)

seclarg <- gf_data4wk %>%
  top_n(n= 2, wt = gf) %>%
  pull(gf) %>% min()

nudge_gf_val <- (seclarg-1)- (labels_gf$gf_MA7 - 1)


PLOTgf_twoweeks <- ggplot(gf_data4wk, aes(x=date))+
  annotate("rect", xmin = min(gf_data4wk$date)-0.5, xmax = max(gf_data4wk$date)+0.5, ymin = 0, ymax =  Inf, fill = gf_red, alpha = 0.15) + 
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
    , nudge_x = -2
  ) +
  scale_x_date(
    name = NULL
    , labels = date_format("%b-%d")
    , date_breaks = "4 days"
    , expand = c(0, 0)
  )+
  scale_y_continuous(minor_breaks = NULL) + 
  labs(
    title = "USA National Daily and 7-Day Moving Average Growth Factor Last Two Weeks"
    , subtitle =  "Growth Factor Difference from One" 
    , caption = "A growth factor less than one (difference less than zero) means new cases are decreasing"
  ) +
  theme_minimal()+
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