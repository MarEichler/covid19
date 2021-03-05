

# set labels, top of second wave 
labels_dc <- covid19_US %>% 
  filter(date >= as.Date("2020-07-01"), date <= as.Date("2020-10-01")) %>% 
  mutate(max = ifelse(case_new == max(case_new), 1, 0)) %>%
  filter(max == 1)

#daily cases plot
PLOToverview_dailycases <- ggplot(covid19_US, aes(x=date, y=case_new))+
  geom_col(alpha=0.3) +
  geom_line(
    aes(y = case_MA7)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  geom_text_repel(
    seed = 100
    , data = labels_dc
    , aes(y = case_MA7, label = "7-Day\nMoving Average")
    , color = blue_comp
    , nudge_y = 25000
    , nudge_x = -20
  ) +
  ggtitle("New Cases per Day") +
  date_scaling +
  thousands_scaling +
  theme_minimal()+
  theme_general() + 
  theme(
      axis.title.x=element_blank()
    , axis.title.y=element_blank()
    , plot.margin  = margin(t=0, r=15, b=0, l=0)
    , axis.ticks.length = unit(5, "pt")
  ) 

