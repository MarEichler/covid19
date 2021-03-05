

death_prop_max <- covid19_US %>% filter(date >= x_min, date <= x_max) %>% pull(death_prop) %>% max

plot_dp <- ggplot(covid19_US, aes(date, death_prop)) + 
  geom_line(color = "grey40", size = 1) + 
  date_scaling + 
  scale_y_continuous(
    name = "Total Dealths / Total Cases"
    , limits = c(0, death_prop_max)
    , labels = scales::percent
  ) +
  ggtitle("Case Fatality Percentage per Day") + 
  theme_minimal()+
  theme_general()+ 
  theme(
      axis.title.x=element_blank()
    , plot.margin  = margin(t=0, r=15, b=15, l=0)
    , axis.ticks.length = unit(5, "pt")
  )

labels_nd <- covid19_US %>% 
  filter(date >= as.Date("2020-07-01"), date <= as.Date("2020-10-01")) %>% 
  mutate(max = ifelse(death_new == max(death_new), 1, 0)) %>%
  filter(max == 1)

plot_nd <-ggplot(covid19_US, aes(date, death_new)) + 
  geom_col(alpha=0.3) +
  geom_line(
    aes(y = death_MA7)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  )  +
  date_scaling + 
  thousands_scaling +
  geom_text_repel(
    seed = 100
    , data = labels_nd
    , aes(y = death_MA7, label = "7-Day\nMoving Average")
    , color = blue_comp
    , nudge_y = 5000
    , nudge_x = -10
  ) +
  ggtitle("New Deaths per Day") + 
  theme_minimal()+
  theme_general()+ 
  theme(
      axis.title.x=element_blank()
    , axis.title.y=element_blank()
    , plot.margin  = margin(t=15, r=15, b=0, l=0)
    , axis.ticks.length = unit(5, "pt")
  )


PLOToverview_fatalities <- cowplot::plot_grid(
    plot_nd
  , plot_dp
  , nrow = 1
  , align = "h"
)

