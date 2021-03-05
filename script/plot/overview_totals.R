
total_labels <- covid19_US %>%
  filter(date == current_date) %>%
  mutate(
    case_total  = paste("Current Total", formatC(case_total,  big.mark=",", format="d"), sep = "\n")
    , death_total = paste("Current Total", formatC(death_total, big.mark=",", format="d"), sep = "\n")
  )

ggtext_size <-  6

tc <- ggplot(covid19_US, aes(date, case_total)) + 
  geom_area(alpha = 0.4)+
  geom_line(
      size = 1
    , alpha = 0.6
  )+
  date_scaling +
  millions_scaling + 
  geom_text(
     data = total_labels 
   , aes(
        label = case_total
      , y = -Inf
      , x = max(date)
    )
    , vjust = -0.15
    , hjust=1.05 
    , size = ggtext_size
    , fontface = "bold"
    , color = "white"
  ) +
  theme_minimal() +
  ggtitle("Total Cases by Day (in millions)") +
  theme(
      plot.margin  = margin(t=0, r=15, b=0, l=0)
    , axis.ticks.length = unit(5, "pt")
  ) + 
  theme_general()

td <- ggplot(covid19_US, aes(date, death_total)) + 
  geom_area(alpha = 0.4)+
  geom_line(
      size = 1
    , alpha = 0.6
  )+
  date_scaling +
  thousands_scaling + 
  geom_text(
      data = total_labels 
    , aes(
        label = death_total
      , y = -Inf
      , x = max(date)
    )
    , vjust = -0.15
    , hjust=1.05 
    , size = ggtext_size
    , fontface = "bold"
    , color = "white"
  ) +
  theme_minimal() +
  ggtitle("Total Deaths by Day (in thousands)") +
  theme(
      plot.margin  = margin(t=0, r=15, b=0, l=0)
    , axis.ticks.length = unit(5, "pt")
  ) + 
  theme_general()

PLOToverview_totals <- plot_grid(tc, td, nrow = 1)


ggsave(file ="img/PLOToverview_totals.jpg" , plot = PLOToverview_totals, width = default_w, height = 3)
