library(tidyverse)
library(knitr)
library(zoo)
load("data/gf_state_tidy.rda")
load("data/covid19_US.rda")

us_all <- covid19_US %>%
  mutate(state = "Entire US") %>%
  select(state, date, gf = growth_factor)

data <- rbind(gf_state_tidy, us_all)

state_pc <- unique(data$state)
names(state_pc) <- unique(data$state)

x_min <- min(data$date)
x_max <- max(data$date)

#ggplot set ups 
date_scaling <- scale_x_date(
  name = NULL
  , labels = scales::date_format("%b-%d")
  , expand = c(0, 0)
)

ma_k <- 14

min_date <- x_max -28
max_date <- x_max

plot_data <- data %>%
  filter(state == "AR") %>%
  mutate(ma = c(rep(NA, ma_k - 1), rollmean(gf, k=ma_k, na.rm=TRUE))) %>%
  filter(date >= min_date & date <= max_date) %>%
  mutate(
    gf_2plus = ifelse(gf > 2, 2, -1), 
    ma_2plus = ifelse(ma > 2, 2, -1)
  )

ggplot(plot_data, aes(date, gf)) + 
  annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
  geom_area(alpha = 0.3) + #raw gf
  geom_line(
    aes(y = ma)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  geom_point(aes(date, gf_2plus), color = "grey35", size = 3) + #gf > 2
  geom_point(aes(date, ma_2plus), color = blue_comp, size = 4, alpha = 0.3) + #ma_gf > 2
  scale_y_continuous(
    name = "Growth Factor"
    , breaks = c(0, 1, 2)
    , labels = c("0", "1", "2")
    , expand = c(0, .02)
  ) +
  scale_x_date(
    name = NULL
    , labels = scales::date_format("%b-%d")
  ) + 
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() 
##############################


#TABLE 

table_data <- plot_data %>%
  arrange(desc(date)) %>%
  mutate(
    Date = format(date, '%a, %b %d, %Y')
    , `Growth Factor` = round(gf, 2)
    , `Growth Factor Moving Average` = round(ma, 2)
  ) %>%
  mutate(
    `Growth Factor` = cell_spec(
      `Growth Factor`, color = 
        ifelse( gf > 1, gf2plus, colorspace::lighten(gf2plus, 0.4))
      , bold = T
    )) %>%
  select(
    Date
    , `Growth Factor`
    , `Growth Factor Moving Average`
  ) 

kable(table_data)
