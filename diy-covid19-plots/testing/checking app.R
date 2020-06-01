library(tidyverse)
library(knitr)
library(zoo)
library(cowplot)
library(scales)
load("data/covid19_US.rda")
load("data/covid19_state.rda")
source("script/variable/colors.R")



us_all <- covid19_US %>%
  mutate(state = "Entire US") %>%
  select(state, date, gf = growth_factor, nc = new_cases)

data <- rbind(covid19_state, us_all)

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
  filter(state == "MN") %>%
  mutate(
      ma_gf = c(rep(NA, ma_k - 1), rollmean(gf, k=ma_k, na.rm=TRUE))
    , ma_nc = c(rep(NA, ma_k - 1), rollmean(nc, k=ma_k, na.rm=TRUE))
    ) %>%
  filter(date >= min_date & date <= max_date) %>%
  mutate(
    gf_2plus = ifelse(gf > 2, 2, -1), 
    ma_2plus = ifelse(ma_gf > 2, 2, -1)
  )

plot_gf <- ggplot(plot_data, aes(date, gf)) + 
  annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
  geom_area(alpha = 0.3) + #raw gf
  geom_line(
    aes(y = ma_gf)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  geom_point(aes(date, gf_2plus), color = "grey35", size = 3) + #gf > 2
  geom_point(aes(date, ma_2plus), color = blue_comp, size = 4, alpha = 0.3) + #ma_gf > 2
  ggtitle("Growth Factor") +
  scale_y_continuous(
      breaks = c(0, 1, 2)
    , labels = c("0", "1", "2")
    , expand = c(0, .02)
  ) +
  scale_x_date(
    name = NULL
    , labels = scales::date_format("%b-%d")
  ) + 
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal() +
  theme( plot.title = element_text(hjust = 0.5) , axis.title = element_blank())

plot_nc <- ggplot(plot_data, aes(x=date, y=nc))+
  geom_col(alpha=0.3) +
  geom_line(
    aes(y = ma_nc)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  ggtitle("New Cases") +
  date_scaling + 
  scale_y_continuous(
     expand = c(0, .02)
     , labels = comma
  ) +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())

plot_grid(plot_gf, plot_nc, align = "h")
##############################

plot_data %>%
  arrange(desc(date))


