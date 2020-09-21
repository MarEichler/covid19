library(tidyverse)
library(knitr)
library(zoo)
library(cowplot)
library(scales)

#scripts
source("script/variable/colors.R")

#DATA
load("data/covid19_US.rda")
load("data/covid19_state.rda")



us_all <- covid19_US %>%
  mutate(state = "Entire US") %>%
  select(state, date, gf = growth_factor, nc = new_cases, nd = new_deaths)



data <- covid19_state %>%
  select(state, date, gf, nc, nd) %>%
  mutate(state = as.character(state))
  rbind(., us_all)

unique(covid19_state$state)


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

ma_k <- 7

min_date <- x_min
max_date <- x_max


plot_data <- data %>%
  filter(state == "MN") %>%
  mutate(
      ma_nc = c(rep(NA, ma_k - 1), rollmean(nc, k=ma_k, na.rm=TRUE))
    , ma_nd = c(rep(NA, ma_k - 1), rollmean(nd, k=ma_k, na.rm=TRUE))
    , ma_gf = c(rep(NA, ma_k - 1), rollmean(nd, k=ma_k, na.rm=TRUE))
    ) %>%
  filter(date >= min_date & date <= max_date) 

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
  #   expand = c(0, 0.02)
     , labels = comma
  ) +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())

plot_nd <- ggplot(plot_data, aes(x=date, y=nd))+
  geom_col(alpha=0.3) +
  geom_line(
    aes(y = ma_nd)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  ggtitle("New Deaths") +
  date_scaling + 
  scale_y_continuous(
    , labels = comma
  ) +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())


ggplot(plot_data, aes(x=date, y=nd))+
  geom_col(alpha=0.3) +
  geom_line(
    aes(y = ma_nd)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  ggtitle("New Deaths") +
  date_scaling + 
  scale_y_continuous(
    , labels = comma
  ) +
  theme_minimal()+
  theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())

plot_gf <- ggplot(plot_data, aes(x=date, y= ma_gf - 1))+
  annotate("rect", xmin = x_min, xmax = x_max, ymin = 0, ymax =  Inf, fill = gf2plus, alpha = 0.2) + 
  geom_area(alpha = 0.3, fill = blue_comp) +
  date_scaling +
  scale_y_continuous(
      expand = c(0, 0)
    , breaks = c(-1, 0, 4)
    , labels = c(0, 1, 5)
    , minor_breaks = NULL
  ) +
  coord_cartesian(ylim = c(-1, 4)) + 
  labs(
      title = "7-Day Moving Average of Growth Factor"
    , caption = "NOTE: Plot cut-off at GF greater than 5"
  ) + 
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
    , axis.title.x=element_blank()
    , axis.title.y=element_blank()
    , plot.margin  = margin(t=0, r=15, b=0, l=0)
    , axis.ticks.length = unit(5, "pt")
  ) 


plot_grid(
    plot_nc
  , plot_nd
  , plot_gf
  , ncol = 1
  , align = "v"
)




##############################


 
data %>% 
  filter(state == "MN") %>%
  summarize(
      `Total Cases`  = formatC(sum(nc), big.mark=",", format="d")
    , `Total Deaths` = formatC(sum(nd), big.mark=",", format="d")
    ) %>%
  kable(align = c("r", "r"), label = "MN")

