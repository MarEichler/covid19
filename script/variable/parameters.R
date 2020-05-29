#global parameters 

#moving average - pick two weeks
ma_k <- 14

#beginning date for overview plots 
startdate <- "2020-03-15"
#3/18 - start 'lockdown' ; after large growth factors↕

load("data/covid19_US.rda")
covid19_US_adj <- covid19_US  %>%
  #only look at data after a given start date
  filter(date >= as.Date(startdate))


#plot parametrs
title_size <- 14
subtitle_size <- 12

current_date <- max(covid19_US_adj$date)

x_min <- as.Date(startdate) #-.5
x_max <- as.Date(current_date) #+1

#ggplot set ups 
date_scaling <- scale_x_date(
  name = NULL
  , labels = date_format("%b-%d")
  , expand = c(0, 0)
)

thousands_scaling <- scale_y_continuous(
  name = NULL
  , limits = c(0, NA)
  , labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 1, big.mark = ",")
  , expand = c(0, 0)
)