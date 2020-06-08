#global parameters and ggplot formatting 

#moving average
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


#set up consistent date breaks
my_date_breaks <- pretty_breaks()(as.Date(c(x_min, x_max)))

#date scaling (want consistency)
date_scaling <- scale_x_date(
    name = NULL
  , labels = date_format("%b-%d")
  , breaks = my_date_breaks
  , expand = c(0, 0)
)

# for US plot; 10,000 -> 10k
thousands_scaling <- scale_y_continuous(
    name = NULL
  , limits = c(0, NA)
  , labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 1, big.mark = ",")
  , expand = c(0, 0)
)

#caption for gf_plots that gives info 
cap_gf <- paste(
  strwrap("A growth factor below 1 means new cases are decreasing,\ni.e. the pandemic is slowing"
          , 30)
  , collapse = "\n"
)
gf_info_cap <- annotate("text", x = x_min + 1, y = 0, label = cap_gf, vjust=-0.3, hjust= -0.1, size = 3, color = "grey15") 

#red box on gf plots 
red_box <- annotate("rect", xmin = x_min, xmax = x_max, ymin = 1, ymax =  Inf, fill = gf2plus, alpha = 0.2) 

