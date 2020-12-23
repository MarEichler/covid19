#global parameters and ggplot formatting 


gf0     <- "grey80"
gf0_1   <- brewer.pal(5, "YlOrRd")[2]
gf1_2   <- brewer.pal(5, "YlOrRd")[3]
gf2plus <- brewer.pal(5, "YlOrRd")[4]

color_palette <- list(
  "0"   = gf0
  , "0-1" = gf0_1
  , "1-2" = gf1_2
  , "2+"  = gf2plus
)


#beginning date for overview plots 
startdate <- "2020-03-15"
#3/18 - start 'lockdown' ; after large growth factors↕


#plot parametrs
title_size <- 14
subtitle_size <- 12

current_date <- max(covid19$date)

x_min <- as.Date(startdate) -.5
x_max <- as.Date(current_date) + .5


#set up consistent date breaks
#my_date_breaks <- pretty_breaks()(as.Date(c(x_min, x_max)))
my_date_breaks <- seq.Date(as.Date("2020-04-01"), as.Date(x_max), "2 month")

#date scaling (want consistency)
date_scaling <- scale_x_date(
    name = NULL
  , labels = date_format("%b %Y")
  , breaks = my_date_breaks
  , expand = c(0, 0)
  , limits = c(x_min, x_max) 
)

# for US plot; 10,000 -> 10k
thousands_scaling <- scale_y_continuous(
    name = NULL
  , limits = c(0, NA)
  , labels = scales::unit_format(unit = "k", scale = 1e-3, accuracy = 1, big.mark = ",")
  , expand = c(0, 0)
)

# for US plot; 10,000 -> 10k
millions_scaling <- scale_y_continuous(
  name = NULL
  , limits = c(0, NA)
  , labels = scales::unit_format(unit = "M", scale = 1e-6, accuracy = 0.1)
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


#colors from NPR 
#https://www.npr.org/sections/health-shots/2020/03/16/816707182/map-tracking-the-spread-of-the-coronavirus-in-the-u-s

nc_perc_palette <- list(
  "-100%" = "#549990" 
  ,  "-50%" = "#7dc3ae" #GnYlRd[2]
  ,   "-5%" = "#ebe3a7" #GnYlRd[5]
  ,   "+5%" = "#eeb97a" #GnYlRd[6]
  ,  "+50%" = "#ea8e4f" #GnYlRd[7]
  , "+100%" = "#df6222" #GnYlRd[8]
)

chacter_breaks <- labels(nc_perc_palette)
number_breaks <- c(-1, -.5, -.05, .05, .5, 1, Inf)

NA_grey <- "grey80"

blue_comp <- "#2085f0"


state_abbrv_color <- "grey20"

line_state <- "grey20"


#NPR COLORS

NPR_darkgreen <- "#549990" #dark green 
NPR_litegreen <- "#7dc3ae" #light green
NPR_yellow    <- "#ebe3a7" #yellow
NPR_yellorang <- "#eeb97a" #yellow/orange
NPR_orange    <- "#ea8e4f" #orange
NPR_red       <- "#df6222"


### risk groups 

risk_group_breaks <- c(-Inf, 1, 10, 25, 50, Inf) #[)
risk_group_labels <- c("<1 Low", "[1,10) Medium", "[10,25) High", "[25,50) Very High", "50+ Extremely High")


risk_group_colors <- c(
    NPR_litegreen#"#FCFFA4FF" #<1 
  , "#F7D340FF" # [1,10)
  , "#E55C30FF" # [10,25)
  , "#B1325AFF" # [25, 50)
  , "#87216BFF" #[50, inf)
)

names(risk_group_colors) <-  risk_group_labels


SET_FONT_COLOR <- function(vec){
  case_when(
      vec == risk_group_labels[1] ~ "black"
    , vec == risk_group_labels[2] ~ "black"
    , vec == risk_group_labels[3] ~ "white"
    , vec == risk_group_labels[4] ~ "white"
    , vec == risk_group_labels[5] ~ "white"
  )
}



