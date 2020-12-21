
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





risk_group_colors <- c(
    "#FCFFA4FF" #<1 
  , "#F7D340FF" # [1,10)
  , "#E55C30FF" # [10,25)
  , "#B1325AFF" # [25, 50)
  , "#87216BFF" #[50, inf)
  )

names(risk_group_colors) <-  risk_group_labels



