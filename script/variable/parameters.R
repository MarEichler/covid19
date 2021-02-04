############################
##### GGPLOT ###############

#beginning date for overview plots 
startdate <- "2020-03-15"


#plot parametrs

current_date <- min(
  max(covid19$date)
  #, max(covid19_county$date)
  )

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

#output_type is Rmd or RShiny; affects the font sizes 

#global theme 
font_size_title       <- case_when(
    output_type == "Rmd"    ~ 14
  , output_type == "RShiny" ~ 21
)
font_size_subtitle    <- case_when(
    output_type == "Rmd"    ~ 12
  , output_type == "RShiny" ~ 18
)
font_size_axis_title  <- case_when(
    output_type == "Rmd"    ~ 13
  , output_type == "RShiny" ~ 19.5
)
font_size_axis_text   <- case_when(
    output_type == "Rmd"    ~ 08
  , output_type == "RShiny" ~ 12
)


theme_general <- function(){
  theme(
      plot.title    = element_text(size = font_size_title,      hjust = 0.5)
    , plot.subtitle = element_text(size = font_size_subtitle,   hjust = 0.5)
    , axis.title    = element_text(size = font_size_axis_title, hjust = 0.5)
    , axis.text     = element_text(size = font_size_axis_text,  hjust = 0.5)

  )
  
  
}


title_size    <- 14
subtitle_size <- 12

############################
##### COLORS ###############

gf_red <- "#F03B20"
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
    "#FCFFA4FF" #<1 
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


### quantiles groups 

quant_perc <- c(0.2, 0.4, 0.6, 0.8) #[)
quant_labels <- c("<20% quantile", "[20%, 40%) quantile", "[40%, 60%) quantile", "[60%, 80%) quantile", "80%+ quantile")


quant_colors <- c(
    "#FCFFA4FF" # <20% 
  , "#F7D340FF" # [20%,40%)
  , "#E55C30FF" # [40%,60%)
  , "#B1325AFF" # [60%,80%)
  , "#87216BFF" # [80%, inf)
)

names(quant_colors) <-  quant_labels


SET_FONT_COLOR_quant <- function(vec){
  case_when(
      vec == quant_labels[1] ~ "black"
    , vec == quant_labels[2] ~ "black"
    , vec == quant_labels[3] ~ "white"
    , vec == quant_labels[4] ~ "white"
    , vec == quant_labels[5] ~ "white"
  )
}
