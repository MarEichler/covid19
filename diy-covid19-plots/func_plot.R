##### SET COLORS 
blue_comp <- "#2085f0"
gf2plus   <- "#F03B20"

###### FUNCTION TO CREATE PLOTS (WITH MOVING AVERAGE) 
plot_ma <- function(plot_data, min_date, max_date, gf_min, gf_max){
  
  plot_nc <- ggplot(plot_data, aes(x=date, y=nc))+
    geom_col(alpha=0.3) +
    geom_line(
      aes(y = ma_nc)
      , size = 1.25
      , color = blue_comp
      , alpha = 0.6
    ) +
    ggtitle("New Cases by Day and Moving Average") +
    scale_x_date(
        name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
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
    ggtitle("New Deaths by Day and Moving Average") +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_gf <- ggplot(plot_data, aes(x=date, y= ma_gf - 1))+
    annotate("rect", xmin = min(plot_data$date), xmax = max(plot_data$date), ymin = 0, ymax =  Inf, fill = gf2plus, alpha = 0.2) +
    geom_area(alpha = 0.3, fill = blue_comp) +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    scale_y_continuous(
      expand = c(0, 0)
      , breaks = c(gf_min - 1, 0, gf_max - 1)
      , labels = c(gf_min, 1, gf_max)
      , minor_breaks = NULL
    ) +
    coord_cartesian(ylim = c(gf_min - 1, gf_max - 1)) +
    labs(
      title = "Growth Factor Moving Average"
      , caption = paste("NOTE: Plot is cut-off to values less than", gf_min, "and greater than", gf_max)
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
  
  
} # END OF MA + PLOTS 

###### FUNCTION TO CREATE PLOTS (NO MOVING AVERAGE) 
plot <- function(plot_data, min_date, max_date, gf_min, gf_max){
  
  plot_nc <- ggplot(plot_data, aes(x=date, y=nc))+
    geom_col(alpha=0.3) +
    ggtitle("New Cases by Day") +
    scale_x_date(
        name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_nd <- ggplot(plot_data, aes(x=date, y=nd))+
    geom_col(alpha=0.3) +
    ggtitle("New Deaths by Day") +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_gf <- ggplot(plot_data, aes(x=date, y= gf - 1))+
    annotate("rect", xmin = min(plot_data$date), xmax = max(plot_data$date), ymin = 0, ymax =  Inf, fill = gf2plus, alpha = 0.2) +
    geom_area(alpha = 0.3) +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    scale_y_continuous(
      expand = c(0, 0)
      , breaks = c(gf_min - 1, 0, gf_max - 1)
      , labels = c(gf_min, 1, gf_max)
      , minor_breaks = NULL
    ) +
    coord_cartesian(ylim = c(gf_min - 1, gf_max - 1)) +
    labs(
      title = "Growth Factor by Day"
      , caption = paste("NOTE: Plot is cut-off to values less than", gf_min, "and greater than", gf_max)
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
  

  
} # END OF JUST PLOTS 