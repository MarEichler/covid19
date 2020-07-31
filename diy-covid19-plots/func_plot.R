##### SET COLORS 
blue_comp <- "#2085f0"

###### FUNCTION TO CREATE PLOTS (WITH MOVING AVERAGE) 
plot_ma <- function(plot_data, min_date, max_date){
  
  plot_nc <- ggplot(plot_data, aes(x=date, y=nc))+
    geom_col(alpha=0.3) +
    geom_line(
      aes(y = ma_nc)
      , size = 1.25
      , color = blue_comp
      , alpha = 0.6
    ) +
    ggtitle("New Cases and Moving Average") +
    scale_x_date(
        name = NULL
      , labels = scales::date_format("%b-%d")
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
    ggtitle("New Deaths and Moving Average") +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_grid(plot_nc, plot_nd, nrow = 2, align = "v")
  
} # END OF MA + PLOTS 

###### FUNCTION TO CREATE PLOTS (NO MOVING AVERAGE) 
plot <- function(plot_data, min_date, max_date){
  
  plot_nc <- ggplot(plot_data, aes(x=date, y=nc))+
    geom_col(alpha=0.3) +
    ggtitle("New Cases") +
    scale_x_date(
        name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_nd <- ggplot(plot_data, aes(x=date, y=nd))+
    geom_col(alpha=0.3) +
    ggtitle("New Deaths") +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(labels = comma, limits = c(0, NA)) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_grid(plot_nc, plot_nd, nrow = 2, align = "v") 

  
} # END OF JUST PLOTS 