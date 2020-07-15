library(RColorBrewer)

##### SET COLORS 
gf0     <- "grey80"
gf0_1   <- brewer.pal(5, "YlOrRd")[2]
gf1_2   <- brewer.pal(5, "YlOrRd")[3]
gf2plus <- brewer.pal(5, "YlOrRd")[4]
blue_comp <- "#2085f0"

###### FUNCTION TO CREATE PLOTS (WITH MOVING AVERAGE) 
gf_plot_ma <- function(plot_data, min_date, max_date){
  
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
    scale_y_continuous(labels = comma) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_nc
  
} # END OF GF_PLOT_MA

###### FUNCTION TO CREATE PLOTS (NO MOVING AVERAGE) 
gf_plot <- function(plot_data, min_date, max_date){

  
  plot_nc <- ggplot(plot_data, aes(x=date, y=nc))+
    geom_col(alpha=0.3) +
    ggtitle("New Cases") +
    scale_x_date(
        name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(labels = comma) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_nc 
  
} # END OF GF_PLOT 