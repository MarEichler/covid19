library(RColorBrewer)
library(colorspace)

gf0     <- "grey80"
gf0_1   <- brewer.pal(5, "YlOrRd")[2]
gf1_2   <- brewer.pal(5, "YlOrRd")[3]
gf2plus <- brewer.pal(5, "YlOrRd")[4]

blue_comp <- "#2085f0"


state_abbrv_color <- "grey20"

line_state <- "grey20"


gf_plot_ma <- function(plot_data, min_date, max_date){
  
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
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(
      expand = c(0, .02)
      , labels = comma
    ) +
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_grid(plot_gf, plot_nc, align = "h")
  
}


gf_plot <- function(plot_data, min_date, max_date){
  
  plot_gf <- ggplot(plot_data, aes(date, gf)) + 
    annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
    geom_area(alpha = 0.3) + #raw gf
    geom_point(aes(date, gf_2plus), color = "grey35", size = 3) + #gf > 2
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
    ggtitle("New Cases") +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    scale_y_continuous(
      expand = c(0, .02)
      , labels = comma
    )+
    theme_minimal()+
    theme( plot.title = element_text(hjust = 0.5), axis.title = element_blank())
  
  plot_grid(plot_gf, plot_nc, align = "h")
  
}