
gf_plot_ma <- function(plot_data, min_date, max_date){
  
  ggplot(plot_data, aes(date, gf)) + 
    annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
    geom_area(alpha = 0.3) + #raw gf
    geom_line(
      aes(y = ma)
      , size = 1.25
      , color = blue_comp
      , alpha = 0.6
    ) +
    geom_point(aes(date, gf_2plus), color = "grey35", size = 3) + #gf > 2
    geom_point(aes(date, ma_2plus), color = blue_comp, size = 4, alpha = 0.3) + #ma_gf > 2
    scale_y_continuous(
      name = "Growth Factor"
      , breaks = c(0, 1, 2)
      , labels = c("0", "1", "2")
      , expand = c(0, .02)
    ) +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    coord_cartesian(ylim = c(0, 2)) +
    theme_minimal() 
  
}


gf_plot <- function(plot_data, min_date, max_date){
  
  ggplot(plot_data, aes(date, gf)) + 
    annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
    geom_area(alpha = 0.3) + #raw gf
    geom_point(aes(date, gf_2plus), color = "grey35", size = 3) + #gf > 2
    scale_y_continuous(
      name = "Growth Factor"
      , breaks = c(0, 1, 2)
      , labels = c("0", "1", "2")
      , expand = c(0, .01)
    ) +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
    ) + 
    coord_cartesian(ylim = c(0, 2)) +
    theme_minimal() 
  
}