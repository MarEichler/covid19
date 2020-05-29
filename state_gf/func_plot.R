
gf_plot_ma <- function(plot_data, min_date, max_date){
  
  gf_plus2 <- plot_data %>%
    filter(gf > 2 )
  
  ma_plus2 <- plot_data %>%
    filter(ma > 2 )
  
  ggplot(plot_data, aes(date, gf)) + 
    annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
    geom_area(alpha = 0.3) + #raw gf
    geom_line(
      aes(y = ma)
      , size = 1.25
      , color = blue_comp
      , alpha = 0.6
    ) +
    geom_point( data = gf_plus2, aes(date, gf), color = "grey35", size = 2) + #gf > 2
    geom_point( data = ma_plus2, aes(date, ma), color = blue_comp, size = 3, alpha = 0.3) + #ma_gf > 2
    scale_y_continuous(
      name = "Growth Factor"
      , limits = c(0, 2)
      , breaks = c(0, 1, 2)
      , labels = c("0", "1", "2")
      , oob = scales::squish
    ) +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    theme_minimal() 
  
}


gf_plot <- function(plot_data, min_date, max_date){
  
  gf_plus2 <- plot_data %>%
    filter(gf > 2 )
  
  
  ggplot(plot_data, aes(date, gf)) + 
    annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.2) +
    geom_area(alpha = 0.3) + #raw gf
    geom_point( data = gf_plus2, aes(date, gf), color = "grey35", size = 2) + #gf > 2
    scale_y_continuous(
      name = "Growth Factor"
      , limits = c(0, 2)
      , breaks = c(0, 1, 2)
      , labels = c("0", "1", "2")
      , oob = scales::squish
    ) +
    scale_x_date(
      name = NULL
      , labels = scales::date_format("%b-%d")
      , expand = c(0, 0)
    ) + 
    theme_minimal() 
  
}