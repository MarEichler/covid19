#LOESS CURVE

title_l <- "LOESS Smoothing "

loess_smooth <- stat_smooth(
  geom="line"
  , size = 1.25
  , color = blue_comp
  , se = FALSE 
  , alpha = 0.6
  , method = "loess"
  , formula = y~x
  , span = 0.9
)



plot_dc_l <- plot_dc +
  loess_smooth +
  ggtitle(title_l) 

title_ma <- paste("Moving Average, k =", ma_k)


plot_dc_ma <- plot_dc +
  geom_line(
    aes(y = MA_new_cases)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) +
  ggtitle(title_ma)


grid.arrange(
  plot_dc_l
  , plot_dc_ma
  , nrow = 1
  ,top=textGrob(
    paste("New Cases per Day", sep = "\n")
    , gp=gpar(fontsize=title_size, fontface = "bold")
  )
)



title_l <- "LOESS Smoothing "

loess_smooth <- stat_smooth(
  geom="line"
  , size = 1.25
  , color = blue_comp
  , se = FALSE 
  , alpha = 0.6
  , method = "loess"
  , formula = y~x
  , span = 0.9
)

plot_gf_l  <- plot_gf +
  loess_smooth +
  ggtitle(title_l)

title_ma <- paste("Moving Average, k =", ma_k)


plot_gf_ma  <- plot_gf + 
  geom_line(
    aes(y = MA_growth_factor)
    , size = 1.25
    , color = blue_comp
    , alpha = 0.6
  ) + 
  ggtitle(title_ma)



grid.arrange(
  plot_gf_l
  , plot_gf_ma
  , nrow = 1
  ,top=textGrob(
    paste("Growth Factor per Day", sep = "\n")
    , gp=gpar(fontsize=title_size, fontface = "bold")
  )
)