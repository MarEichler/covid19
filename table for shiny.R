
p("Growth factor and moving averages are rounded to 2 decimal places."),
tableOutput("Table")







output$Table <- renderTable({
  
  ma_k <- input$ma_k
  
  min_date <- input$dates[1]
  max_date <- input$dates[2]
  
  plot_data <- data %>%
    filter(state == input$state) %>%
    mutate(ma = c(rep(NA, ma_k - 1), rollmean(gf, k=ma_k, na.rm=TRUE))) %>%
    filter(date >= min_date & date <= max_date) %>%
    mutate(
      gf_2plus = ifelse(gf > 2, 2, -1), 
      ma_2plus = ifelse(ma > 2, 2, -1)
    )
  
  
  table_data <- plot_data %>%
    arrange(desc(date)) %>%
    mutate(
      Date = format(date, '%a, %b %d, %Y')
      , `Growth Factor` = round(gf, 2)
      , `Growth Factor Moving Average` = round(ma, 2)
    ) %>%
    mutate(
      `Growth Factor` = cell_spec(
        `Growth Factor`, color = 
          ifelse( gf > 1, gf2plus, colorspace::lighten(gf2plus, 0.4))
        , bold = T
      ), 
      `Growth Factor Moving Average` = cell_spec(
        `Growth Factor Moving Average`, color = 
          ifelse( ma > 1, gf2plus, colorspace::lighten(gf2plus, 0.4))
        , bold = T
      )
    ) %>%
    select(
      Date
      , `Growth Factor`
      , `Growth Factor Moving Average`
    ) 
  
  table_data
  
}, align = c("lrr"), sanitize.text.function = function(x) x) 