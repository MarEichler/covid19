

#var <- "test_total_PC_100k"

HEX_MAP_FUNC <- function(var){

if ( str_detect(var, "death_MA7") == TRUE ) {
  dig = 1
  acc = 0.1
} else {
  dig = 0
  acc=  1
}

data_date <- unique(hex_data$date)

  title <- title_df %>%
    filter(var_name == var) %>%
    pull(var_title)
  
  subtitle <- paste0("National US value: ", scales::comma(round(pull(USA_data, var), dig), acc))
  
  
  ggplot(data = hex_data, aes(x = long, y = lat, group = group, fill = pull(hex_data, var))) + 
    geom_polygon() +
    geom_polygon( color = "white" , size = 1, show.legend = FALSE) +
    scale_fill_gradient(
        name = NULL
      , labels = scales::comma
    ) +
    geom_text(
      data = hex_labels
      , aes(x=x, y=y+0.55, label=id)
      , color = "white"
      , fontface = "bold"
    ) +
    geom_text(
      data = hex_labels
      , aes(x=x, y=y-0.7, label=scales::comma(round(pull(hex_labels, var), dig), accuracy = acc))
      , color = "white"
      , size = 2.5
    )+
    theme_void() +
    coord_map() +
    labs(
      title = title
      , subtitle = subtitle
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
      , plot.subtitle = element_text(hjust = 0.5, size = 12)
      , legend.spacing.x = unit(0, 'cm')
      , legend.margin=margin(t = .5, unit='cm')
    )

} #end function 