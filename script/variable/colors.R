library(RColorBrewer)
library(colorspace)

gf0     <- "grey80"
gf0_1   <- brewer.pal(5, "YlOrRd")[2]
gf1_2   <- brewer.pal(5, "YlOrRd")[3]
gf2plus <- brewer.pal(5, "YlOrRd")[4]

blue_comp <- "#2085f0"


state_abbrv_color <- "grey20"

line_state <- "grey20"


color_palette <- list(
    "0"   = gf0
  , "0-1" = gf0_1
  , "1-2" = gf1_2
  , "2+"  = gf2plus
)
