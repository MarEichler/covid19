library(colorspace)
library(cowplot)
library(forcats)
library(geofacet) #https://hafen.github.io/geofacet/
library(ggmap)
library(ggrepel)
library(grid)
library(gridExtra)
library(kableExtra)
library(knitr)
library(latex2exp)
library(lubridate)
library(moderndive)
library(scales)
library(sf)
library(statebins)
library(tidyverse)
library(tidyquant)
library(RColorBrewer)
library(zoo)





knitr::opts_chunk$set(fig.width=8, fig.height = 4, dpi=300, echo=FALSE, warning = FALSE, message = FALSE)


#use code from the fundamentals of data vizualization book: 
# https://github.com/clauswilke/dataviz/blob/master/geospatial_data.Rmd


adjust_labels <- as_labeller(
  function(x) {
    case_when(
      x == "New Hampshire" ~ "N. Hampshire",
      x == "District of Columbia" ~ "DC",
      TRUE ~ x
    )
  }
)