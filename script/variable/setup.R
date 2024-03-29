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
library(DescTools)
library(urbnmapr) #https://github.com/UrbanInstitute/urbnmapr


default_w <- 8
default_h <- 5


knitr::opts_chunk$set(fig.width=default_w, fig.height=default_h, dpi=300, echo=FALSE, warning = FALSE, message = FALSE)


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