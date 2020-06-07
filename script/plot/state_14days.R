library(tidyverse)
library(forcats)
library(ggmap)
library(statebins)
library(sf)
library(lubridate)
library(geofacet) #https://hafen.github.io/geofacet/
library(moderndive)
library(cowplot) 
library(png)

# https://github.com/clauswilke/dataviz/blob/master/geospatial_data.Rmd

load("data/covid19_state_ndays.rda")



adjust_labels <- as_labeller(
  function(x) {
    case_when(
      x == "New Hampshire" ~ "N. Hampshire",
      x == "District of Columbia" ~ "DC",
      TRUE ~ x
    )
  }
)


facet_data <- covid19_state_ndays 

plus3 <- facet_data %>%
  filter(gf > 2) 


x_min_recent <- min(facet_data$date)
x_max_recent <- max(facet_data$date)
ndays <- length(unique(facet_data$date))
source("script/variable/colors.R")

caption <- "Points represent a growth factor greater than 2 on a given day."


title <- paste("Daily Growth Rate over last", ndays, "days")
subtitle <- paste(x_min_recent, "to", x_max_recent)

facet_map <- ggplot(facet_data, aes(date, gf)) +
  annotate("rect", xmin = x_min_recent, xmax = x_max_recent, ymin =  1, ymax =  Inf,   fill = gf2plus,   alpha = 0.45) +
  geom_line( color = "grey35") +
  geom_area(color = "grey35" , alpha = 0.6) +
  geom_point( data = plus3, aes(date, gf), color = "grey35", size = 1.5) +
  scale_y_continuous(
      name = "Growth Factor"
    , limits = c(0, 2)
    , expand = c(0, 0)
    , breaks = c(0, 1, 2)
    , labels = c("0", "1", "2")
    , oob = scales::squish
  ) +
  scale_x_date(
      name = NULL
    , expand = c(0, 0)
    , breaks = NULL
  ) +
  coord_cartesian(clip = "off") +
  facet_geo(~state, grid = "us_state_grid2" , labeller = adjust_labels) +
  labs(
    title = title
    , subtitle = subtitle
  ) +
  theme_minimal() + 
  theme(
      strip.text = element_text(margin = margin(3, 3, 3, 3), face = "bold")
    , panel.spacing.x = grid::unit(5, "pt")
    , panel.spacing.y = grid::unit(5, "pt")
    , panel.grid = element_blank()
    , axis.text = element_text(size = 7)
    , axis.title.x = element_text(margin = margin(20, 0, 0, 0))
    , axis.ticks.length.y = unit(5, "pt")
    , axis.ticks = element_line(color = "grey75")
    , plot.title = element_text(face = "bold", hjust = 0.5)
    , plot.subtitle = element_text(hjust = 0.5, size = 12)
  ) 


ggsave("img/facet_map.png", plot = facet_map, width = 7, height = 5, units = c("in"), dpi = 300)


#cant add annotation to plot using gLabel (bug for geo_facet)
#save the image; import image and add label and then resave 


img <- readPNG("img/facet_map.png")

#get size
h<-dim(img)[1]
w<-dim(img)[2]

#open new file for output
png("img/facet_map.png", width=w, height=h)
par(mar=c(0,0,0,0), xpd=NA, mgp=c(0,0,0), oma=c(0,0,0,0), ann=F)
plot.new()
plot.window(0:1, 0:1)

#fill plot with image
usr<-par("usr")    
rasterImage(img, usr[1], usr[3], usr[2], usr[4])

#add text
text(.4,.8, caption, cex=3, col=rgb(0,0,0,.65))

#close image
dev.off()


