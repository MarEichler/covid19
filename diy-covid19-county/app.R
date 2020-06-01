library(shiny)
library(tidyverse)
library(knitr)

source("func_plot.R")

## DATA
load("county_week.rda")

counties_sf <- get_urbn_map("counties", sf = TRUE) %>%
    mutate(county_fips = as.numeric(county_fips))

state_sf <- get_urbn_map("states", sf = TRUE) 


total_weeks <- length(unique(county_week$week))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: Growth Factor per Day"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right", 
        sidebarPanel(
        
    #    helpText("Build your own growth factor plot using the variables below."), 
        
        sliderInput("week", "Week Number"
                    , value = total_weeks, min = 1, max = total_weeks, step = 1
                    , animate = animationOptions(interval = 2000, loop = TRUE)
                    ) #end of slider input
        
        ), #end of side par PANEL 

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
           
        ) #end main panel 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$Plot <- renderPlot({
        
        plot_week <- input$week
        
        plot_data <- county_week %>%
            filter(week == plot_week)
        
        
        county_map(plot_data)
        
    }) # end render output 
    

}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp('diy-covid19-plots', account = 'mareichler')