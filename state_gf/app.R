library(tidyverse)
load("gf_state_tidy.rda")
load("covid19_US.rda")


us_all <- covid19_US %>%
    mutate(state = "Entire US") %>%
    select(state, date, gf = growth_factor)

data <- rbind(gf_state_tidy, us_all)

state_pc <- unique(data$state)
names(state_pc) <- unique(data$state)

x_min <- min(data$date)
x_max <- max(data$date)

x_min
#ggplot set ups 
date_scaling <- scale_x_date(
    name = NULL
    , labels = scales::date_format("%b-%d")
    , expand = c(0, 0)
)


state_pc <- unique(data$state)

state_names <- state.name[match(state_pc, state.abb)]
state_names[52] <- "Entire USA"

names(state_pc) <- state_names



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Growth Rate per Day"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right", 
        sidebarPanel(
    
        #input state 
        selectInput("state", label = "Select State", 
                    choices = state_pc #data$state
                    , selected = "Entire US"
                    ),  # end of select input 
        #input date 
        dateRangeInput("dates", label = "Select Date Range",
                       start = "2020-03-15",  end = x_max,
                       min = x_min, max = x_max
                       ) #end of date range input 
        
        ), #end of side par PANEL 

        # Show a plot of the generated distribution
        mainPanel(
            "Points Represent a growth rate greater than 2 on a given day", 
           plotOutput("Plot"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Plot <- renderPlot({
        
        min_date <- input$dates[1]
        max_date <- input$dates[2]
        
        plot_data <- data %>%
            filter(state == input$state) %>%
            filter(date >= min_date & date <= max_date)
        
        plus3 <- plot_data %>%
            filter(gf > 2 )

        ggplot(plot_data, aes(date, gf)) + 
            annotate("rect", xmin = min_date, xmax = max_date, ymin = 1, ymax =  2, fill = "#F03B20", alpha = 0.45) +
            geom_line(alpha=0.3) +
            geom_area(alpha = 0.6) +
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
            geom_point( data = plus3, aes(date, gf), color = "grey35", size = 2) +
            theme_minimal() 
     
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
