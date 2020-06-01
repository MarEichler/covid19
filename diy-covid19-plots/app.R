library(shiny)
library(tidyverse)
library(knitr)
library(zoo)
library(cowplot)
library(scales)
load("covid19_state.rda")
load("covid19_US.rda")
source("func_plot.R")


us_all <- covid19_US %>%
    mutate(state = "Entire US") %>%
    select(state, date, gf = growth_factor, nc = new_cases)

data <- rbind(covid19_state, us_all)

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



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19: Growth Factor per Day"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        position = "right", 
        sidebarPanel(
        
        helpText("Build your own growth factor plot using the variables below."), 
    
        #input state 
        selectInput("state", label = "Select State", 
                    choices = state_pc #data$state
                    , selected = "Entire US"
                    ),  # end of select input 
        #input date 
        dateRangeInput("dates", label = "Select Date Range",
                       start = x_max - 28,  end = x_max,
                       min = x_min, max = x_max
                       ), #end of date range input 
        
        #yes/no show moving average
        checkboxInput("show_ma", "Show Moving-Average", value = TRUE), 
        
        #k for moving average 
        numericInput("ma_k", "Moving Average k (i.e. number of days for moving average)", value =14)
        
        ), #end of side par PANEL 

        # Show a plot of the generated distribution
        mainPanel(
            p("Recall that the growth factor = new cases today / new cases yesterday."),
            "Points Represent a growth factor or moving average greater than 2 on a given day", 
           plotOutput("Plot"),
           br(), 
           p("Large variations in growth factor may be due to other variables such as testing capabilities") 
           
        ) #end main panel 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$Plot <- renderPlot({
        
        ma_k <- input$ma_k
        
        min_date <- input$dates[1]
        max_date <- input$dates[2]
        
        plot_data <- data %>%
            filter(state == input$state) %>%
            mutate(
                ma_gf = c(rep(NA, ma_k - 1), rollmean(gf, k=ma_k, na.rm=TRUE))
                , ma_nc = c(rep(NA, ma_k - 1), rollmean(nc, k=ma_k, na.rm=TRUE))
            ) %>%
            filter(date >= min_date & date <= max_date) %>%
            mutate(
                gf_2plus = ifelse(gf > 2, 2, -1), 
                ma_2plus = ifelse(ma_gf > 2, 2, -1)
            )
            
        
       if (input$show_ma == TRUE) {gf_plot_ma(plot_data, min_date, max_date) } 
       else {gf_plot(plot_data, min_date, max_date)}
        
    }) # end render output 
    

}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp('diy-covid19-plots', account = 'mareichler')