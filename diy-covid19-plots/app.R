library(shiny)
library(tidyverse)
library(knitr)
library(zoo)
library(cowplot)
library(scales)

#script
source("func_plot.R")

#data
load("covid19_state.rda")
load("covid19_US.rda")


us_all <- covid19_US %>%
    mutate(state = "Entire US") %>%
    select(state, date, gf = growth_factor, nc = new_cases, nd = new_deaths)

data <- covid19_state %>%
    select(state, date, gf, nc, nd) %>%
    mutate(state = as.character(state)) %>%
    rbind(., us_all)




state_pc <- unique(data$state)
names(state_pc) <- unique(data$state)

x_min <- min(data$date)
x_max <- max(data$date)



state_pc <- unique(data$state)

state_names <- state.name[match(state_pc, state.abb)]
state_names[8] <- "District of Columbia (DC)"
state_names[52] <- "Entire USA"

names(state_pc) <- state_names



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19"),

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
                       start = as.Date("2020-03-15"),  end = x_max,
                       min = x_min, max = x_max
                       ), #end of date range input 
        
        #yes/no show moving average
        checkboxInput("show_ma", "Show Moving-Average", value = TRUE), 
        
        #k for moving average 
        numericInput("ma_k", "Moving Average k (i.e. number of days for moving average)", value = 7), 
        
        #upper bound for gf plot
        numericInput("gf_max", "Upper Bound for Growth Factor Plot", value = 1.5, min = 1.01), 
        
        #upper bound for gf plot
        numericInput("gf_min", "Lower Bound for Growth Factor Plot", value = 0.75, min = 0, max = 0.95) 
        
        ), #end of side par PANEL 

        # Show a plot of the generated distribution
        mainPanel(
          # helpText("Points Represent a growth factor or moving average greater than 2 on a given day"), 
           plotOutput("Plot", height = 600),
           br(), 
           tableOutput("Table"), 
           br(), 
           h4("Data Limitations"),
           p(
             "A large limitation for this data is that reported new cases (and thus the growth factor) 
             may not consistently and accurately represent the true number of new cases each day.  
             As mentioned before, this could be due to test availability, reporting protocols, and a number of other variables.
             It is important to note that this information is a helpful tool in trying to understand the pandemic, 
             but it may not reflect the entire story.  "
           ), 
           br(),
           h4("Data Source"),
           p(
             "This data is downloaded from",
             a("USA Facts.", href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/"), 
            "I use two of the three datasets available: total cases and total deaths.
            Both of these datasets are broken down by state and county.  
            This data requires additional formatting, calculation, and aggregation.  
            USA Facts gets data by county on a daily basis, 
            this is totaled to get values for each day for individual states and the entire US. "
           )
           
        ) #end main panel 
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$Plot <- renderPlot({
        
        gf_max <- input$gf_max
        gf_min <- input$gf_min
        
        ma_k <- input$ma_k
        
        min_date <- input$dates[1]
        max_date <- input$dates[2]
        
        plot_data <- data %>%
            filter(state == input$state) %>%
            mutate(
                ma_nc = c(rep(NA, ma_k - 1), rollmean(nc, k=ma_k, na.rm=TRUE))
              , ma_nd = c(rep(NA, ma_k - 1), rollmean(nd, k=ma_k, na.rm=TRUE))
              , ma_gf = c(rep(NA, ma_k - 1), rollmean(gf, k=ma_k, na.rm=TRUE))
            ) %>%
            filter(date >= min_date & date <= max_date) 
            
        
       if (input$show_ma == TRUE) {plot_ma(plot_data, min_date, max_date, gf_min, gf_max) } 
       else {plot(plot_data, min_date, max_date, gf_min, gf_max)}
        
    }) # end render output PLOT
    
    output$Table <- renderTable({
        
        
        table <- data %>%
            filter(state == input$state) %>%
            summarize(
                 `Total Cases`  = formatC(sum(nc), big.mark=",", format="d")
                , `Total Deaths` = formatC(sum(nd), big.mark=",", format="d")
            ) 
        
        table
        
    }, align = c("rr"))  #end of output table 
    

}

# Run the application 
shinyApp(ui = ui, server = server)


# rsconnect::deployApp('diy-covid19-plots', account = 'mareichler')