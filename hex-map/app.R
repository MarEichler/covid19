source("setup.R")
source("covid19_data.R")
source("hex-map-function.R")

data_date <- max(hex_data$date)

library(shiny)

# Define UI for application that draws a histogram
ui <- fillPage( 

  titlePanel(paste0("COVID-19 Values by Population as of ", data_date)),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Plot",
                 br(),
                 fluidRow(
                   sidebarPanel(
                     selectInput("val", h3("Select Variable"), 
                                 choices = list(
                                   "Cases"  = "case", 
                                   "Deaths" = "death",
                                   "Tests"  = "test"), 
                                 selected = "case"), 
                     radioButtons("type", label = NULL, 
                                  choices = list(
                                    "Total"                      = "_total_PC_100k", 
                                    "Daily"                      = "_new_PC_100k",
                                    "Daily 7-Day Moving Average" = "_MA7_PC_100k"),
                                  selected = "_total_PC_100k")
                   ),  #end sidebarPanel
                   plotOutput("hex_pc_plots", width = "200px")
                 ) #end fluid Row 
        ) #end Plot tabPanel
       # tabPanel("Summary"),
      #  tabPanel("Table")
      ) #end tabsetPanel
    ) #main Panel
) #fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$hex_pc_plots <- renderPlot({
        var <- paste0(input$val, input$type)
        HEX_MAP_FUNC(var)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
