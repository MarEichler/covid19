source("script/variable/setup.R")
source("script/dataframe/covid19_state.R") #run script for covid19 df
# source("script/dataframe/covid19_county.R") #run script for covid19 df
source("script/variable/parameters.R") #global parameters


source("script/plot/overview_totals.R")
source("script/plot/overview_dailycases.R")
source("script/plot/overview_fatalities.R")

plot_w_perc <- "70%"

library(shiny)

lastmod_date <- file.info("app.R")$mtime %>% with_tz(tzone = "America/Los_Angeles") %>% format("%B, %e %Y %H:%M %Z")
data_date <- covid19 %>% pull(date) %>% max() %>% format("%B, %e %Y")

ui <- fluidPage(
mainPanel(width = 12, 
    titlePanel("COVID-19 Tracking"),
    tabsetPanel(type = "tabs",
                #-- TAB1: OVERVIEW  ---------------------------------
                tabPanel( #1
                    align = "center", 
                    title = "Overview",
                    br(), 
                    fluidRow(column(width = 8, offset = 2, align = "left", #style = "border: solid 1px black;", 
                                    h2("Total Cases and Total Dealths"), 
                                    "These show the cumulative total of cases and deaths by day.  
                                     I have also denoted the current cumulative totals.  
                                     These total values are important; however they are not helpful for figuring out whether the pandemic is slowing down or growing as it is difficult to see trends in cumulative curves like these.
                                    "
                                    )), #fluidRow(column())
                    br(), 
                    fluidRow(imageOutput("PLOToverview_totals", height = "100%")), 
                    br(), 
                    fluidRow(column(width = 8, offset = 2, align = "left", #style = "border: solid 1px black;", 
                                    h2("New Cases"), 
                                    "Looking at new cases each day can help us see if the pandemic is slowing.  
                                     A decreasing number of new cases per day is evidence that the pandemic is slowing down.
                                    ", br(), br(),  
                                    "There can be a lot of variability in the daily case totals due to a variety of variables.  
                                      One example is the availability of tests; cases will go down if there is a scarcity of tests and rise dramatically when more tests become available.  
                                      There is also a cyclical nature to the daily new cases with counts often being lower on weekends and higher on weekdays. 
                                      One way to help get a better sense of the overall trend is by smoothing the data using a moving average.  
                                    "
                    )), #fluidRow(column())
                    br(), 
                    fluidRow(imageOutput("PLOToverview_dailycases", height = "100%")),
                    br(), 
                    fluidRow(column(width = 8, offset = 2, align = "left", #style = "border: solid 1px black;", 
                                    h2("New Deaths and Case Fatality"), 
                                    "COVID-19 is much deadlier than the common flu.  
                                     One way to measure the impact is to look at the case fatality percentage, which is the total number of deaths divided by the total number of cases. 
                                    ", br(), br(),  
                                    "Similar to new cases there is a cyclical nature to daily deaths.  
                                     This may be due to reporting times where counts on weekdays are often higher than those on weekends.  
                                      Other variability in the data can also occurs when states change how they are assigning COVID-19 deaths, such as counting nursing home deaths and/or pneumonia deaths as COVID-19 deaths.
                                    "
                    )), #fluidRow(column())
                    br(), 
                    fluidRow(imageOutput("PLOToverview_fatalities", height = "100%"))
                ), #tabPanel1  
                #-- TAB2 ---------------------------------
                tabPanel( #2
                    title = "TAB",
                    br(), 
                    sidebarPanel(sliderInput("bins",
                                             "Number of bins:",
                                             min = 1,
                                             max = 50,
                                             value = 30)
                    ),
                    plotOutput("distPlot")
                ) #tabPanel2
    ), #tabsetPanel
    #-- FOOTER ROW ---------------------------------
    br(), br(), 
    fluidRow(
        column(
            width = 12, 
            align = "center", 
            div(paste0("JH data as of ", data_date)), 
            div(paste0("Code last updated ", lastmod_date)), 
            br(), br()
        ) #end column
    ) #end fluidRow
) #mainPanel
) #fluidPage

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    output$PLOToverview_totals <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOToverview_totals_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOToverview_totals, width = default_w, height = 3)
        list( src = normalizePath(outfile)
            , width = out_w 
            , contentType = "image/jpg"
            #, alt = "alttext"
            )
    }, deleteFile = TRUE)
    
    output$PLOToverview_dailycases <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOToverview_dailycases_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOToverview_dailycases, width = default_w, height = default_h)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    output$PLOToverview_fatalities <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOToverview_fatalities_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOToverview_fatalities, width = default_w, height = default_h)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
} #end of server 

# Run the application 
shinyApp(ui = ui, server = server)
