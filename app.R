source("script/variable/setup.R")
source("script/dataframe/covid19_state.R") #run script for covid19 df
source("script/dataframe/covid19_county.R") #run script for covid19 df
source("script/variable/parameters.R") #global parameters


source("script/plot/overview_totals.R")
source("script/plot/overview_dailycases.R")
source("script/plot/overview_fatalities.R")
source("script/plot/gf_longterm.R")
source("script/plot/gf_twoweeks.R")
source("script/plot/state_newcases.R")
source("script/plot/state_twoweeks.R")
source("script/plot/state_twoweeksgf.R")
source("script/plot/nc.R")
source("script/plot/tc.R")


max_modtime <- max(
  file.info("script/variable/setup.R")$mtime
, file.info("script/dataframe/covid19_state.R")$mtime #run script for covid19 df
, file.info("script/dataframe/covid19_county.R")$mtime #run script for covid19 df
, file.info("script/variable/parameters.R")$mtime #global parameters
, file.info("script/plot/overview_totals.R")$mtime
, file.info("script/plot/overview_dailycases.R")$mtime
, file.info("script/plot/overview_fatalities.R")$mtime
, file.info("script/plot/gf_longterm.R")$mtime
, file.info("script/plot/gf_twoweeks.R")$mtime
, file.info("script/plot/state_newcases.R")$mtime
, file.info("script/plot/state_twoweeks.R")$mtime
, file.info("script/plot/state_twoweeksgf.R")$mtime
, file.info("script/plot/nc.R")$mtime
, file.info("script/plot/tc.R")$mtime
, file.info("app.R")$mtime
)


plot_w_perc <- "70%"

library(shiny)

lastmod_date <- max_modtime %>% with_tz(tzone = Sys.timezone()) %>% format("%B, %e %Y %H:%M %Z")
data_date <- covid19 %>% pull(date) %>% max() %>% format("%B, %e %Y")


ui <- fluidPage(
    # section below allows in-line LaTeX via $ in mathjax. 
    # https://stackoverflow.com/questions/54876731/inline-latex-equations-in-shiny-app-with-mathjax
    tags$div(HTML("<script type='text/x-mathjax-config' >
            MathJax.Hub.Config({
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
            });
            </script >
            ")), 
mainPanel(width = 12, 
    titlePanel("Tracking COVID-19 in the United States"),
    tabsetPanel(type = "tabs", selected = 1, 
                #-- TAB1: OVERVIEW  ---------------------------------
                tabPanel( #1
                    align = "center", 
                    value = 1,
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
                    fluidRow(column(width = 8, offset = 2, align = "left",  
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
                    fluidRow(column(width = 8, offset = 2, align = "left", 
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
                #-- TAB2: GROWTH FACTOR   ---------------------------------
                tabPanel( #2
                    align = "center", 
                    value = 2, 
                    title = "Growth Factor",
                    br(), 
                    fluidRow(column(width = 8, offset = 2, align = "left", 
                                    h2("Is the pandemic slowing down?"), 
                                    "One important calcualtion is the growth factor, as outlined in",
                                    tags$a("3Blue1Brown's youtube video on exponential growth.", href = "https://www.youtube.com/watch?v=Kas0tIxDvrg", target = "_blank"), 
                                    "The growth factor is calculated as follows: ", 
                                    withMathJax("$$ \\text{Growth Factor} = \\frac{ \\text{New-Cases}_N}{\\text{New-Cases}_{N-1}} $$"),
                                    "where", withMathJax("$N$"), "is a given day.  Essentialy this is taking the amount of new cases today and dividing them by the amount of new cases yesterday.", 
                                    "The growth factor can be very helpful in determining if the pandemic is slowing. 
                                     If the growth factor is less than 1, this means that the amount of new cases today is less than yesterday. 
                                     Once there are multiple days with a growth factor less than 1 it is a strong sign that the pandemic is slowing down.
                                    ", 
                                    h3("Adjustment to Growth Factor"),
                                    "What if there were 0 cases yesterday? This would make the growth factor undefined. 
                                    This makes it difficult to look at trends. I have adjusted the growth factor so that if the previous day had 0 cases, the current day’s growth factor is equal to the number of new cases:
                                    ",  
                                    withMathJax(
                                     "$$
                                     \\text{Growth Factor} = \\begin{cases}
                                        \\frac{ \\text{New-Cases}_N}{\\text{New-Cases}_{N-1}} & \\text{if } \\text{New-Cases}_{N-1} \\neq 0 
                                        \\\\[1ex]
                                        \\text{New-Cases}_N & \\text{if } \\text{New-Cases}_{N-1} = 0 
                                        \\end{cases}
                                     $$")
                    )), #fluidRow(column())
                    br(), 
                    fluidRow(column(width = 8, offset = 2, align = "left",
                                    h2("Growth Factor Plots"), 
                                    "Similar to the new cases per day, there can be a lot of variability in growth factors  
                                     In order to get a better sense of the trend I am showing a 7-day moving average of the growth factor.  
                                    ", br(), br(),  
                                    "Between mid-April and mid-June the growth factor hovered around 1, showing that although cases were decreasing there was not substantial decrease in growth.  
                                     After states began to re-open there was a dramatic increase in new-cases starting in mid-June.  
                                     Between mid-June and mid-July cases started increasing dramatically which lead lead to the 7-day moving average growth factor to be above 1 for a month.  
                                     From mid-July  to October the 7-day moving average growth factor has oscillating around 1.  
                                     Starting in the Fall with the dramatic increase of new cases, the 7-day moving average growth factor has been consistently above 1 during the massive third wave.
                                     In February 2021 as new cases decreased the growth factor was consistently below 1. 
                                    "
                    )), #fluidRow(column())
                    br(), 
                    fluidRow(imageOutput("PLOTgf_longterm", height = "100%")),
                    br(), br(), 
                    fluidRow(imageOutput("PLOTgf_twoweeks", height = "100%"))
                ), #tabPanel2
                #-- TAB3: STATE FACET -----------------------
                tabPanel( #3
                    align = "center", 
                    value = 3, 
                    title = "State Trends",
                    br(), 
                    fluidRow(imageOutput("PLOTstate_newcases", height = "100%")),
                    br(), br(), 
                    fluidRow(imageOutput("PLOTstate_twoweeks", height = "100%")),
                    br(), br(), 
                    fluidRow(imageOutput("PLOTstate_twoweeksgf", height = "100%"))
                ), #tabPanel3
                #-- TAB4: NEW CASES  -----------------------
                tabPanel( #4
                    align = "center",
                    value = 4,
                    title = "Current New Cases",
                    br(), 
                    fluidRow(imageOutput("PLOTnc", height = "100%"))
                ), #tabPanel4
                #-- TAB5: NEW CASES  -----------------------
                tabPanel( #5
                    align = "center",
                    value = 5,
                    title = "Total Cases",
                    br(),
                    fluidRow(imageOutput("PLOTtc", height = "100%"))
                ), #tabPanel5
                #-- TAB 00 ---------------------------------
                # tabPanel( #00
                #     title = "TAB",
                #     br(), 
                #     sidebarPanel(sliderInput("bins",
                #                              "Number of bins:",
                #                              min = 1,
                #                              max = 50,
                #                              value = 30)
                #     ),
                #     plotOutput("distPlot")
                # ) #tabPanel 00
                #-- TAB99: DATA INFO  -----------------------
                tabPanel( #99
                    align = "center", 
                    value = 99, 
                    title = "Data Source",
                    br(), 
                    fluidRow(column(width = 8, offset = 2, align = "left", #style = "border: solid 1px black;", 
                                    "COVID-19 cases at the county level are taken from",
                                    tags$a("Johns Hopkins.", href = "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series", target = "_blank"),
                                    "Population numbers are taken from the",
                                    tags$a("US Census State Population Totals", href = "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_1873399417", target = "_blank"),
                                    "and the", 
                                    tags$a("US Census County Population Totals.", href = "https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html", target = "_blank"),
                                    "The projected population for 2019 values were used to calculate per capita measures.", 
                                    br(), 
                                    h4("Plot Dates"),
                                    "Many of the plots have been restricted to show data on March 15, 2020 and after. 
                                     This is when case numbers started to rise and preventative measures started to increase dramatically.
                                    ",
                                    h4("Data Limitations"), 
                                    "A large limitation for this data is that reported new cases, and thus the growth factor, may not consistently and accurately represent the true number of new cases each day. 
                                     As mentioned before, this could be due to test availability, reporting protocols, and a number of other variables. 
                                     It is important to note that this information is a helpful tool in trying to understand the pandemic, but it may not reflect the entire story.
                                    "
                    )), #fluidRow(column())
                ) #tabPanel99
    ), #tabsetPanel
    #-- FOOTER ROW ---------------------------------
    br(), br(), 
    fluidRow(
        column(
            width = 12, 
            align = "center", 
            div(paste0("JH data as of ", data_date)), 
            div(tags$a("Code", href = "https://github.com/MarEichler/covid19", target = "_blank"), paste0("last updated ", lastmod_date)),
            #div(paste0("Code last updated ", lastmod_date)), 
            br(), br()
        ) #end column
    ) #end fluidRow
) #mainPanel
) #fluidPage



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #-- TAB1: OVERVIEW -----------------------
    output$PLOToverview_totals <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOToverview_totals_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOToverview_totals, width = default_w, height = default_h*0.6)
        list( src = normalizePath(outfile)
            , width = out_w 
            , contentType = "image/jpg"
            #, alt = "alttext"
            )
    }, deleteFile = TRUE)
    
    output$PLOToverview_dailycases <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOToverview_dailycases_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOToverview_dailycases, width = default_w, height = default_h*0.8)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    output$PLOToverview_fatalities <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOToverview_fatalities_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOToverview_fatalities, width = default_w, height = default_h*0.8)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    #-- TAB2: GROTWTH FACTOR  -----------------------
    output$PLOTgf_longterm<- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTgf_longterm_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTgf_longterm, width = default_w, height = default_h)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    output$PLOTgf_twoweeks<- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTgf_twoweeks_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTgf_twoweeks, width = default_w, height = default_h)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    #-- TAB3: STATE FACET -----------------------
    output$PLOTstate_newcases<- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTstate_newcases_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTstate_newcases, width = default_w, height = default_h*1.15)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    output$PLOTstate_twoweeks<- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTstate_twoweeks_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTstate_twoweeks, width = default_w, height = default_h*1.15)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    output$PLOTstate_twoweeksgf<- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTstate_twoweeksgf_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTstate_twoweeksgf, width = default_w, height = default_h*1.15)
        list( src = normalizePath(outfile)
              , width = out_w 
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    
    #-- TAB4: NEW CASES -----------------------
    output$PLOTnc <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTnc_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTnc, width = default_w, height = default_h*2.4)
        list( src = normalizePath(outfile)
              , width = out_w
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)

    #-- TAB5: TOTAL CASES -----------------------
    output$PLOTtc <- renderImage({
        out_w <- ifelse(session$clientData$output_PLOTtc_width <= 1000, "100%", plot_w_perc)
        outfile <- tempfile(fileext = ".jpg")
        ggsave(file = outfile, plot = PLOTtc, width = default_w, height = default_h*2.4)
        list( src = normalizePath(outfile)
              , width = out_w
              , contentType = "image/jpg"
              #, alt = "alttext"
        )
    }, deleteFile = TRUE)
    
    #-- TAB00 -----------------------
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
} #end of server 

# Run the application 
shinyApp(ui = ui, server = server)
