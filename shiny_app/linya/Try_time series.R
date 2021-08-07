library(shiny)

# ui.R

ui <- shinyUI(fluidPage(
  title = 'Initial run of time range update breaks sliderInput',
  fluidRow(
    column(width = 12, 
           sliderInput("timeRange", label = "Time range",
                       min = as.POSIXct("2014-01-23 17:00:00",tz = "GMT"),
                       max = as.POSIXct("2014-01-23 21:35:00",tz = "GMT"),
                       value = c(
                         as.POSIXct("2014-01-23 17:00:00",tz = "GMT"),
                         as.POSIXct("2014-01-23 21:30:00",tz = "GMT")
                       )),
           fluidRow(
             column(4, verbatimTextOutput("range")),
             column(4, verbatimTextOutput("range2"))
           )
           
    )
  )))

server <- shinyServer(function(input, output, session) {
  output$from <- renderText(input$timeRange[1]);
  output$to <- renderText(input$timeRange[2]);
  output$range <- renderPrint({ data$timestamp>=ymd_hms(input$timeRange[1])&data$timestamp<=ymd_hms(input$timeRange[2]) });
  output$range2 <- renderPrint({ ymd_hms(input$timeRange[2],tz = "GMT") })
  })


shinyApp(ui = ui,server = server)
