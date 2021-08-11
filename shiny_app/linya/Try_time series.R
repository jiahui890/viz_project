library(shiny)

# ui.R

ui <- shinyUI(fluidPage(
  title = 'Initial run of time range update breaks sliderInput',
  fluidRow(
    column(width = 12, 
           sliderInput("timeRange", label = "Time range",
                       min = as.POSIXct("2014-01-23 17:00:00"),
                       max = as.POSIXct("2014-01-23 21:35:00"),
                       value = c(
                         as.POSIXct("2014-01-23 17:00:00"),
                         as.POSIXct("2014-01-23 21:30:00")
                       )),
           fluidRow(
             column(4, verbatimTextOutput("range2")),
             column(4, verbatimTextOutput("range"))
           )
           
    )
  )))

server <- shinyServer(function(input, output, session) {
  #output$range <- renderText(input$timeRange[1]);
  #output$range2 <- renderText(input$timeRange[2]);
  output$range <- renderPrint({ data$timestamp>=ymd_hms(input$timeRange[1])+ hours(8)&data$timestamp<=ymd_hms(input$timeRange[2])+ hours(8) });
  output$range2 <- renderPrint({ ymd_hms(input$timeRange[2])+ hours(8) })
  #})
})

shinyApp(ui = ui,server = server)




