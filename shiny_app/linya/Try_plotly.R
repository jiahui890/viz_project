library(shiny)
library(plotly)
library(data.table)

nPoints <- 100
DT <- data.table(x = Sys.time()-seq_len(nPoints), y = runif(nPoints, 0, 10))

ui <- fluidPage(
  plotlyOutput("scatterPlot"),
  plotlyOutput("barPlot")
)

server <- function(input, output, session) {
  
  output$scatterPlot <- renderPlotly({
    p_scatter <- plot_ly(
      DT,
      x = ~ x,
      y = ~ y,
      type = "scatter",
      mode = "lines"
    ) %>%
      layout(
        xaxis = list(
          rangeslider = list(type = "date")
        ))
  })
  
  xRangeRaw <- reactiveVal()
  xRange <- xRangeRaw %>% debounce(100)
  
  observeEvent(event_data("plotly_relayout"), {
    xRangeRaw(event_data("plotly_relayout")$xaxis.range)
  })
  
  output$barPlot <- renderPlotly({
    if(is.null(xRange())){
      filteredDT <- DT
    } else {
      filteredDT <- DT[x %between% as.POSIXct(xRange(), format = "%Y-%m-%d %H:%M:%OS")]
    }
    
    p_bar <- plot_ly(
      na.omit(filteredDT),
      x = ~ x,
      y = ~ y,
      type = "bar"
    )
    
  })
}

shinyApp(ui = ui, server = server)