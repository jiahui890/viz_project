library(shiny)
library(plotly)
library(data.table)


ui <- fluidPage(
  plotlyOutput("scatterPlot")

)

server <- function(input, output, session) {
  
  output$scatterPlot <- renderPlotly({
  plot_ly(
      counts,
      x = ~ time_1min,
      y = ~ count,
      type = "scatter",
      mode = "lines",
      marker=list(color=~type, size = 1)
    )
  
  })
}

shinyApp(ui = ui, server = server)