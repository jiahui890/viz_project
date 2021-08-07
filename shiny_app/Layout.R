library(shiny)
library(ggplot2)



ui <- fluidPage(
  theme = bslib::bs_theme(),
  navbarPage(
  "Visual Dashboard for Real-Time Analysis of Social Media",
  tabPanel("Introduction", "one"),
  tabPanel("EDA", "two"),
  tabPanel("Topic Modeling", "three"),
  tabPanel("Network Analysis", "four"),
  tabPanel("Geo Analysis", "five")
  )
)

server <- function(input,output){

}

shinyApp(ui=ui, server=server)
