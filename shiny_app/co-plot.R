library(textplot)
library(shiny)
library(tidyverse)
library(tidytext)

data<-read.csv("cleaned_data/data.csv")

ui <- fluidPage(
  titlePanel("Exploratory Data Analysis"),
    
  tabsetPanel(
     tabPanel("Tweet", plotOutput("wordcloud_tweet")),
  )
)

server <- function(input, output, session) {
  output$wordcloud_tweet<-renderPlot({
    x<-data %>% 
      unnest_tokens(word,cleaned)
    
    textplot_cooccurrence(x,
                          title = "Re-tweet Words Co-occurrences", top_n = 100)
  })
}

shinyApp(ui = ui,server = server)
