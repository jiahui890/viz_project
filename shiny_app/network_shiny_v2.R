library(shiny)
library(visNetwork)
library(plotly)
library(tidyverse)
library(lubridate)
library(tidygraph)
library(igraph)
library(dplyr)


data<-read.csv("cleaned_data/data.csv")

ui <- fluidPage(
  titlePanel("Retweet Network Analysis"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      sliderInput("timeRange", label = "Time range",
                  min = as.POSIXct("2014-01-23 17:00:00"),
                  max = as.POSIXct("2014-01-23 21:35:00"),
                  value = c(
                    as.POSIXct("2014-01-23 17:00:00"),
                    as.POSIXct("2014-01-23 21:35:00")
                  )),
      
      selectInput("var1", "Select Centrality Measurement:",
                 c("In Degree","Out Degree","Closeness","Eigenvector")),
      "In Degree:",
      br(),
      "Out Degree:",
      br(),
      "Closeness:",
      br(),
      "Eigenvector:",
      br(),
      actionButton("do", "Apply Change")
      
    ),
  mainPanel(
    visNetworkOutput("net"),
    br(),

    plotlyOutput("hist",height = "200px")
    )

))



server <- function(input, output){
  
  model <- reactiveValues(Data=NULL)
  
  observeEvent(input$do,{
    
    RT_edges <- data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))%>% 
      filter(type=='mbdata')%>% filter(RT_from!="") %>% 
      transmute(source_label = author, target_label = as.character(RT_from))
    
    
    
    RT_edges_agg <- RT_edges %>% 
      count(source_label, target_label) %>%
      rename(from = source_label, to = target_label) %>% 
      ungroup
    
    
    RT_source <- distinct(RT_edges, node = RT_edges$source_label)
    RT_target <- distinct(RT_edges, node = as.character(RT_edges$target_label))
    RT_node <- bind_rows(RT_source, RT_target)
    RT_node <- RT_node %>% 
      distinct(node) %>% 
      rename(id = node)
    
    
    RT_graph <- tbl_graph(nodes = RT_node,
                          edges= RT_edges_agg,
                          directed=TRUE)
    
  
    if(input$var1=='In Degree'){
      V(RT_graph)$size <- centr_degree(RT_graph, mode = "in")$res
    }
    else{
      if(input$var1=='Out Degree'){
        V(RT_graph)$size <-centr_degree(RT_graph, mode = "out")$res
      }
    else{
      if(input$var1=='Closeness'){
        V(RT_graph)$size <-centr_clo(RT_graph)$res
    }else{
      V(RT_graph)$size<-centr_eigen(RT_graph)$vector
    }}}
    
    
    nodes <- get.data.frame(RT_graph, what="vertices") 
    nodes$size<-round(nodes$size,3)
    nodes$title<-paste(nodes$id, "<br> Value <br>",nodes$size)
    
    model$net<-visNetwork(nodes, RT_edges_agg, height = "500px", width = "100%") %>%
      visOptions(selectedBy = "size", highlightNearest = list(enabled = T, hover = T), 
                 nodesIdSelection = TRUE)%>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visLayout(randomSeed = 1234) 
    
    # model$hist<-plot_ly(nodes,x=~size,type="histogram")%>%
    #   layout(title = 'Distribution of Centrality of Nodes',
    #          xaxis = list(title = 'size of centrality',
    #                       zeroline = TRUE),
    #         yaxis = list(title = 'number of nodes')
    #                      )
    
    
    model$hist<-ggplotly(
      ggplot_build(
        ggplot(nodes, aes(x=size)) + 
          geom_histogram() +
          theme_minimal())$data[[1]] %>% 
        ggplot(aes(x=factor(x), y = count, text = paste0("range: ",round(xmin, 3), " - ", round(xmax,3)," count:",count))) + 
        geom_bar(stat="identity") + 
        xlab("Centrality Measurement Value")+
        ggtitle("Distribution of Centrality of Nodes")+
        theme(panel.border = element_blank(), 
              panel.background = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), 
              axis.line = element_line(colour = "black"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()),
      tooltip = c("text"))

    },ignoreNULL = F);
  
  
  output$hist<-renderPlotly({
    model$hist
  });

  
  output$net<-renderVisNetwork({
    model$net
  })
    
}


shinyApp(ui = ui, server = server)



