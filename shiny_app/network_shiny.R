library(shiny)

ui <- fluidPage(
  titlePanel("Network Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("timeRange", label = "Time range",
                  min = as.POSIXct("2014-01-23 17:00:00",tz = "UTC"),
                  max = as.POSIXct("2014-01-23 21:35:00",tz = "UTC"),
                  value = c(
                    as.POSIXct("2014-01-23 17:00:00",tz = "UTC"),
                    as.POSIXct("2014-01-23 21:35:00",tz = "UTC")
                  )),
      
      selectInput("var1", "Select Centrality Measurement:",
                 c("Degree","Closeness","Eigenvector")),
      actionButton("do", "Apply Change")
      
    ),
  mainPanel(
    visNetworkOutput("net"),
    plotlyOutput("hist",height = "200px")
    )

))



server <- function(input, output){
  
  model <- reactiveValues(Data=NULL)
  
  observeEvent(input$do,{
    
    mdata <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]%>% 
    filter(RT_from!="") %>% 
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
    
  
    if(input$var1=='Degree'){
      V(RT_graph)$size <- centr_degree(RT_graph, mode = "all")$res
    }
    else{
      if(input$var1=='Closeness'){
        V(RT_graph)$size <-centr_clo(RT_graph)$res
    }else{
      V(RT_graph)$size<-centr_eigen(RT_graph)$vector
    }}
    
    
    nodes <- get.data.frame(RT_graph, what="vertices") 
    
    model$net<-visNetwork(nodes, RT_edges_agg, height = "500px", width = "100%") %>%
      visOptions(selectedBy = "size", highlightNearest = list(enabled = T, hover = T), 
                 nodesIdSelection = TRUE)%>% 
      visIgraphLayout(layout = "layout_with_fr") %>% 
      visLayout(randomSeed = 1234) 
    
    model$hist<-plot_ly(nodes,x=~size,type="histogram")

    
    },ignoreNULL = F);
  
  
  output$hist<-renderPlotly({
    model$hist
  });

  
  output$net<-renderVisNetwork({
    model$net
  })
    
}


shinyApp(ui = ui, server = server)



