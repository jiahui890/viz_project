library(shiny)



ui <- fluidPage(
  titlePanel("Input for LDA Modeling"),
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput("timeRange", label = "Time range",
                  min = as.POSIXct("2014-01-23 17:00:00",tz = "UTC"),
                  max = as.POSIXct("2014-01-23 21:35:00",tz = "UTC"),
                  value = c(
                    as.POSIXct("2014-01-23 17:00:00",tz = "UTC"),
                    as.POSIXct("2014-01-23 21:35:00",tz = "UTC")
                  )),
      numericInput("k", "Number of Topics:", 10, min = 2, max = 20),
      numericInput("iter", "Number of Iterations:", 200, min = 50, max = 500),
      numericInput("topn", "Top n words in topic:", 10, min = 1, max = 20),
      numericInput("topuser", "Top n users in topic:", 5, min = 1, max = 20),
      actionButton("do", "Apply Change")

    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Top words in topic", plotOutput("plot1")), 
      tabPanel("Topic Trend during selected time period", plotOutput("plot2")),
      tabPanel("User Engagement", plotOutput("plot3"),
               DT::dataTableOutput("table"))
    )
)
)
)


select_init <- subset(topic_data,select = c(timestamp,author,message,topic))%>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .))

server <- function(input, output) {
  
  model <- reactiveValues(Data=NULL)
  
  observeEvent(input$do, {
    
    mdata <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])] %>% 
      filter(type=='mbdata')

    mbdata$id<-seq.int(nrow(mbdata))
    
    wordcorpus <- Corpus(VectorSource(as.character(mbdata$cleaned)))  
    dtm <- DocumentTermMatrix(wordcorpus,
                              control = list(
                                wordLengths=c(2, Inf),               # limit word length
                                bounds = list(global = c(5,Inf)),    # minimum word frequency
                                removeNumbers = TRUE,                #remove Numbers
                                weighting = weightTf,                #weighted term frequency
                                encoding = "UTF-8"))
    
    rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
    dtm.new   <- dtm[rowTotals> 0, ] #remove 0 dtm rows of matrix
    
    topic=LDA(dtm.new,k=input$k,method="Gibbs",conrol=list(seed=2021,alpha=0.01,iter=input$iter))
    

    ap_topics <- tidy(topic, matrix = "beta")
    
    
    ap_top_terms <- ap_topics %>%
      group_by(topic) %>%
      slice_max(beta, n = input$topn) %>% 
      ungroup() %>%
      arrange(topic, -beta)
    
    model$plot1<-ap_top_terms %>%
      mutate(term = reorder_within(term, beta, topic)) %>%
      ggplot(aes(beta, term, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered()
    
    topic_gamma <- tidy(topic, matrix = "gamma")
    topic_gamma <- topic_gamma %>% 
      group_by(document) %>% 
      slice(which.max(gamma))
    
    topic_gamma$document<-as.numeric(topic_gamma$document)
    
    
    id_time <- mbdata %>% select(c("id","time_1min","message","author"))
    
    topic_data<-left_join(topic_gamma,id_time,by=c("document"="id"))%>%
      mutate_if(is.character, ~gsub('[^ -~]', '', .))
    
  
    
    model$plot2<-topic_data %>% group_by(time_1min,topic) %>% count() %>% 
      ggplot(aes(x=time_1min))+
      geom_bar(aes(y=n), stat = "identity",fill = "black")+
      facet_wrap(~topic)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())+
      ggtitle("LDA Topics Trend of Selected Time Period")
    
    
    model$plot3<-topic_data %>% group_by(topic,author) %>% 
      summarize(count=n()) %>% 
      slice_max(count, n = input$topuser) %>% 
      ungroup()%>%
      ggplot(aes(x=reorder(author,count), y=count, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      scale_y_reordered()+
      coord_flip()
    
    
  },ignoreNULL = F);

  
  
  
  output$plot1 <- renderPlot({
    model$plot1
  });
  
  output$plot2 <- renderPlot({
    model$plot2
  });
  
 
  output$plot3 <- renderPlot({
    model$plot3
  });
  
  # output$table <- DT::renderDataTable({
  #   model$selected
  #   });

}


shinyApp(ui = ui, server = server)