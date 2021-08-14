

library(shiny)
library(topicmodels)
library(DT)
library(tidytext)
library(tm)
library(plotly)
library(dplyr)
library(lubridate)
library(stringr)


data<-read.csv("cleaned_data/data.csv")

ui <- fluidPage(
  titlePanel("LDA Topic Modeling"),
  br(),

  sidebarLayout(

    sidebarPanel(width = 3,
                 h2(strong("Filter Panel"), style = "font-size:20px;"),
      sliderInput("timeRange", label = "Select Time Range",
                  min = as.POSIXct("2014-01-23 17:00:00"),
                  max = as.POSIXct("2014-01-23 21:35:00"),
                  value = c(
                    as.POSIXct("2014-01-23 17:00:00"),
                    as.POSIXct("2014-01-23 21:35:00")
                  )),
      numericInput("k", "Number of Topics:", 10, min = 2, max = 20),
      numericInput("alpha", "Alpha - hyperparameter for topic proportions:", 0.01, min = 0.0000001, max = 1),
      numericInput("iter", "Number of Iterations:", 200, min = 50, max = 500),
      numericInput("topn", "Top n words in topic:", 10, min = 1, max = 20),
      actionButton("do", strong("Apply Change"))

    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Topic-Word Probability", plotOutput("plot1")), 
      tabPanel("Topic Trend", plotlyOutput("plot2")),
      tabPanel("User Engagement", 
               fluidRow(textInput("topicn","Input Topic Index Here (split by comma)")),
               # splitLayout(
               #             plotOutput("plot3"),
               #             DT::dataTableOutput("table"))
               fluidRow(
                 column(6,plotOutput("plot3")),
                 column(6,DT::dataTableOutput("table"))
               
               )
    )
)
)
))




server <- function(input, output) {
  model <- reactiveValues(Data=NULL)
  
  observeEvent(input$do, {
    
    mbdata <- data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))%>% 
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
    
    topic=LDA(dtm.new,k=input$k,method="Gibbs",conrol=list(seed=2021,alpha=input$alpha,iter=input$iter))
    
    
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
      scale_y_reordered()+theme_light()+ggtitle("LDA Topic-Word Beta Probabilities")
    
    topic_gamma <- tidy(topic, matrix = "gamma")
    topic_gamma <- topic_gamma %>% 
      group_by(document) %>% 
      slice(which.max(gamma))
    
    topic_gamma$document<-as.numeric(topic_gamma$document)
    
    
    id_time <- mbdata %>% select(c("id","time_1min","message","author","timestamp"))
    
    topic_data<-left_join(topic_gamma,id_time,by=c("document"="id"))
    
    model$data<-topic_data
    
    p1<-topic_data %>% group_by(time_1min,topic) %>% count() %>% ungroup() %>% 
      mutate(time=time_1min+hours(16))
    
    p2<-ggplot(p1,aes(x=time))+
      geom_bar(aes(y=n), stat = "identity",fill = "black")+
      ggtitle("LDA Topics Trend of Selected Time Period")+
      theme_minimal()+
      facet_wrap(~topic)
    
    model$plot2<- ggplotly(p2)
    
    
    # model$plot2<-topic_data %>% group_by(time_1min,topic) %>% count() %>% 
    #   ggplot(aes(x=time_1min))+
    #   geom_bar(aes(y=n), stat = "identity",fill = "black")+
    #   facet_wrap(~topic)+
    #   theme(axis.title.x=element_blank(),
    #         axis.text.x=element_blank(),
    #         axis.ticks.x=element_blank())+
    #   ggtitle("LDA Topics Trend of Selected Time Period")
    
    
    
  },ignoreNULL = F);
  
  
  
  
  output$plot1 <- renderPlot({
    model$plot1
  },height = 800);
  
  output$plot2 <- renderPlotly({
    model$plot2
  });
  
  
  output$plot3<-renderPlot({
    
    validate(
      #need(input$topicn!="", 'Please input valid topic number'),
      need(str_detect(input$topicn,"[0-9]+((,[0-9]+)+)?"), 'Please input valid number')
    )
    
    topic_data<-model$data
    topic1<-topic_data %>% group_by(topic,author) %>% summarise(topics_count=n()) %>% ungroup()
    topic2<-topic_data %>% group_by(author) %>% summarise(total_count=n())%>% ungroup()
    topic3<-left_join(topic1,topic2,by=c("author"="author"))
    topic3$User_Topic_Ratio<-topic3$topics_count/topic3$total_count
    
    selected_topic<-as.integer(str_split(input$topicn,",")[[1]])
    topic4<-topic3 %>% filter(topic %in% selected_topic)
    
    ggplot(topic4,aes(x=topic,y=author,color=factor(topic),size=User_Topic_Ratio))+
      geom_point(alpha=0.5)+
      theme(legend.position="bottom")+
      scale_x_discrete()+
      ggtitle("User Engagement %")
    
  },height = 1600, width = 300);
  
  output$table<-DT::renderDataTable({
    
    selected_topic<-as.integer(str_split(input$topicn,",")[[1]])
    
    
    topic5<-subset(model$data,select = c(timestamp,author,message,topic)) %>%
      filter(topic %in% selected_topic)
    
    
    datatable(data =topic5)
    
  });
  
   
  
}


shinyApp(ui = ui, server = server)