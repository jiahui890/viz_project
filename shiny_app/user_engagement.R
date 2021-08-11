library(shiny)
library(DT)


ui <- fluidPage(
    textInput("topicn","Input Topic Number Here (split by comma)"),
    fluidRow(
      column(width = 6,plotOutput("plot3")),
      column(width = 6,DT::dataTableOutput("table"))
    )
  )

server <- function(input, output) {
  
  
  
  
  output$plot3<-renderPlot({
    
    validate(
      #need(input$topicn!="", 'Please input valid topic number'),
      need(str_detect(input$topicn,"[0-9]+((,[0-9]+)+)?"), 'Please input valid number')
    )
    

    topic1<-topic_data %>% group_by(topic,author) %>% summarise(topics_count=n()) %>% ungroup()
    topic2<-topic_data %>% group_by(author) %>% summarise(total_count=n())%>% ungroup()
    topic3<-left_join(topic1,topic2,by=c("author"="author"))
    topic3$User_Topic_Ratio<-topic3$topics_count/topic3$total_count

    selected_topic<-as.integer(str_split(input$topicn,",")[[1]])
    topic4<-topic3 %>% filter(topic %in% selected_topic)

    ggplot(topic4,aes(x=topic,y=author,color=factor(topic),size=User_Topic_Ratio))+
      geom_point(alpha=0.5)+
      theme_light()+
      scale_x_discrete()+
      ggtitle("User Engagement in Major Events")

  },height = 1200, width = 600);
  
  output$table<-DT::renderDataTable({
    selected_topic<-as.integer(str_split(input$topicn,",")[[1]])

    topic5<-subset(topic_data,select = c(timestamp,author,message,topic)) %>%
    filter(topic %in% selected_topic)%>% mutate_if(is.character, ~gsub('[^ -~]', '', .))
    
    datatable(data = topic5)

  });
    
}

shinyApp(ui, server)
