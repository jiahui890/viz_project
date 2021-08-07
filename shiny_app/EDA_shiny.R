library(shiny)
library(plotly)

ui <- fluidPage(
  titlePanel("EDA"),
  fluidRow(
    column(width = 6, 
           sliderInput("timeRange", label = "Time range",
                       min = as.POSIXct("2014-01-23 17:00:00",tz = "UTC"),
                       max = as.POSIXct("2014-01-23 21:35:00",tz = "UTC"),
                       value = c(
                         as.POSIXct("2014-01-23 17:00:00",tz = "UTC"),
                         as.POSIXct("2014-01-23 21:35:00",tz = "UTC")
                       )
           ),
    plotlyOutput("Lineplot1",height = 200),
    plotlyOutput("Lineplot2",height = 200)
    ),
  column(width = 6,
  tabsetPanel(
    tabPanel("wordcloud_tweet", plotOutput("wordcloud_tweet")), 
    tabPanel("hashtag", plotOutput("wordcloud_hashtag")), 
    tabPanel("retweet", plotOutput("wordcloud_retweet")), 
    tabPanel("usermentioned", plotOutput("wordcloud_usermentioned")), 
    tabPanel("cc", plotOutput("wordcloud_cc"))
  )
  )))


server <- function(input, output, session) {
  

  # output$Lineplot1<- renderPlot({
  #   
  #   data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]
  #   data2$time_1min <- cut(data2$timestamp, breaks="1 min")
  #   data2$time_1min <- ymd_hms(data2$time_1min)
  #   
  #   counts <- data2 %>%
  #      group_by(type,time_1min) %>%
  #      summarise(count = n())
  # 
  #   ggplot(counts,
  #          aes(x=time_1min,
  #              y=count,
  #              group=type))+
  #     geom_line(aes(color=type))+
  #     ggtitle("Total Number of Events Over Time")+
  #     xlab("Time") + ylab("Number of Tweets or Calls")+
  #     xlim( min(counts$time_1min), max(counts$time_1min))+
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #     theme(plot.title = element_text(hjust = 0.5))
  # });
  
  output$Lineplot1 <- renderPlotly({
     data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]
     data2$time_1min <- cut(data2$timestamp, breaks="1 min")
     data2$time_1min <- ymd_hms(data2$time_1min)
     
     counts <- data2 %>%
        group_by(type,time_1min) %>%
        summarise(count = n()) 
     
     plot_ly(
       counts,
       x = ~ time_1min,y = ~ count,type = "scatter",mode = "lines",color=~type)%>% 
       layout(title = 'Tweet and Calls Trend',
              xaxis = list(title = 'Selected Time Period'),
              yaxis = list(title = 'Number of Tweets/Calls'))
      
});
  
  # output$Lineplot2<- renderPlot({
  #   
  #   data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]
  #   data2$time_1min <- cut(data2$timestamp, breaks="1 min")
  #   data2$time_1min <- ymd_hms(data2$time_1min)
  #   
  #   data_rt2<-data2 %>% 
  #     group_by(time_1min) %>% 
  #     summarise(post=n(),
  #               rt_post=sum(RT_from!=""))
  #   
  #   data_rt2$time_1min=ymd_hms(data_rt2$time_1min)
  #   
  #   
  #   ggplot(data_rt2,aes(x=time_1min)) +
  #     geom_line(aes(y=post,color="tweet")) +
  #     geom_line(aes(y=rt_post,color="retweet")) +
  #     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  #     ggtitle("Tweet and Re-Tweets Trend")
  # });
  
  output$Lineplot2 <- renderPlotly({
      data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])] %>% 
        filter(type=='mbdata')
      data2$time_1min <- cut(data2$timestamp, breaks="1 min")
      data2$time_1min <- ymd_hms(data2$time_1min)

      data_rt2<-data2 %>%
        group_by(time_1min) %>%
        summarise(post=n(),
                  rt_post=sum(RT_from!=""))

      data_rt2$time_1min=ymd_hms(data_rt2$time_1min)
    
      plot_ly(
        data_rt2,
        x = data_rt2$time_1min,y = ~ post,name='tweet',type = "scatter",mode = "lines",
        line = list(color = 'rgb(22, 96, 167)'))%>%
        add_trace(y = ~rt_post, name = 'retweet', line = list(color = 'rgb(205, 12, 24)'))%>% 
        layout(title = 'Tweet and Retweet Trend',
               xaxis = list(title = 'Selected Time Period'),
               yaxis = list(title = 'Number of Posts'))
  });
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordcloud_tweet<- renderPlot({
    data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])] %>% 
      filter(type=='mbdata')
    docs <- Corpus(VectorSource(as.character(data2$cleaned)))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    wordcloud(words = df$word, freq = df$freq, min.freq = 5,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
    });
  
  
  output$wordcloud_cc<- renderPlot({
    data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])] %>% 
      filter(type=='ccdata')
    docs <- Corpus(VectorSource(as.character(data2$cleaned)))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    wordcloud(words = df$word, freq = df$freq, min.freq = 5,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  });    
    
    output$wordcloud_retweet<- renderPlot({
      data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]
      docs <- Corpus(VectorSource(as.character(data2$RT_message_cleaned)))
      dtm <- TermDocumentMatrix(docs) 
      matrix <- as.matrix(dtm) 
      words <- sort(rowSums(matrix),decreasing=TRUE) 
      df <- data.frame(word = names(words),freq=words)
      wordcloud(words = df$word, freq = df$freq, min.freq = 5,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
      });
  
      
      
      
      output$wordcloud_hashtag<- renderPlot({
        data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]
        
        hashtags.df <- tibble(
          Hashtags = data2$message %>% map_chr(.f = ~ GetHashtags(tweet = .x))
        )
        hashtags.df<-na.omit(hashtags.df) 
        hashtags.df$id<-seq(nrow(hashtags.df))
        hashtags.df$Hashtags<-str_remove(hashtags.df$Hashtags," ")
        lst = strsplit(hashtags.df$Hashtags, ",")
        names(lst) = hashtags.df$id
        hashtags.df2 = melt(lst)
        hashtags.df2<-hashtags.df2 %>% group_by(value) %>% count() %>% ungroup()
        wordcloud(words = hashtags.df2$value ,freq = hashtags.df2$n, min.freq = 5,     
                  max.words=200, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))  
        });
  
  
        output$wordcloud_usermentioned<- renderPlot({
          data2 <- data[data$timestamp>=ymd_hms(input$timeRange[1]) & data$timestamp<=ymd_hms(input$timeRange[2])]
          docs <- Corpus(VectorSource(as.character(data2$user_mentioned)))
          #inspect(docs[1:2])
          
          # Create a document-term-matrix
          dtm <- TermDocumentMatrix(docs) 
          matrix <- as.matrix(dtm) 
          words <- sort(rowSums(matrix),decreasing=TRUE) 
          
          # words and frequency dataframe
          df <- data.frame(word = names(words),freq=words)
          
          #word cloud
          wordcloud(words = df$word, freq = df$freq, min.freq = 5,scale=c(8,.3),           
                    max.words=200, random.order=FALSE, rot.per=0.35,            
                    colors=brewer.pal(8, "Dark2"))

        });
}

shinyApp(ui = ui,server = server)
