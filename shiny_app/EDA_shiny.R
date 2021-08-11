library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(wordcloud)
library(tm)


data<-read.csv("cleaned_data/data.csv")

ui <- fluidPage(
  titlePanel("Exploratory Data Analysis"),
  fluidRow(
    column(width = 6, 
           sliderInput("timeRange", label = "Select Time Range",
                       min = as.POSIXct("2014-01-23 17:00:00"),
                       max = as.POSIXct("2014-01-23 21:35:00"),
                       value = c(
                         as.POSIXct("2014-01-23 17:00:00"),
                         as.POSIXct("2014-01-23 21:35:00")
                       )
           ),
           titlePanel("Microblog and Call Cender Trend"),       
    plotlyOutput("Lineplot1",height = 200),
    br(),
    plotlyOutput("Lineplot2",height = 200)
    ),
  column(width = 6,
         br(),
         titlePanel("WordCloud Panel"),
         fluidRow(
           column(3,numericInput("min", "Min Frequency", 5, min = 1, max = 20)),
           column(3,numericInput("max", "Max Words", 100, min = 10, max = 200))
           ),
         
  tabsetPanel(
    tabPanel("Tweet", plotOutput("wordcloud_tweet")),
    tabPanel("Retweet", plotOutput("wordcloud_retweet")), 
    tabPanel("Hashtag", plotOutput("wordcloud_hashtag")), 
    tabPanel("Tagged User", plotOutput("wordcloud_usermentioned")), 
    tabPanel("Call Center", plotOutput("wordcloud_cc"))
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
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))
     
    counts <- data2 %>%
        group_by(type,time_1min) %>%
        summarise(count = n()) %>% ungroup()
     
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
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))

      data_rt2<-data2 %>%
        group_by(time_1min) %>%
        summarise(post=n(),
                  rt_post=sum(RT_from!="")) %>% ungroup()

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
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8)) %>% 
      filter(type=='mbdata')
    
    docs <- Corpus(VectorSource(as.character(data2$cleaned)))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    wordcloud(words = df$word, freq = df$freq, min.freq = input$min,
              max.words=input$max, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
    });
  
  
  output$wordcloud_cc<- renderPlot({
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8)) %>% 
      filter(type=='ccdata')
    docs <- Corpus(VectorSource(as.character(data2$cleaned)))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    wordcloud(words = df$word, freq = df$freq, min.freq = input$min,
              max.words=input$max, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  });    
    
    output$wordcloud_retweet<- renderPlot({
      data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
        filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))
      docs <- Corpus(VectorSource(as.character(data2$RT_message_cleaned)))
      dtm <- TermDocumentMatrix(docs) 
      matrix <- as.matrix(dtm) 
      words <- sort(rowSums(matrix),decreasing=TRUE) 
      df <- data.frame(word = names(words),freq=words)
      wordcloud(words = df$word, freq = df$freq, min.freq = input$min,
                max.words=input$max, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
      });
  
      
      
      
      output$wordcloud_hashtag<- renderPlot({
        data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
          filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))
        
        GetHashtags <- function(tweet) {
          
          hashtag.vector <- str_extract_all(string = tweet, pattern = '#\\S+', simplify = TRUE) %>% 
            as.character()
          
          hashtag.string <- NA
          
          if (length(hashtag.vector) > 0) {
            
            hashtag.string <- hashtag.vector %>% str_c(collapse = ', ')
            
          } 
          
          return(hashtag.string)
        }
        
        hashtags.df <- tibble(
          Hashtags = data$message %>% map_chr(.f = ~ GetHashtags(tweet = .x))
        )
        
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
        wordcloud(words = hashtags.df2$value ,freq = hashtags.df2$n, min.freq = input$min,     
                  max.words=input$max, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"))  
        });
  
  
        output$wordcloud_usermentioned<- renderPlot({
          data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
            filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))
          docs <- Corpus(VectorSource(as.character(data2$user_mentioned)))
          #inspect(docs[1:2])
          
          # Create a document-term-matrix
          dtm <- TermDocumentMatrix(docs) 
          matrix <- as.matrix(dtm) 
          words <- sort(rowSums(matrix),decreasing=TRUE) 
          
          # words and frequency dataframe
          df <- data.frame(word = names(words),freq=words)
          
          #word cloud
          wordcloud(words = df$word, freq = df$freq, min.freq =input$min,scale=c(8,.3),           
                    max.words=input$max, random.order=FALSE, rot.per=0.35,            
                    colors=brewer.pal(8, "Dark2"))

        });
}

shinyApp(ui = ui,server = server)
