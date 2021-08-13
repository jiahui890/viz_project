library(shiny)
library(plotly)
library(tidyverse)
library(lubridate)
library(wordcloud)
library(tm)
library(reshape)
library(tidytext)
library(udpipe)
library(textplot)


data<-read.csv("cleaned_data/data.csv")

ui <- fluidPage(
  titlePanel("Exploratory Data Analysis"),
  fluidRow(
    column(width = 5, 
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
  column(width = 7,
         br(),
         titlePanel("WordCloud Panel"),
         fluidRow(
           column(4,numericInput("min", "Min Frequency", 5, min = 1, max = 20)),
           column(4,numericInput("max", "Max Words", 100, min = 10, max = 200)),
           column(4,numericInput("topn", "Top n Cooccurance", 20, min = 1, max = 200))
           ),
         
         tabsetPanel(
           tabPanel("Tweet", 
                    fluidRow(column(6,plotOutput("wordcloud_tweet")),
                             column(6,plotOutput("co_tweet")))
                    ),
           tabPanel("Retweet", fluidRow(column(6,plotOutput("wordcloud_retweet")),
                                        column(6,plotOutput("co_retweet")))
           ), 
           tabPanel("Call Center", fluidRow(column(6,plotOutput("wordcloud_cc")),
                                            column(6,plotOutput("co_cctweet")))
           ),
           tabPanel("Hashtag", plotOutput("wordcloud_hashtag")), 
           tabPanel("Tagged User", plotOutput("wordcloud_usermentioned"))
           
         )
  )))


server <- function(input, output, session) {

  
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
              yaxis = list(title = 'Number of Tweets/Calls'),hovermode = "x unified")
      
});
  

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
               yaxis = list(title = 'Number of Posts'),hovermode = "x unified")
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
  
  output$co_tweet<- renderPlot({
    
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8)) %>% 
      filter(type=='mbdata')
    
    x<-data2 %>% 
      unnest_tokens(word,cleaned)
    x<-cooccurrence(x, group = "id", term = "word")
    x$cooc<-log10(x$cooc)
    textplot_cooccurrence(x,
                          title = "Co-occurrences", top_n = input$topn)
    
    
    
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
  
  
  output$co_cctweet<- renderPlot({
    
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8)) %>% 
      filter(type=='ccdata')
    
    x<-data2 %>% 
      unnest_tokens(word,cleaned)
    x<-cooccurrence(x, group = "id", term = "word")
    x$cooc<-log10(x$cooc)
    textplot_cooccurrence(x,
                          title = "Co-occurrences", top_n = input$topn)
    
    
    
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
  
  output$co_retweet<- renderPlot({
    
    data2<-data %>% mutate(timestamp=ymd_hms(data$timestamp),time_1min=ymd_hms(data$time_1min)) %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8)) %>% 
      filter(type=='mbdata')
    
    x<-data2 %>% 
      unnest_tokens(word,RT_message_cleaned)
    x<-cooccurrence(x, group = "id", term = "word")
    x$cooc<-log10(x$cooc)
    textplot_cooccurrence(x,
                          title = "Co-occurrences", top_n = input$topn)
    
    
    
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
