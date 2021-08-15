
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
library(igraph)
library(ggraph)
library(topicmodels)
library(DT)
library(tidytext)
library(tm)
library(dplyr)
library(stringr)
library(tidygraph)
library(igraph)
library(visNetwork)
library(reshape2)
library(mapview)
library(tmap)
library(leaflet)
library(crosstalk)
library(sf)



data<-read.csv("cleaned_data/data.csv")



# reading shape files
Abila <- st_read(dsn = "cleaned_data/Geospatial", layer = "Abila")

p = npts(Abila, by_feature = TRUE)
Abila_st <- cbind(Abila, p) %>%
    mutate(ID = 1:nrow(Abila)) %>%  #giving unique ID for each line
    filter(p>1)  #removing orphan points

merged_final<-st_read(dsn = "cleaned_data/merged_final", layer = "merged_final")



# ======== Hexagon ==========
Abila_hex <- st_read(dsn = "cleaned_data/geo2", layer = "hex_final")


#########################################################################


ui <- fluidPage(
    theme = bslib::bs_theme(),
    navbarPage(
        "Visual Dashboard for Real-Time Analysis of Social Media",
        tabPanel("Introduction", 
                 h3("Introduction"),
                 br(),
                 p("Social media has played a pervasive role in the way people behave and think.
             Nowadays, people are also using time-stamped, geo -located data to share live information 
               about what’s happening in their surroundings, which enables the public, 
               government and researchers to identify abnormal events in community more quickly and take immediate actions."),
                 p("In this dashboard, We applied text analytics methods and visually driven data analysis techniques 
               in R language and R shiny and provided an interactive and integrated dashboard for streaming online
               social-media analysis. "),
                 fluidRow(column(6,  h3("Design Concept"),
                                 img(src="design.png",height = 430, width = 750)
                 ),
                 column(6,  h3("Related Links"),
                        h5("Project Website", a("Link", href="https://isss608.netlify.app/lesson")),
                        h5("User Guide", a("Link", href="https://isss608.netlify.app/lesson")),
                        h5("Github", a("Link", href="https://github.com/jiahui890/viz_project")),
                        
                 ))),
        tabPanel("Exploratory Data Analysis", 
                 
                 fluidRow(
                     column(width = 5, 
                            sliderInput("timeRange", label = "Select Time Range",
                                        min = as.POSIXct("2014-01-23 09:00:00"),
                                        max = as.POSIXct("2014-01-23 13:35:00"),
                                        value = c(
                                            as.POSIXct("2014-01-23 09:00:00"),
                                            as.POSIXct("2014-01-23 13:35:00")
                                        )
                            ),
                            titlePanel("Microblog and Call Center Trend"),       
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
                                column(4,numericInput("topnco", "Top n Cooccurence Word", 20, min = 1, max = 200))
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
                     ))
                 
                 
                 ),
        tabPanel("Topic Modeling", 
                 # titlePanel("LDA Topic Modeling"),
                 # br(),
                 # 
                 sidebarLayout(
                     
                     sidebarPanel(width = 3,
                                  h2(strong("Filter Panel"), style = "font-size:40px;"),
                                  sliderInput("timeRange", label = "Select Time Range",
                                              min = as.POSIXct("2014-01-23 09:00:00"),
                                              max = as.POSIXct("2014-01-23 13:35:00"),
                                              value = c(
                                                  as.POSIXct("2014-01-23 09:00:00"),
                                                  as.POSIXct("2014-01-23 13:35:00")
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
                 )
                 
                 ),
        tabPanel("Network Analysis", 
                 
                 # titlePanel("Retweet Network Analysis"),
                 
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  h2(strong("Filter Panel"), style = "font-size:40px;"),
                                  sliderInput("timeRange", label = "Selecte Time range",
                                              min = as.POSIXct("2014-01-23 09:00:00"),
                                              max = as.POSIXct("2014-01-23 13:35:00"),
                                              value = c(
                                                  as.POSIXct("2014-01-23 09:00:00"),
                                                  as.POSIXct("2014-01-23 13:35:00")
                                              )),
                                  
                                  selectInput("var1", "Select Centrality Measurement:",
                                              c("In Degree","Out Degree","Closeness","Eigenvector")),
                                  "In Degree: the number of ties directed to the node (popularity).",
                                  br(),
                                  "Out Degree: the number of ties that the node directs to others (gregariousness).",
                                  br(),
                                  "Closeness: a measure of the closeness of a node to all other nodes.",
                                  br(),
                                  "Eigenvector: a measure of the influence of a node in a network.",
                                  br(),
                                  actionButton("do1", "Apply Change")
                                  
                     ),
                     mainPanel(
                         visNetworkOutput("net"),
                         br(),
                         
                         plotlyOutput("hist",height = "200px")
                     )
                     
                 )
                 
                 ),
        tabPanel("Geo Analysis", 
                 sidebarLayout(
                     sidebarPanel(width = 3,
                                  h2(strong("Filter Panel"), style = "font-size:20px;"),
                                  sliderInput("timeRange3", label = "Select Time Range",
                                              min = as.POSIXct("2014-01-23 09:00:00"),
                                              max = as.POSIXct("2014-01-23 13:35:00"),
                                              value = c(
                                                  as.POSIXct("2014-01-23 09:00:00"),
                                                  as.POSIXct("2014-01-23 13:35:00")
                                              )),
                                  selectInput("var2", "Select Data Type:",
                                              c("All","Call-Center","Microblog")),
                                  actionButton("do2", strong("Apply Change"))
                     ),
                     
                     
                     mainPanel(
                         h4("Hover and click onto the map for more details."),
                         br(),
                         strong("CC and MB data:"),
                         p("Plot shows the various geolocation for microblogs and call-center messages filtered by time and type."),
                         br(),
                         strong("Hexagon Plot:"),
                         p("Shows the hotspots (by frequency of messages)"),
                         br(),
                         tabsetPanel(
                             tabPanel("CC and MB Data", tmapOutput("plot11")), 
                             tabPanel("Hexagon Plot", tmapOutput("plot22"))
                         )
                     )
                 )
                 
                 
                 )
    )
    
        
    )



server <- function(input, output){
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
                              title = "Co-occurrences", top_n = input$topnco)
        
        
        
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
    
    model2 <- reactiveValues(Data=NULL)
    
    observeEvent(input$do1,{
        
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
        #nodes$size<-round(nodes$size,3)
        nodes$title<-paste("id: ",nodes$id, "<br> Value:",nodes$size)
        
        model2$net<-visNetwork(nodes, RT_edges_agg, height = "500px", width = "100%") %>%
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
        
        
        model2$hist<-ggplotly(
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
        model2$hist
    });
    
    
    output$net<-renderVisNetwork({
        model2$net
    });
    
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
    
    model3 <<- reactiveValues(Data=NULL)
    
    observeEvent(input$do2, {
        
        merged_final<-left_join(merged_final,data,by="id")
        
        #merged_final$timestamp<-ymd_hms(timestamp)
        
        merged_final2 <- merged_final %>% 
            filter(timestamp>=ymd_hms(input$timeRange3[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange3[2])+ hours(8))
        

        
        if(input$var2=="Call-Center"){
            merged_final2<-merged_final2 %>% filter(type=="ccdata")
        } else if (input$var2=="Microblog"){
            merged_final2<-merged_final2 %>% filter(type=="mbdata")
        } else {
            merged_final2<-merged_final2
        }
        
        
        
        # === plot ====
        
        tmap_mode("view")
        
        model3$plot11 <- tm_shape(Abila_st) +
            tm_lines() +
            tm_shape(merged_final2) +
            tm_dots(col = "type", palette = c(cc='red',mb='blue'), 
                    popup.vars = c("timestamp","type","author","message")) +
            tm_layout(title= 'Geo-locations cc and mb')
        
        
        # Hexagon
        
        intersection<-st_set_geometry(st_intersection(merged_final2,Abila_hex), NULL)
        intersection<-left_join(intersection,Abila_hex,by=c('left','bottom','right','top'))
        
        #count number of posts in each polygon
        intersection<-intersection %>% group_by(left,bottom,right,top) %>% 
            mutate(count = n())
        
        intersection<-st_as_sf(intersection)
        
        
        # === plot ===
        
        model3$plot22<- tm_shape(Abila_hex) +
            tm_polygons() +
            tm_shape(Abila_st) +
            tm_lines() +
            tm_shape(intersection) +
            tm_polygons(col='count')
        
        
        
    },ignoreNULL = F);
    
    # render plots
    
    output$plot11 <- renderTmap({
        model3$plot11
        
    });
    
    output$plot22 <- renderTmap({
        model3$plot22
    });
    
   
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)
