# data clean for EDA

#import packages
packages= c(
  'tidyverse','data.table','lubridate',
  'textclean','tm','wordcloud','text2vec','textdata',
  'topicmodels','tidytext','textmineR','quanteda',
  'BTM','textplot','concaveman','ggwordcloud',
  'qdapDictionaries','textstem','devtools','textnets',
  'igraph', 'tidygraph', 
  'ggraph', 'visNetwork','udpipe','grid','sp','sf','gridExtra','mapview','tmap')

for(p in packages){
  if(!require(p,character.only= T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#import data

data_17_1830=read_csv("data/csv-1700-1830.csv")
data_1830_20=read_csv("data/csv-1831-2000.csv")
data_20_2130=read_csv("data/csv-2001-2131.csv")

data=rbindlist(list(data_17_1830,data_1830_20,data_20_2130))
data$timestamp <- ymd_hms(data$`date(yyyyMMddHHmmss)`,tz = "GMT")



data$id <- seq.int(nrow(data))

#data clean

ppl <- "@([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.)"
rt <- "RT @([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.) "
hash <- "#([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"


data$cleaned<-data$message %>% 
  str_replace_all(rt,"")%>%
  str_replace_all(ppl,"")%>%
  str_replace_all(hash,"") %>% 
  tolower()%>%   # transform all message to lower cases
  replace_contraction()%>%   #replace contractions with long form
  replace_word_elongation()%>% #remove the same letter (case insensitive) appears 3 times consecutively
  str_replace_all("[0-9]", "") %>% #removing numbers
  str_replace_all("([[:punct:]]+)","")%>% #remove punctuations
  #str_replace_all("rt|pokrally|kronosstar","")%>%
  #|#hi|#pok|#pokrally|
  # #abilapost|#kronosstar|#centralbulletin|@centralbulletin|@kronosstar|rally|aliba") #remove hashtag and rt
  removeWords(stopwords("english"))%>% 
  str_squish()%>% #trim whitespace from a string 
  lemmatize_strings()#removes whitespace from start and end of string



timeRange1=as.POSIXct("2014-01-23 18:35:00")
timeRange2=as.POSIXct("2014-01-23 18:50:00")

data2<-data %>% filter(data$timestamp >= ymd_hms(timeRange1) & data$timestamp<= ymd_hms(timeRange2))


data2$time_1min <- cut(data2$timestamp, breaks="1 min")
data2$time_1min <- ymd_hms(data2$time_1min)

counts <- data2 %>%
  group_by(type,time_1min) %>%
  summarise(post = n())




data2<-data #%>% filter(timestamp>=ymd_hms(timeRange1),timestamp<=ymd_hms(timeRange2))

data2$time_1min <- cut(data2$timestamp, breaks="1 min")
data2$time_1min <- ymd_hms(data2$time_1min)
counts <- data2 %>%
  group_by(type,time_1min) %>%
  summarise(count = n_distinct(message))

ggplot(counts,
       aes(x=time_1min,
           y=count,
           group=type))+
  geom_line(aes(color=type))+
  ggtitle("Total Number of Events Over Time")+
  xlab("Time") + ylab("number of tweets or calls")+
  xlim( min(counts$time_1min), max(counts$time_1min))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))


#convert dataframe to corpus
docs <- Corpus(VectorSource(as.character(data$cleaned)))
#inspect(docs[1:2])

# Create a document-term-matrix

dtm <- TermDocumentMatrix(docs) 
matrix <- as.matrix(dtm) 
words <- sort(rowSums(matrix),decreasing=TRUE) 

# words and frequency dataframe
df <- data.frame(word = names(words),freq=words)

# get RT

# Retrieve RT/ hashtags from message by using regex techniques

regex <- "RT @([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.)"
regex2 <- "@([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.)"
regex3 <- "RT @([A-Za-z]+[A-Za-z0-9_-]+)(?![A-Za-z0-9_]*\\.) "
regex4 <- "#([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"
hash <- "#([A-Za-z]+[A-Za-z0-9_]+)(?![A-Za-z0-9_]*\\.)"

data$RT_pattern<-str_extract_all(data$message, regex, simplify = TRUE)
data$RT_from<-  str_extract_all(data$RT_pattern, regex2, simplify = TRUE)
data$RT_from<-  str_replace(data$RT_from, "@","")
data$RT_message <- str_replace_all(data$message,regex3,"")

data$RT_message_cleaned <- ifelse(data$RT_from=="","",data$cleaned)

data$user_mentioned <- str_extract_all(data$message, regex2, simplify = FALSE)


data$user_mentioned<-data$user_mentioned%>% 
  str_replace_all("character\\(0\\)|c\\(|\\)|,","")



data$time_1min <- cut(data$timestamp, breaks="1 min")
data_rt2<-data %>% 
  #filter(str_detect(message, "fire")) %>% 
  select(c("author","time_1min","message","RT_from")) %>% 
  group_by(time_1min) %>% 
  summarise(post=n(),
            rt_post=sum(RT_from!=""))

data_rt2$time_1min=ymd_hms(data_rt2$time_1min)


ggplot(data_rt2,aes(x=time_1min)) +
  geom_line(aes(y=post,color="tweet")) +
  geom_line(aes(y=rt_post,color="retweet")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Tweet and Re-Tweets Trend")





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


hashtags.df<-na.omit(hashtags.df) 

hashtags.df$id<-seq(nrow(hashtags.df))

hashtags.df$Hashtags<-str_remove(hashtags.df$Hashtags," ")


lst = strsplit(hashtags.df$Hashtags, ",")
names(lst) = hashtags.df$id
hashtags.df2 = melt(lst)

hashtags.df2<-hashtags.df2 %>% group_by(value) %>% count() %>% ungroup()


#######################################################try plotly#####################################################
wordcloud(words = hashtags.df2$value ,freq = hashtags.df2$n, min.freq = 5,     
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


data2 <- data%>% 
  filter(type='mbdata')

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
  


plot_ly(
  counts,
  x = ~ time_1min,y = ~ count,type = "scatter",mode = "lines",color=~type)%>% 
    layout(title = 'Tweet and Calls Trend',
           xaxis = list(title = 'Selected Time Period'),
           yaxis = list(title = 'Number of Tweets/Calls'))
#######################################################try plotly#####################################



####################################################LDA modeling###########################################

mbdata<-data %>% filter(type=='mbdata')
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

topic=LDA(dtm.new,k=10,method="Gibbs",conrol=list(seed=2021,alpha=0.01,iter=300))

ap_topics <- tidy(topic, matrix = "beta")


ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
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


id_time <- mbdata %>% select(c("id","time_1min","timestamp","message",
                               "author","cleaned"))

topic_data<-left_join(topic_gamma,id_time,by=c("document"="id"))


topic_data %>% group_by(time_1min,topic) %>% count() %>% 
  ggplot(aes(x=time_1min))+
  geom_bar(aes(y=n), stat = "identity",fill = "black")+
  facet_wrap(~topic)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("LDA Topics Trend of Selected Time Period")




# data %>% unnest_tokens(word, cleaned) %>% 
#   inner_join(get_sentiments("nrc"))

topic_data %>% group_by(topic,author) %>% 
  summarize(count=n()) %>% 
  slice_max(count, n = 5) %>% 
  ungroup()%>%
  ggplot(aes(x=reorder(author,count), y=count, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()+
  coord_flip()



topic_data2<-topic_data %>% group_by(topic,author) %>% 
  summarize(count=n()) %>% 
  slice_max(count, n = 5) %>% 
  ungroup()

data2<-data2%>% mutate_if(is.character, ~gsub('[^ -~]', '', .)) # remove characters non UTF-8
DT::datatable(data = data2)

table<-subset(topic_data,select = c(timestamp,author,message,topic))%>%
  mutate_if(is.character, ~gsub('[^ -~]', '', .))

DT::datatable(data = table)





############################Network Data Clean################################




RT_edges <- data %>% 
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

#visIgraph(RT_graph)
#Inclass_ex_9 slide32 centr_betw(),centr_clo(),centr_eigen()

V(RT_graph)$size <- centr_degree(RT_graph, mode = "all")$res #all,in,out
#V(RT_graph)$size <-centr_betw(RT_graph)$res
V(RT_graph)$size <-centr_clo(RT_graph)$res
V(RT_graph)$size<-centr_eigen(RT_graph)$vector


nodes <- get.data.frame(RT_graph, what="vertices") 


visNetwork(nodes, RT_edges_agg, height = "500px", width = "100%") %>%
  visOptions(selectedBy = "size", highlightNearest = list(enabled = T, hover = T), 
             nodesIdSelection = TRUE)%>% 
     visIgraphLayout(layout = "layout_with_fr") %>% 
     visLayout(randomSeed = 1234) 

#Degree distribution
#plotly
library(plotly)
plot_ly(nodes,x=~indegree,type="histogram")
#ggplpt
ggplot(nodes, aes(x=size)) + 
  geom_histogram() +
  theme_minimal()

#to improve
#to add network knowledge theory below the selection panel
#?size of the node closeness/eigen scale bigger?
#plotly tooltip, title 
#highlight/show size when click/hover node
