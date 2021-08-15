#<-read.csv("integrated/cleaned_data/data.csv")
library(shiny)
library(plotly)
library(tmap)
library(visNetwork)
ui <- fluidPage(
  theme = bslib::bs_theme(),
  navbarPage(
    "Visual Dashboard for Real-Time Analysis of Social Media",
    tabPanel("Introduction", 
             
             h3("Introduction"),
             br(),
             p("Social media has played a pervasive role in the way people behave and think.
             Nowadays, people are using time-stamped, geo-located data to share live information 
               about what’s happening in their surroundings, which enables the public, 
               government and researchers to identify abnormal events in community more quickly and take immediate actions."),
             p("In this dashboard, we applied text analytics methods and visually driven data analysis techniques 
               in R language and R shiny and provided an interactive and integrated dashboard for streaming online
               social-media analysis based on ",a("VAST 2021 Mini-Challenge 3.", href="https://vast-challenge.github.io/2021/MC3.html"),),
             
             fluidRow(column(6,  h3("Design Concept"),br(),
                             img(src="design.png",height = 200, width = 550)
                             ),
                      column(3,  h3("Related Links"),br(),
                             h5("Project Website", a("Link", href="https://visual-shiny-g1-g9.netlify.app/")),
                             p("Visit project website and explore more. "),
                             h5("User Guide", a("Link", href="https://visual-shiny-g1-g9.netlify.app/images/ShinyApp_UserGuide.pdf")),
                             p("Read the guide to optimize user experience."),
                             h5("Github", a("Link", href="https://github.com/jiahui890/viz_project")),
                             p("Github repository for all work.")
                    
                      ),
                      column(3,h3("Author"),
                             br(),
                             strong("Huang Linya, Lim Jiahui, Zhang Ying"),
                             br(),
                             br(),
                             strong("Singapore Management University"))
                      )
            
             ),
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



server <- function(input, output){}

shinyApp(ui = ui, server = server)