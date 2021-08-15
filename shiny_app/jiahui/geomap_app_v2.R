
library(shiny)
library(mapview)
library(tmap)
library(leaflet)
library(crosstalk)
library(lubridate)
library(dplyr)
library(sf)
library(tidyverse)

###########################################################################

# import data
data <- read_csv('data/data.csv')

# reading shape files
Abila <- st_read(dsn = "data/Geospatial", layer = "Abila")

p = npts(Abila, by_feature = TRUE)
Abila_st <- cbind(Abila, p) %>%
  mutate(ID = 1:nrow(Abila)) %>%  #giving unique ID for each line
  filter(p>1)  #removing orphan points

merged_final<-st_read(dsn = "merged_final", layer = "merged_final")

merged_final<-left_join(merged_final,data,by="id")

# ======== Hexagon ==========
Abila_hex <- st_read(dsn = "geo2", layer = "hex_final")




#############################################################################

ui <- fluidPage(
  titlePanel("Geospatial Analysis"),
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
                 selectInput("var1", "Select Data Type:",
                             c("All","Call-Center","Microblog")),
                 actionButton("do", strong("Apply Change"))
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
        tabPanel("CC and MB Data", tmapOutput("plot12")), 
        tabPanel("Hexagon Plot", tmapOutput("plot22"))
      )
    )
  ))


server <- function(input, output) {
  
  model3 <<- reactiveValues(Data=NULL)
  
  observeEvent(input$do, {
    
    
    merged_final2 <- merged_final %>% 
      filter(timestamp>=ymd_hms(input$timeRange[1])+ hours(8) & timestamp<=ymd_hms(input$timeRange[2])+ hours(8))
    
    if(input$var1=="Call-Center"){
      merged_final2<-merged_final2 %>% filter(type=="ccdata")
    } else if (input$var1=="Microblog"){
      merged_final2<-merged_final2 %>% filter(type=="mbdata")
    } else {
      merged_final2<-merged_final2
    }
    
    
    # === plot ====
    
    tmap_mode("view")
    
    model3$plot1 <- tm_shape(Abila_st) +
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
    
    model3$plot2 <- tm_shape(Abila_hex) +
      tm_polygons() +
      tm_shape(Abila_st) +
      tm_lines() +
      tm_shape(intersection) +
      tm_polygons(col='count')
    
    
    
  },ignoreNULL = F);
  
  # render plots
  
  output$plot11 <- renderTmap({
    model3$plot1
  });
  
  output$plot22 <- renderTmap({
    model3$plot2
  });
  
}

# Run the application 
shinyApp(ui = ui, server = server)