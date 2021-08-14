
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

# separating cc and mb data
cc <- data %>% filter(type=="ccdata")
mb <- data %>% filter(type=="mbdata")

# filtering 
mb2 <- mb %>% filter(!is.na(longitude))
cc2 <- cc %>% filter(location!="N/A")

p = npts(Abila, by_feature = TRUE)
Abila_st <- cbind(Abila, p) %>%
  mutate(ID = 1:nrow(Abila)) %>%  #giving unique ID for each line
  filter(p>1)  #removing orphan points

mb_geo<-st_as_sf(mb2,
                 coords = c("longitude","latitude"),
                 crs= 4326)

mb_geo<-mb_geo %>% mutate_if(is.character, ~gsub('[^ -~]', '', .)) 
mb_geo <- subset(mb_geo, select = -c(location))

# === plotting ccdata ===

# only the addresses without junctions or separators "/"
cc_streets <- cc2 %>% 
  separate(location, c("number", "street"), sep = "(?<=[0-9]) ", convert = T) %>%
  filter(!is.na(street)) %>%
  mutate(number = as.integer(number)) %>%
  mutate(number_rd = floor(number/100)*100)

# manipulating shape file  
Abila_st2 <- Abila_st %>%
  mutate(num = pmin(FRADDL,FRADDR,TOADDL,TOADDR, na.rm = TRUE)) %>%
  mutate(num = floor(num/100)*100) %>%
  unite(add, FEDIRP:FENAME, sep = ". ", remove = FALSE, na.rm = TRUE) %>%
  unite(str_add, c(add, FETYPE), sep = " ") %>%
  mutate(geometry = st_centroid(geometry))

# for cross junctions
Abila_st3 <- Abila_st %>%
  mutate(num = pmin(FRADDL,FRADDR,TOADDL,TOADDR, na.rm = TRUE)) %>%
  mutate(num = floor(num/100)*100) %>%
  unite(add, FEDIRP:FENAME, sep = ". ", remove = FALSE, na.rm = TRUE) %>%
  unite(str_add, c(add, FETYPE), sep = " ") 


# matching them both
match_points <- Abila_st2 %>%
  inner_join(cc_streets, by = c("num" = "number_rd", 
                                "str_add" = "street"))

# for plotting
cc_geo <- match_points %>%
  select(id,type,timestamp,author,message,geometry)


# merging them together
merged <- dplyr::bind_rows(cc_geo,mb_geo)


# getting cross junctions out
cross_junc <- cc2 %>% 
  separate(location, c("number", "street"), sep = "(?<=[0-9]) ", convert = T) %>%
  filter(is.na(street)) %>%
  mutate(street = strsplit(as.character(number), " / ")) %>%
  unnest(street)

#removed id=26 and 108 for now as street is "N. Parla St from Egeou Ave North to N. Alm St" and "ALL UNITS"
remove_id <- c(419,1945)
cross_junc2 <- cross_junc[!cross_junc$id %in% remove_id,]

cross_junc_df <- as.data.frame(matrix(ncol=2, nrow=0))
idx <- unique(cross_junc2$id)
for (i in 1:length(idx)){
  skip_to_next <- FALSE
  tryCatch({
    first <- cross_junc2[cross_junc2$id==idx[i],]$street[1]
    sec <- cross_junc2[cross_junc2$id==idx[i],]$street[2]
    
    first_geo <- Abila_st3 %>% filter(str_add==first)
    sec_geo <- Abila_st3 %>% filter(str_add==sec)
    
    cross_junc_df[i,1] <- idx[i]
    cross_junc_df[i,2] <- st_as_sf(st_intersection(first_geo, sec_geo)$geometry)
  }, error = function(e){skip_to_next <- TRUE})
  
  if(skip_to_next){next}
}

# renaming
names(cross_junc_df)[1]<-"id"
names(cross_junc_df)[2]<-"geometry"


cross_junc_df$timestamp <- as.POSIXct("2014-01-23 17:00:00 UTC")
cross_junc_df$message <- "-"
for (j in 1:nrow(cross_junc_df)){
  for (i in 1:nrow(cross_junc2)){
    if(cross_junc_df$id[j] == cross_junc2$id[i]){
      cross_junc_df$timestamp[j]<-cross_junc2$timestamp[i]
      cross_junc_df$message[j]<-cross_junc2$message[i]
    }
  }
}

# minus 8 hours due to R studio set timing
cross_junc_df$timestamp<- cross_junc_df$timestamp - hours(8)

cross_junc_df <- cross_junc_df %>% filter(!geometry %in% c("NULL","NA"))
cross_junc_df$type <- "ccdata"
cross_junc_df <- st_as_sf(cross_junc_df)

# fixing timestamp issues
merged_final <- dplyr::bind_rows(merged,cross_junc_df)
for (i in 1:nrow(merged_final)){
  if(hour(merged_final$timestamp[i]) < 17){
    merged_final$timestamp[i] <- merged_final$timestamp[i] + hours(8)
  }
}


# ======== Hexagon ==========
Abila_hex <- st_read(dsn = paste0(working_dir,"geo2"), layer = "hex_final")



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
            tabPanel("CC and MB Data", tmapOutput("plot1")), 
            tabPanel("Hexagon Plot", tmapOutput("plot2"))
        )
    )
))


server <- function(input, output) {

  model <<- reactiveValues(Data=NULL)
  
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
    
    model$plot1 <- tm_shape(Abila_st) +
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
  
    model$plot2 <- tm_shape(Abila_hex) +
      tm_polygons() +
      tm_shape(Abila_st) +
      tm_lines() +
      tm_shape(intersection) +
      tm_polygons(col='count')
    
    
    
  },ignoreNULL = F);
  
  # render plots
  
  output$plot1 <- renderTmap({
    model$plot1
  });
  
  output$plot2 <- renderTmap({
    model$plot2
  });

}

# Run the application 
shinyApp(ui = ui, server = server)
