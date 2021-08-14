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
csv1 <- read_csv('data/csv-1700-1830.csv')
csv2 <- read_csv('data/csv-1831-2000.csv')
csv3 <- read_csv('data/csv-2001-2131.csv')
data <- rbind(csv1, csv2, csv3)

# reading shape files
Abila <- st_read(dsn = "data/Geospatial", layer = "Abila")

# changing date format
data$datetime <- ymd_hms(data$`date(yyyyMMddHHmmss)`)
data$time <- format(as.POSIXct(data$datetime), format = "%H:%M:%S")

# separating cc and mb data
cc <- data %>% filter(type=="ccdata")
mb <- data %>% filter(type=="mbdata")

# filtering 
mb2 <- mb %>% filter(!is.na(longitude))
cc2 <- cc %>% mutate(id = 1:nrow(cc)) %>% filter(location!="N/A")

p = npts(Abila, by_feature = TRUE)
Abila_st <- cbind(Abila, p) %>%
  mutate(ID = 1:nrow(Abila)) %>%  #giving unique ID for each line
  filter(p>1)  #removing orphan points

mb_geo<-st_as_sf(mb2,
                 coords = c("longitude","latitude"),
                 crs= 4326)

mb_geo2<-subset(mb_geo,select = c(datetime,author,message,geometry))

# remove characters non UTF-8
mb_geo2<-mb_geo2%>% mutate_if(is.character, ~gsub('[^ -~]', '', .)) 

mb_geo2$type<-"mb"


# ccdata 

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
cc_geo1 <- match_points %>%
  select(datetime, message, geometry) %>%
  mutate(type = "cc")


# merging them together
merged <- dplyr::bind_rows(cc_geo1,mb_geo2)



# getting cross junctions out
cross_junc <- cc2 %>% 
  separate(location, c("number", "street"), sep = "(?<=[0-9]) ", convert = T) %>%
  filter(is.na(street)) %>%
  mutate(street = strsplit(as.character(number), " / ")) %>%
  unnest(street)

#removed id=26 and 108 for now as street is "N. Parla St from Egeou Ave North to N. Alm St" and "ALL UNITS"
remove_id <- c(26,108)
cross_junc2 <- cross_junc[!cross_junc$id %in% remove_id,]


#cross_junc2$geometry <- "-"
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


cross_junc_df2 <- cross_junc_df
cross_junc_df2$datetime <- as.POSIXct("2014-01-23 17:00:00 UTC")
cross_junc_df2$message <- "-"
for (j in 1:nrow(cross_junc_df2)){
  for (i in 1:nrow(cross_junc2)){
    if(cross_junc_df$id[j] == cross_junc2$id[i]){
      cross_junc_df2$datetime[j]<-cross_junc2$datetime[i]
      cross_junc_df2$message[j]<-cross_junc2$message[i]
    }
  }
}

# minus 8 hours due to R studio set timing
cross_junc_df2$datetime <- cross_junc_df2$datetime - hours(8)

#filter out NULL and NA
cross_junc_df2 <- cross_junc_df2 %>% filter(!geometry %in% c("NULL","NA"))
cross_junc_df2$type <- "cc"
cross_junc_df2 <- cross_junc_df2[,c("datetime","message","geometry","type")]
cross_junc_df2 <- st_as_sf(cross_junc_df2)


# merging them together
merged <- dplyr::bind_rows(merged,cross_junc_df2)


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
                  actionButton("do", strong("Apply Change"))
                  ),

        
        mainPanel(
          tabsetPanel(
            tabPanel("CC and MB Data", tmapOutput("plot1")), 
            tabPanel("Hexagon Plot", plotOutput("plot2"))
        )
    )
))


server <- function(input, output) {

  # data configuration
  
  model <<- reactiveValues(Data=NULL)
  
  observeEvent(input$do, {
    
    merged2 <- merged %>% 
      filter(datetime>=ymd_hms(input$timeRange[1])+ hours(8) & datetime<=ymd_hms(input$timeRange[2])+ hours(8))
    
    # === plot ====
    
    tmap_mode("view")
    model$plot1 <- tm_shape(Abila_st) +
      tm_lines() +
      tm_shape(merged2) +
      tm_dots(col = "type", palette = c(cc='red',mb='blue'), popup.vars = TRUE) +
      tm_layout(title= 'Geo-locations cc and mb')
    
    
    
    
    
  },ignoreNULL = F);
  
  # render plots
  
  output$plot1 <- renderTmap({
    model$plot1
  });
  

}

# Run the application 
shinyApp(ui = ui, server = server)
