
rm(list=ls())
cat("\014")

packages = c('lubridate','tidyverse', 'ggplot2', 'ggraph', 'igraph', 
             'lsa', 'DT','tm', 'wordcloud', 'tidytext', 'dplyr',
             'textmineR', 'ggExtra', 'stringr','TSstudio',
             'hrbrthemes','plotly', 'ggridges','dygraphs','tokenizers',
             'data.table','leaflet','crosstalk','reshape2','sf', 'tmap',
             'mapview')

for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# import data
working_dir <- "C:/Users/jiahu/Desktop/Visual Analytics/Project/shiny app/mc3_geo/R/"

data <- read_csv(paste0(working_dir,'data/data.csv'))
data <- data %>% select(id,type,timestamp,author,message,latitude,longitude,location)

# csv1 <- read_csv(paste0(working_dir,'data/csv-1700-1830.csv'))
# csv2 <- read_csv(paste0(working_dir,'data/csv-1831-2000.csv'))
# csv3 <- read_csv(paste0(working_dir,'data/csv-2001-2131.csv'))
# 
# data <- rbind(csv1, csv2, csv3)

# changing date format
# data$datetime <- ymd_hms(data$`date(yyyyMMddHHmmss)`,tz = "UTC")
# data$time <- format(as.POSIXct(data$datetime), format = "%H:%M:%S")


# separating cc and mb data
cc <- data %>% filter(type=="ccdata")
mb <- data %>% filter(type=="mbdata")



# filtering 
mb2 <- mb %>% filter(!is.na(longitude))
#cc2 <- cc %>% mutate(id = 1:nrow(cc)) %>% filter(location!="N/A")
cc2 <- cc %>% filter(location!="N/A")


# reading shape files
Abila <- st_read(dsn = paste0(working_dir,"data/Geospatial"), layer = "Abila")
# Kronos <- st_as_sf(st_read(dsn = "data/Geospatial", layer = "Kronos_Island"))
# Kronos_small <- st_crop(Kronos, 
#                       c(xmin = 24.8232, xmax = 24.91075, 
#                         ymin = 36.0445, ymax = 36.09543))


# ==== plot with mbdata only ====

library(mapview)
library(tmap)

p = npts(Abila, by_feature = TRUE)
Abila_st <- cbind(Abila, p) %>%
  mutate(ID = 1:nrow(Abila)) %>%  #giving unique ID for each line
  filter(p>1)  #removing orphan points

mb_geo<-st_as_sf(mb2,
                 coords = c("longitude","latitude"),
                 crs= 4326)

#mb_geo2<-subset(mb_geo,select = c(timestamp,author,message,geometry))

# remove characters non UTF-8
mb_geo<-mb_geo %>% mutate_if(is.character, ~gsub('[^ -~]', '', .)) 
mb_geo <- subset(mb_geo, select = -c(location))
#mb_geo2$type<-"mb"


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


#cross_junc_df2 <- cross_junc_df
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

#filter out NULL and NA
cross_junc_df <- cross_junc_df %>% filter(!geometry %in% c("NULL","NA"))
cross_junc_df$type <- "ccdata"
#cross_junc_df2 <- cross_junc_df2[,c("datetime","message","geometry","type")]
cross_junc_df <- st_as_sf(cross_junc_df)


# merging them together
merged_final <- dplyr::bind_rows(merged,cross_junc_df)
for (i in 1:nrow(merged_final)){
  if(hour(merged_final$timestamp[i]) < 17){
    merged_final$timestamp[i] <- merged_final$timestamp[i] + hours(8)
  }
}



# sample plot
# tmap_mode("view") 
# tm_shape(Abila_st) +
#   tm_lines() +
#   tm_shape(gps_sf2) +
#   tm_dots() +
#   tm_shape(map_plot) +
#   tm_dots(col = "red") +
#   tm_shape(cross_junc_df2) +
#   tm_dots(col = "red") +
#   tm_layout(title= 'Geo-locations cc and mb')
# 


tmap_mode("view") 
tm_shape(Abila_st) +
  tm_lines() +
  tm_shape(merged_final) +
  tm_dots(col = "type", palette = c(cc='red',mb='blue'), popup.vars = TRUE) +
  tm_layout(title= 'Geo-locations cc and mb')



###########################################################################
# Hexagonal plot

Abila_hex <- st_read(dsn = paste0(working_dir,"geo2"), layer = "hex_final")


# === mbdata ===
all_mb <- merged_final %>% filter(type=="mbdata")

intersection2<-st_set_geometry(st_intersection(all_mb,Abila_hex), NULL)

intersection2<-left_join(intersection2,Abila_hex,by=c('left','bottom','right','top'))

#count number of posts in each polygon
intersection2<-intersection2 %>% group_by(left,bottom,right,top) %>% 
  mutate(count = n())

intersection2<-st_as_sf(intersection2)

tmap_mode("view")
tm_shape(Abila_hex)+
  tm_polygons()+
  tm_shape(Abila_st)+
  tm_lines()+
  tm_shape(intersection2)+
  tm_polygons(col='count')



# === ccdata ===
all_cc <- merged_final %>% filter(type=="ccdata")

intersection3<-st_set_geometry(st_intersection(all_cc,Abila_hex), NULL)

intersection3<-left_join(intersection3,Abila_hex,by=c('left','bottom','right','top'))

#count number of posts in each polygon
intersection3<-intersection3 %>% group_by(left,bottom,right,top) %>% 
  mutate(count = n())

intersection3<-st_as_sf(intersection3)

tmap_mode("view")
tm_shape(Abila_hex)+
  tm_polygons()+
  tm_shape(Abila_st)+
  tm_lines()+
  tm_shape(intersection3)+
  tm_polygons(col='count')






intersection3<-st_set_geometry(st_intersection(merged_final,Abila_hex), NULL)

intersection3<-left_join(intersection3,Abila_hex,by=c('left','bottom','right','top'))

#count number of posts in each polygon
intersection3<-intersection3 %>% group_by(left,bottom,right,top) %>% 
  mutate(count = n())

intersection3<-st_as_sf(intersection3)

tmap_mode("view")
tm_shape(Abila_hex)+
  tm_polygons()+
  tm_shape(Abila_st)+
  tm_lines()+
  tm_shape(intersection3)+
  tm_polygons(col='count')





