# Libraries
packages= c('tidyverse','sp','sf','mapview','tmap')

for(p in packages){
  if(!require(p,character.only= T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#load map
Abila_st <- st_read(dsn = "data/Geospatial", layer = "Abila")
Abila_hex <- st_read(dsn = "geo2", layer = "hex_final")

#mapview npts clean
p = npts(Abila_st, by_feature = TRUE)

Abila_st <- cbind(Abila_st, p) %>%
  filter(p>1) %>% 
  mutate(tract_area = st_area(geometry))

#filter location points from data file
location_test<-data %>% filter(latitude!="")

location_test<-st_as_sf(location_test,
         coords = c("longitude","latitude"),
         crs= 4326)


# ues st_intersection to find conjucntions of polygon and points
intersection2<-st_set_geometry(st_intersection(location_test,Abila_hex), NULL)
#join Abila_hexfile
intersection2<-left_join(intersection2,Abila_hex,by=c('left','bottom','right','top'))
#count number of posts in each polygon
intersection2<-intersection2 %>% group_by(left,bottom,right,top) %>% 
  mutate(count = n())


#trnasfer to sf file for tmap to plot
intersection2<-st_as_sf(intersection2)

tmap_mode("view")

tm_shape(Abila_hex)+
  tm_polygons()+
tm_shape(Abila_st)+
  tm_lines()+
  tm_shape(intersection2)+
  tm_polygons(col='count')


