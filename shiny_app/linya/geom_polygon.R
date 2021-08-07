# Libraries
packages= c('tidyverse','sp','sf','mapview')

for(p in packages){
  if(!require(p,character.only= T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

#load map
Abila_st <- st_read(dsn = "data/Geospatial", layer = "Abila")

p = npts(Abila_st, by_feature = TRUE)

Abila_st <- cbind(Abila_st, p) %>%
  filter(p>1) %>% 
  mutate(tract_area = st_area(geometry))


ggplot()+
  geom_sf(data=Abila_st,size=0.2,color="black",fill="cyan1")





library(sp)
library(raster)
library(sf)
#devtools::install_github("hrbrmstr/overpass")
library(overpass)

query_airport <- '
(node["aeroway"="aerodrome"](50.8, -1.6,51.1, -1.1);
 way["aeroway"="aerodrome"](50.8, -1.6,51.1, -1.1);
 relation["aeroway"="aerodrome"](50.8, -1.6,51.1, -1.1);
); 
out body;
>;
out skel qt;
'
# Run query
shp_airports <- overpass::overpass_query(query_airport, quiet = TRUE)
crs(shp_airports) <- CRS("+init=epsg:4326")

sf_airports <- st_as_sf(shp_airports) 
sf_airports_polygons <- st_polygonize(sf_airports)
shp_airports <- as(sf_airports_polygons, "Spatial") # If you want sp
class(shp_airports)

plot(shp_airports, axes = T)
  