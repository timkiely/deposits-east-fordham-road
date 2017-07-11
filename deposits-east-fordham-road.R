

# the modern geo-spatial r toolbox:
library(tidyverse)
library(sf)
library(mapedit)
library(mapview)




# data from: https://cdr.ffiec.gov/public/
NYC_FDIC <- read_rds("data/NYC_FDIC_data_20170511.rds")

# pair down to just important variables
nyc_fdic <- 
  NYC_FDIC %>% 
  select(year,uninumbr,namefull,addresbr,citybr,cntynamb
         ,stalpbr,zipbr,depsumbr,city2br,namebr,stnamebr
         ,"lat" = sims_latitude,"lon" = sims_longitude)

fdic_sf <-
  nyc_fdic %>% 
  mutate(lat = as.numeric(lat), lon = as.numeric(lon)) %>% 
  filter(!is.na(lat)) %>% 
  st_as_sf(coords = c("lon","lat"), crs = 4326, na.fail = F)


# view all banks in the Bronx
bronx_banks <-
  fdic_sf %>% 
  filter(year==2016) %>% 
  filter(cntynamb=="Bronx") %>% 
  filter(stnamebr=="New York") 

bronx_banks %>% 
  as("Spatial") %>%
  leaflet() %>% 
  addTiles() %>% 
  addMarkers(popup = ~addresbr)


# manually select the corridor with editmap
e_fordham_road_shapefile<-
  bronx_banks %>% 
  leaflet() %>% 
  addTiles() %>% 
  addMarkers() %>% 
  editMap()

# extract the line
e_fordham_road_sf <- e_fordham_road_shapefile$finished

# show the line
e_fordham_road_sf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines()

# buffer the line
e_fordham_buffer<- 
  e_fordham_road_sf %>% 
  st_transform(crs = 32618) %>% 
  st_buffer(50) %>% 
  st_transform(crs = 4326) 

set_view_coords <- 
e_fordham_buffer$geometry %>% 
  st_transform(4326) %>%
  st_centroid() %>% 
  st_coordinates() %>% 
  as_data_frame()

# show the corridor
e_fordham_buffer %>% 
  st_transform(4326) %>%
  leaflet() %>% 
  addTiles() %>% 
  setView(lng = set_view_coords$X, lat = set_view_coords$Y, zoom = 16) %>% 
  addPolylines(data=e_fordham_road_sf, color="green") %>% 
  addPolygons() %>% 
  addMarkers(data = bronx_banks)



# filter for banks from the original dataset
contain_vec st_contains(e_fordham_buffer,fdic_sf)




