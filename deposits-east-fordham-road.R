

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
# (Not run)
# e_fordham_road_shapefile<-
#   bronx_banks %>%
#   leaflet() %>%
#   addTiles() %>%
#   addMarkers() %>%
#   editMap()

# extract the line
# e_fordham_road_sf <- e_fordham_road_shapefile$finished

# st_write(e_fordham_road_sf, "data/e_fordham_road_linestring.geojson",driver="GEOjson", delete_dsn = T)
e_fordham_road_sf <- st_read("E Fordham Road/data/e_fordham_road_linestring.geojson")

# show the line
e_fordham_road_sf %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolylines()

# buffer the line
e_fordham_buffer<- 
  e_fordham_road_sf %>% 
  st_transform(crs = 32618) %>% 
  st_buffer(25) %>% 
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
contain_vec <- st_contains(e_fordham_buffer,fdic_sf)[[1]]

# convert dollar character to numeric
fix_dollar <- function(x) as.numeric(stringr::str_replace(x,"$|,",""))


fordham_street <-
fdic_sf %>% 
  filter(row_number()%in%contain_vec) %>% 
  st_set_geometry(NULL) %>% 
  group_by(year) %>% 
  summarise(Fordham_Deposits = sum(fix_dollar(depsumbr),na.rm=T))
  

all_bronx <- 
fdic_sf %>% 
  filter(cntynamb=="Bronx") %>% 
  filter(stnamebr=="New York") %>% 
  st_set_geometry(NULL) %>% 
  group_by(year) %>% 
  summarise(Bronx_Deposits = sum(fix_dollar(depsumbr),na.rm=T))


compare_deposits <- left_join(fordham_street,all_bronx, by = "year")

percent_change <-
  compare_deposits %>% 
  filter(year%in%c(min(year),max(year))) %>% 
  mutate_if(.predicate=is.numeric,.funs = function(x) x = scales::percent((x - lag(x,1))/lag(x,1))) %>% 
  filter(year==max(year))

compare_deposits %>% 
  rename("All Bronx" = Bronx_Deposits, "E. Fordham Street" = Fordham_Deposits) %>% 
  gather(Var, Value, -year) %>% 
  ggplot()+
  aes(x = year, y = Value, group = Var, color = Var, fill = Var)+
  geom_line()+
  facet_wrap(~Var, ncol = 1, scales = "free_y")+
  theme_minimal()+
  ggthemes::scale_color_fivethirtyeight()+
  scale_y_continuous(labels = scales::comma)+
  labs(col=NULL
       , y = NULL
       , x = NULL)



