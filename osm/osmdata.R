library(tidyverse)
library(osmdata)
library(sf)
library(Rcpp)
library(ggmap)
library(mapview)
library(leaflet)

# define bounding box via county shape files
cville_bounds <- readRDS("../spatial_units/data/cville_tracts.RDS")
cville_bbox <- st_bbox(cville_bounds)


# Schools ----
region_schools <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "school") %>% 
  osmdata_sf()

schools1 <- region_schools$osm_points[!is.na(region_schools$osm_points$name),] 
schools2 <- region_schools$osm_polygons[!is.na(region_schools$osm_polygons$name),] 
schools3 <- region_schools$osm_multipolygons[!is.na(region_schools$osm_multipolygons$name),] 

# align crs
# cville_bounds is 4269; schools is 4326
cville_bounds <- sf::st_transform(cville_bounds, 4326)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = schools1, color = "blue") %>% 
  addPolygons(data = schools2, color = "orange") %>% 
  addPolygons(data = schools3, color = "turquoise")

# remove entities ouside of county bounds
schools1_bounds <- st_intersection(schools1, cville_bounds)
schools2_bounds <- st_intersection(schools2, cville_bounds)
schools3_bounds <- st_intersection(schools3, cville_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = schools1_bounds, color = "blue") %>% 
  addPolygons(data = schools2_bounds, color = "orange") %>% 
  addPolygons(data = schools3_bounds, color = "turquoise")

# how to combine the three?
schools1_bounds <- schools1_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry)

schools2_bounds <- schools2_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) %>% 
  select(-access)

schools3_bounds <- schools3_bounds %>% 
  mutate(ele = NA_character_) %>% 
  select(osm_id:amenity, ele, STATEFP:geometry)

schools <- rbind(schools1_bounds, schools2_bounds, schools3_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(schools, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(schools, "POLYGON"), color = "orange")



# Parks ----
# Cville parks map
region_parks <- opq(cville_bbox) %>% 
  add_osm_feature(key = "leisure", value = "park") %>% 
  osmdata_sf()

parks1 <- region_parks$osm_points[!is.na(region_parks$osm_points$name),] 
parks2 <- region_parks$osm_polygons[!is.na(region_parks$osm_polygons$name),] 
parks3 <- region_parks$osm_multipolygons[!is.na(region_parks$osm_multipolygons$name),] 

# align crs
# cville_bounds is 4269; schools is 4326
cville_bounds <- sf::st_transform(cville_bounds, 4326)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = parks1, color = "blue") %>% 
  addPolygons(data = parks2, color = "orange") %>% 
  addPolygons(data = parks3, color = "turquoise")

# remove entities outside of county bounds
parks1_bounds <- st_intersection(parks1, cville_bounds)
parks2_bounds <- st_intersection(parks2, cville_bounds)
parks3_bounds <- st_intersection(st_make_valid(parks3), cville_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = parks1_bounds, color = "blue") %>% 
  addPolygons(data = st_collection_extract(parks2_bounds, "POLYGON"), color = "orange") %>% 
  addPolygons(data = parks3_bounds, color = "turquoise")

# how to combine the three?
parks1_bounds <- parks1_bounds %>% 
  select(osm_id:addr.street, ele, STATEFP:geometry) 

parks2_bounds <- parks2_bounds %>% 
  select(osm_id, name, addr.city:addr.street, ele, STATEFP:geometry) %>% 
  select(-addr.country)

parks3_bounds <- parks3_bounds %>% 
  mutate(ele = NA_character_,
         addr.city = NA_character_,
         addr.housenumber = NA_character_,
         addr.postcode = NA_character_,
         addr.state = NA_character_,
         addr.street = NA_character_) %>% 
  select(osm_id, name, addr.city:addr.street, ele, STATEFP:geometry)

parks <- rbind(parks1_bounds, parks2_bounds, parks3_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(parks, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(parks, "POLYGON"), color = "orange")



# Schools ----
# Albemarle schools map
alb_schools <- opq(getbb ("albemarle county usa")) %>% 
  add_osm_feature(key = "amenity", value = "school") %>% 
  osmdata_sf()

alb_schools_map <- get_map(getbb("albemarle county usa"), maptype = "roadmap")

ggmap(alb_schools_map) +
  geom_sf(data = alb_schools$osm_polygons,
          inherit.aes = F)

# Cville schools map
cville_schools <- opq(getbb("charlottesville usa")) %>% 
  add_osm_feature(key = "amenity", value = "school") %>% 
  osmdata_sf()

cville_schools_map <- get_map(getbb("charlottesville usa"), maptype = "roadmap")

ggmap(cville_schools_map) +
  geom_sf(data = cville_schools$osm_polygons,
          inherit.aes = F,
          color = "blue")
