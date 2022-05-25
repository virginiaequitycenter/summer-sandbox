library(tidyverse)
library(osmdata)
library(sf)
library(Rcpp)
library(ggmap)
library(mapview)
library(leaflet)

# This script largely copies Michele's code in "osmdata.R" to try and produce 
# a data frame with locations of interest in the Charlottesville region like:
# schools
# parks
# college or university -- could be good 
# library -- YES
# kindergarten -- YES (For children too young for a regular school (also known as preschool, playschool or nursery school), in some countries including afternoon supervision of primary school children.)
# Health care options: clinic (A medium-sized medical facility or health center.), 
# doctors (A doctor's practice / surgery.), dentist (A dentist practice / surgery.),
# hospital (A hospital providing in-patient medical treatment. Often used in conjunction with emergency=* to note whether the medical centre has emergency facilities (A&E (brit.) or ER (am.))),
# nursing_home -- NO
# social_facility -- NO (area A facility that provides social services: group & nursing homes, workshops for the disabled, homeless shelters, etc.)
# courthouse -- NO
# bus_station -- YES, but we have other sources 
# fire_station or police -- YES
# internet_cafe -- YES 


# define bounding box via county shape files
cville_bounds <- readRDS("../spatial_units/data/cville_tracts.RDS")
cville_bbox <- st_bbox(cville_bounds)


### Schools ----------------------
region_schools <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "school") %>% 
  osmdata_sf()

schools1 <- region_schools$osm_points[!is.na(region_schools$osm_points$name),] 
schools2 <- region_schools$osm_polygons[!is.na(region_schools$osm_polygons$name),] 
schools3 <- region_schools$osm_multipolygons[!is.na(region_schools$osm_multipolygons$name),] 
## It doesn't look like schools3 has any schools we actually want to include--one is a military academy,
# one is in Goochland, and one is "Parent Resource Center Piedmont Regional Education Program"

# align crs
# cville_bounds is 4269; schools is 4326
cville_bounds <- sf::st_transform(cville_bounds, 4326)

## I wonder what the best way is the converge the polygons and points--there appears to be some
# redundancy across both (i.e., points within polygons) ... perhaps take the centroid of the polygons?

# remove entities ouside of county bounds
schools1_bounds <- st_intersection(schools1, cville_bounds)
schools2_bounds <- st_intersection(schools2, cville_bounds)

# Trying to combine them
schools1_bounds <- schools1_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) %>%
  select(-addr.unit)

schools2_bounds <- schools2_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) %>% 
  select(-access)

schools <- rbind(schools1_bounds, schools2_bounds)

## Ideas: could try to tag some as elementary, high, etc. 
# maybe figure out a way to distinguish public and private?

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(schools, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(schools, "POLYGON"), color = "orange")


### Schools ----------------------
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

# parks 1 has a lot of "districts"? Not sure what those are
# In the map, there are a lot of obvious green spaces that aren't 
# covered by a polygon or dot

# remove entities outside of county bounds
parks1_bounds <- st_intersection(parks1, cville_bounds)
parks2_bounds <- st_intersection(parks2, cville_bounds)
parks3_bounds <- st_intersection(st_make_valid(parks3), cville_bounds)

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

### Libraries ----------------------

# Cville libraries map
region_libraries <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "library") %>% 
  osmdata_sf()

libraries1 <- region_libraries$osm_points[!is.na(region_libraries$osm_points$name),] 
libraries2 <- region_libraries$osm_polygons[!is.na(region_libraries$osm_polygons$name),] 
libraries3 <- region_libraries$osm_multipolygons[!is.na(region_libraries$osm_multipolygons$name),] 

# align crs
# cville_bounds is 4269; schools is 4326
cville_bounds <- sf::st_transform(cville_bounds, 4326)

# remove entities outside of county bounds
libraries1_bounds <- st_intersection(libraries1, cville_bounds)
libraries2_bounds <- st_intersection(libraries2, cville_bounds)
libraries3_bounds <- st_intersection(st_make_valid(libraries3), cville_bounds)

# how to combine the three?

# Trying to combine them
libraries1_bounds <- libraries1_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry)

libraries2_bounds <- libraries2_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) 

libraries3_bounds <- libraries3_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) 

libraries <- rbind(libraries1_bounds, libraries2_bounds, libraries3_bounds)

libraries1_bounds <- libraries1_bounds %>% 
  select(osm_id:addr.street, ele, STATEFP:geometry) 

libraries2_bounds <- libraries2_bounds %>% 
  select(osm_id, name, addr.city:addr.street, ele, STATEFP:geometry) %>% 
  select(-addr.country)

libraries3_bounds <- libraries3_bounds %>% 
  mutate(ele = NA_character_,
         addr.city = NA_character_,
         addr.housenumber = NA_character_,
         addr.postcode = NA_character_,
         addr.state = NA_character_,
         addr.street = NA_character_) %>% 
  select(osm_id, name, addr.city:addr.street, ele, STATEFP:geometry)

libraries <- rbind(libraries1_bounds, libraries2_bounds, libraries3_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(libraries, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(libraries, "POLYGON"), color = "orange")



