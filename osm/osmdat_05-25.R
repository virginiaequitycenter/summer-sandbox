library(tidyverse)
library(osmdata)
library(sf)
library(Rcpp)
library(ggmap)
library(mapview)
library(leaflet)

## Open street map amenity key: https://wiki.openstreetmap.org/wiki/Key:amenity

# This script largely copies Michele's code in "osmdata.R" to try and produce 
# a data frame with locations of interest in the Charlottesville region like:
# schools -- pulls 91
# parks -- pulls 87
# library -- pulls 15
# college -- pulls 2 (but one of them is a building at UVA)
# university -- maybe (basically just pulls multiple obs that are all just UVA)
# kindergarten (For children too young for a regular school (also known as preschool, playschool or nursery school) -- only pulls 2
# Health care options: 
# clinic (A medium-sized medical facility or health center.) -- pulls 18, concentrated in Cville
# doctors (A doctor's practice / surgery.) -- pulls a whopping 0 
# dentist (A dentist practice / surgery.) -- pulls 5 
# hospital (A hospital providing in-patient medical treatment) -- pulls 4
# bus_station -- pulls 1 
# fire_station -- pulls 48
# police -- pulls 10 
# internet_cafe -- pulls 0 

# define bounding box via county shape files
cville_bounds <- readRDS("../spatial_units/data/cville_tracts.RDS")
cville_bbox <- st_bbox(cville_bounds)

# align crs
cville_bounds <- sf::st_transform(cville_bounds, 4326)

### 1. Schools ----------------------------------------------------------------------------------------
region_schools <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "school") %>% 
  osmdata_sf()

schools1 <- region_schools$osm_points[!is.na(region_schools$osm_points$name),] 
schools2 <- region_schools$osm_polygons[!is.na(region_schools$osm_polygons$name),] 
schools3 <- region_schools$osm_multipolygons[!is.na(region_schools$osm_multipolygons$name),] 
## It doesn't look like schools3 has any schools we actually want to include--one is a military academy,
# one is in Goochland, and one is "Parent Resource Center Piedmont Regional Education Program"

# remove entities outside of county bounds
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

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(schools, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(schools, "POLYGON"), color = "orange")

## Notes/questions about schools:
## I wonder what the best way is the converge the polygons and points--there appears to be some
# redundancy across both (i.e., points within polygons) ... perhaps take the centroid of the polygons?
## Ideas: could try to tag some as elementary, high, etc. 
# maybe figure out a way to distinguish public and private?

### 2. Parks ----------------------------------------------------------------------------------------
# Cville parks map
region_parks <- opq(cville_bbox) %>% 
  add_osm_feature(key = "leisure", value = "park") %>% 
  osmdata_sf()

parks1 <- region_parks$osm_points[!is.na(region_parks$osm_points$name),] 
parks2 <- region_parks$osm_polygons[!is.na(region_parks$osm_polygons$name),] 
parks3 <- region_parks$osm_multipolygons[!is.na(region_parks$osm_multipolygons$name),] 

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
  addCircles(data = st_collection_extract(parks, "POINT"), color = "blue",
             popup = st_collection_extract(parks, "POINT")$name) %>% 
  addPolygons(data = st_collection_extract(parks, "POLYGON"), color = "orange",
              popup = st_collection_extract(parks, "POLYGON")$name)

## Notes/questions about parks:
# parks 1 has a lot of "districts"? Not sure what those are
# In the map, there are a lot of obvious green spaces that aren't 
# covered by a polygon or dot

### 3. Libraries ----------------------------------------------------------------------------------------

# Cville libraries map
region_libraries <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "library") %>% 
  osmdata_sf()

libraries1 <- region_libraries$osm_points[!is.na(region_libraries$osm_points$name),] 
libraries2 <- region_libraries$osm_polygons[!is.na(region_libraries$osm_polygons$name),] 
libraries3 <- region_libraries$osm_multipolygons[!is.na(region_libraries$osm_multipolygons$name),] 

# remove entities outside of county bounds
libraries1_bounds <- st_intersection(libraries1, cville_bounds)
libraries2_bounds <- st_intersection(libraries2, cville_bounds)
libraries3_bounds <- st_intersection(st_make_valid(libraries3), cville_bounds)

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

## Notes/questions about libraries:
# Certainly there are more than 15 libraries in the region.... 

### 4. Colleges ----------------------------------------------------------------------------------------
region_college <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "college") %>% 
  osmdata_sf()

college1 <- region_college$osm_points[!is.na(region_college$osm_points$name),] 
college2 <- region_college$osm_polygons[!is.na(region_college$osm_polygons$name),] 
college3 <- region_college$osm_multipolygons[!is.na(region_college$osm_multipolygons$name),] 

# remove entities outside of county bounds
college1_bounds <- st_intersection(college1, cville_bounds)
college2_bounds <- st_intersection(college2, cville_bounds)
college3_bounds <- st_intersection(college3, cville_bounds)

# Trying to combine them
college1_bounds <- college1_bounds %>% 
  mutate(ele = NA_character_,
         addr.city = NA_character_,
         addr.housenumber = NA_character_,
         addr.postcode = NA_character_,
         addr.state = NA_character_,
         addr.street = NA_character_) %>% 
  select(osm_id, name, addr.city:addr.street, ele, STATEFP:geometry)

college2_bounds <- college2_bounds %>% 
  select(osm_id, name, addr.city:addr.street, ele, STATEFP:geometry) %>% 
  select(-addr.country, -addr.housename)

college <- rbind(college1_bounds, college2_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(college, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(college, "POLYGON"), color = "orange")

## Notes/questions about colleges:
# It only pulls Brown college (which is just a building at UVA) and Piedmont Virginia Community College

### 5. Universities ----------------------------------------------------------------------------------------
region_university <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "university") %>% 
  osmdata_sf()

university1 <- region_university$osm_points[!is.na(region_university$osm_points$name),] 
university2 <- region_university$osm_polygons[!is.na(region_university$osm_polygons$name),] 
university3 <- region_university$osm_multipolygons[!is.na(region_university$osm_multipolygons$name),] 

# remove entities outside of county bounds
university1_bounds <- st_intersection(university1, cville_bounds)
university2_bounds <- st_intersection(university2, cville_bounds)
university3_bounds <- st_intersection(university3, cville_bounds)

## Notes/questions about universities:
# All three chunks only include UVA-operated institutions. For example, Darden and the law school are listed separately. 
# International Residential College is listed as being operated by UVA. I'm not sure this is worth having. 

### 6. Kindergarten ----------------------------------------------------------------------------------------
region_kindergarten <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "kindergarten") %>% 
  osmdata_sf()

kindergarten1 <- region_kindergarten$osm_points[!is.na(region_kindergarten$osm_points$name),] 
kindergarten2 <- region_kindergarten$osm_polygons[!is.na(region_kindergarten$osm_polygons$name),] 
# kindergarten3 <- region_kindergarten$osm_multipolygons[!is.na(region_kindergarten$osm_multipolygons$name),] 
# last one is null

# remove entities outside of county bounds
kindergarten1_bounds <- st_intersection(kindergarten1, cville_bounds)
kindergarten2_bounds <- st_intersection(kindergarten2, cville_bounds)

## Notes/questions about kindergartens:
# only pulls 2 in the whole region -- Bright Beginnings and KinderCare (both in Charlottesville City) 


### 7. clinics ----------------------------------------------------------------------------------------
region_clinic <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "clinic") %>% 
  osmdata_sf()

clinic1 <- region_clinic$osm_points[!is.na(region_clinic$osm_points$name),] 
clinic2 <- region_clinic$osm_polygons[!is.na(region_clinic$osm_polygons$name),] 
# clinic3 <- region_clinic$osm_multipolygons[!is.na(region_clinic$osm_multipolygons$name),] 
# last one is null 

# remove entities outside of county bounds
clinic1_bounds <- st_intersection(clinic1, cville_bounds)
clinic2_bounds <- st_intersection(clinic2, cville_bounds)

# Trying to combine them
clinic1_bounds <- clinic1_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) %>%
  select(-addr.unit)

clinic2_bounds <- clinic2_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry)

clinic <- rbind(clinic1_bounds, clinic2_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(clinic, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(clinic, "POLYGON"), color = "orange")

## Notes/questions about clinics 
# Once again, this surely isn't all of the clinics, but it seems to have more than some of the other amenities. 

### 8. doctors ----------------------------------------------------------------------------------------
region_doctor <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "doctor") %>% 
  osmdata_sf()

# doctor1 <- region_doctor$osm_points[!is.na(region_doctor$osm_points$name),] 
# doctor2 <- region_doctor$osm_polygons[!is.na(region_doctor$osm_polygons$name),] 
# doctor3 <- region_doctor$osm_multipolygons[!is.na(region_doctor$osm_multipolygons$name),] 

# All three are empty! Looks like no doctors 

### 9. dentists ----------------------------------------------------------------------------------------
region_dentist <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "dentist") %>% 
  osmdata_sf()

dentist1 <- region_dentist$osm_points[!is.na(region_dentist$osm_points$name),] 
dentist2 <- region_dentist$osm_polygons[!is.na(region_dentist$osm_polygons$name),] 
# dentist3 <- region_dentist$osm_multipolygons[!is.na(region_dentist$osm_multipolygons$name),] 
# last one is null 

# remove entities outside of county bounds
dentist1_bounds <- st_intersection(dentist1, cville_bounds)
# dentist2_bounds <- st_intersection(dentist2, cville_bounds)
# All dentist 2 is out of bounds

# Trying to combine them
dentist1_bounds <- dentist1_bounds %>% 
  select(osm_id:amenity, STATEFP:geometry) %>%
  select(-addr.country, -addr.unit)

# dentist2_bounds <- dentist2_bounds %>% 
#   select(osm_id:amenity, STATEFP:geometry)

dentist <- dentist1_bounds

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(dentist, "POINT"), color = "blue") 

## Notes/questions about dentists
# only pulls 5. My dentist isn't on here. There's also a place called Affordable Dentures which I'm not sure if 
# it's even a dentist, or just a place that sells dentures. 

### 10. hospitals ----------------------------------------------------------------------------------------
region_hospital <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "hospital") %>% 
  osmdata_sf()

hospital1 <- region_hospital$osm_points[!is.na(region_hospital$osm_points$name),] 
hospital2 <- region_hospital$osm_polygons[!is.na(region_hospital$osm_polygons$name),] 
# hospital3 <- region_hospital$osm_multipolygons[!is.na(region_hospital$osm_multipolygons$name),] 
# last one is null 

# remove entities outside of county bounds
hospital1_bounds <- st_intersection(hospital1, cville_bounds)
hospital2_bounds <- st_intersection(hospital2, cville_bounds)

# Trying to combine them
hospital1_bounds <- hospital1_bounds %>% 
  select(osm_id:amenity, STATEFP:geometry) 

hospital2_bounds <- hospital2_bounds %>% 
  select(osm_id:amenity, STATEFP:geometry)

hospital <- rbind(hospital1_bounds, hospital2_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(hospital, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(hospital, "POLYGON"), color = "orange")

## Notes/questions about hospitals 
# Pulls 6, but 3 of them are all the same. Wind up with just UVA main hospital, Martha Jefferson, UVA HealthSouth Rehabilitation Hospital, 
# and Sentara Crozet Family Medicine

### 11. bus_station ----------------------------------------------------------------------------------------
region_bus_station <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "bus_station") %>% 
  osmdata_sf()

# bus_station1 <- region_bus_station$osm_points[!is.na(region_bus_station$osm_points$name),] 
bus_station2 <- region_bus_station$osm_polygons[!is.na(region_bus_station$osm_polygons$name),] 
# bus_station3 <- region_bus_station$osm_multipolygons[!is.na(region_bus_station$osm_multipolygons$name),] 
# first one is empty, second only has 1 observation, last one is null

# Only pulls one bus station--obviously not the best source for this

### 12. fire_station ----------------------------------------------------------------------------------------
region_fire_station <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "fire_station") %>% 
  osmdata_sf()

fire_station1 <- region_fire_station$osm_points[!is.na(region_fire_station$osm_points$name),] 
fire_station2 <- region_fire_station$osm_polygons[!is.na(region_fire_station$osm_polygons$name),] 
# fire_station3 <- region_fire_station$osm_multipolygons[!is.na(region_fire_station$osm_multipolygons$name),] 
# last one is null 
s
# remove entities outside of county bounds
fire_station1_bounds <- st_intersection(fire_station1, cville_bounds)
fire_station2_bounds <- st_intersection(fire_station2, cville_bounds)

# Trying to combine them
fire_station1_bounds <- fire_station1_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) %>%
  select(-addr.county, -alt_amenity, -alt_name)

fire_station2_bounds <- fire_station2_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry)

fire_station <- rbind(fire_station1_bounds, fire_station2_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(fire_station, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(fire_station, "POLYGON"), color = "orange")

## Notes/questions about fire_stations 
# This actually seems much more reasonable/reliable at first glance. There are however remarkably few fire stations in Lovingston. 

### 13. police ----------------------------------------------------------------------------------------
region_police <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "police") %>% 
  osmdata_sf()

police1 <- region_police$osm_points[!is.na(region_police$osm_points$name),] 
police2 <- region_police$osm_polygons[!is.na(region_police$osm_polygons$name),] 
# police3 <- region_police$osm_multipolygons[!is.na(region_police$osm_multipolygons$name),] 
# last one is null 

# remove entities outside of county bounds
police1_bounds <- st_intersection(police1, cville_bounds)
police2_bounds <- st_intersection(police2, cville_bounds)

# Trying to combine them
police1_bounds <- police1_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry) %>%
  select(-addr.city, -addr.housenumber, -addr.postcode, -addr.street)

police2_bounds <- police2_bounds %>% 
  select(osm_id:amenity, ele, STATEFP:geometry)

police <- rbind(police1_bounds, police2_bounds)

# view
leaflet(cville_bounds) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1, fill = 0, color = "black") %>% 
  addCircles(data = st_collection_extract(police, "POINT"), color = "blue") %>% 
  addPolygons(data = st_collection_extract(police, "POLYGON"), color = "orange")

## Notes/questions about police
# Wind up with 10--can't possbily be all of them because most of them are concentrated in Cville
# Also a few appear to be redundant. There are two in Louisa that are right on top of each other -- Louisa Police Department and 
# Louisa County Sheriff's Office (I think) 

### 14. internet_cafe ----------------------------------------------------------------------------------------
region_internet_cafe <- opq(cville_bbox) %>% 
  add_osm_feature(key = "amenity", value = "internet_cafe") %>% 
  osmdata_sf()

# internet_cafe1 <- region_internet_cafe$osm_points[!is.na(region_internet_cafe$osm_points$name),] 
# internet_cafe2 <- region_internet_cafe$osm_polygons[!is.na(region_internet_cafe$osm_polygons$name),] 
# internet_cafe3 <- region_internet_cafe$osm_multipolygons[!is.na(region_internet_cafe$osm_multipolygons$name),] 

## All three are empty! 


