library(tidyverse)
library(jsonlite)
library(sf)
library(leaflet)

# get spatial extent for api query
cville_tracts <- readRDS("../spatial_units/data/cville_tracts.RDS")
st_bbox(cville_tracts)

# api query (add the xmin/xmax, ymin/ymax to query)
full_path <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/Store_Locations/FeatureServer/0/query?where=Longitude%20%3E%3D%20-79.17213%20AND%20Longitude%20%3C%3D%20-77.68729%20AND%20Latitude%20%3E%3D%2037.53564%20AND%20Latitude%20%3C%3D%2038.47553&outFields=*&outSR=4326&f=json"

# Retrieve data
stores_json <- fromJSON(full_path)

# Extract data frame from list
stores <- stores_json$features$attributes

# make it an SF object, assign CRS
stores_4326 <- st_as_sf(stores,
                        coords = c("Longitude", "Latitude"),
                        crs = 4326)

# From these, we want only grocery stores, supermarkets
# not convenience stories, pharmacies, dollar stores, warehouse clubs
# so need to devise a mostly reproducible way of identifying these from the list

# Then, we can map these as points on a map (e.g., with leaflet and some popup/hover info)

# And repeat for the Eastern Shore