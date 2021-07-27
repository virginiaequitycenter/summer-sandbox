library(tidyverse)
library(jsonlite)
library(sf)
library(leaflet)

# get spatial extent for api query
eastshore_tracts <- readRDS("~/DemofData/summer-sandbox/eastern_shore_collection/data/eastshore_tracts.RDS")
st_bbox(eastshore_tracts)

# api query (add the xmin/xmax, ymin/ymax to query)
eastern_path <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/Store_Locations/FeatureServer/0/query?where=Longitude%20%3E%3D%20-76.23646%20AND%20Longitude%20%3C%3D%20-75.16643%20AND%20Latitude%20%3E%3D%2036.99071%20AND%20Latitude%20%3C%3D%2038.02793&outFields=*&outSR=4326&f=json"

# Retrieve data
easternstores_json <- fromJSON(eastern_path)

# Extract data frame from list
easternstores <- easternstores_json$features$attributes

# make it an SF object, assign CRS
easternstores_4326 <- st_as_sf(easternstores,
                        coords = c("Longitude", "Latitude"),
                        crs = 4326)

# Filter to main counties
easternCounties <- c("ACCOMACK", "NORTHAMPTON")

easternstores <- easternstores %>% 
  filter(County %in% easternCounties)

easternstores_4326 <- easternstores_4326 %>% 
  filter(County %in% easternCounties)


# Manually filtering to only grocery stores, supermarkets 
# not convenience stories, pharmacies, dollar stores, warehouse clubs, etc.

write_csv(easternstores, path = "rawEasternStores.csv")   # just so i can split screen / make counting easier

included <- easternstores[c(1, 14:15, 17, 19:20, 33:34, 39:40, 46, 58:59, 65), ] 
#14: mexican grocery store
#15: el crucero-- fresh produce so including
#34: general store but sells produce
#65: including bc supermarket in the name, but store doesn't seem to exist? doesn't come up in google search (even when searching address)

excluded <- easternstores[c(2:13, 16, 18, 21:32, 35:38, 41:45, 47:57, 60:64), ] 
#20: seafood only
#23: el romelino-- general contracters? don't think they sell food of any kind
#28: seafood only
#42: ^


# Reproducible Method

storeNames <- dplyr::pull(easternstores, Store_Name) # creating vector to use in str_detect

keyWords <- c("Enterprises|Markets|Dollar|Deli|Royal|Remolino|Variety|Seafood|
              |Roses|Casa|Shore|Convenience|CVS|Goose|Walgreens|E & C")

snap_eastern <- easternstores %>% 
  filter(str_detect(storeNames, regex(keyWords, ignore_case = T), negate = T))

snap_eastern[!snap_eastern$Store_Name%in%included$Store_Name,] # looks good :)

# Save csv

write_csv(snap_eastern, path = "snap_eastern.csv")


# Map
lon <- pull(snap_eastern, Longitude)
lat <- pull(snap_eastern, Latitude)

PopUpInfo <- paste0(snap_eastern$Store_Name, "<br>",
                    snap_eastern$Address, "<br>",
                    snap_eastern$City, ", ", snap_eastern$State, " ", snap_eastern$Zip5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  setView(lng = mean(lon), lat = mean(lat), zoom = 9) %>% 
  addMarkers(lng = lon, lat = lat, popup = PopUpInfo,
             clusterOptions = markerClusterOptions())