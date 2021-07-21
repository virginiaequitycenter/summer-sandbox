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

# Filter to main counties
easternCounties <- c("ACCOMACK", "NORTHAMPTON")

easternstores <- easternstores %>% 
  filter(County %in% easternCounties)

# Filtering to only grocery stores, supermarkets
# not convenience stories, pharmacies, dollar stores, warehouse clubs, etc.

write_csv(easternstores, path = "rawEasternStores.csv")   # split screen / make counting easier

included <- easternstores[c(1, 14:15, 17:19, ), ] 
#14: mexican grocery store
#15: el crucero-- fresh produce so including

excluded <- easternstores[c(2:13, 16, 20:), ] # continue from 23
#20: seafood only
