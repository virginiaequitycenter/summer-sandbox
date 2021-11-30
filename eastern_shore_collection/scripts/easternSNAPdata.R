# Get Low-Income Energy Affordability 2018/Energy Burden
# Khalila Karefa-Kargbo, Michele Claibourn
# Created: 2021-07
# Last updated: 2021-11-30

library(tidyverse)
library(jsonlite)
library(sf)

# get spatial extent for api query
eastshore_tracts <- readRDS("data/eastshore_tracts.RDS")
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

stores <- easternstores %>% 
  filter(County %in% easternCounties)

stores_4326 <- easternstores_4326 %>% 
  filter(County %in% easternCounties)


# Distinguish between food retailer types
# E.g., https://www.ers.usda.gov/webdocs/publications/85442/eib-180.pdf
# (1) large stores comprised of supermarkets, supercenters, large grocery stores, and club stores (“large grocery stores”)
# (2) small grocery and specialty stores such as seafood markets, bakeries, and ethnic grocery stores (“small grocery stores”);
# (3) convenience stores, gas stations, pharmacies, and dollar stores (“convenience stores”)
# USDA has used (1) as a proxy for healthy and affordable food retailers

large_grocery <- c("supermarket|valu|lion|walmart|iga")
convenience <- c("e&c|roses|dollar|walgreens|cvs|stop|royal|convenience|e & c")

stores <- stores %>% 
  mutate(type = case_when(
    str_detect(Store_Name, regex(large_grocery, ignore_case = T), negate = F) ~ "large_grocery",
    str_detect(Store_Name, regex(convenience, ignore_case = T), negate = F) ~ "convenience",
    TRUE ~ "small_grocery"
  ))

stores %>% count(type)

# Save csv
write_csv(stores, path = "data/food_retail_eastern.csv")
