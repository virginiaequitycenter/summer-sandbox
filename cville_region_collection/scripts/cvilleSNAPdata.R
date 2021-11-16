# Get Low-Income Energy Affordability 2018/Energy Burden
# Khalila Karefa-Kargbo, Michele Claibourn
# Created: 2021-07
# Last updated: 2021-11-16

library(tidyverse)
library(jsonlite)
library(sf)

# get spatial extent for api query
cville_tracts <- readRDS("data/cville_tracts.RDS")
st_bbox(cville_tracts)

# api query (add the xmin/xmax, ymin/ymax from st_bbox to query)
full_path <- "https://services1.arcgis.com/RLQu0rK7h4kbsBq5/arcgis/rest/services/Store_Locations/FeatureServer/0/query?where=Longitude%20%3E%3D%20-79.17213%20AND%20Longitude%20%3C%3D%20-77.68729%20AND%20Latitude%20%3E%3D%2037.53564%20AND%20Latitude%20%3C%3D%2038.47553&outFields=*&outSR=4326&f=json"

# Retrieve data
stores_json <- fromJSON(full_path)

# Extract data frame from list
stores <- stores_json$features$attributes

# Filter to main counties
cvilleCounties <- c("CHARLOTTESVILLE", "ALBEMARLE", "LOUISA", "NELSON", "GREENE", "FLUVANNA")

stores <- stores %>%
  filter(County %in% cvilleCounties)

# Distinguish between food retailer types
# E.g., https://www.ers.usda.gov/webdocs/publications/85442/eib-180.pdf
# (1) large stores comprised of supermarkets, supercenters, large grocery stores, and club stores (“large grocery stores”)
# (2) small grocery and specialty stores such as seafood markets, bakeries, and ethnic grocery stores (“small grocery stores”);
# (3) convenience stores, gas stations, pharmacies, and dollar stores (“convenience stores”)
# USDA has used (1) as a proxy for healthy and affordable food retailers

large_grocery <- c("aldi|costco|valu|lion|giant|teeter|kroger|lidl|target|thumb|trader|walmart|wegmans|whole")
convenience <- c("quik|exxon|eleven|mini|kwik|circle|cvs|dollar|pit|fas|easy|quick|fuel|sheetz|speedway|walgreens|wawa|mini|stop|pac|lots|the market|the maket")

stores <- stores %>% 
  mutate(type = case_when(
    str_detect(Store_Name, regex(large_grocery, ignore_case = T), negate = F) ~ "large_grocery",
    str_detect(Store_Name, regex(convenience, ignore_case = T), negate = F) ~ "convenience",
    TRUE ~ "small_grocery"
  ))

stores %>% count(type)

# save csv 
write_csv(stores, path = "data/food_retail_cville.csv")
