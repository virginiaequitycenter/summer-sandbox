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

# Filter to main counties
cvilleCounties <- c("CHARLOTTESVILLE", "ALBEMARLE", "LOUISA", "NELSON", "GREENE", "FLUVANNA")

stores_4326 <- stores_4326 %>% 
  filter(County %in% cvilleCounties)

stores <- stores %>% 
  filter(County %in% cvilleCounties)

# Manually filtering to only grocery stores, supermarkets 
# not convenience stories, pharmacies, dollar stores, warehouse clubs, etc.

write_csv(stores_4326, path = "rawCvilleStores.csv")   # split screen / make counting easier

included <- stores_4326[c(4, 6, 10:13, 19:20, 33:34, 44:45, 47:49, 52:53, 56:58,
                          65, 74, 76, 78, 80, 84, 88:89, 92, 94, 101, 105:107, 
                          111:112, 114:115, 117, 125, 127, 144, 146, 156, 161:163,
                          165, 167, 169), ] 
#20: international food grocery stores so may not fit criteria of "wide variety" of healthy foods, but does for some people so going to include
#44: african market place ^
#49: Rebecca's Natural Food-- definitely not a large grocery store but it does provide healthy food options?
#57: integral yoga-- same issue ^
#65: asian market-- int'l food...
#112: La Guadalupana ^
#127: indian bazar-- int'l food
#161: latin market...
#169: indian & nepali

excluded <- stores_4326[c(1:3, 5, 7:9, 14:18, 21:32, 35:43, 46, 50:51, 54:55, 59:64,
                          66:73, 75, 77, 79, 81:83, 85:87, 90:91, 93, 95:100, 102:104,
                          108:110, 113, 116, 118:124, 126, 128:143, 145, 147:155,
                          157:160, 164, 166, 168, 170:171), ]
#16: scottsville farmers market only open 9am-1pm on saturdays -- not accessible
#28: bakery
#38: greene farmers market only open sat 8am-12pm -- not accessible
#103: market street market-- seems niche: cheeses and beers
#117: aghan grand market-- int'l food but seems too small to actually be very useful
      # exclude african market on same premise?
#118: Mineral Farmer's Market -- inaccessible hours
#149: big lots-- no fresh produce
#151: market central-- Saturdays 8:00 am-12:00 pm .. not accessible

# if not mentioned by row specifically, it fell into category of convenience, dollar store, gas station, etc.


# Reproducible method

keyWords <- c("Lion|Kroger|Whole Foods|Reid|Hilltop|Tio|Depot|African|Trader|Natural|
              |Millers|Teeter|Yoga|Valu|Asian|Aldi|Medina|Target|Blue|Nx|Thomas INC|
              |Caul’s|Guadalupana|Mercado|Oriental|Walmart|Indian|Giant|Latino|Lidl|
              |amores|Wegmans|Express Grocery|Caul")
# to pull from included vector

storeNames <- dplyr::pull(stores, Store_Name) # creating vector to use in str_detect

snap_cville <- stores %>% 
  filter(str_detect(storeNames, regex(keyWords, ignore_case = T), negate = F))

snap_cville[!snap_cville$Store_Name%in%included$Store_Name,] # looks good :)

# save csv 
write_csv(snap_cville, path = "snap_cville.csv")

# Map
lon <- pull(snap_cville, Longitude)
lat <- pull(snap_cville, Latitude)

PopUpInfo <- paste0(snap_cville$Store_Name, "<br>",
                    snap_cville$Address, "<br>",
                    snap_cville$City, ", ", snap_cville$State, " ", snap_cville$Zip5)

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  setView(lng = mean(lon), lat = mean(lat), zoom = 8) %>% 
  addMarkers(lng = lon, lat = lat, popup = PopUpInfo,
  clusterOptions = markerClusterOptions())

# not sure about clusterOptions argument because I worry it kind of takes away from the data for this region

