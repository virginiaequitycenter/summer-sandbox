library(tidyverse)
library(jsonlite)
library(sf)
library(ggplot2)
library(ggmap)
library(leaflet)

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
#82: sam's club-- wholesale club
#103: market street market-- seems niche: cheeses and beers
#109: costco--wholesale club
#117: aghan grand market-- int'l food but seems too small to actually be very useful
      # exclude african market on same premise?
#118: Mineral Farmer's Market -- inaccessible hours
#149: big lots-- no fresh produce
#151: market central-- Saturdays 8:00 am-12:00 pm .. not accessible

# if not mentioned by row specifically, it fell into category of convenience, dollar store, gas station, etc.


# reproducible method
keyWords <- c("Lion|Kroger|Whole Foods|Reid|Hilltop|Tio|Depot|African|Trader|Natural|
              |Millers|Teeter|Yoga|Valu|Asian|Aldi|Medina|Target|Blue|Nx|Thomas INC|
              |Caul’s|Guadalupana|Mercado|Oriental|Walmart|Indian|Giant|Latino|Lidl|
              |amores|Wegmans|Express Grocery|Caul")

storeNames <- dplyr::pull(stores, Store_Name) # creating vector to use in str_detect

groceryOnly <- stores %>% 
  filter(str_detect(storeNames, regex(keyWords, ignore_case = T), negate = F))

groceryOnly[!groceryOnly$Store_Name%in%included$Store_Name,] # looks good :)


# Maps ... :(

# creating a sample data.frame with your lat/lon points
lon <- pull(groceryOnly, Longitude)
lat <- pull(groceryOnly, Latitude)
storeName <- pull(groceryOnly, Store_Name)
county <- pull(groceryOnly, County)

df <- as.data.frame(cbind(lon,lat))


# getting the map
mapgilbert <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 11,
                      maptype = "satellite")
view(mapgilbert)

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8))

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = df,
              fillColor = ~pal(storeNames),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("City: ", groceryOnly$City, "<br>",
                             "Name: ", groceryOnly$Store_Name)
  ) %>% 
  addLegend("bottomright", pal = pal, values = groceryOnly$Store_Name, 
            title = "# of Grocery Stores", opacity = 0.7)

