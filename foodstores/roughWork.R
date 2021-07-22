library(tidyverse)
library(jsonlite)
library(sf)
library(ggplot2)
library(ggmap)
library(leaflet)


# Maps ... :(

# creating a sample data.frame with your lat/lon points
lon <- pull(groceryOnly, Longitude)
lat <- pull(groceryOnly, Latitude)

df <- as.data.frame(cbind(lon,lat))


# getting the map
map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 9,
                      maptype = "roadmap")
view(map)

# plotting the map with some points on it
ggmap(map) +
  geom_point(data = df, aes(x = lon, y = lat, col = "green3", fill = "green2", alpha = 0.8))


pal <- colorNumeric("plasma", reverse = TRUE, domain = groceryOnly$Longitude, groceryOnly$Latitude) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = map,
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
?popupOptions

PopUpInfo <- paste(groceryOnly[c("Store_Name", "Address", "City")], sep = "/n", collapse = "")

leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%  
  setView(lng = mean(lon), lat = mean(lat), zoom = 8) %>% 
  addMarkers(lng = lon, lat = lat, popup = groceryOnly$Store_Name) %>% 
  markerOptions(riseOnHover = T)

