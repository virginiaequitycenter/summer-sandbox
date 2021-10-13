# Exploring Location Affordability Index

library(tidyverse)
library(leaflet)
library(sf)

lai <- read_csv("/Users/marisalemma/Desktop/Equity_Center/Location_Affordability_Index_v.3.csv")
cvillefips <- c("003", "065", "079", "109", "125", "540")

va <- lai %>% 
  filter(STATE=="51")
cville <- va %>% 
  filter(COUNTY%in%cvillefips)

hh1 <- cville %>% 
  select(GEOID:CNTY_FIPS, contains("hh1"))

cville_tracts <- readRDS("/Users/marisalemma/Desktop/Equity_Center/summer-sandbox/cville_region_collection/data/cville_tracts.RDS")
cvlshapes <- merge(cville_tracts, cville, by = 'GEOID', all.x = T)
cvlshapes <- st_transform(cvlshapes, crs = 4326) # to WGS84, given error

# Maps by household models - ht is the most important variable
# Household 1: Median-Income Family
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh1_model_h_cost)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh1_model_h_cost),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Modeled Housing Cost: ", round(cvlshapes$hh1_model_h_cost, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh1_model_h_cost,
            title = "Modeled Housing Costs", opacity = 0.7)

pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh1_ht)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh1_ht),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Housing and Transportation Costs as % of Income: ", round(cvlshapes$hh1_ht, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh1_ht,
            title = "Modeled Housing and Transportation Costs", opacity = 0.7)

# Household 2: Very Low-Income Individual
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh2_model_h_cost)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh2_model_h_cost),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Modeled Housing Cost: ", round(cvlshapes$hh2_model_h_cost, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh2_model_h_cost,
            title = "Modeled Housing Costs", opacity = 0.7)

pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh2_ht)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh2_ht),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Housing and Transportation Costs as % of Income: ", round(cvlshapes$hh2_ht, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh2_ht,
            title = "Modeled Housing and Transportation Costs", opacity = 0.7)

# Household 3: Working Individual
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh3_model_h_cost)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh3_model_h_cost),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Modeled Housing Cost: ", round(cvlshapes$hh3_model_h_cost, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh3_model_h_cost,
            title = "Modeled Housing Costs", opacity = 0.7)

pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh3_ht)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh3_ht),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Housing and Transportation Costs as % of Income: ", round(cvlshapes$hh3_ht, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh3_ht,
            title = "Modeled Housing and Transportation Costs", opacity = 0.7)

# Household 4: Single Professional
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh4_model_h_cost)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh4_model_h_cost),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Modeled Housing Cost: ", round(cvlshapes$hh4_model_h_cost, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh4_model_h_cost,
            title = "Modeled Housing Costs", opacity = 0.7)

pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$hh4_ht)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(hh4_ht),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$GEOID, "<br>",
                             "Housing and Transportation Costs as % of Income: ", round(cvlshapes$hh4_ht, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$hh4_ht,
            title = "Modeled Housing and Transportation Costs", opacity = 0.7)

