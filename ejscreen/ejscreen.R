# This is the 2020 EJSCREEN data

library(tidyverse)
library(leaflet)
library(sf)

# Load data
ejscreen <- read_csv("/Users/marisalemma/Downloads/EJSCREEN_2020_USPR.csv")

# Get rid of demographic variables (since we don't need those)
ejscreen_clean <- ejscreen %>% 
  select(-c("OBJECTID", "ACSTOTPOP":"PRE1960", "VULEOPCT":"DISPEO", "D_LDPNT_2":"D_PM25_2", 
            "ST_ABBREV":"P_OVR64PCT", "P_VULEOPCT", "P_LDPNT_D2":"B_VULEOPCT", "B_LDPNT_D2":"T_VULEOPCT", 
            "T_LDPNT_D2", "T_DSLPM_D2", "T_CANCR_D2", "T_RESP_D2", "T_PTRAF_D2", "T_PWDIS_D2", 
            "T_PNPL_D2", "T_PRMP_D2", "T_PTSDF_D2", "T_OZONE_D2", "T_PM25_D2", "Shape_Length", "Shape_Area"))

# Filter to just Cville area
virginia <- ejscreen_clean %>% 
  filter(STATE_NAME=="Virginia")
cville_area <- virginia %>% 
  filter(ID%in%510030101001:510030114004 |
           ID%in%510650201011:510650203003 |
           ID%in%510790301011:510790302004 |
           ID%in%511099501001:511099505003 |
           ID%in%511259501001:511259503004 |
           ID%in%515400002011:515400010003)


# Some spatial explorations
# Load shapefile
cville_blkgps <- readRDS("/Users/marisalemma/Desktop/Equity Center/summer-sandbox/cville_region_collection/data/cville_blkgps.RDS")
cville_area <- cville_area %>% rename(GEOID = ID)

cvilleshapes <- merge(cville_blkgps, cville_area, by = 'GEOID', all.x = T)
cvilleshapes <- st_transform(cvilleshapes, crs = 4326) # to WGS84, given error

# Spatial distribution for proximity to treatment storage and disposal facilities (PTSDF)
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvilleshapes$PTSDF)
leaflet(cvilleshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvilleshapes,
              fillColor = ~pal(PTSDF),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", cvilleshapes$GEOID, "<br>",
                             "Proximity to TSDF: ", cvilleshapes$T_PTSDF)) %>%
  addLegend("bottomright", pal = pal, values = cvilleshapes$PTSDF,
            title = "Proximity to TSDF", opacity = 0.7)


# Spatial distribution for proximity to traffic (PTRAF)
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvilleshapes$PTRAF)
leaflet(cvilleshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvilleshapes,
              fillColor = ~pal(PTRAF),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", cvilleshapes$GEOID, "<br>",
                             "Proximity to traffic: ", cvilleshapes$T_PTRAF)) %>%
  addLegend("bottomright", pal = pal, values = cvilleshapes$PTRAF,
            title = "Traffic Proximity", opacity = 0.7)


# PM2.5, ozone, and NATA indicators are measured at the census tract level
  # So each block group within that census tract is assigned the same value
  # That means this isn't super useful for a lot of these measures (especially PM2.5)

# Some questions that I have:
# What do the NAs represent? Are they 0s? 
  # NAs only exist for PTRAF and PWDIS
  # In the case of traffic proximity, would that mean that there are no roads in the block group?
  # I think NAs are probably 0s but I'm not completely sure


# Comparison to PM2.5 data
# Create tract variable
cville_area %>% 
  str_sub(GEOID, 1, 11)

# Load data
airquality <- read_csv("/Users/marisalemma/Desktop/Equity Center/summer-sandbox/cville_region_collection/data/airquality_cville_tract.csv")