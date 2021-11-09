# Location Affordability Index

library(tidyverse)
library(leaflet)
library(sf)

lai <- read_csv("/Users/marisalemma/Desktop/Equity_Center/Location_Affordability_Index_v.3.csv")
cvillefips <- c("003", "065", "079", "109", "125", "540")

va <- lai %>% 
  filter(STATE=="51")
cville <- va %>% 
  filter(COUNTY%in%cvillefips)

lai_cville <- cville %>% 
  select(GEOID:CNTY_FIPS, contains(c("hh1", "hh3")))

write.csv(lai_cville, "cville_region_collection/data//locationaffordability_cville_tract.csv", row.names = F)
