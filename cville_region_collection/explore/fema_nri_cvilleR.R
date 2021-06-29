# Describing FEMA National Risk Index data
# Michele Claibourn
# 2021-06-24

# 0. Load libraries and data
library(tidyverse)
library(stargazer) # for summary table
library(janitor) # for tabyl
library(tigris) # for shapefiles
library(sf) # for spatial joins
library(leaflet) # for map
library(rcartocolor)
library(RColorBrewer)
library(viridis)

nri <- read_csv("data/fema_nri_cville_tract.csv")
cvillefips <- c("540", "003", "065", "079", "109", "125")


# 1. Data citation
# Source: FEMA, National Risk Index, October 2020 release.
# Download URL: https://nri-data-downloads.s3.amazonaws.com/NRI_Table_CensusTracts.zip
# About: https://hazards.geoplatform.gov/portal/apps/MapSeries/index.html?appid=ddf915a24fb24dc8863eed96bc3345f8


# 2. Variable descriptions
glimpse(nri)

# Observations are census tract estimates of...
#  Population, building value, agricultural value within tract, area of tract
#  Natural hazards include: CFLD - coastal flooding, DRGT - drought, HWAV - heat wave, HRCN - hurricane, RFLD - riverine flooding, SWND - strong wind
#  Hazard measures include: EVNTS - number of events in recording period, AFREQ - annualized frequency (# events/# years in recording period)
#  Exposure measures include: EXPB - building value exposure, EXPP - population exposure, EXPE - population equivalence exposure, EXPA - agricultural value exposure
#  Historic loss ratio measures include: HLRB - historic loss ratio for building value, HLRA - historicla loss ratio for agriculture, HLRP - historical loss ratio for population, HLRR - historic loss ratio overall


# 3. Summaries 
# 5-number summaries of (non-missing) numeric variables (remove tract identifiers)
nri %>% select(-c(OID_:STATEFIPS, COUNTYTYPE:TRACTFIPS, NRI_VER)) %>% 
  select(where(~is.numeric(.x) && !is.na(.x))) %>% summary(.)

# neater
nri %>% select(-c(OID_:STATEFIPS, COUNTYTYPE:TRACTFIPS, NRI_VER)) %>% 
  select(where(~is.numeric(.x) && !is.na(.x))) %>% 
  as.data.frame() %>% 
  stargazer(., type = "text", title = "Summary Statistics", digits=0,
            summary.stat = c("n", "mean", "sd", "min", "median", "max"))

# Summaries of (non-missing) character variables (remove tract identifiers)
nri %>% select(-c(OID_:STATEFIPS, COUNTYTYPE:TRACTFIPS, NRI_VER)) %>% 
  select(where (~is.character(.x))) %>% map(tabyl)


# 4. Visual distribution
# How about a grouped series of histograms
# Tract assets
nri %>% select(TRACTFIPS:AREA) %>% 
  pivot_longer(-TRACTFIPS, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  scale_fill_viridis(option = "plasma", discrete = TRUE, guide = FALSE) +
  geom_histogram() + 
  facet_wrap(~measure, scales = "free")

# Tract hazards: DRGT
nri %>% select(contains("DRGT"), TRACTFIPS) %>% select(-contains("HLRR")) %>% 
  pivot_longer(-TRACTFIPS, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() + 
  scale_fill_viridis(option = "plasma", discrete = TRUE, guide = FALSE) +
  facet_wrap(~measure, scales = "free")

# Tract hazards: HWAV
nri %>% select(contains("HWAV"), TRACTFIPS) %>% select(-contains("HLRR")) %>% 
  pivot_longer(-TRACTFIPS, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() + 
  scale_fill_viridis(option = "plasma", discrete = TRUE, guide = FALSE) +
  facet_wrap(~measure, scales = "free")

# Tract hazards: HRCN
nri %>% select(contains("HRCN"), TRACTFIPS) %>% select(-contains("HLRR")) %>% 
  pivot_longer(-TRACTFIPS, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() + 
  scale_fill_viridis(option = "plasma", discrete = TRUE, guide = FALSE) +
  facet_wrap(~measure, scales = "free")

# Tract hazards: RFLD
nri %>% select(contains("RFLD"), TRACTFIPS) %>% select(-contains("HLRR")) %>% 
  pivot_longer(-TRACTFIPS, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() + 
  scale_fill_viridis(option = "plasma", discrete = TRUE, guide = FALSE) +
  facet_wrap(~measure, scales = "free")

# Tract hazards: SWND
nri %>% select(contains("SWND"), TRACTFIPS) %>% select(-contains("HLRR")) %>% 
  pivot_longer(-TRACTFIPS, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() + 
  scale_fill_viridis(option = "plasma", discrete = TRUE, guide = FALSE) +
  facet_wrap(~measure, scales = "free")


# 5. Maps: just annualized frequency of 5 relevant hazards
# Don't currently have shapefiles with this so will download and attach them now
# then save to call into rmarkdown more quickly
cville_tracts <- tracts(state = "51", county = cvillefips)
saveRDS(cville_tracts, file = "data/cville_tracts.RDS")

cville_nri <- cville_tracts %>% 
  left_join(nri, by = c("TRACTCE" = "TRACT"))

cville_nri <- st_transform(cville_nri, crs = 4326) # to WGS84, given error

# pal <- colorNumeric(carto_pal(7, "Temps"), domain = cville_nri$DRGT_AFREQ) # TealRose (rcartocolor)
# pal <- colorNumeric("Specral", domain = cville_nri$DRGT_AFREQ) # RdYlBu (RColorBrewer)

# DRGT
pal <- colorNumeric("plasma", reverse = TRUE, domain = cville_nri$DRGT_AFREQ) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = cville_nri,
              fillColor = ~pal(DRGT_AFREQ),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", cville_nri$NAME, "<br>",
                             "Ann. Freq.: ", round(cville_nri$DRGT_AFREQ, 1))
  ) %>% 
  addLegend("bottomright", pal = pal, values = cville_nri$DRGT_AFREQ, 
            title = "Drought-#/year", opacity = 0.7)


# HWAV
pal <- colorNumeric("plasma", reverse = TRUE, domain = cville_nri$HWAV_AFREQ) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = cville_nri,
              fillColor = ~pal(HWAV_AFREQ),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", cville_nri$NAME, "<br>",
                             "Ann. Freq.: ", round(cville_nri$HWAV_AFREQ, 1))
  ) %>% 
  addLegend("bottomright", pal = pal, values = cville_nri$HWAV_AFREQ, 
            title = "Heat Wave-#/year", opacity = 0.7)

# HRCN
pal <- colorNumeric("plasma", reverse = TRUE, domain = cville_nri$HRCN_AFREQ) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = cville_nri,
              fillColor = ~pal(HRCN_AFREQ),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", cville_nri$NAME, "<br>",
                             "Ann. Freq.: ", round(cville_nri$HRCN_AFREQ, 1))
  ) %>% 
  addLegend("bottomright", pal = pal, values = cville_nri$HRCN_AFREQ, 
            title = "Hurricane-#/year", opacity = 0.7)

# RFLD
pal <- colorNumeric("plasma", reverse = TRUE, domain = cville_nri$RFLD_AFREQ) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = cville_nri,
              fillColor = ~pal(RFLD_AFREQ),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", cville_nri$NAME, "<br>",
                             "Ann. Freq.: ", round(cville_nri$RFLD_AFREQ, 1))
  ) %>% 
  addLegend("bottomright", pal = pal, values = cville_nri$RFLD_AFREQ, 
            title = "Riverine Flooding-#/year", opacity = 0.7)

# SWND
pal <- colorNumeric("plasma", reverse = TRUE, domain = cville_nri$SWND_AFREQ) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = cville_nri,
              fillColor = ~pal(SWND_AFREQ),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", cville_nri$NAME, "<br>",
                             "Ann. Freq.: ", round(cville_nri$SWND_AFREQ, 1))
  ) %>% 
  addLegend("bottomright", pal = pal, values = cville_nri$SWND_AFREQ, 
            title = "Strong Wind-#/year", opacity = 0.7)


# 6. Nota Bene
