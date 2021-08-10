# This is my attempt to recreate the Vox lead exposure measures found here: https://github.com/voxmedia/data-projects/tree/master/vox-lead-exposure-risk 
# They did their data cleaning/analysis in Python so I recreated it in R
# Their work was inspired by the Washington State Department of Health; they replicated it nationally
# Age of the buildings (indicating likelihood of exposure to lead paint) and poverty levels were found to be the biggest indicators of lead exposure, which is why they look specifically at those

library(tidyverse)

# import data
# data came from US Census Bureau
housing <- read_csv("vox-lead-exposure/housing_data_2014.csv")
poverty <- read_csv("vox-lead-exposure/poverty_data_2014.csv")

# delete second header row
housing <- housing [-c(1), ]
poverty <- poverty [-c(1), ]

# remove census tracts with no data and census tracts in Puerto Rico
housing <- housing %>% 
  filter(HD01_VD01!=0) %>% 
  filter(!grepl('Puerto Rico', `GEO.display-label`))
poverty <- poverty %>% 
  filter(HC01_EST_VC01!=0) %>% 
  filter(!grepl('Puerto Rico', `GEO.display-label`))

# convert character to numeric
housing$HD01_VD01 <- as.numeric(housing$HD01_VD01)
housing$HD01_VD02 <- as.numeric(housing$HD01_VD02)
housing$HD01_VD03 <- as.numeric(housing$HD01_VD03)
housing$HD01_VD04 <- as.numeric(housing$HD01_VD04)
housing$HD01_VD05 <- as.numeric(housing$HD01_VD05)
housing$HD01_VD06 <- as.numeric(housing$HD01_VD06)
housing$HD01_VD07 <- as.numeric(housing$HD01_VD07)
housing$HD01_VD08 <- as.numeric(housing$HD01_VD08)
housing$HD01_VD09 <- as.numeric(housing$HD01_VD09)
housing$HD01_VD10 <- as.numeric(housing$HD01_VD10)

# create age categories (these essentially represent number of houses in tract that are at lead risk)
housing <- housing %>% 
  mutate(age_39 = HD01_VD10 * 0.68,
         age40_59 = (HD01_VD09+HD01_VD08)*0.43,
         age60_79 = (HD01_VD07+HD01_VD06)*0.08,
         age80_99 = (HD01_VD05+HD01_VD04)*0.03,
         age00_10 = (HD01_VD03+HD01_VD02)*0,
         total = HD01_VD01)

# calculate housing risk for census tract
housing <- housing %>% 
  mutate(sum_housing_risk = age_39 + age40_59 + age60_79 + age80_99 + age00_10,
         housing_risk = (sum_housing_risk/total)*100)

# convert character to numeric (poverty)
poverty$HC01_EST_VC01 <- as.numeric(poverty$HC01_EST_VC01)
poverty$HC01_EST_VC49 <- as.numeric(poverty$HC01_EST_VC49)

# calculate poverty risk for census tract
poverty <- poverty %>% 
  rename(total_under_125pct = HC01_EST_VC49,
         total_poverty_status = HC01_EST_VC01) %>% 
  mutate(poverty_risk = (total_under_125pct/total_poverty_status)*100)

# filter to just variables of interest
housing <- housing %>% 
  select("GEO.id":"GEO.display-label", "age_39":"housing_risk")
poverty <- poverty %>% 
  select("GEO.id":"GEO.display-label", "total_poverty_status", "total_under_125pct", "poverty_risk")

# merge two datasets together
combined_risk <- inner_join(housing, poverty)

# calculate z-scores for housing and poverty risk (to standardize them)
combined_risk <- combined_risk %>% 
  mutate(z_housing_risk = (housing_risk - mean(housing_risk))/sd(housing_risk),
         z_poverty_risk = (poverty_risk - mean(poverty_risk))/sd(poverty_risk))

# calculate weighted lead risk, with poverty weighted 0.42 and housing weighted 0.58
combined_risk <- combined_risk %>% 
  mutate(weighted_housing_risk = z_housing_risk*0.58,
         weighted_poverty_risk = z_poverty_risk*0.42)

# add together weighted lead risks
combined_risk <- combined_risk %>% 
  mutate(leadriskscore_raw = weighted_housing_risk + weighted_poverty_risk)

# create deciles for lead risk score
combined_risk <- combined_risk %>% 
  mutate(lead_risk_rank = ntile(leadriskscore_raw, 10))

# filter to just Cville region
combined_risk <- combined_risk %>% 
  mutate(GEO.id = str_sub(GEO.id, 10, 20))

cville_area <- combined_risk %>% 
  filter(GEO.id%in%51003010100:51003011400 | 
           GEO.id%in%51065020101:51065020300 | 
           GEO.id%in%51079030101:51079030200 |
           GEO.id%in%51109950100:51109950500 | 
           GEO.id%in%51125950100:51125950300 | 
           GEO.id%in%51540000201:51540001000)

cville_shapes <- readRDS("cville_region_collection/data/cville_tracts.RDS")
cville_area <- cville_area %>% rename(GEOID = GEO_ID)
leadrisk <- merge(cville_shapes, cville_area, by = 'GEOID', all.x = TRUE)
leadrisk <- st_transform(leadrisk, crs = 4326)

pal <- colorFactor("viridis", reverse = TRUE, domain = leadrisk$lead_risk_rank)
leaflet(leadrisk) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = leadrisk,
              fillColor = ~pal(lead_risk_rank),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0(leadrisk$NAME.y, "<br>",
                             "Lead Risk Rank: ", leadrisk$lead_risk_rank)) %>% 
  addLegend("bottomright", pal = pal, values = leadrisk$lead_risk_rank,
            title = "Lead Risk Rank", opacity = 0.7)

