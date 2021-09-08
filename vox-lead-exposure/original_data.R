# This is my attempt to recreate the Vox lead exposure measures found here: https://github.com/voxmedia/data-projects/tree/master/vox-lead-exposure-risk 
# They did their data cleaning/analysis in Python so I recreated it in R
# Their work was inspired by the Washington State Department of Health; they replicated it nationally
# Age of the buildings (indicating likelihood of exposure to lead paint) and poverty levels were found to be the biggest indicators of lead exposure, which is why they look specifically at those

library(tidyverse)
library(leaflet)
library(sf)
library(tidycensus)

# import data
poverty <- get_acs(geography = "tract",
                   state = c("01":"02", "04":"06", "08":"10", "12":"13", "15":"42", "44":"51", "53":"56"),
                   variables = c("S1701_C01_001", "S1701_C01_039"),
                   year = 2014,
                   output = "wide")

housing <- get_acs(geography = "tract",
                   state = c("01":"02", "04":"06", "08":"10", "12":"13", "15":"42", "44":"51", "53":"56"),
                   table = "B25034",
                   year = 2014,
                   output = "wide")

# remove census tracts with no data
housing <- housing %>% 
  filter(B25034_001E!=0)
poverty <- poverty %>% 
  filter(S1701_C01_001E!=0)

# create age categories (these essentially represent number of houses in tract that are at lead risk)
housing <- housing %>% 
  mutate(age_39 = B25034_010E * 0.68,
         age40_59 = (B25034_009E+B25034_008E)*0.43,
         age60_79 = (B25034_007E+B25034_006E)*0.08,
         age80_99 = (B25034_005E+B25034_004E)*0.03,
         age00_10 = (B25034_003E+B25034_002E)*0,
         total = B25034_001E)

# calculate housing risk for census tract
housing <- housing %>% 
  mutate(sum_housing_risk = age_39 + age40_59 + age60_79 + age80_99 + age00_10,
         housing_risk = (sum_housing_risk/total)*100)

# calculate poverty risk for census tract
poverty <- poverty %>% 
  rename(total_under_125pct = S1701_C01_039E,
         total_poverty_status = S1701_C01_001E) %>% 
  mutate(poverty_risk = (total_under_125pct/total_poverty_status)*100)

# filter to just variables of interest
housing <- housing %>% 
  select("GEOID":"NAME", "age_39":"housing_risk")
poverty <- poverty %>% 
  select("GEOID":"NAME", "total_poverty_status", "total_under_125pct", "poverty_risk")

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
cville_area <- combined_risk %>% 
  filter(GEOID%in%51003010100:51003011400 | 
           GEOID%in%51065020101:51065020300 | 
           GEOID%in%51079030101:51079030200 |
           GEOID%in%51109950100:51109950500 | 
           GEOID%in%51125950100:51125950300 | 
           GEOID%in%51540000201:51540001000)

cville_shapes <- readRDS("cville_region_collection/data/cville_tracts.RDS")
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

