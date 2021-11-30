# Create Lead Exposure Risk Data using Vox method
# Marisa Lemma
# Last updated: 2021-11-18

# Recreate the Vox lead exposure data with data from 2015-2019 ACS
# based on a method developed by the Washington State Department of Health 
# (Vox uses 2010-2014 data)
# https://github.com/voxmedia/data-projects/tree/master/vox-lead-exposure-risk

library(tidyverse)
library(tidycensus)


# Define area of interest
# Cville area
# localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
localfips <- c("001", "131")


# import data
poverty <- get_acs(geography = "tract",
                   state = c("01":"02", "04":"06", "08":"10", "12":"13", "15":"42", "44":"51", "53":"56"),
                   variables = c("S1701_C01_001", "S1701_C01_039"),
                   year = 2019,
                   output = "wide")

housing <- get_acs(geography = "tract",
                   state = c("01":"02", "04":"06", "08":"10", "12":"13", "15":"42", "44":"51", "53":"56"),
                   table = "B25034",
                   year = 2019,
                   output = "wide")

# remove census tracts with no data
housing <- housing %>% 
  filter(B25034_001E!=0)
poverty <- poverty %>% 
  filter(S1701_C01_001E!=0)

# create age categories 
# these represent number of houses in tract that are at risk of containing lead
# times a nationally-derived estimate of the  proportion of housing from each era with lead risks
housing <- housing %>% 
  mutate(age_39 = B25034_011E * 0.68,
         age40_59 = (B25034_010E+B25034_009E)*0.43,
         age60_79 = (B25034_008E+B25034_007E)*0.08,
         age80_99 = (B25034_006E+B25034_005E)*0.03,
         age00_plus = (B25034_004E+B25034_003E+B25034_002E)*0,
         total = B25034_001E)

# calculate housing risk for census tract
housing <- housing %>% 
  mutate(sum_housing_risk = age_39 + age40_59 + age60_79 + age80_99 + age00_plus,
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

# calculate weighted lead risk score and deciles
# poverty weighted 0.42 and housing weighted 0.58, summed
combined_risk <- combined_risk %>% 
  mutate(weighted_housing_risk = z_housing_risk*0.58,
         weighted_poverty_risk = z_poverty_risk*0.42,
         leadriskscore_raw = weighted_housing_risk + weighted_poverty_risk,
         lead_risk_rank = ntile(leadriskscore_raw, 10))

# filter to Cville region
eastern_area <- combined_risk %>% 
  mutate(statefips = str_sub(GEOID, 1,2),
         countyfips = str_sub(GEOID, 3,5),
         tractfips = str_sub(GEOID, 6,11)) %>% 
  filter(statefips == "51" & countyfips %in% localfips)  %>% 
  select(GEOID, NAME, countyfips, tractfips, leadriskscore_raw, lead_risk_rank,
         housing_risk, poverty_risk)

# export to csv file
write.csv(eastern_area, "data//leadexposure_eastern_tract.csv", row.names = F)
