## ......................................
## Combine cville collection data for app
## Starting with tract data
##   (add county, block group, blocks later)
## 
## Authors: Michele Claibourn
## Created: 2022-01-31
## Updated: 
## ......................................

# Setup ----

library(tidyverse)
library(janitor)
library(sf)


# Read data ----
tract_shape <- readRDS("data/cville_tracts.RDS") %>% 
  clean_names() %>% 
  select(geoid, locality = countyfp, name,
         geometry)

# tract_files <- dir(path = "data", pattern = "tract.csv") 
# data <- tract_files %>%
#     map(~ read_csv(file.path("data", .)))

acs <- read_csv("data/acs_cville_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  select(geoid, locality, tract, ends_with("E"))

air <- read_csv("data/airquality_cville_tract.csv") %>% 
  mutate(geoid = as.character(gid)) %>% 
  select(geoid, locality = countyfp,
         tract, pm2_5_1981, pm2_5_2016, percentile_1981, percentile_2016,
         pm_change_1981_2016, pctile_change_1981_2016)

cdc <- read_csv("data/cdcplaces_cville_tract.csv") %>% 
  mutate(geoid = as.character(locationname)) %>% 
  clean_names() %>% 
  select(geoid, countyname:annual_checkup2018)

daym <- read_csv("data/daymet_cville_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  filter(year == 2020) %>% 
  select(geoid, locality = COUNTYFP,
         June_AvgMaxTF:TotpercInch)

fccbb <- read_csv("data/fcc_broadband_cville_tract.csv") %>% 
  mutate(geoid = as.character(tract)) %>% 
  select(geoid, !c(tract, bbmin_dl, bbmin_up))

nri <- read_csv("data/fema_nri_cville_tract.csv") %>% 
  mutate(geoid = as.character(TRACTFIPS)) %>% 
  select(geoid, locality = COUNTYFIPS, 
         BUILDVALUE:AREA, ends_with("AFREQ"), ends_with("HLRB"),
         ends_with("HLRP"), ends_with("HLRA")) %>% 
  select(!starts_with("CFLD")) %>% 
  clean_names()

ls8 <- read_csv("data/landsat8_cville_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  filter(start_date == as.Date("2020-07-04")) %>% 
  select(geoid, min_temp = min, max_temp = max, mean_temp = mean,
         med_temp = median)

lead <- read_csv("data/lead_cville_tract.csv") %>% 
  mutate(geoid = as.character(FIP)) %>% 
  select(geoid, locality = county_fip, 
         totalelep:avg_hh_exp, percentburdened,
         percent_burdened_owners, percent_burdened_renters)

lexp <- read_csv("data/leadexposure_cville_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  select(geoid, locality = countyfips, leadriskscore_raw, lead_risk_rank)

nlcd <- read_csv("data/nlcd_cville_tract.csv") %>% 
  mutate(geoid = as.character(GEOID)) %>% 
  select(geoid, locality = COUNTYFP, 
         tree_can:imp_surf, percent_dev:percent_for)


# ADD LATER
# hmda, fema_nfipclaims, fema_nfip, location affordability, lodes_emloyment,
# lodes_rescoummutepattenrs, lodes_residentcommute, lodes_workercommute
# food_retail


# Merge data ----
df <- acs %>% 
  left_join(air) %>% 
  left_join(cdc) %>% 
  left_join(daym) %>% 
  left_join(fccbb) %>% 
  left_join(lead) %>% 
  left_join(lexp) %>% 
  left_join(nlcd) %>% 
  left_join(nri) %>% 
  select(-state)

df_sf <- tract_shape %>% 
  left_join(df) 

# Will need to add easy names and maybe indicator for 
#   selectable variables vs others
#   or categories of variables later


# Save Rdata ----
save(df, df_sf, file = "appideas/cvl_dat.RData")
saveRDS(df, file = "appideas/cvl_data.RDS")
saveRDS(df_sf, file = "appideas/cvl_data_geo.RDS")
# load("appideas/cvl_dat.RData")

