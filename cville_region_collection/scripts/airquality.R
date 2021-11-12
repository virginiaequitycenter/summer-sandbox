# Get Census Tract Air Quality Data
# Marisa Lemma, Michele Claibourn
# Created: 2021-07-01
# Last updated: 2021-11-11

library(haven) # to import .dta
library(tidyverse)
library(sf)
library(tigris)
library(areal) # to interpolate 2000 tracts to 2010 tracts


# Define area of interest
# Cville area
localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
# localfips <- c("001", "131")

# Import data from source ----
# if (!dir.exists("dataraw")) {dir.create("dataraw")}
# 
# url <- "https://dataverse.lib.virginia.edu/api/access/datafile/24474"
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "airpollution.zip", sep = ""))
# unzip("dataraw/airpollution.zip", exdir = paste(getwd(), "/dataraw/airpollution/", sep = ""))
# 
# Read in Stata dataset
wide <- read_dta("dataraw/airpollution/Master_Data_Wide.dta")


# Read in 2000 and 2010 tracts ----
# We need the data in 2010 census tracts, but here it is in 2000 tracts
us_states <- unique(fips_codes$state)[1:51]
continental_states <- us_states[!us_states %in% c("AK", "HI")]
us_tracts <- rbind_tigris(
  lapply(
    continental_states, function(x) {
      tracts(state = x, cb = TRUE, year = 2010, class = "sf")
    }
  )
)

us_tracts_2000 <- rbind_tigris(
  lapply(
    continental_states, function(x) {
      tracts(state = x, cb = TRUE, year = 2000, class = "sf")
    }
  )
)

# need a planar CRS (not lat/lon) for interpolation
us_tracts_3857 <- st_transform(us_tracts, crs = 3857)
us_tracts_2000_3857 <- st_transform(us_tracts_2000, crs = 3857)

# Join data to 2000 tracts
# create gid in both
us_tracts_2000_3857 <- us_tracts_2000_3857 %>% 
  mutate(sid = paste0(STATEFP, COUNTYFP, TRACT))

us_tracts_3857 <- us_tracts_3857 %>% 
  mutate(tid = paste0(STATEFP, COUNTYFP, TRACT))

us_tracts_2000_3857_aq <- us_tracts_2000_3857 %>% 
  left_join(wide, by = c("sid" = "CTIDFP00")) 

# save image here so we don't have to restart if
# process below terminates!
save.image("dataraw/airpollution/crosswalksteps.Rdata")


# load("dataraw/airpollution/crosswalksteps.Rdata")

# ..................................
# Areal interpolation, US ----
# of 2000 tract AQ data to 2010 tract
# all PM25 vars

vlist <- wide %>% 
  select(contains("Corrected")) %>% 
  names()

# check
ar_validate(target = us_tracts_3857, source = us_tracts_2000_3857_aq, 
            varList = vlist, 
            method = "aw", verbose = TRUE)

# interpolate AQ to 2010 shape files
us_tracts_2010_aq <- aw_interpolate(us_tracts_3857, 
                                    tid = tid,
                                    source = us_tracts_2000_3857_aq, 
                                    sid = sid,
                                    weight = "sum", 
                                    output = "tibble",
                                    intensive = vlist)


# Rename and process ----
airquality <- us_tracts_2010_aq %>% 
  # Rename (remove Corrected)
  rename_with(~str_remove(., pattern = "Corrected"), everything()) %>% 
  rename_with(tolower, everything()) %>% 
  # Adjust units for PM2.5
  mutate(across(contains("pm2_5"), ~ .x/100)) %>% 
  # Recover state and county fips
  mutate(STATEFP00 = str_sub(trtid10, 1, 2),
         COUNTYFP00 = str_sub(trtid10, 3, 5)) %>% 
  # Create percentiles for each year
  mutate(percentile_1981 = round(percent_rank(pm2_5_1981)*100,1),
         percentile_2016 = round(percent_rank(pm2_5_2016)*100,1)) %>% 
  # Create variables for change in PM2.5 levels
  mutate(pm_change = (pm2_5_2016 - pm2_5_1981),
         pctile_change = (percentile_2016 - percentile_1981))
  
# When authors calculated percentiles, they included one that was 
# weighted. We don't do that here.

# Filter to area of interest
cville_area <- airquality %>% 
  filter(STATEFP00=="51" & COUNTYFP00%in%cvillefips)

# Export to CSV ----
write_csv(airquality, "cville_region_collection/data//airquality_cville_tract.csv", row.names = F)


# ..................................
# Areal Interpolation, VA ----
# The first attempt at interpolating generated erros; 
# step through with reduced set to diagnose
# Just VA, just 2016 pm25
va_tracts_3857 <- us_tracts_3857 %>% 
  filter(STATEFP == "51")

va_tracts_2000_3857_aq <- us_tracts_2000_3857_aq %>% 
  filter(STATEFP == "51") 

# check
ar_validate(target = va_tracts_3857, source = va_tracts_2000_3857_aq, 
            varList = "CorrectedPM2_5_2016", 
            method = "aw", verbose = TRUE)

# Intersect data
intersect <- va_tracts_3857 %>%
  aw_intersect(source = va_tracts_2000_3857_aq, areaVar = "area")
# intersect

# Calculate total area
intersect <- intersect %>%
  aw_total(source = va_tracts_2000_3857_aq, id = sid, areaVar = "area", 
           totalVar = "totalArea", type = "intensive", weight = "sum") 
# intersect

# Calculate areal weight
intersect <- intersect %>%
  aw_weight(areaVar = "area", totalVar = "totalArea",
            areaWeight = "areaWeight") 
# intersect

# Calculate estimates
intersect <- intersect %>%
  aw_calculate(value = vlist, areaWeight = "areaWeight") 
# intersect

# Aggregate by target id
result <- intersect %>%
  aw_aggregate(target = va_tracts_3857, tid = tid, interVar = CorrectedPM2_5_2016)
result

ggplot(data = result) +
  geom_sf(aes(fill = CorrectedPM2_5_2016)) +
  theme_void()

# All together (for VA only)
va_tracts_2010_aq <- aw_interpolate(va_tracts_3857, 
                                    tid = tid,
                                    source = va_tracts_2000_3857_aq, 
                                    sid = sid,
                                    weight = "sum", 
                                    output = "tibble",
                                    intensive = vlist)
# Works!

# Rename and process ----
va_airquality <- va_tracts_2010_aq %>% 
  # Rename (remove Corrected)
  rename_with(~str_remove(., pattern = "Corrected"), everything()) %>% 
  rename_with(tolower, everything()) %>% 
  # Adjust units for PM2.5
  mutate(across(contains("pm2_5"), ~ .x/100)) %>% 
  # Create percentiles for each year
  mutate(percentile_1981 = round(percent_rank(pm2_5_1981)*100,1),
         percentile_2016 = round(percent_rank(pm2_5_2016)*100,1)) %>% 
  # Create variables for change in PM2.5 levels
  mutate(pm_change_1981_2016 = (pm2_5_2016 - pm2_5_1981),
         pctile_change_1981_2016 = (percentile_2016 - percentile_1981))

# Filter to area of interest and select vars
cville_area <- va_airquality %>% 
  filter(countyfp %in% localfips) %>% 
  select(statefp, countyfp, tract, gid = tid, pm2_5_1981:pctile_change_1981_2016)

# eastern_area <- va_airquality %>% 
# filter(countyfp %in% localfips) %>% 
#   select(statefp, countyfp, tract, gid = tid, pm2_5_1981:pctile_change_1981_2016)


# Export to CSV ----
write_csv(cville_area, "data/airquality_cville_tract.csv")

# write_csv(eastern_area, "data/airquality_eastern_tract.csv")
