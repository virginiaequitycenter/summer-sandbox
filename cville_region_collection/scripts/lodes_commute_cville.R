
# Author: Lee LeBoeuf
# Last updated: 07/26/2021

# This script uses the LODES function to pull LEHD LODES Origin-destination data for the Charlottesville area, and creates separate 
# data files with the following variables (1 for census blocks, 1 for census block groups, and 1 for census tracts):
# -- The average commuting distance for residents of each SU in the Charlottesville region (in miles) calculated for the following groups:
# (1) People who live *and* work in the Charlottesville region
# (2) Charlottesville area residents who work within 150 miles of their home census block (avgc_within150)
# (3) Charlottesville area residents who work in a tract outside the Charlottesville area that employs at least 25 Charlottsville area residents (avgc_25_employees)
# (4) All charlottesville area residents represented in the data 

# All distances are "as the crow flies" and calulated based on the latitude and longitudes of the centroids of each
# census block. 

# Areas included in these data:
# Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# Loading required packages
# devtools::install_git("https://github.com/hrbrmstr/lodes.git")
# invisible(lapply(list('tidyverse', 'stargazer', 'janitor', 'tigris', 'sf', 'leaflet', 'rcartocolor', 
#                      'RColorBrewer', 'viridis', 'rgdal', 'lodes', 'psych', 'reshape2', 'googlesheets4', 
#                      'sp', 'geosphere', 'stringr'),
#                 function(pkg) library(pkg, character.only = TRUE)))

# LODES data is available for Virginia for the years 2002 to 2018. 
# In this script, I focus on data from 2018, though the process could be repeated for any year in that range by simply changing the year in the read_lodes argument. 
# LEHD does not recommend using LODES data for longitudinal analysis.

# Charlottesville area 
cvlfips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# od ----
va_lodes_od <- read_lodes(state = "VA", type = "od", year = 2018, 
                          segment = "main", job_type = "JT00")

# Narrowing the VA OD file to workers who live in Cville region
# and selecting out on the necessary variables 

cvl_lodes_od <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(h_county %in% cvlfips) %>%
  rename(jobs = S000) %>%
  select(w_geocode, h_geocode, jobs, w_county, h_county)

# Downloading a shape file:
allfips <- unique(cvl_lodes_od$w_county)
allfips <- str_sub(allfips, 3, 5)
blocks <- blocks(state = "51", county = allfips)
blocks <- st_transform(blocks, crs = 4326)

# Finding centroids of workplace blocks
cvl_lodes_od$w_geocode <- as.character(cvl_lodes_od$w_geocode)
blocks$w_geocode <- as.character(blocks$GEOID10)
cvl_lodes_jobs <- merge(blocks, cvl_lodes_od, by = 'w_geocode', all.y = T)
cvl_lodes_jobs$work_centroids <- st_centroid(cvl_lodes_jobs$geometry)
cvl_lodes_jobs[,c("work_lat", "work_long")] <- NA
cvl_lodes_jobs[,c("work_lat", "work_long")] <- st_coordinates(cvl_lodes_jobs$work_centroids)
cvl_lodes_jobs <- st_drop_geometry(cvl_lodes_jobs)
cvl_lodes_jobs$h_geocode <- as.character(cvl_lodes_jobs$h_geocode)
cvl_lodes_jobs <- cvl_lodes_jobs %>%
  select(w_geocode, h_geocode, jobs, work_lat, work_long)

# Finding centroids of resident blocks 
blocks$h_geocode <- blocks$w_geocode
cvl_resident <- merge(blocks, cvl_lodes_od, by = 'h_geocode', all.y = T)
cvl_resident$res_centroids <- st_centroid(cvl_resident$geometry)
cvl_resident[,c("res_lat", "res_long")] <- NA
cvl_resident[,c("res_lat", "res_long")] <- st_coordinates(cvl_resident$res_centroids)
cvl_resident <- st_drop_geometry(cvl_resident)
cvl_resident <- cvl_resident %>%
  select(h_geocode, res_lat, res_long)
cvl_resident$h_geocode <- as.character(cvl_resident$h_geocode)


# Merging the dataframes together to calculate the distances between centroids
cvl_lodes_full <- merge(cvl_lodes_jobs, unique(cvl_resident[c("res_lat","res_long", "h_geocode")]), by = "h_geocode")

# Calculating distances using the Vincenty Ellipsoid method 
cvl_lodes_full$Vince_meters <- NA
for(i in 1:length(cvl_lodes_full$Vince_meters)){
  cvl_lodes_full$Vince_meters[i] <- distm(c(cvl_lodes_full$work_long[i], cvl_lodes_full$work_lat[i]), c(cvl_lodes_full$res_long[i], cvl_lodes_full$res_lat[i]), fun = distVincentyEllipsoid)
}
# Converting to miles 
cvl_lodes_full$Vince_mile <- (cvl_lodes_full$Vince_meters * 0.00062137)

# We now have the distances for every home and workplace census block combination for all Charlottesville region residents. 
# Next, I subset the data into the different groups of interest and then calculate the average commute distance by census block. 

# Creating necessary binary variables 
cvl_lodes_full <- cvl_lodes_full %>%
  mutate(w_tract = str_sub(w_geocode, 1, 11),
         w_county = str_sub(w_geocode, 1, 5),
         within_150 = ifelse(Vince_mile <= 150, 1, 0))

cvl_lodes_full <- cvl_lodes_full %>%
  group_by(w_tract) %>%
  mutate(employs25 = ifelse(sum(jobs) >= 25 | w_county %in% cvlfips, 1, 0)) %>%
  ungroup()

# Calculating average distances for people who live and work in the Charlottesville region
cvl_only <- cvl_lodes_full %>%
  filter(w_county %in% cvlfips) %>%
  group_by(h_geocode) %>%
  summarise(avgc_workinRegion = mean(na.omit(Vince_mile))) %>%
  select(h_geocode, avgc_workinRegion)

# Calculating average distances for Charlottesville region residents who work within 150 miles of home
cvl_within150 <- cvl_lodes_full %>%
  group_by(h_geocode) %>%
  filter(within_150 == 1) %>%
  summarise(avgc_within150 = mean(na.omit(Vince_mile))) %>%
  select(h_geocode, avgc_within150)

# Calculating average distances for people who work in a tract that employs 25 or more Charlottesville region residents
cvl_25ormore <- cvl_lodes_full %>%
  group_by(h_geocode) %>%
  filter(employs25 == 1) %>%
  summarise(avgc_25_employees = mean(na.omit(Vince_mile))) %>%
  select(h_geocode, avgc_25_employees)

# Calculating average distances for all Charlottesville region residents 
cvl_lodes_h <- cvl_lodes_full %>%
  group_by(h_geocode) %>%
  summarise(avgc_all = mean(Vince_mile)) %>%
  mutate(blkgroup = str_sub(h_geocode, 1, 12),
         tract = str_sub(h_geocode, 1, 11),
         county = str_sub(h_geocode, 1, 5)) %>%
  select(h_geocode, avgc_all, blkgroup, tract, county)

# Merging the data back together
cvl_lodes_final <- merge(cvl_lodes_h, cvl_within150, all.x = T, by = "h_geocode")
cvl_lodes_final <- merge(cvl_lodes_final, cvl_25ormore, all.x = T, by = "h_geocode")
cvl_lodes_final <- merge(cvl_lodes_final, cvl_only, all.x = T, by = "h_geocode")

# Writing out the Block level version to a csv ---------------------------------------
write.csv(cvl_lodes_final, "lodes_commute_cville_block.csv", row.names = F)

# Creating a block group level version and writing it out to a csv -------------------
cvl_lodes_finalblkgr <- cvl_lodes_final %>%
  group_by(blkgroup) %>%
  summarise(avgc_workinRegion = mean(na.omit(avgc_workinRegion)),
            avgc_within150 = mean(na.omit(avgc_within150)),
            avgc_25_employees = mean(na.omit(avgc_25_employees)),
            avgc_all = mean(na.omit(avgc_all))) %>%
  mutate(county = str_sub(blkgroup, 1, 5))

write.csv(cvl_lodes_finalblkgr, "lodes_commute_cville_blkgp.csv", row.names = F)

# Creating a tract level version and writing it out to a csv -------------------------
cvl_lodes_finaltract <- cvl_lodes_final %>%
  group_by(tract) %>%
  summarise(avgc_workinRegion = mean(na.omit(avgc_workinRegion)),
            avgc_within150 = mean(na.omit(avgc_within150)),
            avgc_25_employees = mean(na.omit(avgc_25_employees)),
            avgc_all = mean(na.omit(avgc_all))) %>%
  mutate(county = str_sub(tract, 1, 5))

write.csv(cvl_lodes_finaltract, "lodes_commute_cville_tract.csv", row.names = F)



