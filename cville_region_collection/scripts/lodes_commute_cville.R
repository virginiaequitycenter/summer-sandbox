
# Author: Lee LeBoeuf
# Last updated: 07/28/2021

# This script uses the LODES function to pull LEHD LODES Origin-destination data for the Charlottesville area, and creates separate 
# data files with the following variables (1 for census blocks, 1 for census block groups, and 1 for census tracts):
# -- The average and median commuting distance for residents of each SU in the Charlottesville region (in miles) calculated for the following groups:
# (1) People who live *and* work in the Charlottesville region
# (2) Charlottesville area residents who work within 40 miles of their home census block (avgc_within40) (This number was used as a cutoff based on the US military's and others' 
# definitions of a reasonable commute as 50 miles--ten miles were subtracted here due to the fact that distances are "as the crow flies" and likely to be underestimates)
# (3) Charlottesville area residents who work in a tract outside the Charlottesville area that employs at least 25 Charlottsville area residents (avgc_25_employees)
# (4) All charlottesville area residents represented in the data 
# It also creates a column of the number of residents of each SU who fall into each of these groups 

# All distances are "as the crow flies" and calulated based on the latitude and longitudes of the centroids of each
# census block. Distances are calculated using the Vincenty Ellipsoid method. 

# Areas included in these data:
# Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# At the end of the script, there is code to create an additional dataframe which enables county-level comparison of where 
# Charlottesville region residents commute most often. One column is the county code of the work-place destination, and the second column
# is just the number of Charlottesville region residents who are employed in that county. 

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

# Downloading a shape file (a shape file for the entire state is necessary for the distance calculations):
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

# In order to calculate the average commute distances for each census block, we need to weight the distance between any two census block pairs by the number
# of people making that trip. So, we need the total number of people leaving any given census block. 
cvl_lodes_full <- cvl_lodes_full %>%
  group_by(h_geocode) %>%
  mutate(commutersInBlock = sum(jobs)) %>%
  ungroup()

# We now have the distances for every home and workplace census block combination for all Charlottesville region residents. 
# Next, I subset the data into the different groups of interest and then calculate the average and median commute distance by census block 
# (averages are weighted by the number of people making that trip). 

# Creating necessary binary variables 
# If the commute is within 40 miles, within_40 = 1
cvl_lodes_full <- cvl_lodes_full %>%
  mutate(w_county = str_sub(w_geocode, 1, 5), # Will need this variable later
         within_40 = ifelse(Vince_mile <= 40, 1, 0))

# If the commute is to a census tract that employs at least 25 Cville region residents, employs25 = 1
cvl_lodes_full <- cvl_lodes_full %>%
  mutate(w_tract = str_sub(w_geocode, 1, 11)) %>%
  group_by(w_tract) %>%
  mutate(employs25 = ifelse(sum(jobs) >= 25 | w_county %in% cvlfips, 1, 0)) %>%
  ungroup()

# Calculating average distances for all Charlottesville region residents 
cvl_lodes_all <- cvl_lodes_full %>%
  group_by(h_geocode) %>%
  summarise(avgc_all = sum(Vince_mile * jobs) / commutersInBlock,
            medc_all = median(rep.int(Vince_mile, times = jobs)),
            commutersInBlockall = commutersInBlock) %>%
  distinct()

# Calculating average distances for people who live and work in the Charlottesville region
cvl_only <- cvl_lodes_full %>%
  filter(w_county %in% cvlfips) %>%
  group_by(h_geocode) %>%
  mutate(commuterinRegion = sum(jobs)) %>%
  summarise(avgc_workinRegion = sum(Vince_mile * jobs) / commuterinRegion,
            medc_workinRegion = median(rep(Vince_mile, times = jobs)),
            commuterinRegion = commuterinRegion) %>%
  distinct()

# Calculating average distances for Charlottesville region residents who work within 40 miles of home
cvl_within40 <- cvl_lodes_full %>%
  filter(within_40 == 1) %>%
  group_by(h_geocode) %>%
  mutate(commuterw40 = sum(jobs)) %>%
  summarise(avgc_within40 = sum(Vince_mile * jobs) / commuterw40,
            medc_within40 = median(rep(Vince_mile, times = jobs)),
            commuterw40 = commuterw40) %>%
  distinct()

# Calculating average distances for people who work in a tract that employs 25 or more Charlottesville region residents
cvl_25ormore <- cvl_lodes_full %>%
  filter(employs25 == 1) %>%
  group_by(h_geocode) %>%
  mutate(commuter25 = sum(jobs)) %>%
  summarise(avgc_25_employees = sum(Vince_mile * jobs) / commuter25,
            medc_25_employees = median(rep(Vince_mile, times = jobs)),
            commuter25 = commuter25) %>%
  distinct()

# Merging the data back together
cvl_lodes_final <- merge(cvl_lodes_all, cvl_within40, all.x = T, by = "h_geocode")
cvl_lodes_final <- merge(cvl_lodes_final, cvl_25ormore, all.x = T, by = "h_geocode")
cvl_lodes_final <- merge(cvl_lodes_final, cvl_only, all.x = T, by = "h_geocode")

# Writing out the Block level version to a csv ---------------------------------------
write.csv(cvl_lodes_final, "lodes_commute_cville_block.csv", row.names = F)

# Creating a block group level version and writing it out to a csv -------------------
cvl_lodes_final <- cvl_lodes_final %>%
  mutate(blkgroup = str_sub(h_geocode, 1, 12))

cvl_lodes_finalblkgr <- cvl_lodes_final %>%
  group_by(blkgroup) %>%
  summarise(medc_allblk = median(rep(avgc_all, times = commutersInBlockall)),
            medc_workinRegionblk = median(rep(avgc_workinRegion[is.na(commuterinRegion) == F], times = commuterinRegion[is.na(commuterinRegion) == F])),
            medc_within40blk = median(rep(avgc_within40[is.na(commuterw40) == F], times = commuterw40[is.na(commuterw40) == F])),
            medc_25_employeesblk = median(rep(avgc_25_employees[is.na(commuter25) == F], times = commuter25[is.na(commuter25) == F])),
            commutersInBlgr = sum(commutersInBlockall, na.rm = T),
            commuterinRegionblk = sum(commuterinRegion, na.rm = T),
            commuterw40blk = sum(commuterw40, na.rm = T),
            commuter25blk = sum(commuter25, na.rm = T),
            avgc_allblk = sum(avgc_all * commutersInBlockall, na.rm = T) / commutersInBlgr,
            avgc_workinRegionblk = sum(avgc_workinRegion * commuterinRegion, na.rm = T) / commuterinRegionblk,
            avgc_within40blk = sum(avgc_within40 * commuterw40, na.rm = T) / commuterw40blk,
            avgc_25_employeesblk = sum(avgc_25_employees * commuter25, na.rm = T) / commuter25blk) %>%
  mutate(county = str_sub(blkgroup, 1, 5)) %>%
  distinct()

write.csv(cvl_lodes_finalblkgr, "lodes_commute_cville_blkgp.csv", row.names = F)

# Creating a tract level version and writing it out to a csv -------------------------
cvl_lodes_final <- cvl_lodes_final %>%
  mutate(tract = str_sub(h_geocode, 1, 11))

cvl_lodes_finalTr <- cvl_lodes_final %>%
  group_by(tract) %>%
  summarise(medc_alltr = median(rep(avgc_all, times = commutersInBlockall)),
            medc_workinRegiontr = median(rep(avgc_workinRegion[is.na(commuterinRegion) == F], times = commuterinRegion[is.na(commuterinRegion) == F])),
            medc_within40tr = median(rep(avgc_within40[is.na(commuterw40) == F], times = commuterw40[is.na(commuterw40) == F])),
            medc_25_employeestr = median(rep(avgc_25_employees[is.na(commuter25) == F], times = commuter25[is.na(commuter25) == F])),
            commutersIntr = sum(commutersInBlockall, na.rm = T),
            commuterinRegiontr = sum(commuterinRegion, na.rm = T),
            commuterw40tr = sum(commuterw40, na.rm = T),
            commuter25tr = sum(commuter25, na.rm = T),
            avgc_alltr = sum(avgc_all * commutersInBlockall, na.rm = T) / commutersIntr,
            avgc_workinRegiontr = sum(avgc_workinRegion * commuterinRegion, na.rm = T) / commuterinRegiontr,
            avgc_within40tr = sum(avgc_within40 * commuterw40, na.rm = T) / commuterw40tr,
            avgc_25_employeestr = sum(avgc_25_employees * commuter25, na.rm = T) / commuter25tr) %>%
  mutate(county = str_sub(tract, 1, 5)) %>%
  distinct()

write.csv(cvl_lodes_finalTr, "lodes_commute_cville_tract.csv", row.names = F)


# ---------------------------------------------------------------------------------
# Creating a commuting patterns csv that just lists the county codes and the number of Charlottesville region residents who commute to that 
# county
cvl_lodes_commutepatterns <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(h_county %in% cvlfips) %>%
  group_by(w_county) %>%
  summarise(commuters = sum(S000))

write.csv(cvl_lodes_commutepatterns, "lodes_commutepatterns_county.csv", row.names = F)









