# Get LEHD Origin-Destination data
# Regional Worker Commute Data
# Author: Lee LeBoeuf
# Last updated: 09/15/2021

# This script uses the LODES function to pull LEHD LODES Origin-destination data for the Charlottesville area, and creates separate 
# data files with the following variables (1 for census blocks, 1 for census block groups, and 1 for census tracts):
# -- The average and median commuting distance for EMPLOYEES in each SU in the Charlottesville region (in miles) calculated for the following groups:
# (1) Charlottesville area workers who live within 40 miles of their work-place census block (This number was used as a cutoff based on the US military's and others' 
# definitions of a reasonable commute as 50 miles--ten miles were subtracted here due to the fact that distances are "as the crow flies" and likely to be underestimates)
# (2) Charlottesville area workers who live in a tract that (a) is outside the Charlottesville area and (b) is home to 25 or more Cville workers
# (3) All charlottesville area workers represented in the data 
# It also creates a column of the number of workers of each SU who fall into each of these groups, as well as the number of workers in 
# each SU that live inside or outside of the Charlottesville region, and the percent of workers in each SU that live inside or outside the
# Charlottesville region

# All distances are "as the crow flies" and calulated based on the latitude and longitudes of the centroids of each
# census block. Distances are calculated using the Vincenty Ellipsoid method. 

# Areas included in these data:
# Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# At the end of the script, there is code to create an additional dataframe which enables county-level comparison of where 
# Charlottesville region workers commute from most often (i.e., where do people who work in the Cville region typically live).
# One column is the county code of the residence, and the second column
# is just the number of Charlottesville region workers who live in that county. 

# Loading required packages
# devtools::install_git("https://github.com/hrbrmstr/lodes.git")
invisible(lapply(list('tidyverse', 'lodes', 'sf', 'tigris', 'geosphere'),
                 function(pkg) library(pkg, character.only = TRUE)))

# LODES data is available for Virginia for the years 2002 to 2018. 
# In this script, I focus on data from 2018, though the process could be repeated for any year in that range by simply changing the year in the read_lodes argument. 
# LEHD does not recommend using LODES data for longitudinal analysis.

# Charlottesville area 
cvlfips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# od ----
va_lodes_od <- read_lodes(state = "VA", type = "od", year = 2018, 
                          segment = "main", job_type = "JT00")

# Narrowing the VA OD file to workers who work in Cville region
# and selecting out on the necessary variables 

cvl_lodes_od <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(w_county %in% cvlfips) %>%
  rename(jobs = S000) %>%
  select(w_geocode, h_geocode, jobs, w_county, h_county)

# Downloading a shape file (a shape file for the entire state is necessary for the distance calculations):
allfips <- unique(cvl_lodes_od$h_county)
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
# of people making that trip. So, we need the total number of people travelling to any given census block. 
cvl_lodes_full <- cvl_lodes_full %>%
  group_by(w_geocode) %>%
  mutate(h_county = str_sub(h_geocode, 1, 5),
         workersinBlock = sum(jobs))

# We now have the distances for every home and workplace census block combination for all Charlottesville region workers. 
# Next, I subset the data into the different groups of interest and then calculate the average and median commute distance by census block 
# (averages are weighted by the number of people making that trip). 

# Creating necessary binary variables 
# If the commute is within 40 miles, within_40 = 1
cvl_lodes_full <- cvl_lodes_full %>%
  mutate(within_40 = ifelse(Vince_mile <= 40, 1, 0))

# If the commute is from a census tract outside the Cville region where at least 25 Cville region workers live, resid25 = 1
cvl_lodes_full <- cvl_lodes_full %>%
  mutate(h_tract = str_sub(h_geocode, 1, 11)) %>%
  group_by(h_tract) %>%
  mutate(resid25 = ifelse(sum(jobs) >= 25 & (h_geocode %in% cvlfips == F), 1, 0)) %>%
  ungroup()

# Calculating average distances for all Charlottesville region workers 
# Also adding a column for the number of people who work in the cville area, but live outside, and the percent of workers in each block that live outside the Cville area. 
cvl_lodes_all <- cvl_lodes_full %>%
  group_by(w_geocode) %>%
  summarise(avgc_allworkers = sum(Vince_mile * jobs) / workersinBlock,
            medc_allworkers = median(rep.int(Vince_mile, times = jobs)),
            workersinBlockall = workersinBlock,
            liveoutsideRegion = sum(jobs[which(h_county %in% cvlfips == F)]),
            liveinsideRegion = sum(jobs[which(h_county %in% cvlfips == T)]),
            perc_workers_liveinRegion = (liveinsideRegion/workersinBlock *100),
            perc_workers_liveoutsideRegion = (liveoutsideRegion/workersinBlock * 100)) %>%
  distinct()

# Calculating average distances for Charlottesville region workers who live within 40 miles of work
cvl_within40 <- cvl_lodes_full %>%
  filter(within_40 == 1) %>%
  group_by(w_geocode) %>%
  mutate(workerw40 = sum(jobs)) %>%
  summarise(avgc_livewithin40 = sum(Vince_mile * jobs) / workerw40,
            medc_livewithin40 = median(rep(Vince_mile, times = jobs)),
            workerw40 = workerw40) %>%
  distinct()

# Calculating average distances for people who live outside the Charlottesville region but 
# commute from a census tract where at least 25 Cville region workers live
cvl_25ormore <- cvl_lodes_full %>%
   filter(resid25 == 1) %>%
   group_by(w_geocode) %>%
   mutate(workers25 = sum(jobs)) %>%
   summarise(avgc_25_res = sum(Vince_mile * jobs) / workers25,
             medc_25_res = median(rep(Vince_mile, times = jobs)),
             workers25 = workers25) %>%
   distinct()

# Merging the data back together
cvl_lodes_final1 <- merge(cvl_lodes_all, cvl_within40, all.x = T, by = "w_geocode")
cvl_lodes_final2 <- merge(cvl_lodes_final1, cvl_25ormore, by = "w_geocode")

# Writing out the Block level version to a csv ---------------------------------------
write.csv(cvl_lodes_final2, "data/lodes_workercommute_cville_block.csv", row.names = F)

# Creating a block group level version and writing it out to a csv -------------------
cvl_lodes_final2 <- cvl_lodes_final2 %>%
  mutate(blkgroup = str_sub(w_geocode, 1, 12))

cvl_lodes_finalblkgr <- cvl_lodes_final2 %>%
  group_by(blkgroup) %>%
  summarise(medc_allworkersblkgr = median(rep(avgc_allworkers, times = workersinBlockall)),
            medc_livewithin40blkgr = median(rep(avgc_livewithin40[is.na(workerw40) == F], times = workerw40[is.na(workerw40) == F])),
            medc_25_resblkgr = median(rep(avgc_25_res[is.na(workers25) == F], times = workers25[is.na(workers25) == F])),
            workersInblkgr = sum(workersinBlockall, na.rm = T),
            liveoutsideRegionblkgr = sum(liveoutsideRegion),
            liveinsideRegionblkgr = sum(liveinsideRegion),
            perc_workers_liveinRegionblkgr = (liveinsideRegionblkgr/workersInblkgr *100),
            perc_workers_liveoutsideRegionblkgr = (liveoutsideRegionblkgr/workersInblkgr * 100),
            workersw40blkgr = sum(workerw40, na.rm = T),
            workers25blkgr = sum(workers25, na.rm = T),
            avgc_allworkersblkgr = sum(avgc_allworkers * workersinBlockall, na.rm = T) / workersInblkgr,
            avgc_livewithin40blkgr = sum(avgc_livewithin40 * workerw40, na.rm = T) / workersw40blkgr,
            avgc_25_resblkgr = sum(avgc_25_res * workers25, na.rm = T) / workers25blkgr) %>%
  mutate(county = str_sub(blkgroup, 1, 5)) %>%
  distinct()

write.csv(cvl_lodes_finalblkgr, "data/lodes_workercommute_cville_blkgp.csv", row.names = F)

# Creating a tract level version and writing it out to a csv -------------------------
cvl_lodes_final2 <- cvl_lodes_final2 %>%
  mutate(tract = str_sub(w_geocode, 1, 11))

cvl_lodes_finalTr <- cvl_lodes_final2 %>%
  group_by(tract) %>%
  summarise(medc_allworkerstr = median(rep(avgc_allworkers, times = workersinBlockall)),
            medc_livewithin40tr = median(rep(avgc_livewithin40[is.na(workerw40) == F], times = workerw40[is.na(workerw40) == F])),
            medc_25_restr = median(rep(avgc_25_res[is.na(workers25) == F], times = workers25[is.na(workers25) == F])),
            workersIntr = sum(workersinBlockall, na.rm = T),
            liveoutsideRegiontr = sum(liveoutsideRegion),
            liveinsideRegiontr = sum(liveinsideRegion),
            perc_workers_liveinRegiontr = (liveinsideRegiontr/workersIntr *100),
            perc_workers_liveoutsideRegiontr = (liveoutsideRegiontr/workersIntr * 100),
            workersw40tr = sum(workerw40, na.rm = T),
            workers25tr = sum(workers25, na.rm = T),
            avgc_allworkerstr = sum(avgc_allworkers * workersinBlockall, na.rm = T) / workersIntr,
            avgc_livewithin40tr = sum(avgc_livewithin40 * workerw40, na.rm = T) / workersw40tr,
            avgc_25_restr = sum(avgc_25_res * workers25, na.rm = T) / workers25tr) %>%
  mutate(county = str_sub(tract, 1, 5)) %>%
  distinct()

write.csv(cvl_lodes_finalTr, "data/lodes_workercommute_cville_tract.csv", row.names = F)


# ---------------------------------------------------------------------------------
# Creating a commuting patterns csv that just lists the county codes and the number of Charlottesville region workers who commute to that 
# county
cvlworkers_lodes_commutepatterns <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(w_county %in% cvlfips) %>%
  group_by(h_county) %>%
  summarise(commuterstoRegion = sum(S000))

write.csv(cvlworkers_lodes_commutepatterns, "data/lodes_workercommutepatterns_cvlcounty.csv", row.names = F)



