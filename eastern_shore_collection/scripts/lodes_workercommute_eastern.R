# Get LEHD Origin-Destination data
# Regional Worker Commute Data
# Author: Lee LeBoeuf
# Last updated: 09/15/2021

# This script uses the LODES function to pull LEHD LODES Origin-destination data for the Eastern Shore, and creates separate 
# data files with the following variables (1 for census blocks, 1 for census block groups, and 1 for census tracts):
# -- The average and median commuting distance for people employed in each SU in the Eastern Shore (in miles) calculated for the following groups:
# (1) Eastern Shore workers who live within 40 miles of their work-place census block (This number was used as a cutoff based on the US military's and others' 
# definitions of a reasonable commute as 50 miles--ten miles were subtracted here due to the fact that distances are "as the crow flies" and likely to be underestimates)
# (2) Eastern Shore workers who live in a tract that (a) is outside the Eastern Shore and (b) is home to 25 or more Eastern Shore workers
# (3) All Eastern Shore workers represented in the data 
# It also creates a column of the number of workers of each SU who fall into each of these groups, as well as the number of workers in 
# each SU that live inside or outside of the Eastern Shore, and the percent of workers in each SU that live inside or outside the
# Eastern Shore

# All distances are "as the crow flies" and calulated based on the latitude and longitudes of the centroids of each
# census block. Distances are calculated using the Vincenty Ellipsoid method. 

# Areas included in these data:
# The Eastern Shore area: Accomack (51001), Northampton (51131)

# At the end of the script, there is code to create an additional dataframe which enables county-level comparison of where 
# Eastern Shore workers commute from most often (i.e., where do people who work in the Eastern Shore typically live).
# One column is the county code of the residence, and the second column
# is just the number of Eastern Shore workers who live in that county. 

# Loading required packages
# devtools::install_git("https://github.com/hrbrmstr/lodes.git")
invisible(lapply(list('tidyverse', 'lodes', 'sf', 'tigris', 'geosphere'),
                 function(pkg) library(pkg, character.only = TRUE)))

# LODES data is available for Virginia for the years 2002 to 2018. 
# In this script, I focus on data from 2018, though the process could be repeated for any year in that range by simply changing the year in the read_lodes argument. 
# LEHD does not recommend using LODES data for longitudinal analysis.

# Eastern Shore 
eastfips <- c("51001", "51131")

# od ----
va_lodes_od <- read_lodes(state = "VA", type = "od", year = 2018, 
                          segment = "main", job_type = "JT00")

va_lodes_od <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5))

unique(va_lodes_od$w_county) # In 2018, it's missing data for Northampton county (51131)

# Narrowing the VA OD file to workers who work in Eastern Shore
# and selecting out on the necessary variables 

east_lodes_od <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(w_county %in% eastfips) %>%
  rename(jobs = S000) %>%
  select(w_geocode, h_geocode, jobs, w_county, h_county)

unique(east_lodes_od$w_county)

# Downloading a shape file with all the necessary blocks for distance calculations:
allfips <- unique(east_lodes_od$h_county) # Including all the fips where Eastern Shore workers live
allfips <- str_sub(allfips, 3, 5)
blocks <- blocks(state = "51", county = allfips)
blocks <- st_transform(blocks, crs = 4326)

# Finding centroids of workplace blocks
east_lodes_od$w_geocode <- as.character(east_lodes_od$w_geocode)
blocks$w_geocode <- as.character(blocks$GEOID10)
east_lodes_jobs <- merge(blocks, east_lodes_od, by = 'w_geocode', all.y = T)
east_lodes_jobs$work_centroids <- st_centroid(east_lodes_jobs$geometry)
east_lodes_jobs[,c("work_lat", "work_long")] <- NA
east_lodes_jobs[,c("work_lat", "work_long")] <- st_coordinates(east_lodes_jobs$work_centroids)
east_lodes_jobs <- st_drop_geometry(east_lodes_jobs)
east_lodes_jobs$h_geocode <- as.character(east_lodes_jobs$h_geocode)
east_lodes_jobs <- east_lodes_jobs %>%
  select(w_geocode, h_geocode, jobs, work_lat, work_long)

# Finding centroids of resident blocks 
blocks$h_geocode <- blocks$w_geocode
east_resident <- merge(blocks, east_lodes_od, by = 'h_geocode', all.y = T)
east_resident$res_centroids <- st_centroid(east_resident$geometry)
east_resident[,c("res_lat", "res_long")] <- NA
east_resident[,c("res_lat", "res_long")] <- st_coordinates(east_resident$res_centroids)
east_resident <- st_drop_geometry(east_resident)
east_resident <- east_resident %>%
  select(h_geocode, res_lat, res_long)
east_resident$h_geocode <- as.character(east_resident$h_geocode)

# Merging the dataframes together to calculate the distances between centroids
east_lodes_full <- merge(east_lodes_jobs, unique(east_resident[c("res_lat","res_long", "h_geocode")]), by = "h_geocode")

# Calculating distances using the Vincenty Ellipsoid method 
east_lodes_full$Vince_meters <- NA
for(i in 1:length(east_lodes_full$Vince_meters)){
  east_lodes_full$Vince_meters[i] <- distm(c(east_lodes_full$work_long[i], east_lodes_full$work_lat[i]), c(east_lodes_full$res_long[i], east_lodes_full$res_lat[i]), fun = distVincentyEllipsoid)
}
# Converting to miles 
east_lodes_full$Vince_mile <- (east_lodes_full$Vince_meters * 0.00062137)

# In order to calculate the average commute distances for each census block, we need to weight the distance between any two census block pairs by the number
# of people making that trip. So, we need the total number of people travelling to any given census block. 
east_lodes_full <- east_lodes_full %>%
  group_by(w_geocode) %>%
  mutate(h_county = str_sub(h_geocode, 1, 5),
         workersinBlock = sum(jobs))

# We now have the distances for every home and workplace census block combination for all Eastern Shore workers. 
# Next, I subset the data into the different groups of interest and then calculate the average and median commute distance by census block 
# (averages are weighted by the number of people making that trip). 

# Creating necessary binary variables 
# If the commute is within 40 miles, within_40 = 1
east_lodes_full <- east_lodes_full %>%
  mutate(within_40 = ifelse(Vince_mile <= 40, 1, 0))

# If the commute is from a census tract outside the Eastern Shore where at least 25 Eastern Shore workers live, resid25 = 1
east_lodes_full <- east_lodes_full %>%
  mutate(h_tract = str_sub(h_geocode, 1, 11)) %>%
  group_by(h_tract) %>%
  mutate(resid25 = ifelse(sum(jobs) >= 25 & (h_geocode %in% eastfips == F), 1, 0)) %>%
  ungroup()

# Calculating average distances for all Eastern Shore workers 
# Also adding a column for the number of people who work in the Eastern Shore area, but live outside, and the percent of workers in each block that live outside the Eastern Shore area. 
east_lodes_all <- east_lodes_full %>%
  group_by(w_geocode) %>%
  summarise(avgc_allworkers = sum(Vince_mile * jobs) / workersinBlock,
            medc_allworkers = median(rep.int(Vince_mile, times = jobs)),
            workersinBlockall = workersinBlock,
            liveoutsideRegion = sum(jobs[which(h_county %in% eastfips == F)]),
            liveinsideRegion = sum(jobs[which(h_county %in% eastfips == T)]),
            perc_workers_liveinRegion = (liveinsideRegion/workersinBlock *100),
            perc_workers_liveoutsideRegion = (liveoutsideRegion/workersinBlock * 100)) %>%
  distinct()

# Calculating average distances for Eastern Shore workers who live within 40 miles of work
east_within40 <- east_lodes_full %>%
  filter(within_40 == 1) %>%
  group_by(w_geocode) %>%
  mutate(workerw40 = sum(jobs)) %>%
  summarise(avgc_livewithin40 = sum(Vince_mile * jobs) / workerw40,
            medc_livewithin40 = median(rep(Vince_mile, times = jobs)),
            workerw40 = workerw40) %>%
  distinct()

# Calculating average distances for people who live outside the Eastern Shore but 
# commute from a census tract where at least 25 Eastern Shore workers live
east_25ormore <- east_lodes_full %>%
  filter(resid25 == 1) %>%
  group_by(w_geocode) %>%
  mutate(workers25 = sum(jobs)) %>%
  summarise(avgc_25_res = sum(Vince_mile * jobs) / workers25,
            medc_25_res = median(rep(Vince_mile, times = jobs)),
            workers25 = workers25) %>%
  distinct()

# Merging the data back together
east_lodes_final1 <- merge(east_lodes_all, east_within40, all.x = T, by = "w_geocode")
east_lodes_final2 <- merge(east_lodes_final1, east_25ormore, by = "w_geocode")

# Writing out the Block level version to a csv ---------------------------------------
write.csv(east_lodes_final2, "data/lodes_workercommute_east_block.csv", row.names = F)

# Creating a block group level version and writing it out to a csv -------------------
east_lodes_final2 <- east_lodes_final2 %>%
  mutate(blkgroup = str_sub(w_geocode, 1, 12))

east_lodes_finalblkgr <- east_lodes_final2 %>%
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

write.csv(east_lodes_finalblkgr, "data/lodes_workercommute_east_blkgp.csv", row.names = F)

# Creating a tract level version and writing it out to a csv -------------------------
east_lodes_final2 <- east_lodes_final2 %>%
  mutate(tract = str_sub(w_geocode, 1, 11))

east_lodes_finalTr <- east_lodes_final2 %>%
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

write.csv(east_lodes_finalTr, "data/lodes_workercommute_east_tract.csv", row.names = F)


# ---------------------------------------------------------------------------------
# Creating a commuting patterns csv that just lists the county codes and the number of Eastern Shore workers who commute to that 
# county
eastworkers_lodes_commutepatterns <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(w_county %in% eastfips) %>%
  group_by(h_county) %>%
  summarise(commuterstoRegion = sum(S000))

write.csv(eastworkers_lodes_commutepatterns, "data/lodes_workercommutepatterns_eastcounty.csv", row.names = F)


