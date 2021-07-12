# Air Quality Data Exploration
# Marisa Lemma
# 7/1/21

library(haven) #to import .dta
library(tidyverse)

# Import data from source
leadurl <- "https://dataverse.lib.virginia.edu/api/access/datafile/24474"
download.file(url = leadurl,
              destfile = paste(getwd(), "/", "airpollution.zip", sep = ""))
unzip("airpollution.zip", exdir = paste(getwd(), "/data/", sep = ""))

masterdata <- read_dta("data/Master_Data.dta")

# Adjust units for PM2.5
masterdata <- masterdata %>% 
  mutate(CorrectedPM2_5_ = (CorrectedPM2_5_/100))

# Drop unnecessary variables
masterdata <- masterdata %>% 
  select(CTIDFP00, COUNTYFP00, NAME00, CorrectedPM2_5_, percentile)

# Filter by year
master_1981 <- masterdata %>% 
  filter(Year=="1981")
master_2016 <- masterdata %>% 
  filter(Year=="2016")

# Rename PM2.5 values
master_1981 <- master_1981 %>% 
  rename(PM2_5_1981 = CorrectedPM2_5_)
master_2016 <- master_2016 %>% 
  rename(PM2_5_2016 = CorrectedPM2_5_)

# Prepare data frames for merge
master_1981$Year <- NULL
master_2016$Year <- NULL

# Merge datasets
airquality <- inner_join(master_1981, master_2016)

# Export to csv to use in Stata (to convert to 2010 tracts)
write.csv(airquality, "~/airquality-replication/airquality_tract.csv", row.names = F)
# Bridging data from https://s4.ad.brown.edu/Projects/Diversity/Researcher/Bridging.htm

# Import new dataset with 2010 census tract data (from Stata)
airquality_2010 <- read_dta("~/airquality-replication/airquality_2000-2010.dta")

# Create percentiles for each year
airquality_2010 <- airquality_2010 %>% 
  mutate(percentile_1981 = percent_rank(pm2_5_1981),
         percentile_2016 = percent_rank(pm2_5_2016))
airquality_2010 <- airquality_2010 %>% 
  mutate(percentile_1981 = (percentile_1981*100),
         percentile_2016 = (percentile_2016*100))
# Is there a way to round these percentile scores?
# Also: when they calculated percentiles, they also made one that was weighted. Not sure if/how to do that

airquality_2010 <- airquality_2010 %>% 
  mutate(STATEFP00 = str_sub(trtid10, 1, 2),
         COUNTYFP00 = str_sub(trtid10, 3, 5))

# Define area of interest
cvillefips <- c("540", "003", "065", "079", "109", "125")

# Filter to area of interest
virginia <- airquality_2010 %>% 
  filter(STATEFP00=="51")
cville_area <- virginia %>% 
  filter(COUNTYFP00%in%cvillefips)

# Create variable for change in PM2.5 levels
airquality <- cville_area %>% 
  mutate(PM_change = (pm2_5_2016 - pm2_5_1981))
# All these values are negative - should we swap 1981 and 2016 so that all the values are positive?

# Create variable for change in percentile
airquality <- airquality %>% 
  mutate(pctile_change = (percentile_2016 - percentile_1981))
# These values are also negative

# Export to CSV
write.csv(airquality, "cville_region_collection/data//airquality_cville_tract.csv", row.names = F)
