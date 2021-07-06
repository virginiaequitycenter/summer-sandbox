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

# Define area of interest
easternshore_fips <- c("001", "131")

# Adjust units for PM2.5
masterdata <- masterdata %>% 
  mutate(CorrectedPM2_5_ = (CorrectedPM2_5_/100))

# Create percentiles for each year
masterdata <- masterdata %>% 
  group_by(Year) %>% 
  mutate(percentile = percent_rank(CorrectedPM2_5_))
masterdata <- masterdata %>% 
  mutate(percentile = (percentile*100))
# Is there a way to round these percentile scores?
# Also: when they calculated percentiles, they also made one that was weighted. Not sure if/how to do that

# Checking that it worked
year1981 <- masterdata %>% 
  filter(Year=="1981") %>% 
  arrange(percentile)

# Filter to area of interest
virginia <- masterdata %>% 
  filter(STATEFP00=="51")
easternshore <- virginia %>% 
  filter(COUNTYFP00%in%easternshore_fips)

# Drop unnecessary variables
easternshore <- easternshore %>% 
  select(CTIDFP00, COUNTYFP00, NAME00, CorrectedPM2_5_, percentile)

# Filter by year
eastern_1981 <- easternshore %>% 
  filter(Year=="1981")
eastern_2016 <- easternshore %>% 
  filter(Year=="2016")

# Rename percentile scores
eastern_1981 <- eastern_1981 %>% 
  rename(percentile_1981 = percentile)
eastern_2016 <- eastern_2016 %>% 
  rename(percentile_2016 = percentile)

# Rename PM2.5 values
eastern_1981 <- eastern_1981 %>% 
  rename(PM2_5_1981 = CorrectedPM2_5_)
eastern_2016 <- eastern_2016 %>% 
  rename(PM2_5_2016 = CorrectedPM2_5_)

# Prepare data frames for merge
eastern_1981$Year <- NULL
eastern_2016$Year <- NULL

# Merge datasets
airquality <- inner_join(eastern_1981, eastern_2016)

# Create variable for change in PM2.5 levels
airquality <- airquality %>% 
  mutate(PM_change = (PM2_5_2016 - PM2_5_1981))
# All these values are negative - should we swap 1981 and 2016 so that all the values are positive?

# Create variable for change in percentile
airquality <- airquality %>% 
  mutate(pctile_change = (percentile_2016 - percentile_1981))
# These values are also negative

# Export to CSV
write.csv(airquality, "data//airquality_eastern_tract.csv", row.names = F)
