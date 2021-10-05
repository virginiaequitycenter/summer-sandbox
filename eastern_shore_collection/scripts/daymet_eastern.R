# Get DAYMET temperature and precipitation Data
# 2021-07-28
# Tolu Odukoya
### Last updated 09/22 by Lee LeBoeuf

## This script downloads daymet data using the FedData function to pull the following variables at the block,
# block group, tract, and county levels
# (1-3) June_AvgMaxTF, July_AvgMaxTF, and Aug_AvgMaxTF = The daily average maximum temperature aggregated over the given month (June, July, or August)
# FedData gives these measurements in celsius, here they are converted to Fahrenheit
# (4) AvgAnnMaxTF = The daily average aggregated over the entire year 
# (5) TotpercInch = The total annual precipitation--FedData gives these measurements in milimeters, here they are converted to Inches

# The cleaning process for each level of SU is very similar, and the data need only be downloaded once, and then 
# re-projected to the appropriate spatial unit as is shown below. 
# For each spatial unit, dataframes/objects are created during the cleaning process and then used at different stages
# Because the cleaning process is the same for each level of SU, the names are repeated for many of these objects 
# Therefore, the user must be cautious when using this script to make sure they are not alternating between different SU's --
# Only run one SU at a time to avoid issues with the repeat object names 

# Data are pulled from the years 1980-2020. In the future, more data can be added one year at a time 

# Need to download the development version of FedData in order to be able to use the tempo argument. As of 09/20, it works!
devtools::install_github("ropensci/FedData")

#Load Libraries 
invisible(lapply(list('tidyverse', 'sf', 'raster', 'mapview', 'ggplot2', 'transformr', 'FedData', 'magrittr', 
                      'daymetr','maps', 'tigris', 'measurements','ncdf4', 'tools', 'geosphere', 'sp'),
                 function(pkg) library(pkg, character.only = TRUE)))

# Create two conversion functions that will be used later to convert temps to fahrenheit and milimeters to inches
# Converting temps from celsius to fahrenheit
converttemp <- function(tempC) {
  tempF <- (tempC * (9/5) + 32)
  return(round(tempF, 2))
}

# Converting precipitation from milimeters to inches
convertp <- function(precipmm) {
  precipI <- (precipmm / 25.4)
  return(round(precipI, 2))
}

# Reading in shape files 
eastern_blocks <- readRDS('eastshore_blocks.RDS')
eastern_blkgrp <- readRDS('eastshore_blkgps.RDS')
eastern_tracts <- readRDS('eastshore_tracts.RDS')
eastern_counties <- readRDS('eastshore_counties.RDS')

# Define years of interest
yearswant <- 1980:2020 # Can change this to whichever year of interest--should not need to download data from before 2020 again,
# unless interested in changing the variables included. 

######## Downloading data ########## 
# The tempo argument defines whether we're downloading daily, monthly, or annual summaries. Here I download
# the monthly summaries in order to get the average maximum temperatures in the summer months
# and the annual summaries to get the average yearly maximum temperatures and the annual precipiation. 
# Could repeat the process used below for the monthly temperature data with monthly precipiation data
# if we decide we're interested in precipitation levels during specific months during the year. 

eastern_daymetblock <- FedData::get_daymet(template = eastern_blocks, 
                                          label = "east",
                                          elements = c("prcp", "tmax"), 
                                          years = yearswant, 
                                          tempo = "mon")

eastern_daymetblockyear <- FedData::get_daymet(template = eastern_blocks, 
                                              label = "east",
                                              elements = c("prcp", "tmax"), 
                                              years = yearswant, 
                                              tempo = "ann")

# Check the results
# check CRS
crs(eastern_daymetblock$tmax@crs) # check coordinate system: WGS84
st_crs(eastern_blocks) # check locality polygons for reference: NAD83

## Cleaning data at the block level ## --------------------------------------------------------------------------------------------

# Temperature Max -----------------------------------
# reproject polygons to same CRS as eastern_daymetblock
eastern_blmon <- st_transform(eastern_blocks, projection(eastern_daymetblock$tmax@crs))
eastern_blann <- st_transform(eastern_blocks, projection(eastern_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
eastern_daymet_tmax <- raster::extract(eastern_daymetblock$tmax, eastern_blmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_tmaxyear <- raster::extract(eastern_daymetblockyear$tmax, eastern_blann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
eastern_daymet_tmn <- st_as_sf(eastern_daymet_tmax)
eastern_daymet_tyear <- st_as_sf(eastern_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(eastern_daymet_tmn) <- NULL
st_geometry(eastern_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
eastern_daymet_tmn1 <- eastern_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID10, NAME10, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
eastern_daymet_tyear <- eastern_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID10, NAME10, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
eastern_daymet_tmn1[,5:127] <- sapply(eastern_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
eastern_daymet_tyear[,5:45] <- sapply(eastern_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
eastern_daymet_tmn1long <- pivot_longer(eastern_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID10, NAME10),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tmn1) * length(1980:2020)

# Rename columns 
eastern_daymet_tmn1long <- eastern_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
eastern_daymet_tyearlong <- pivot_longer(eastern_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID10, NAME10),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tyear) * length(1980:2020)

# Rename columns 
eastern_daymet_tyearlong <- eastern_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
eastern_alltempdat <- merge(eastern_daymet_tmn1long, eastern_daymet_tyearlong, by = c('GEOID10', 'year', 'STATEFP', 'COUNTYFP', 'NAME10'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as eastern_daymet
# eastern_pmonth <- st_transform(eastern_blocks, projection(eastern_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use eastern_pmonth in the same process as what's done below with annual precipitation. 
eastern_pyear <- st_transform(eastern_blocks, projection(eastern_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# eastern_daymet_pdatmonth <- raster::extract(eastern_daymetblock$prcp, eastern_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_pdatyear <- raster::extract(eastern_daymetblockyear$prcp, eastern_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# eastern_daymet_pdatmonthsf <- st_as_sf(eastern_daymet_pdatmonth)
eastern_daymet_pdatyearsf <- st_as_sf(eastern_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(eastern_daymet_pdatmonthsf) <- NULL
st_geometry(eastern_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
eastern_daymet_pdatyearsf1 <- eastern_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID10, NAME10, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
eastern_daymet_pdatyearsf1[,5:45] <- sapply(eastern_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
eastern_daymet_pdatyearsf1long <- pivot_longer(eastern_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID10, NAME10),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
eastern_daymet_pdatyearsf1long <- eastern_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
eastern_alldaymetblock <- merge(eastern_alltempdat, eastern_daymet_pdatyearsf1long, by = c('GEOID10', 'year', 'STATEFP', 'COUNTYFP', 'NAME10'))

# Adding county names 
eastern_alldaymetblock$CountyName <- eastern_alldaymetblock$COUNTYFP %>%
  recode("001" = "Accomack", "131" = "Northampton")

## Writing out block level data to csv ## 
write.csv(eastern_alldaymetblock, 'daymet_eastern_block.csv', row.names = F)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Re-projecting to block group level --------------------------------------------------------------------------------------------
## To get the block group level data, we will not need to re-download data, but just specify the new boundaries while 
# extracting raster data. 
# The same process is then repeated

# Temperature Max -----------------------------------
# reproject polygons to same CRS as eastern_daymetblock
eastern_blkgrmon <- st_transform(eastern_blkgrp, projection(eastern_daymetblock$tmax@crs))
eastern_blkgrann <- st_transform(eastern_blkgrp, projection(eastern_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
eastern_daymet_tmax <- raster::extract(eastern_daymetblock$tmax, eastern_blkgrmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_tmaxyear <- raster::extract(eastern_daymetblockyear$tmax, eastern_blkgrann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
eastern_daymet_tmn <- st_as_sf(eastern_daymet_tmax)
eastern_daymet_tyear <- st_as_sf(eastern_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(eastern_daymet_tmn) <- NULL
st_geometry(eastern_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
eastern_daymet_tmn1 <- eastern_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
eastern_daymet_tyear <- eastern_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
eastern_daymet_tmn1[,5:127] <- sapply(eastern_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
eastern_daymet_tyear[,5:45] <- sapply(eastern_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
eastern_daymet_tmn1long <- pivot_longer(eastern_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tmn1) * length(1980:2020)

# Rename columns 
eastern_daymet_tmn1long <- eastern_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
eastern_daymet_tyearlong <- pivot_longer(eastern_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tyear) * length(1980:2020)

# Rename columns 
eastern_daymet_tyearlong <- eastern_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
eastern_alltempdat <- merge(eastern_daymet_tmn1long, eastern_daymet_tyearlong, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as eastern_daymet
# eastern_pmonth <- st_transform(eastern_blkgrp, projection(eastern_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use eastern_pmonth in the same process as what's done below with annual precipitation. 
eastern_pyear <- st_transform(eastern_blkgrp, projection(eastern_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# eastern_daymet_pdatmonth <- raster::extract(eastern_daymetblock$prcp, eastern_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_pdatyear <- raster::extract(eastern_daymetblockyear$prcp, eastern_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# eastern_daymet_pdatmonthsf <- st_as_sf(eastern_daymet_pdatmonth)
eastern_daymet_pdatyearsf <- st_as_sf(eastern_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(eastern_daymet_pdatmonthsf) <- NULL
st_geometry(eastern_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
eastern_daymet_pdatyearsf1 <- eastern_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
eastern_daymet_pdatyearsf1[,5:45] <- sapply(eastern_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
eastern_daymet_pdatyearsf1long <- pivot_longer(eastern_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
eastern_daymet_pdatyearsf1long <- eastern_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
eastern_alldaymetblkgr <- merge(eastern_alltempdat, eastern_daymet_pdatyearsf1long, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Adding county names 
eastern_alldaymetblkgr$CountyName <- eastern_alldaymetblkgr$COUNTYFP %>%
  recode("001" = "Accomack", "131" = "Northampton")

## Writing out block level data to csv ## 
write.csv(eastern_alldaymetblkgr, 'daymet_eastern_blkgrps.csv', row.names = F)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Re-projecting to tract level --------------------------------------------------------------------------------------------
## Just like with the block group level, we just need to use the tract level polygons during extraction to get tract
# level data 

# Temperature Max -----------------------------------
# reproject polygons to same CRS as eastern_daymetblock
eastern_trmon <- st_transform(eastern_tracts, projection(eastern_daymetblock$tmax@crs))
eastern_trann <- st_transform(eastern_tracts, projection(eastern_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
eastern_daymet_tmax <- raster::extract(eastern_daymetblock$tmax, eastern_trmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_tmaxyear <- raster::extract(eastern_daymetblockyear$tmax, eastern_trann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
eastern_daymet_tmn <- st_as_sf(eastern_daymet_tmax)
eastern_daymet_tyear <- st_as_sf(eastern_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(eastern_daymet_tmn) <- NULL
st_geometry(eastern_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
eastern_daymet_tmn1 <- eastern_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
eastern_daymet_tyear <- eastern_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# Wrote a function for the conversion to allow for simple sapply over all columns
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
eastern_daymet_tmn1[,5:127] <- sapply(eastern_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
eastern_daymet_tyear[,5:45] <- sapply(eastern_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
eastern_daymet_tmn1long <- pivot_longer(eastern_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tmn1) * length(1980:2020)

# Rename columns 
eastern_daymet_tmn1long <- eastern_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
eastern_daymet_tyearlong <- pivot_longer(eastern_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tyear) * length(1980:2020)

# Rename columns 
eastern_daymet_tyearlong <- eastern_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
eastern_alltempdat <- merge(eastern_daymet_tmn1long, eastern_daymet_tyearlong, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as eastern_daymet
# eastern_pmonth <- st_transform(eastern_tracts, projection(eastern_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use eastern_pmonth in the same process as what's done below with annual precipitation. 
eastern_pyear <- st_transform(eastern_tracts, projection(eastern_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# eastern_daymet_pdatmonth <- raster::extract(eastern_daymetblock$prcp, eastern_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_pdatyear <- raster::extract(eastern_daymetblockyear$prcp, eastern_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# eastern_daymet_pdatmonthsf <- st_as_sf(eastern_daymet_pdatmonth)
eastern_daymet_pdatyearsf <- st_as_sf(eastern_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(eastern_daymet_pdatmonthsf) <- NULL
st_geometry(eastern_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
eastern_daymet_pdatyearsf1 <- eastern_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
eastern_daymet_pdatyearsf1[,5:45] <- sapply(eastern_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
eastern_daymet_pdatyearsf1long <- pivot_longer(eastern_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
eastern_daymet_pdatyearsf1long <- eastern_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
eastern_alldaymettract <- merge(eastern_alltempdat, eastern_daymet_pdatyearsf1long, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Adding county names 
eastern_alldaymettract$CountyName <- eastern_alldaymettract$COUNTYFP %>%
  recode("001" = "Accomack", "131" = "Northampton")

## Writing out block level data to csv ## 
write.csv(eastern_alldaymettract, 'daymet_eastern_tract.csv', row.names = F)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Re-projecting to county level --------------------------------------------------------------------------------------------
## Just like with the tract level, we just need to use the county level polygons during extraction to get tract
# level data 

# Temperature Max -----------------------------------
# reproject polygons to same CRS as eastern_daymetblock
eastern_cmon <- st_transform(eastern_counties, projection(eastern_daymetblock$tmax@crs))
eastern_cann <- st_transform(eastern_counties, projection(eastern_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
eastern_daymet_tmax <- raster::extract(eastern_daymetblock$tmax, eastern_cmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_tmaxyear <- raster::extract(eastern_daymetblockyear$tmax, eastern_cann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
eastern_daymet_tmn <- st_as_sf(eastern_daymet_tmax)
eastern_daymet_tyear <- st_as_sf(eastern_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(eastern_daymet_tmn) <- NULL
st_geometry(eastern_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
eastern_daymet_tmn1 <- eastern_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(eastern_daymet_tmn1) <- str_replace(colnames(eastern_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(eastern_daymet_tyear) <- str_replace(colnames(eastern_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
eastern_daymet_tyear <- eastern_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# Wrote a function for the conversion to allow for simple sapply over all columns
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
eastern_daymet_tmn1[,5:127] <- sapply(eastern_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
eastern_daymet_tyear[,5:45] <- sapply(eastern_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
eastern_daymet_tmn1long <- pivot_longer(eastern_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tmn1) * length(1980:2020)

# Rename columns 
eastern_daymet_tmn1long <- eastern_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
eastern_daymet_tyearlong <- pivot_longer(eastern_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(eastern_daymet_tyear) * length(1980:2020)

# Rename columns 
eastern_daymet_tyearlong <- eastern_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
eastern_alltempdat <- merge(eastern_daymet_tmn1long, eastern_daymet_tyearlong, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as eastern_daymet
# eastern_pmonth <- st_transform(eastern_counties, projection(eastern_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use eastern_pmonth in the same process as what's done below with annual precipitation. 
eastern_pyear <- st_transform(eastern_counties, projection(eastern_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# eastern_daymet_pdatmonth <- raster::extract(eastern_daymetblock$prcp, eastern_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
eastern_daymet_pdatyear <- raster::extract(eastern_daymetblockyear$prcp, eastern_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# eastern_daymet_pdatmonthsf <- st_as_sf(eastern_daymet_pdatmonth)
eastern_daymet_pdatyearsf <- st_as_sf(eastern_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(eastern_daymet_pdatmonthsf) <- NULL
st_geometry(eastern_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
eastern_daymet_pdatyearsf1 <- eastern_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(eastern_daymet_pdatyearsf1) <- str_replace(colnames(eastern_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
eastern_daymet_pdatyearsf1[,5:45] <- sapply(eastern_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
eastern_daymet_pdatyearsf1long <- pivot_longer(eastern_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
eastern_daymet_pdatyearsf1long <- eastern_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
eastern_alldaymetcounty <- merge(eastern_alltempdat, eastern_daymet_pdatyearsf1long, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

## Writing out block level data to csv ## 
write.csv(eastern_alldaymetcounty, 'daymet_eastern_county.csv', row.names = F)

