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
cville_blocks <- readRDS('cville_blocks.RDS')
cville_blkgrp <- readRDS('cville_blkgps.RDS')
cville_tracts <- readRDS('cville_tracts.RDS')
cville_counties <- readRDS('cville_counties.RDS')

# Define years of interest
yearswant <- 1980:2020 # Can change this to whichever year of interest--should not need to download data from before 2020 again,
# unless interested in changing the variables included. 

######## Downloading data ########## 
# The tempo argument defines whether we're downloading daily, monthly, or annual summaries. Here I download
# the monthly summaries in order to get the average maximum temperatures in the summer months
# and the annual summaries to get the average yearly maximum temperatures and the annual precipiation. 
# Could repeat the process used below for the monthly temperature data with monthly precipiation data
# if we decide we're interested in precipitation levels during specific months during the year. 

cville_daymetblock <- FedData::get_daymet(template = cville_blocks, 
                                     label = "cvl",
                                     elements = c("prcp", "tmax"), 
                                     years = yearswant, 
                                     tempo = "mon")

cville_daymetblockyear <- FedData::get_daymet(template = cville_blocks, 
                                          label = "cvl",
                                          elements = c("prcp", "tmax"), 
                                          years = yearswant, 
                                          tempo = "ann")

# Check the results
# check CRS
crs(cville_daymetblock$tmax@crs) # check coordinate system: WGS84
st_crs(cville_blocks) # check locality polygons for reference: NAD83

## Cleaning data at the block level ## --------------------------------------------------------------------------------------------

# Temperature Max -----------------------------------
# reproject polygons to same CRS as cville_daymetblock
cville_blmon <- st_transform(cville_blocks, projection(cville_daymetblock$tmax@crs))
cville_blann <- st_transform(cville_blocks, projection(cville_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
cville_daymet_tmax <- raster::extract(cville_daymetblock$tmax, cville_blmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_tmaxyear <- raster::extract(cville_daymetblockyear$tmax, cville_blann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
cville_daymet_tmn <- st_as_sf(cville_daymet_tmax)
cville_daymet_tyear <- st_as_sf(cville_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(cville_daymet_tmn) <- NULL
st_geometry(cville_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
cville_daymet_tmn1 <- cville_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID10, NAME10, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
cville_daymet_tyear <- cville_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID10, NAME10, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
cville_daymet_tmn1[,5:127] <- sapply(cville_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
cville_daymet_tyear[,5:45] <- sapply(cville_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
cville_daymet_tmn1long <- pivot_longer(cville_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID10, NAME10),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tmn1) * length(1980:2020)

# Rename columns 
cville_daymet_tmn1long <- cville_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
cville_daymet_tyearlong <- pivot_longer(cville_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID10, NAME10),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tyear) * length(1980:2020)

# Rename columns 
cville_daymet_tyearlong <- cville_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
cville_alltempdat <- merge(cville_daymet_tmn1long, cville_daymet_tyearlong, by = c('GEOID10', 'year', 'STATEFP', 'COUNTYFP', 'NAME10'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as cville_daymet
# cville_pmonth <- st_transform(cville_blocks, projection(cville_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use cville_pmonth in the same process as what's done below with annual precipitation. 
cville_pyear <- st_transform(cville_blocks, projection(cville_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# cville_daymet_pdatmonth <- raster::extract(cville_daymetblock$prcp, cville_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_pdatyear <- raster::extract(cville_daymetblockyear$prcp, cville_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# cville_daymet_pdatmonthsf <- st_as_sf(cville_daymet_pdatmonth)
cville_daymet_pdatyearsf <- st_as_sf(cville_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(cville_daymet_pdatmonthsf) <- NULL
st_geometry(cville_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
cville_daymet_pdatyearsf1 <- cville_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID10, NAME10, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
cville_daymet_pdatyearsf1[,5:45] <- sapply(cville_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
cville_daymet_pdatyearsf1long <- pivot_longer(cville_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID10, NAME10),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
cville_daymet_pdatyearsf1long <- cville_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
cville_alldaymetblock <- merge(cville_alltempdat, cville_daymet_pdatyearsf1long, by = c('GEOID10', 'year', 'STATEFP', 'COUNTYFP', 'NAME10'))

# Adding county names 
cville_alldaymetblock$CountyName <- cville_alldaymetblock$COUNTYFP %>%
  recode("003" = "Albemarle", "540" = "Charlottesville", "079" = "Greene", "065" = "Fluvanna", "109" = "Louisa", "125" =
           "Nelson")

## Writing out block level data to csv ## 
write.csv(cville_alldaymetblock, 'daymet_cville_block.csv', row.names = F)

## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Re-projecting to block group level --------------------------------------------------------------------------------------------
## To get the block group level data, we will not need to re-download data, but just specify the new boundaries while 
# extracting raster data. 
# The same process is then repeated

# Temperature Max -----------------------------------
# reproject polygons to same CRS as cville_daymetblock
cville_blkgrmon <- st_transform(cville_blkgrp, projection(cville_daymetblock$tmax@crs))
cville_blkgrann <- st_transform(cville_blkgrp, projection(cville_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
cville_daymet_tmax <- raster::extract(cville_daymetblock$tmax, cville_blkgrmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_tmaxyear <- raster::extract(cville_daymetblockyear$tmax, cville_blkgrann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
cville_daymet_tmn <- st_as_sf(cville_daymet_tmax)
cville_daymet_tyear <- st_as_sf(cville_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(cville_daymet_tmn) <- NULL
st_geometry(cville_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
cville_daymet_tmn1 <- cville_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
cville_daymet_tyear <- cville_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
cville_daymet_tmn1[,5:127] <- sapply(cville_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
cville_daymet_tyear[,5:45] <- sapply(cville_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
cville_daymet_tmn1long <- pivot_longer(cville_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tmn1) * length(1980:2020)

# Rename columns 
cville_daymet_tmn1long <- cville_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
cville_daymet_tyearlong <- pivot_longer(cville_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tyear) * length(1980:2020)

# Rename columns 
cville_daymet_tyearlong <- cville_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
cville_alltempdat <- merge(cville_daymet_tmn1long, cville_daymet_tyearlong, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as cville_daymet
# cville_pmonth <- st_transform(cville_blkgrp, projection(cville_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use cville_pmonth in the same process as what's done below with annual precipitation. 
cville_pyear <- st_transform(cville_blkgrp, projection(cville_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# cville_daymet_pdatmonth <- raster::extract(cville_daymetblock$prcp, cville_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_pdatyear <- raster::extract(cville_daymetblockyear$prcp, cville_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# cville_daymet_pdatmonthsf <- st_as_sf(cville_daymet_pdatmonth)
cville_daymet_pdatyearsf <- st_as_sf(cville_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(cville_daymet_pdatmonthsf) <- NULL
st_geometry(cville_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
cville_daymet_pdatyearsf1 <- cville_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
cville_daymet_pdatyearsf1[,5:45] <- sapply(cville_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
cville_daymet_pdatyearsf1long <- pivot_longer(cville_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
cville_daymet_pdatyearsf1long <- cville_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
cville_alldaymetblkgr <- merge(cville_alltempdat, cville_daymet_pdatyearsf1long, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Adding county names 
cville_alldaymetblkgr$CountyName <- cville_alldaymetblkgr$COUNTYFP %>%
  recode("003" = "Albemarle", "540" = "Charlottesville", "079" = "Greene", "065" = "Fluvanna", "109" = "Louisa", "125" =
           "Nelson")

## Writing out block level data to csv ## 
write.csv(cville_alldaymetblkgr, 'daymet_cville_blkgrps.csv', row.names = F)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Re-projecting to tract level --------------------------------------------------------------------------------------------
## Just like with the block group level, we just need to use the tract level polygons during extraction to get tract
# level data 

# Temperature Max -----------------------------------
# reproject polygons to same CRS as cville_daymetblock
cville_trmon <- st_transform(cville_tracts, projection(cville_daymetblock$tmax@crs))
cville_trann <- st_transform(cville_tracts, projection(cville_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
cville_daymet_tmax <- raster::extract(cville_daymetblock$tmax, cville_trmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_tmaxyear <- raster::extract(cville_daymetblockyear$tmax, cville_trann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
cville_daymet_tmn <- st_as_sf(cville_daymet_tmax)
cville_daymet_tyear <- st_as_sf(cville_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(cville_daymet_tmn) <- NULL
st_geometry(cville_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
cville_daymet_tmn1 <- cville_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
cville_daymet_tyear <- cville_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# Wrote a function for the conversion to allow for simple sapply over all columns
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
cville_daymet_tmn1[,5:127] <- sapply(cville_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
cville_daymet_tyear[,5:45] <- sapply(cville_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
cville_daymet_tmn1long <- pivot_longer(cville_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tmn1) * length(1980:2020)

# Rename columns 
cville_daymet_tmn1long <- cville_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
cville_daymet_tyearlong <- pivot_longer(cville_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tyear) * length(1980:2020)

# Rename columns 
cville_daymet_tyearlong <- cville_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
cville_alltempdat <- merge(cville_daymet_tmn1long, cville_daymet_tyearlong, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as cville_daymet
# cville_pmonth <- st_transform(cville_tracts, projection(cville_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use cville_pmonth in the same process as what's done below with annual precipitation. 
cville_pyear <- st_transform(cville_tracts, projection(cville_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# cville_daymet_pdatmonth <- raster::extract(cville_daymetblock$prcp, cville_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_pdatyear <- raster::extract(cville_daymetblockyear$prcp, cville_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# cville_daymet_pdatmonthsf <- st_as_sf(cville_daymet_pdatmonth)
cville_daymet_pdatyearsf <- st_as_sf(cville_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(cville_daymet_pdatmonthsf) <- NULL
st_geometry(cville_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
cville_daymet_pdatyearsf1 <- cville_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
cville_daymet_pdatyearsf1[,5:45] <- sapply(cville_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
cville_daymet_pdatyearsf1long <- pivot_longer(cville_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
cville_daymet_pdatyearsf1long <- cville_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
cville_alldaymettract <- merge(cville_alltempdat, cville_daymet_pdatyearsf1long, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Adding county names 
cville_alldaymettract$CountyName <- cville_alldaymettract$COUNTYFP %>%
  recode("003" = "Albemarle", "540" = "Charlottesville", "079" = "Greene", "065" = "Fluvanna", "109" = "Louisa", "125" =
           "Nelson")

## Writing out block level data to csv ## 
write.csv(cville_alldaymettract, 'daymet_cville_tract.csv', row.names = F)

# ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Re-projecting to county level --------------------------------------------------------------------------------------------
## Just like with the tract level, we just need to use the county level polygons during extraction to get tract
# level data 

# Temperature Max -----------------------------------
# reproject polygons to same CRS as cville_daymetblock
cville_cmon <- st_transform(cville_counties, projection(cville_daymetblock$tmax@crs))
cville_cann <- st_transform(cville_counties, projection(cville_daymetblockyear$tmax@crs))

# Extract and summarize tmax
# Using mean as the summary statistic 
cville_daymet_tmax <- raster::extract(cville_daymetblock$tmax, cville_cmon, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_tmaxyear <- raster::extract(cville_daymetblockyear$tmax, cville_cann, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
cville_daymet_tmn <- st_as_sf(cville_daymet_tmax)
cville_daymet_tyear <- st_as_sf(cville_daymet_tmaxyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
st_geometry(cville_daymet_tmn) <- NULL
st_geometry(cville_daymet_tyear) <- NULL

# Select variables we  want to keep and renaming columns
## Month
cville_daymet_tmn1 <- cville_daymet_tmn %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(c(".06.", ".07", ".08"))) # Selecting columns for the 
# months June, July, and August -- the numeric values in the contains argument refers to the 
# month, column names are year.month.day

# Renaming columns to specify temperature and have the month name rather than number 
# All columns have the same day value, thankfully, so that's included in the str_replace
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".06.16"), "_June_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".07.16"), "_July_AvgMaxTC")
colnames(cville_daymet_tmn1) <- str_replace(colnames(cville_daymet_tmn1), fixed(".08.16"), "_Aug_AvgMaxTC")

## Year
# Renaming columns to specify temperature and have the month name rather than number 
# Here some of the columns have 01 for the day, some have 02, so the same code is repeated to 
# capture both varriations 
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.01", "_AvgAnnMaxTC")
colnames(cville_daymet_tyear) <- str_replace(colnames(cville_daymet_tyear), ".07.02", "_AvgAnnMaxTC")
cville_daymet_tyear <- cville_daymet_tyear %>% 
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains("_AvgAnnMaxTC"))

# Converting Celcius temperatures to Fahrenheit
# Wrote a function for the conversion to allow for simple sapply over all columns
# column numbers = the columns that have precipitation values, will need to be changed with new data 
# (the first four columns should still be geocodes, so should still start with column 5, but the end column
# number will be different when pulling for fewer years at a time)
## Month
cville_daymet_tmn1[,5:127] <- sapply(cville_daymet_tmn1[,5:127], FUN = converttemp)

## Year 
cville_daymet_tyear[,5:45] <- sapply(cville_daymet_tyear[,5:45], FUN = converttemp)

# Converting data frames from wide to long before merging them # 
## Month
cville_daymet_tmn1long <- pivot_longer(cville_daymet_tmn1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                       names_to = c('year', '.value'),
                                       names_pattern = 'X(\\d+)_(\\w+)_AvgMaxTC')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tmn1) * length(1980:2020)

# Rename columns 
cville_daymet_tmn1long <- cville_daymet_tmn1long %>% 
  rename(June_AvgMaxTF = June, July_AvgMaxTF = July, Aug_AvgMaxTF = Aug)

## Year
cville_daymet_tyearlong <- pivot_longer(cville_daymet_tyear, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                        names_to = c('year', '.value'),
                                        names_pattern = 'X(\\d+)_(\\w+)')
# Check that the number of rows in output is correct (i.e., original nrow times number of years):
nrow(cville_daymet_tyear) * length(1980:2020)

# Rename columns 
cville_daymet_tyearlong <- cville_daymet_tyearlong %>% 
  rename(AvgAnnMaxTF = AvgAnnMaxTC)

# Merging all the temperature data together
cville_alltempdat <- merge(cville_daymet_tmn1long, cville_daymet_tyearlong, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

# Precipitation ------------------------------------
# reproject polygons to same CRS as cville_daymet
# cville_pmonth <- st_transform(cville_counties, projection(cville_daymetblock$prcp@crs)) would run this if interested in monthly
# precipitation, and then use cville_pmonth in the same process as what's done below with annual precipitation. 
cville_pyear <- st_transform(cville_counties, projection(cville_daymetblockyear$prcp@crs))

# Extract and summarize precipitation
# Using mean as the summary statistic 
# cville_daymet_pdatmonth <- raster::extract(cville_daymetblock$prcp, cville_pmonth, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)
cville_daymet_pdatyear <- raster::extract(cville_daymetblockyear$prcp, cville_pyear, df = TRUE, fun = mean, na.rm = TRUE, sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
# cville_daymet_pdatmonthsf <- st_as_sf(cville_daymet_pdatmonth)
cville_daymet_pdatyearsf <- st_as_sf(cville_daymet_pdatyear)

# Make Geography null to turn the above objects into dataframes for easier manipulation
# st_geometry(cville_daymet_pdatmonthsf) <- NULL
st_geometry(cville_daymet_pdatyearsf) <- NULL

# Select variables we  want to keep and renaming columns
## Year
cville_daymet_pdatyearsf1 <- cville_daymet_pdatyearsf %>%
  dplyr::select(STATEFP, COUNTYFP, GEOID, NAMELSAD, contains(".07"))

# Rename columns to specify year and total annual precipitation
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.01"), "_totprec")
colnames(cville_daymet_pdatyearsf1) <- str_replace(colnames(cville_daymet_pdatyearsf1), fixed(".07.02"), "_totprec")

# Converting the annual precipitation from mm to inches 
# column numbers = the columns that have precipitation values, will need to be changed with new data 
cville_daymet_pdatyearsf1[,5:45] <- sapply(cville_daymet_pdatyearsf1[,5:45], FUN = convertp)

# Converting data frames from wide to long before merging them
cville_daymet_pdatyearsf1long <- pivot_longer(cville_daymet_pdatyearsf1, cols = -c(STATEFP, COUNTYFP, GEOID, NAMELSAD),
                                              names_to = c('year', '.value'),
                                              names_pattern = 'X(\\d+)_(\\w+)')

# Rename precipitation column to specify that it's in inches
cville_daymet_pdatyearsf1long <- cville_daymet_pdatyearsf1long %>% 
  rename(TotpercInch = totprec)

# Merging the percipitation data with the temperature data
cville_alldaymetcounty <- merge(cville_alltempdat, cville_daymet_pdatyearsf1long, by = c('GEOID', 'year', 'STATEFP', 'COUNTYFP', 'NAMELSAD'))

## Writing out block level data to csv ## 
write.csv(cville_alldaymetcounty, 'daymet_cville_county.csv', row.names = F)










