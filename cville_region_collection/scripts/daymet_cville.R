# Get DAYMET Temperature Data
# 2021-07-28
# Tolu Odukoya

#Load Libraries 
library(knitr)
library(tidyverse)
library(stargazer)
library(sf)
library(stringi)
library(leaflet)
library(raster)
library(mapview)
library(RColorBrewer)
library(ggplot2)
library(gganimate)
library(gapminder)
library(transformr)
library(gifski)
library(ggrepel)
library(hrbrthemes)
library(FedData)
library(magrittr)
library(daymetr)
library(lubridate)
library(maps)
library(tigris)
library(ncdf4)
library(magick)
library(extrafont)
library(showtext)
library('Cairo')
library(measurements)

#TRACTS###########

# Define localities of interest
cvillefips <- c("540", "003", "065", "079", "109","125")
eastfips <- c("001", "131")

cville_trt <- tracts(state = "51", county = cvillefips)
east_trt <- tracts(state = "51", county = eastfips)

# Download data ----
cville_daymet_1980 <- get_daymet(
  template = cville_trt,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 1980, 
  tempo = "mon")

east_daymet_1980 <- get_daymet(
  template = east_trt,
  label = "est",
  elements = c("prcp", "tmin", "tmax"),
  years = 1980, 
  tempo = "mon")

cville_daymet_1990 <- get_daymet(
  template = cville_trt,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 1990, 
  tempo = "mon")

east_daymet_1990 <- get_daymet(
  template = east_trt,
  label = "est",
  elements = c("prcp", "tmin", "tmax"),
  years = 1990, 
  tempo = "mon")

cville_daymet_2000 <- get_daymet(
  template = cville_trt,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 2000, 
  tempo = "mon")

east_daymet_2000 <- get_daymet(
  template = east_trt,
  label = "est",
  elements = c("prcp", "tmin", "tmax"),
  years = 2000, 
  tempo = "mon")

cville_daymet_2010 <- get_daymet(
  template = cville_trt,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 2010, 
  tempo = "mon")

east_daymet_2010 <- get_daymet(
  template = east_trt,
  label = "est",
  elements = c("prcp", "tmin", "tmax"),
  years = 2010, 
  tempo = "mon")

cville_daymet_2020 <- get_daymet(
  template = cville_trt,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 2020, 
  tempo = "mon")

east_daymet_2020 <- get_daymet(
  template = east_trt,
  label = "est",
  elements = c("prcp", "tmin", "tmax"),
  years = 2020, 
  tempo = "mon")


# Check the results ----
# check CRS

crs(cville_daymet_1980$tmax@crs) # check coordinate system: WGS84
st_crs(cville_trt) # check locality polygons for reference: NAD83

crs(east_daymet_1980$tmax@crs) # check coordinate system: WGS84
st_crs(east_trt) # check locality polygons for reference: NAD83


# reproject polygons to same CRS as cville_daymet

cville_trt80 <- st_transform(cville_trt, projection(cville_daymet_1980$tmax@crs))
east_trt80 <- st_transform(east_trt, projection(east_daymet_1980$tmax@crs))

cville_trt90 <- st_transform(cville_trt, projection(cville_daymet_1990$tmax@crs))
east_trt90 <- st_transform(east_trt, projection(east_daymet_1990$tmax@crs))

cville_trt00 <- st_transform(cville_trt, projection(cville_daymet_2000$tmax@crs))
east_trt00 <- st_transform(east_trt, projection(east_daymet_2000$tmax@crs))

cville_trt10 <- st_transform(cville_trt, projection(cville_daymet_2010$tmax@crs))
east_trt10 <- st_transform(east_trt, projection(east_daymet_2010$tmax@crs))

cville_trt20 <- st_transform(cville_trt, projection(cville_daymet_2020$tmax@crs))
east_trt20 <- st_transform(east_trt, projection(east_daymet_2020$tmax@crs))

# 2. Extract and summarize tmax ----
# a. impervious surface
# use the mean as the summary function for the moment

cville_daymet_80mn <- raster::extract(cville_daymet_1980$tmax, 
                                      cville_trt80, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_80mn <- raster::extract(east_daymet_1980$tmax, 
                                    east_trt80, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_90mn <- raster::extract(cville_daymet_1990$tmax, 
                                      cville_trt90, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_90mn <- raster::extract(east_daymet_1990$tmax, 
                                    east_trt90, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_00mn <- raster::extract(cville_daymet_2000$tmax, 
                                      cville_trt00, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_00mn <- raster::extract(east_daymet_2000$tmax, 
                                    east_trt00, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_10mn <- raster::extract(cville_daymet_2010$tmax, 
                                      cville_trt10, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_10mn <- raster::extract(east_daymet_2010$tmax, 
                                    east_trt10, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_20mn <- raster::extract(cville_daymet_2020$tmax, 
                                      cville_trt20, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_20mn <- raster::extract(east_daymet_2020$tmax, 
                                    east_trt20, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

# the above returned a spatal polygon object; change it back to sf

cville_daymet_80_trt <- st_as_sf(cville_daymet_80mn)
east_daymet_80_trt <- st_as_sf(east_daymet_80mn)

cville_daymet_90_trt <- st_as_sf(cville_daymet_90mn)
east_daymet_90_trt <- st_as_sf(east_daymet_90mn)

cville_daymet_00_trt <- st_as_sf(cville_daymet_00mn)
east_daymet_00_trt <- st_as_sf(east_daymet_00mn)

cville_daymet_10_trt <- st_as_sf(cville_daymet_10mn)
east_daymet_10_trt <- st_as_sf(east_daymet_10mn)

cville_daymet_20_trt <- st_as_sf(cville_daymet_20mn)
east_daymet_20_trt <- st_as_sf(east_daymet_20mn)


#Precipitation 

# reproject polygons to same CRS as cville_daymet

cville_trt80p <- st_transform(cville_trt, projection(cville_daymet_1980$prcp@crs))
east_trt80p <- st_transform(east_trt, projection(east_daymet_1980$prcp@crs))

cville_trt90p <- st_transform(cville_trt, projection(cville_daymet_1990$prcp@crs))
east_trt90p <- st_transform(east_trt, projection(east_daymet_1990$prcp@crs))

cville_trt00p <- st_transform(cville_trt, projection(cville_daymet_2000$prcp@crs))
east_trt00p <- st_transform(east_trt, projection(east_daymet_2000$prcp@crs))

cville_trt10p <- st_transform(cville_trt, projection(cville_daymet_2010$prcp@crs))
east_trt10p <- st_transform(east_trt, projection(east_daymet_2010$prcp@crs))

cville_trt20p <- st_transform(cville_trt, projection(cville_daymet_2020$prcp@crs))
east_trt20p <- st_transform(east_trt, projection(east_daymet_2020$prcp@crs))


# 2. Extract and summarize precipitation ----
# a. impervious surface
# use the mean as the summary function for the moment

cville_daymet_80mnp <- raster::extract(cville_daymet_1980$prcp, 
                                       cville_trt80p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_80mnp <- raster::extract(east_daymet_1980$prcp, 
                                     east_trt80p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_90mnp <- raster::extract(cville_daymet_1990$prcp, 
                                       cville_trt90p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_90mnp <- raster::extract(east_daymet_1990$prcp, 
                                     east_trt90p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_00mnp <- raster::extract(cville_daymet_2000$prcp, 
                                       cville_trt00p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_00mnp <- raster::extract(east_daymet_2000$prcp, 
                                     east_trt00p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_10mnp <- raster::extract(cville_daymet_2010$prcp, 
                                       cville_trt10p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_10mnp <- raster::extract(east_daymet_2010$prcp, 
                                     east_trt10p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_20mnp <- raster::extract(cville_daymet_2020$prcp, 
                                       cville_trt20p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_20mnp <- raster::extract(east_daymet_2020$prcp, 
                                     east_trt20p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

# the above returned a spatal polygon object; change it back to sf

cville_daymet_80_trtp <- st_as_sf(cville_daymet_80mnp)
east_daymet_80_trtp <- st_as_sf(east_daymet_80mnp)

cville_daymet_90_trtp <- st_as_sf(cville_daymet_90mnp)
east_daymet_90_trtp <- st_as_sf(east_daymet_90mnp)

cville_daymet_00_trtp <- st_as_sf(cville_daymet_00mnp)
east_daymet_00_trtp <- st_as_sf(east_daymet_00mnp)

cville_daymet_10_trtp <- st_as_sf(cville_daymet_10mnp)
east_daymet_10_trtp <- st_as_sf(east_daymet_10mnp)

cville_daymet_20_trtp <- st_as_sf(cville_daymet_20mnp)
east_daymet_20_trtp <- st_as_sf(east_daymet_20mnp)

# Precipitation Sum. Make Geography null turning it into a dataframe automatically.

st_geometry(cville_daymet_80_trt) <- NULL
st_geometry(east_daymet_80_trt) <- NULL
st_geometry(cville_daymet_90_trt) <- NULL
st_geometry(east_daymet_90_trt) <- NULL
st_geometry(cville_daymet_00_trt) <- NULL
st_geometry(east_daymet_00_trt) <- NULL
st_geometry(cville_daymet_10_trt) <- NULL
st_geometry(east_daymet_10_trt) <- NULL
st_geometry(cville_daymet_20_trt) <- NULL
st_geometry(east_daymet_20_trt) <- NULL


st_geometry(cville_daymet_80_trtp) <- NULL
st_geometry(east_daymet_80_trtp) <- NULL
st_geometry(cville_daymet_90_trtp) <- NULL
st_geometry(east_daymet_90_trtp) <- NULL
st_geometry(cville_daymet_00_trtp) <- NULL
st_geometry(east_daymet_00_trtp) <- NULL
st_geometry(cville_daymet_10_trtp) <- NULL
st_geometry(east_daymet_10_trtp) <- NULL
st_geometry(cville_daymet_20_trtp) <- NULL
st_geometry(east_daymet_20_trtp) <- NULL

#Create the Total Precipitation Variable

cville_daymet_80_trtp$Totpcp = rowSums(cville_daymet_80_trtp[,c(13:24)], na.rm = TRUE)
east_daymet_80_trtp$Totpcp = rowSums(east_daymet_80_trtp[,c(13:24)], na.rm = TRUE)

cville_daymet_90_trtp$Totpcp = rowSums(cville_daymet_90_trtp[,c(13:24)], na.rm = TRUE)
east_daymet_90_trtp$Totpcp = rowSums(east_daymet_90_trtp[,c(13:24)], na.rm = TRUE)

cville_daymet_00_trtp$Totpcp = rowSums(cville_daymet_00_trtp[,c(13:24)], na.rm = TRUE)
east_daymet_00_trtp$Totpcp = rowSums(east_daymet_00_trtp[,c(13:24)], na.rm = TRUE)

cville_daymet_10_trtp$Totpcp = rowSums(cville_daymet_10_trtp[,c(13:24)], na.rm = TRUE)
east_daymet_10_trtp$Totpcp = rowSums(east_daymet_10_trtp[,c(13:24)], na.rm = TRUE)

cville_daymet_20_trtp$Totpcp = rowSums(cville_daymet_20_trtp[,c(13:24)], na.rm = TRUE)
east_daymet_20_trtp$Totpcp = rowSums(east_daymet_20_trtp[,c(13:24)], na.rm = TRUE)

#Select variables we  want to keep
#Max Temp

cville_daymet_80_trt1 <- cville_daymet_80_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X1980.06.16, X1980.07.16, X1980.08.16)
east_daymet_80_trt1 <- east_daymet_80_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X1980.06.16, X1980.07.16, X1980.08.16)

cville_daymet_90_trt1 <- cville_daymet_90_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X1990.06.16, X1990.07.16, X1990.08.16)
east_daymet_90_trt1 <- east_daymet_90_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X1990.06.16, X1990.07.16, X1990.08.16)

cville_daymet_00_trt1 <- cville_daymet_00_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X2000.06.16, X2000.07.16, X2000.08.16)
east_daymet_00_trt1 <- east_daymet_00_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X2000.06.16, X2000.07.16, X2000.08.16)

cville_daymet_10_trt1 <- cville_daymet_10_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X2010.06.16, X2010.07.16, X2010.08.16)
east_daymet_10_trt1 <- east_daymet_10_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X2010.06.16, X2010.07.16, X2010.08.16)

cville_daymet_20_trt1 <- cville_daymet_20_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X2020.06.16, X2020.07.16, X2020.08.16)
east_daymet_20_trt1 <- east_daymet_20_trt %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, X2020.06.16, X2020.07.16, X2020.08.16)

#Precipitation

cville_daymet_80_trtp1 <- cville_daymet_80_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)
east_daymet_80_trtp1 <- east_daymet_80_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)

cville_daymet_90_trtp1 <- cville_daymet_90_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)
east_daymet_90_trtp1 <- east_daymet_90_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)

cville_daymet_00_trtp1 <- cville_daymet_00_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)
east_daymet_00_trtp1 <- east_daymet_00_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)

cville_daymet_10_trtp1 <- cville_daymet_10_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)
east_daymet_10_trtp1 <- east_daymet_10_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)

cville_daymet_20_trtp1 <- cville_daymet_20_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)
east_daymet_20_trtp1 <- east_daymet_20_trtp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, GEOID, NAMELSAD, Totpcp)


# Rename and Create variables of interest

cville_daymet_80_trt1 <- cville_daymet_80_trt1 %>% mutate(Year = as.numeric(1980), JuneF = conv_unit(X1980.06.16, "C", "F"), JulyF = conv_unit(X1980.07.16, "C", "F"), AugustF = conv_unit(X1980.08.16, "C", "F")) %>%  
  rename(June=X1980.06.16, July=X1980.07.16, August=X1980.08.16)
east_daymet_80_trt1 <- east_daymet_80_trt1 %>% mutate(Year = as.numeric(1980), JuneF = conv_unit(X1980.06.16, "C", "F"), JulyF = conv_unit(X1980.07.16, "C", "F"), AugustF = conv_unit(X1980.08.16, "C", "F")) %>%  
  rename(June=X1980.06.16, July=X1980.07.16, August=X1980.08.16)

cville_daymet_90_trt1 <- cville_daymet_90_trt1 %>% mutate(Year = as.numeric(1990), JuneF = conv_unit(X1990.06.16, "C", "F"), JulyF = conv_unit(X1990.07.16, "C", "F"), AugustF = conv_unit(X1990.08.16, "C", "F")) %>%  
  rename(June=X1990.06.16, July=X1990.07.16, August=X1990.08.16)
east_daymet_90_trt1 <- east_daymet_90_trt1 %>% mutate(Year = as.numeric(1990), JuneF = conv_unit(X1990.06.16, "C", "F"), JulyF = conv_unit(X1990.07.16, "C", "F"), AugustF = conv_unit(X1990.08.16, "C", "F")) %>%  
  rename(June=X1990.06.16, July=X1990.07.16, August=X1990.08.16)

cville_daymet_00_trt1 <- cville_daymet_00_trt1 %>% mutate(Year = as.numeric(2000), JuneF = conv_unit(X2000.06.16, "C", "F"), JulyF = conv_unit(X2000.07.16, "C", "F"), AugustF = conv_unit(X2000.08.16, "C", "F")) %>%  
  rename(June=X2000.06.16, July=X2000.07.16, August=X2000.08.16)
east_daymet_00_trt1 <- east_daymet_00_trt1 %>% mutate(Year = as.numeric(2000), JuneF = conv_unit(X2000.06.16, "C", "F"), JulyF = conv_unit(X2000.07.16, "C", "F"), AugustF = conv_unit(X2000.08.16, "C", "F")) %>%  
  rename(June=X2000.06.16, July=X2000.07.16, August=X2000.08.16)

cville_daymet_10_trt1 <- cville_daymet_10_trt1 %>% mutate(Year = as.numeric(2010), JuneF = conv_unit(X2010.06.16, "C", "F"), JulyF = conv_unit(X2010.07.16, "C", "F"), AugustF = conv_unit(X2010.08.16, "C", "F")) %>%  
  rename(June=X2010.06.16, July=X2010.07.16, August=X2010.08.16)
east_daymet_10_trt1 <- east_daymet_10_trt1 %>% mutate(Year = as.numeric(2010), JuneF = conv_unit(X2010.06.16, "C", "F"), JulyF = conv_unit(X2010.07.16, "C", "F"), AugustF = conv_unit(X2010.08.16, "C", "F")) %>%  
  rename(June=X2010.06.16, July=X2010.07.16, August=X2010.08.16)

cville_daymet_20_trt1 <- cville_daymet_20_trt1 %>% mutate(Year = as.numeric(2020), JuneF = conv_unit(X2020.06.16, "C", "F"), JulyF = conv_unit(X2020.07.16, "C", "F"), AugustF = conv_unit(X2020.08.16, "C", "F")) %>%  
  rename(June=X2020.06.16, July=X2020.07.16, August=X2020.08.16)
east_daymet_20_trt1 <- east_daymet_20_trt1 %>% mutate(Year = as.numeric(2020), JuneF = conv_unit(X2020.06.16, "C", "F"), JulyF = conv_unit(X2020.07.16, "C", "F"), AugustF = conv_unit(X2020.08.16, "C", "F")) %>%  
  rename(June=X2020.06.16, July=X2020.07.16, August=X2020.08.16)

#Precipitation 

cville_daymet_80_trtp1 <- cville_daymet_80_trtp1 %>% mutate(Year = as.numeric(1980), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_80_trtp1 <- east_daymet_80_trtp1 %>% mutate(Year = as.numeric(1980), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_90_trtp1 <- cville_daymet_90_trtp1 %>% mutate(Year = as.numeric(1990),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_90_trtp1 <- east_daymet_90_trtp1 %>% mutate(Year = as.numeric(1990), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_00_trtp1 <- cville_daymet_00_trtp1 %>% mutate(Year = as.numeric(2000), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_00_trtp1 <- east_daymet_00_trtp1 %>% mutate(Year = as.numeric(2000), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_10_trtp1 <- cville_daymet_10_trtp1 %>% mutate(Year = as.numeric(2010),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_10_trtp1 <- east_daymet_10_trtp1 %>% mutate(Year = as.numeric(2010),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_20_trtp1 <- cville_daymet_20_trtp1 %>% mutate(Year = as.numeric(2020),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_20_trtp1 <- east_daymet_20_trtp1 %>% mutate(Year = as.numeric(2020),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

# Gather and merge 

cville_daymet_80_trt2 <- merge(cville_daymet_80_trt1, cville_daymet_80_trtp1)
east_daymet_80_trt2 <- merge(east_daymet_80_trt1, east_daymet_80_trtp1)

cville_daymet_90_trt2 <- merge(cville_daymet_90_trt1, cville_daymet_90_trtp1)
east_daymet_90_trt2 <- merge(east_daymet_90_trt1, east_daymet_90_trtp1)

cville_daymet_00_trt2 <- merge(cville_daymet_00_trt1, cville_daymet_00_trtp1)
east_daymet_00_trt2 <- merge(east_daymet_00_trt1, east_daymet_00_trtp1)

cville_daymet_10_trt2 <- merge(cville_daymet_10_trt1, cville_daymet_10_trtp1)
east_daymet_10_trt2 <- merge(east_daymet_10_trt1, east_daymet_10_trtp1)

cville_daymet_20_trt2 <- merge(cville_daymet_20_trt1, cville_daymet_20_trtp1)
east_daymet_20_trt2 <- merge(east_daymet_20_trt1, east_daymet_20_trtp1)

cville_daymet_t <- do.call("rbind", list(cville_daymet_80_trt2, cville_daymet_90_trt2, cville_daymet_00_trt2, cville_daymet_10_trt2, cville_daymet_20_trt2))
eastern_daymet_t <- do.call("rbind", list(east_daymet_80_trt2, east_daymet_90_trt2, east_daymet_00_trt2, east_daymet_10_trt2, east_daymet_20_trt2))

cville_daymet_t <- cville_daymet_t %>% mutate_if(is.numeric, round, 2)
eastern_daymet_t<- eastern_daymet_t %>% mutate_if(is.numeric, round, 2)


# Save to csv
write_csv(cville_daymet_t, file = "daymet_cville_tract.csv")
write_csv(eastern_daymet_t, file = "daymet_eastern_tract.csv")

#BLOCK GROUP#####

cville_blkgps <- block_groups(state = "51", county = cvillefips)
east_blkgps <- block_groups(state = "51", county = eastfips)


# Check the results ----
# check CRS

crs(cville_daymet_1980$tmax@crs) # check coordinate system: WGS84
st_crs(cville_blkgps) # check locality polygons for reference: NAD83

crs(east_daymet_1980$tmax@crs) # check coordinate system: WGS84
st_crs(east_blkgps) # check locality polygons for reference: NAD83


# reproject polygons to same CRS as cville_daymet

cville_blkgps80 <- st_transform(cville_blkgps, projection(cville_daymet_1980$tmax@crs))
east_blkgps80 <- st_transform(east_blkgps, projection(east_daymet_1980$tmax@crs))

cville_blkgps90 <- st_transform(cville_blkgps, projection(cville_daymet_1990$tmax@crs))
east_blkgps90 <- st_transform(east_blkgps, projection(east_daymet_1990$tmax@crs))

cville_blkgps00 <- st_transform(cville_blkgps, projection(cville_daymet_2000$tmax@crs))
east_blkgps00 <- st_transform(east_blkgps, projection(east_daymet_2000$tmax@crs))

cville_blkgps10 <- st_transform(cville_blkgps, projection(cville_daymet_2010$tmax@crs))
east_blkgps10 <- st_transform(east_blkgps, projection(east_daymet_2010$tmax@crs))

cville_blkgps20 <- st_transform(cville_blkgps, projection(cville_daymet_2020$tmax@crs))
east_blkgps20 <- st_transform(east_blkgps, projection(east_daymet_2020$tmax@crs))

# 2. Extract and summarize tmax ----
# a. impervious surface
# use the mean as the summary function for the moment

cville_daymet_80mn <- raster::extract(cville_daymet_1980$tmax, 
                                      cville_blkgps80, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_80mn <- raster::extract(east_daymet_1980$tmax, 
                                    east_blkgps80, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_90mn <- raster::extract(cville_daymet_1990$tmax, 
                                      cville_blkgps90, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_90mn <- raster::extract(east_daymet_1990$tmax, 
                                    east_blkgps90, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_00mn <- raster::extract(cville_daymet_2000$tmax, 
                                      cville_blkgps00, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_00mn <- raster::extract(east_daymet_2000$tmax, 
                                    east_blkgps00, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_10mn <- raster::extract(cville_daymet_2010$tmax, 
                                      cville_blkgps10, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_10mn <- raster::extract(east_daymet_2010$tmax, 
                                    east_blkgps10, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_20mn <- raster::extract(cville_daymet_2020$tmax, 
                                      cville_blkgps20, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_20mn <- raster::extract(east_daymet_2020$tmax, 
                                    east_blkgps20, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

# the above returned a spatal polygon object; change it back to sf

cville_daymet_80_bgs <- st_as_sf(cville_daymet_80mn)
east_daymet_80_bgs <- st_as_sf(east_daymet_80mn)

cville_daymet_90_bgs <- st_as_sf(cville_daymet_90mn)
east_daymet_90_bgs <- st_as_sf(east_daymet_90mn)

cville_daymet_00_bgs <- st_as_sf(cville_daymet_00mn)
east_daymet_00_bgs <- st_as_sf(east_daymet_00mn)

cville_daymet_10_bgs <- st_as_sf(cville_daymet_10mn)
east_daymet_10_bgs <- st_as_sf(east_daymet_10mn)

cville_daymet_20_bgs <- st_as_sf(cville_daymet_20mn)
east_daymet_20_bgs <- st_as_sf(east_daymet_20mn)


#Precipitation 

# reproject polygons to same CRS as cville_daymet

cville_blkgps80p <- st_transform(cville_blkgps, projection(cville_daymet_1980$prcp@crs))
east_blkgps80p <- st_transform(east_blkgps, projection(east_daymet_1980$prcp@crs))

cville_blkgps90p <- st_transform(cville_blkgps, projection(cville_daymet_1990$prcp@crs))
east_blkgps90p <- st_transform(east_blkgps, projection(east_daymet_1990$prcp@crs))

cville_blkgps00p <- st_transform(cville_blkgps, projection(cville_daymet_2000$prcp@crs))
east_blkgps00p <- st_transform(east_blkgps, projection(east_daymet_2000$prcp@crs))

cville_blkgps10p <- st_transform(cville_blkgps, projection(cville_daymet_2010$prcp@crs))
east_blkgps10p <- st_transform(east_blkgps, projection(east_daymet_2010$prcp@crs))

cville_blkgps20p <- st_transform(cville_blkgps, projection(cville_daymet_2020$prcp@crs))
east_blkgps20p <- st_transform(east_blkgps, projection(east_daymet_2020$prcp@crs))


# 2. Extract and summarize precipitation ----
# a. impervious surface
# use the mean as the summary function for the moment

cville_daymet_80mnp <- raster::extract(cville_daymet_1980$prcp, 
                                       cville_blkgps80p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_80mnp <- raster::extract(east_daymet_1980$prcp, 
                                     east_blkgps80p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_90mnp <- raster::extract(cville_daymet_1990$prcp, 
                                       cville_blkgps90p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_90mnp <- raster::extract(east_daymet_1990$prcp, 
                                     east_blkgps90p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_00mnp <- raster::extract(cville_daymet_2000$prcp, 
                                       cville_blkgps00p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_00mnp <- raster::extract(east_daymet_2000$prcp, 
                                     east_blkgps00p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_10mnp <- raster::extract(cville_daymet_2010$prcp, 
                                       cville_blkgps10p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_10mnp <- raster::extract(east_daymet_2010$prcp, 
                                     east_blkgps10p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_20mnp <- raster::extract(cville_daymet_2020$prcp, 
                                       cville_blkgps20p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_20mnp <- raster::extract(east_daymet_2020$prcp, 
                                     east_blkgps20p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

# the above returned a spatal polygon object; change it back to sf

cville_daymet_80_bgsp <- st_as_sf(cville_daymet_80mnp)
east_daymet_80_bgsp <- st_as_sf(east_daymet_80mnp)

cville_daymet_90_bgsp <- st_as_sf(cville_daymet_90mnp)
east_daymet_90_bgsp <- st_as_sf(east_daymet_90mnp)

cville_daymet_00_bgsp <- st_as_sf(cville_daymet_00mnp)
east_daymet_00_bgsp <- st_as_sf(east_daymet_00mnp)

cville_daymet_10_bgsp <- st_as_sf(cville_daymet_10mnp)
east_daymet_10_bgsp <- st_as_sf(east_daymet_10mnp)

cville_daymet_20_bgsp <- st_as_sf(cville_daymet_20mnp)
east_daymet_20_bgsp <- st_as_sf(east_daymet_20mnp)

# Precipitation Sum. Make Geography null turning it into a dataframe automatically.

st_geometry(cville_daymet_80_bgs) <- NULL
st_geometry(east_daymet_80_bgs) <- NULL
st_geometry(cville_daymet_90_bgs) <- NULL
st_geometry(east_daymet_90_bgs) <- NULL
st_geometry(cville_daymet_00_bgs) <- NULL
st_geometry(east_daymet_00_bgs) <- NULL
st_geometry(cville_daymet_10_bgs) <- NULL
st_geometry(east_daymet_10_bgs) <- NULL
st_geometry(cville_daymet_20_bgs) <- NULL
st_geometry(east_daymet_20_bgs) <- NULL


st_geometry(cville_daymet_80_bgsp) <- NULL
st_geometry(east_daymet_80_bgsp) <- NULL
st_geometry(cville_daymet_90_bgsp) <- NULL
st_geometry(east_daymet_90_bgsp) <- NULL
st_geometry(cville_daymet_00_bgsp) <- NULL
st_geometry(east_daymet_00_bgsp) <- NULL
st_geometry(cville_daymet_10_bgsp) <- NULL
st_geometry(east_daymet_10_bgsp) <- NULL
st_geometry(cville_daymet_20_bgsp) <- NULL
st_geometry(east_daymet_20_bgsp) <- NULL

#Create the total precipitation Variable 

cville_daymet_80_bgsp$Totpcp = rowSums(cville_daymet_80_bgsp[,c(13:24)], na.rm = TRUE)
east_daymet_80_bgsp$Totpcp = rowSums(east_daymet_80_bgsp[,c(13:24)], na.rm = TRUE)

cville_daymet_90_bgsp$Totpcp = rowSums(cville_daymet_90_bgsp[,c(13:24)], na.rm = TRUE)
east_daymet_90_bgsp$Totpcp = rowSums(east_daymet_90_bgsp[,c(13:24)], na.rm = TRUE)

cville_daymet_00_bgsp$Totpcp = rowSums(cville_daymet_00_bgsp[,c(13:24)], na.rm = TRUE)
east_daymet_00_bgsp$Totpcp = rowSums(east_daymet_00_bgsp[,c(13:24)], na.rm = TRUE)

cville_daymet_10_bgsp$Totpcp = rowSums(cville_daymet_10_bgsp[,c(13:24)], na.rm = TRUE)
east_daymet_10_bgsp$Totpcp = rowSums(east_daymet_10_bgsp[,c(13:24)], na.rm = TRUE)

cville_daymet_20_bgsp$Totpcp = rowSums(cville_daymet_20_bgsp[,c(13:24)], na.rm = TRUE)
east_daymet_20_bgsp$Totpcp = rowSums(east_daymet_20_bgsp[,c(13:24)], na.rm = TRUE)

#Select variables we  want to keep
#Max Temp

cville_daymet_80_bgs1 <- cville_daymet_80_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X1980.06.16, X1980.07.16, X1980.08.16)
east_daymet_80_bgs1 <- east_daymet_80_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X1980.06.16, X1980.07.16, X1980.08.16)

cville_daymet_90_bgs1 <- cville_daymet_90_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X1990.06.16, X1990.07.16, X1990.08.16)
east_daymet_90_bgs1 <- east_daymet_90_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X1990.06.16, X1990.07.16, X1990.08.16)

cville_daymet_00_bgs1 <- cville_daymet_00_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X2000.06.16, X2000.07.16, X2000.08.16)
east_daymet_00_bgs1 <- east_daymet_00_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X2000.06.16, X2000.07.16, X2000.08.16)

cville_daymet_10_bgs1 <- cville_daymet_10_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X2010.06.16, X2010.07.16, X2010.08.16)
east_daymet_10_bgs1 <- east_daymet_10_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X2010.06.16, X2010.07.16, X2010.08.16)

cville_daymet_20_bgs1 <- cville_daymet_20_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X2020.06.16, X2020.07.16, X2020.08.16)
east_daymet_20_bgs1 <- east_daymet_20_bgs %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, X2020.06.16, X2020.07.16, X2020.08.16)

#Precipitation

cville_daymet_80_bgsp1 <- cville_daymet_80_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)
east_daymet_80_bgsp1 <- east_daymet_80_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)

cville_daymet_90_bgsp1 <- cville_daymet_90_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)
east_daymet_90_bgsp1 <- east_daymet_90_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)

cville_daymet_00_bgsp1 <- cville_daymet_00_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)
east_daymet_00_bgsp1 <- east_daymet_00_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)

cville_daymet_10_bgsp1 <- cville_daymet_10_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)
east_daymet_10_bgsp1 <- east_daymet_10_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)

cville_daymet_20_bgsp1 <- cville_daymet_20_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)
east_daymet_20_bgsp1 <- east_daymet_20_bgsp %>%
  dplyr::select(STATEFP, COUNTYFP, TRACTCE, BLKGRPCE,  GEOID, NAMELSAD, Totpcp)


# Rename and Create variables of interest

cville_daymet_80_bgs1 <- cville_daymet_80_bgs1 %>% mutate(Year = as.numeric(1980), JuneF = conv_unit(X1980.06.16, "C", "F"), JulyF = conv_unit(X1980.07.16, "C", "F"), AugustF = conv_unit(X1980.08.16, "C", "F")) %>%  
  rename(June=X1980.06.16, July=X1980.07.16, August=X1980.08.16)
east_daymet_80_bgs1 <- east_daymet_80_bgs1 %>% mutate(Year = as.numeric(1980), JuneF = conv_unit(X1980.06.16, "C", "F"), JulyF = conv_unit(X1980.07.16, "C", "F"), AugustF = conv_unit(X1980.08.16, "C", "F")) %>%  
  rename(June=X1980.06.16, July=X1980.07.16, August=X1980.08.16)

cville_daymet_90_bgs1 <- cville_daymet_90_bgs1 %>% mutate(Year = as.numeric(1990), JuneF = conv_unit(X1990.06.16, "C", "F"), JulyF = conv_unit(X1990.07.16, "C", "F"), AugustF = conv_unit(X1990.08.16, "C", "F")) %>%  
  rename(June=X1990.06.16, July=X1990.07.16, August=X1990.08.16)
east_daymet_90_bgs1 <- east_daymet_90_bgs1 %>% mutate(Year = as.numeric(1990), JuneF = conv_unit(X1990.06.16, "C", "F"), JulyF = conv_unit(X1990.07.16, "C", "F"), AugustF = conv_unit(X1990.08.16, "C", "F")) %>%  
  rename(June=X1990.06.16, July=X1990.07.16, August=X1990.08.16)

cville_daymet_00_bgs1 <- cville_daymet_00_bgs1 %>% mutate(Year = as.numeric(2000), JuneF = conv_unit(X2000.06.16, "C", "F"), JulyF = conv_unit(X2000.07.16, "C", "F"), AugustF = conv_unit(X2000.08.16, "C", "F")) %>%  
  rename(June=X2000.06.16, July=X2000.07.16, August=X2000.08.16)
east_daymet_00_bgs1 <- east_daymet_00_bgs1 %>% mutate(Year = as.numeric(2000), JuneF = conv_unit(X2000.06.16, "C", "F"), JulyF = conv_unit(X2000.07.16, "C", "F"), AugustF = conv_unit(X2000.08.16, "C", "F")) %>%  
  rename(June=X2000.06.16, July=X2000.07.16, August=X2000.08.16)

cville_daymet_10_bgs1 <- cville_daymet_10_bgs1 %>% mutate(Year = as.numeric(2010), JuneF = conv_unit(X2010.06.16, "C", "F"), JulyF = conv_unit(X2010.07.16, "C", "F"), AugustF = conv_unit(X2010.08.16, "C", "F")) %>%  
  rename(June=X2010.06.16, July=X2010.07.16, August=X2010.08.16)
east_daymet_10_bgs1 <- east_daymet_10_bgs1 %>% mutate(Year = as.numeric(2010), JuneF = conv_unit(X2010.06.16, "C", "F"), JulyF = conv_unit(X2010.07.16, "C", "F"), AugustF = conv_unit(X2010.08.16, "C", "F")) %>%  
  rename(June=X2010.06.16, July=X2010.07.16, August=X2010.08.16)

cville_daymet_20_bgs1 <- cville_daymet_20_bgs1 %>% mutate(Year = as.numeric(2020), JuneF = conv_unit(X2020.06.16, "C", "F"), JulyF = conv_unit(X2020.07.16, "C", "F"), AugustF = conv_unit(X2020.08.16, "C", "F")) %>%  
  rename(June=X2020.06.16, July=X2020.07.16, August=X2020.08.16)
east_daymet_20_bgs1 <- east_daymet_20_bgs1 %>% mutate(Year = as.numeric(2020), JuneF = conv_unit(X2020.06.16, "C", "F"), JulyF = conv_unit(X2020.07.16, "C", "F"), AugustF = conv_unit(X2020.08.16, "C", "F")) %>%  
  rename(June=X2020.06.16, July=X2020.07.16, August=X2020.08.16)

#Precipitation 

cville_daymet_80_bgsp1 <- cville_daymet_80_bgsp1 %>% mutate(Year = as.numeric(1980), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_80_bgsp1 <- east_daymet_80_bgsp1 %>% mutate(Year = as.numeric(1980), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_90_bgsp1 <- cville_daymet_90_bgsp1 %>% mutate(Year = as.numeric(1990),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_90_bgsp1 <- east_daymet_90_bgsp1 %>% mutate(Year = as.numeric(1990), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_00_bgsp1 <- cville_daymet_00_bgsp1 %>% mutate(Year = as.numeric(2000), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_00_bgsp1 <- east_daymet_00_bgsp1 %>% mutate(Year = as.numeric(2000), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_10_bgsp1 <- cville_daymet_10_bgsp1 %>% mutate(Year = as.numeric(2010),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_10_bgsp1 <- east_daymet_10_bgsp1 %>% mutate(Year = as.numeric(2010),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

cville_daymet_20_bgsp1 <- cville_daymet_20_bgsp1 %>% mutate(Year = as.numeric(2020),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 
east_daymet_20_bgsp1 <- east_daymet_20_bgsp1 %>% mutate(Year = as.numeric(2020),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) 

# Gather and merge 

cville_daymet_80_bgs2 <- merge(cville_daymet_80_bgs1, cville_daymet_80_bgsp1)
east_daymet_80_bgs2 <- merge(east_daymet_80_bgs1, east_daymet_80_bgsp1)

cville_daymet_90_bgs2 <- merge(cville_daymet_90_bgs1, cville_daymet_90_bgsp1)
east_daymet_90_bgs2 <- merge(east_daymet_90_bgs1, east_daymet_90_bgsp1)

cville_daymet_00_bgs2 <- merge(cville_daymet_00_bgs1, cville_daymet_00_bgsp1)
east_daymet_00_bgs2 <- merge(east_daymet_00_bgs1, east_daymet_00_bgsp1)

cville_daymet_10_bgs2 <- merge(cville_daymet_10_bgs1, cville_daymet_10_bgsp1)
east_daymet_10_bgs2 <- merge(east_daymet_10_bgs1, east_daymet_10_bgsp1)

cville_daymet_20_bgs2 <- merge(cville_daymet_20_bgs1, cville_daymet_20_bgsp1)
east_daymet_20_bgs2 <- merge(east_daymet_20_bgs1, east_daymet_20_bgsp1)

cville_daymet_bg <- do.call("rbind", list(cville_daymet_80_bgs2, cville_daymet_90_bgs2, cville_daymet_00_bgs2, cville_daymet_10_bgs2, cville_daymet_20_bgs2))
eastern_daymet_bg <- do.call("rbind", list(east_daymet_80_bgs2, east_daymet_90_bgs2, east_daymet_00_bgs2, east_daymet_10_bgs2, east_daymet_20_bgs2))

cville_daymet_bg <- cville_daymet_bg %>% mutate_if(is.numeric, round, 2)
eastern_daymet_bg <- eastern_daymet_bg %>% mutate_if(is.numeric, round, 2)


# Save to csv
write_csv(cville_daymet_bg, file = "daymet_cville_blkgrps.csv")
write_csv(eastern_daymet_bg, file = "daymet_eastern_blkgrps.csv")

# BLOCKS########

# Define localities of interest

cville_blk <- blocks(state = "51", county = cvillefips)
east_blk <- blocks(state = "51", county = eastfips)


# Check the results ----
# check CRS

crs(cville_daymet_1980$tmax@crs) # check coordinate system: WGS84
st_crs(cville_blk) # check locality polygons for reference: NAD83

crs(east_daymet_1980$tmax@crs) # check coordinate system: WGS84
st_crs(east_blk) # check locality polygons for reference: NAD83


# reproject polygons to same CRS as cville_daymet

cville_blk80 <- st_transform(cville_blk, projection(cville_daymet_1980$tmax@crs))
east_blk80 <- st_transform(east_blk, projection(east_daymet_1980$tmax@crs))

cville_blk90 <- st_transform(cville_blk, projection(cville_daymet_1990$tmax@crs))
east_blk90 <- st_transform(east_blk, projection(east_daymet_1990$tmax@crs))

cville_blk00 <- st_transform(cville_blk, projection(cville_daymet_2000$tmax@crs))
east_blk00 <- st_transform(east_blk, projection(east_daymet_2000$tmax@crs))

cville_blk10 <- st_transform(cville_blk, projection(cville_daymet_2010$tmax@crs))
east_blk10 <- st_transform(east_blk, projection(east_daymet_2010$tmax@crs))

cville_blk20 <- st_transform(cville_blk, projection(cville_daymet_2020$tmax@crs))
east_blk20 <- st_transform(east_blk, projection(east_daymet_2020$tmax@crs))

# 2. Extract and summarize tmax ----
# a. impervious surface
# use the mean as the summary function for the moment

cville_daymet_80mn <- raster::extract(cville_daymet_1980$tmax, 
                                      cville_blk80, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_80mn <- raster::extract(east_daymet_1980$tmax, 
                                    east_blk80, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_90mn <- raster::extract(cville_daymet_1990$tmax, 
                                      cville_blk90, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_90mn <- raster::extract(east_daymet_1990$tmax, 
                                    east_blk90, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_00mn <- raster::extract(cville_daymet_2000$tmax, 
                                      cville_blk00, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_00mn <- raster::extract(east_daymet_2000$tmax, 
                                    east_blk00, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_10mn <- raster::extract(cville_daymet_2010$tmax, 
                                      cville_blk10, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_10mn <- raster::extract(east_daymet_2010$tmax, 
                                    east_blk10, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

cville_daymet_20mn <- raster::extract(cville_daymet_2020$tmax, 
                                      cville_blk20, df = TRUE, 
                                      fun = mean, na.rm = TRUE,
                                      sp = TRUE)
east_daymet_20mn <- raster::extract(east_daymet_2020$tmax, 
                                    east_blk20, df = TRUE, 
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

# the above returned a spatal polygon object; change it back to sf

cville_daymet_80_blk <- st_as_sf(cville_daymet_80mn)
east_daymet_80_blk <- st_as_sf(east_daymet_80mn)

cville_daymet_90_blk <- st_as_sf(cville_daymet_90mn)
east_daymet_90_blk <- st_as_sf(east_daymet_90mn)

cville_daymet_00_blk <- st_as_sf(cville_daymet_00mn)
east_daymet_00_blk <- st_as_sf(east_daymet_00mn)

cville_daymet_10_blk <- st_as_sf(cville_daymet_10mn)
east_daymet_10_blk <- st_as_sf(east_daymet_10mn)

cville_daymet_20_blk <- st_as_sf(cville_daymet_20mn)
east_daymet_20_blk <- st_as_sf(east_daymet_20mn)


#Precipitation 

# reproject polygons to same CRS as cville_daymet

cville_blk80p <- st_transform(cville_blk, projection(cville_daymet_1980$prcp@crs))
east_blk80p <- st_transform(east_blk, projection(east_daymet_1980$prcp@crs))

cville_blk90p <- st_transform(cville_blk, projection(cville_daymet_1990$prcp@crs))
east_blk90p <- st_transform(east_blk, projection(east_daymet_1990$prcp@crs))

cville_blk00p <- st_transform(cville_blk, projection(cville_daymet_2000$prcp@crs))
east_blk00p <- st_transform(east_blk, projection(east_daymet_2000$prcp@crs))

cville_blk10p <- st_transform(cville_blk, projection(cville_daymet_2010$prcp@crs))
east_blk10p <- st_transform(east_blk, projection(east_daymet_2010$prcp@crs))

cville_blk20p <- st_transform(cville_blk, projection(cville_daymet_2020$prcp@crs))
east_blk20p <- st_transform(east_blk, projection(east_daymet_2020$prcp@crs))


# 2. Extract and summarize precipitation ----
# a. impervious surface
# use the mean as the summary function for the moment

cville_daymet_80mnp <- raster::extract(cville_daymet_1980$prcp, 
                                       cville_blk80p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_80mnp <- raster::extract(east_daymet_1980$prcp, 
                                     east_blk80p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_90mnp <- raster::extract(cville_daymet_1990$prcp, 
                                       cville_blk90p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_90mnp <- raster::extract(east_daymet_1990$prcp, 
                                     east_blk90p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_00mnp <- raster::extract(cville_daymet_2000$prcp, 
                                       cville_blk00p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_00mnp <- raster::extract(east_daymet_2000$prcp, 
                                     east_blk00p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_10mnp <- raster::extract(cville_daymet_2010$prcp, 
                                       cville_blk10p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_10mnp <- raster::extract(east_daymet_2010$prcp, 
                                     east_blk10p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

cville_daymet_20mnp <- raster::extract(cville_daymet_2020$prcp, 
                                       cville_blk20p, df = TRUE, 
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)
east_daymet_20mnp <- raster::extract(east_daymet_2020$prcp, 
                                     east_blk20p, df = TRUE, 
                                     fun = mean, na.rm = TRUE,
                                     sp = TRUE)

# the above returned a spatal polygon object; change it back to sf

cville_daymet_80_blkp <- st_as_sf(cville_daymet_80mnp)
east_daymet_80_blkp <- st_as_sf(east_daymet_80mnp)

cville_daymet_90_blkp <- st_as_sf(cville_daymet_90mnp)
east_daymet_90_blkp <- st_as_sf(east_daymet_90mnp)

cville_daymet_00_blkp <- st_as_sf(cville_daymet_00mnp)
east_daymet_00_blkp <- st_as_sf(east_daymet_00mnp)

cville_daymet_10_blkp <- st_as_sf(cville_daymet_10mnp)
east_daymet_10_blkp <- st_as_sf(east_daymet_10mnp)

cville_daymet_20_blkp <- st_as_sf(cville_daymet_20mnp)
east_daymet_20_blkp <- st_as_sf(east_daymet_20mnp)

# Precipitation Sum. Make Geography null turning it into a dataframe automatically.

st_geometry(cville_daymet_80_blk) <- NULL
st_geometry(east_daymet_80_blk) <- NULL
st_geometry(cville_daymet_90_blk) <- NULL
st_geometry(east_daymet_90_blk) <- NULL
st_geometry(cville_daymet_00_blk) <- NULL
st_geometry(east_daymet_00_blk) <- NULL
st_geometry(cville_daymet_10_blk) <- NULL
st_geometry(east_daymet_10_blk) <- NULL
st_geometry(cville_daymet_20_blk) <- NULL
st_geometry(east_daymet_20_blk) <- NULL


st_geometry(cville_daymet_80_blkp) <- NULL
st_geometry(east_daymet_80_blkp) <- NULL
st_geometry(cville_daymet_90_blkp) <- NULL
st_geometry(east_daymet_90_blkp) <- NULL
st_geometry(cville_daymet_00_blkp) <- NULL
st_geometry(east_daymet_00_blkp) <- NULL
st_geometry(cville_daymet_10_blkp) <- NULL
st_geometry(east_daymet_10_blkp) <- NULL
st_geometry(cville_daymet_20_blkp) <- NULL
st_geometry(east_daymet_20_blkp) <- NULL

#Create the Total Precipitation Variable

cville_daymet_80_blkp$Totpcp = rowSums(cville_daymet_80_blkp[,c(18:29)], na.rm = TRUE)
east_daymet_80_blkp$Totpcp = rowSums(east_daymet_80_blkp[,c(18:29)], na.rm = TRUE)

cville_daymet_90_blkp$Totpcp = rowSums(cville_daymet_90_blkp[,c(18:29)], na.rm = TRUE)
east_daymet_90_blkp$Totpcp = rowSums(east_daymet_90_blkp[,c(18:29)], na.rm = TRUE)

cville_daymet_00_blkp$Totpcp = rowSums(cville_daymet_00_blkp[,c(18:29)], na.rm = TRUE)
east_daymet_00_blkp$Totpcp = rowSums(east_daymet_00_blkp[,c(18:29)], na.rm = TRUE)

cville_daymet_10_blkp$Totpcp = rowSums(cville_daymet_10_blkp[,c(18:29)], na.rm = TRUE)
east_daymet_10_blkp$Totpcp = rowSums(east_daymet_10_blkp[,c(18:29)], na.rm = TRUE)

cville_daymet_20_blkp$Totpcp = rowSums(cville_daymet_20_blkp[,c(18:29)], na.rm = TRUE)
east_daymet_20_blkp$Totpcp = rowSums(east_daymet_20_blkp[,c(18:29)], na.rm = TRUE)

#Select variables we  want to keep
#Max Temp

cville_daymet_80_blk1 <- cville_daymet_80_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X1980.06.16, X1980.07.16, X1980.08.16)
east_daymet_80_blk1 <- east_daymet_80_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X1980.06.16, X1980.07.16, X1980.08.16)

cville_daymet_90_blk1 <- cville_daymet_90_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X1990.06.16, X1990.07.16, X1990.08.16)
east_daymet_90_blk1 <- east_daymet_90_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X1990.06.16, X1990.07.16, X1990.08.16)

cville_daymet_00_blk1 <- cville_daymet_00_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X2000.06.16, X2000.07.16, X2000.08.16)
east_daymet_00_blk1 <- east_daymet_00_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X2000.06.16, X2000.07.16, X2000.08.16)

cville_daymet_10_blk1 <- cville_daymet_10_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X2010.06.16, X2010.07.16, X2010.08.16)
east_daymet_10_blk1 <- east_daymet_10_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X2010.06.16, X2010.07.16, X2010.08.16)

cville_daymet_20_blk1 <- cville_daymet_20_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X2020.06.16, X2020.07.16, X2020.08.16)
east_daymet_20_blk1 <- east_daymet_20_blk %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, X2020.06.16, X2020.07.16, X2020.08.16)

#Precipitation

cville_daymet_80_blkp1 <- cville_daymet_80_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)
east_daymet_80_blkp1 <- east_daymet_80_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)

cville_daymet_90_blkp1 <- cville_daymet_90_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)
east_daymet_90_blkp1 <- east_daymet_90_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)

cville_daymet_00_blkp1 <- cville_daymet_00_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)
east_daymet_00_blkp1 <- east_daymet_00_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)

cville_daymet_10_blkp1 <- cville_daymet_10_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)
east_daymet_10_blkp1 <- east_daymet_10_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)

cville_daymet_20_blkp1 <- cville_daymet_20_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)
east_daymet_20_blkp1 <- east_daymet_20_blkp %>%
  dplyr::select(STATEFP10, COUNTYFP10, TRACTCE10, BLOCKCE10, GEOID10, NAME10, Totpcp)


# Rename and Create variables of interest

cville_daymet_80_blk1 <- cville_daymet_80_blk1 %>% mutate(Year = as.numeric(1980), JuneF = conv_unit(X1980.06.16, "C", "F"), JulyF = conv_unit(X1980.07.16, "C", "F"), AugustF = conv_unit(X1980.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X1980.06.16, July=X1980.07.16, August=X1980.08.16)
east_daymet_80_blk1 <- east_daymet_80_blk1 %>% mutate(Year = as.numeric(1980), JuneF = conv_unit(X1980.06.16, "C", "F"), JulyF = conv_unit(X1980.07.16, "C", "F"), AugustF = conv_unit(X1980.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X1980.06.16, July=X1980.07.16, August=X1980.08.16)

cville_daymet_90_blk1 <- cville_daymet_90_blk1 %>% mutate(Year = as.numeric(1990), JuneF = conv_unit(X1990.06.16, "C", "F"), JulyF = conv_unit(X1990.07.16, "C", "F"), AugustF = conv_unit(X1990.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X1990.06.16, July=X1990.07.16, August=X1990.08.16)
east_daymet_90_blk1 <- east_daymet_90_blk1 %>% mutate(Year = as.numeric(1990), JuneF = conv_unit(X1990.06.16, "C", "F"), JulyF = conv_unit(X1990.07.16, "C", "F"), AugustF = conv_unit(X1990.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X1990.06.16, July=X1990.07.16, August=X1990.08.16)

cville_daymet_00_blk1 <- cville_daymet_00_blk1 %>% mutate(Year = as.numeric(2000), JuneF = conv_unit(X2000.06.16, "C", "F"), JulyF = conv_unit(X2000.07.16, "C", "F"), AugustF = conv_unit(X2000.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X2000.06.16, July=X2000.07.16, August=X2000.08.16)
east_daymet_00_blk1 <- east_daymet_00_blk1 %>% mutate(Year = as.numeric(2000), JuneF = conv_unit(X2000.06.16, "C", "F"), JulyF = conv_unit(X2000.07.16, "C", "F"), AugustF = conv_unit(X2000.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X2000.06.16, July=X2000.07.16, August=X2000.08.16)

cville_daymet_10_blk1 <- cville_daymet_10_blk1 %>% mutate(Year = as.numeric(2010), JuneF = conv_unit(X2010.06.16, "C", "F"), JulyF = conv_unit(X2010.07.16, "C", "F"), AugustF = conv_unit(X2010.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X2010.06.16, July=X2010.07.16, August=X2010.08.16)
east_daymet_10_blk1 <- east_daymet_10_blk1 %>% mutate(Year = as.numeric(2010), JuneF = conv_unit(X2010.06.16, "C", "F"), JulyF = conv_unit(X2010.07.16, "C", "F"), AugustF = conv_unit(X2010.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X2010.06.16, July=X2010.07.16, August=X2010.08.16)

cville_daymet_20_blk1 <- cville_daymet_20_blk1 %>% mutate(Year = as.numeric(2020), JuneF = conv_unit(X2020.06.16, "C", "F"), JulyF = conv_unit(X2020.07.16, "C", "F"), AugustF = conv_unit(X2020.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X2020.06.16, July=X2020.07.16, August=X2020.08.16)
east_daymet_20_blk1 <- east_daymet_20_blk1 %>% mutate(Year = as.numeric(2020), JuneF = conv_unit(X2020.06.16, "C", "F"), JulyF = conv_unit(X2020.07.16, "C", "F"), AugustF = conv_unit(X2020.08.16, "C", "F")) %>%  
  rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10, June=X2020.06.16, July=X2020.07.16, August=X2020.08.16)

#Precipitation 

cville_daymet_80_blkp1 <- cville_daymet_80_blkp1 %>% mutate(Year = as.numeric(1980), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)
east_daymet_80_blkp1 <- east_daymet_80_blkp1 %>% mutate(Year = as.numeric(1980), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)

cville_daymet_90_blkp1 <- cville_daymet_90_blkp1 %>% mutate(Year = as.numeric(1990),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)
east_daymet_90_blkp1 <- east_daymet_90_blkp1 %>% mutate(Year = as.numeric(1990), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)

cville_daymet_00_blkp1 <- cville_daymet_00_blkp1 %>% mutate(Year = as.numeric(2000), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)
east_daymet_00_blkp1 <- east_daymet_00_blkp1 %>% mutate(Year = as.numeric(2000), Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)

cville_daymet_10_blkp1 <- cville_daymet_10_blkp1 %>% mutate(Year = as.numeric(2010),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)
east_daymet_10_blkp1 <- east_daymet_10_blkp1 %>% mutate(Year = as.numeric(2010),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)

cville_daymet_20_blkp1 <- cville_daymet_20_blkp1 %>% mutate(Year = as.numeric(2020),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)
east_daymet_20_blkp1 <- east_daymet_20_blkp1 %>% mutate(Year = as.numeric(2020),  Totpcpinch = conv_unit(Totpcp, "mm", "inch")) %>% rename(STATEFP=STATEFP10, COUNTYFP=COUNTYFP10, TRACTCE=TRACTCE10, BLOCKCE=BLOCKCE10, GEOID=GEOID10, NAME=NAME10)

# Gather and merge 

cville_daymet_80_blk2 <- merge(cville_daymet_80_blk1, cville_daymet_80_blkp1)
east_daymet_80_blk2 <- merge(east_daymet_80_blk1, east_daymet_80_blkp1)

cville_daymet_90_blk2 <- merge(cville_daymet_90_blk1, cville_daymet_90_blkp1)
east_daymet_90_blk2 <- merge(east_daymet_90_blk1, east_daymet_90_blkp1)

cville_daymet_00_blk2 <- merge(cville_daymet_00_blk1, cville_daymet_00_blkp1)
east_daymet_00_blk2 <- merge(east_daymet_00_blk1, east_daymet_00_blkp1)

cville_daymet_10_blk2 <- merge(cville_daymet_10_blk1, cville_daymet_10_blkp1)
east_daymet_10_blk2 <- merge(east_daymet_10_blk1, east_daymet_10_blkp1)

cville_daymet_20_blk2 <- merge(cville_daymet_20_blk1, cville_daymet_20_blkp1)
east_daymet_20_blk2 <- merge(east_daymet_20_blk1, east_daymet_20_blkp1)

cville_daymet_bk <- do.call("rbind", list(cville_daymet_80_blk2, cville_daymet_90_blk2, cville_daymet_00_blk2, cville_daymet_10_blk2, cville_daymet_20_blk2))
eastern_daymet_bk <- do.call("rbind", list(east_daymet_80_blk2, east_daymet_90_blk2, east_daymet_00_blk2, east_daymet_10_blk2, east_daymet_20_blk2))

cville_daymet_bk <- cville_daymet_bk %>% mutate_if(is.numeric, round, 2)
eastern_daymet_bk <- eastern_daymet_bk %>% mutate_if(is.numeric, round, 2)


# Save to csv
write_csv(cville_daymet_bk, file = "daymet_cville_block.csv")
write_csv(eastern_daymet_bk, file = "daymet_eastern_block.csv")



