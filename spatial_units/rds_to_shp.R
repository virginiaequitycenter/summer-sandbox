library(tidyverse)
library(sf)

setwd("/Users/chasedawson/dev/uva_equity_center/summer-sandbox")

# cville counties
cville_counties <- readRDS("spatial_units/data/cville_counties.RDS")
st_write(cville_counties, "spatial_units/data/cville_counties.shp")

# cville tracts
cville_tracts <- readRDS("spatial_units/data/cville_tracts.RDS")
st_write(cville_tracts, "spatial_units/data/cville_tracts.shp")

# cville blkgps
cville_blkgps <- readRDS("spatial_units/data/cville_blkgps.RDS")
st_write(cville_blkgps, "spatial_units/data/cville_blkgps.shp")

# cville blocks
cville_blocks <- readRDS("spatial_units/data/cville_blocks.RDS")
cville_blocks <- cville_blocks %>%
  select(-c(STATEFP, COUNTYFP)) %>%
  rename(STATEFP = STATEFP10,
         COUNTYFP = COUNTYFP10,
         TRACTCE = TRACTCE10,
         BLOCKCE = BLOCKCE10,
         GEOID = GEOID10,
         NAME = NAME10,
         MTFCC = MTFCC10,
         UR = UR10,
         UACE = UACE10,
         FUNCSTAT = FUNCSTAT10,
         ALAND = ALAND10,
         AWATER = AWATER10,
         INTPTLAT = INTPTLAT10,
         INTPTLTLON = INTPTLON10
  )
st_write(cville_blocks, "spatial_units/data/cville_blocks.shp")

# eastshore counties
eastshore_counties <- readRDS("spatial_units/data/eastshore_counties.RDS")
st_write(eastshore_counties, "spatial_units/data/eastshore_counties.shp")

# eastshore tracts
eastshore_tracts <- readRDS("spatial_units/data/eastshore_tracts.RDS")
st_write(eastshore_tracts, "spatial_units/data/eastshore_tracts.shp")

# eastshore block groups
eastshore_blkgps <- readRDS("spatial_units/data/eastshore_blkgps.RDS")
st_write(eastshore_blkgps, "spatial_units/data/eastshore_blkgps.shp")

# eastshore blocks
eastshore_blocks <- readRDS("spatial_units/data/eastshore_blocks.RDS")
eastshore_blocks <- eastshore_blocks %>%
  select(-c(STATEFP, COUNTYFP)) %>%
  rename(STATEFP = STATEFP10,
         COUNTYFP = COUNTYFP10,
         TRACTCE = TRACTCE10,
         BLOCKCE = BLOCKCE10,
         GEOID = GEOID10,
         NAME = NAME10,
         MTFCC = MTFCC10,
         UR = UR10,
         UACE = UACE10,
         FUNCSTAT = FUNCSTAT10,
         ALAND = ALAND10,
         AWATER = AWATER10,
         INTPTLAT = INTPTLAT10,
         INTPTLTLON = INTPTLON10
  ) 
st_write(eastshore_blocks, "spatial_units/data/eastshore_blocks.shp")
