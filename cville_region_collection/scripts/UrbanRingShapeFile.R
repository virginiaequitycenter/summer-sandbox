
library(tidyverse)
library(sf)
library(leaflet)

# Read in Cville shape files 

tract <- readRDS("cville_tracts.RDS")
tract <- st_transform(tract, crs = 4326)

blkgr <- readRDS("cville_blkgps.RDS")
blkgr <- st_transform(blkgr, crs = 4326)
blkgr$tract <- paste0(blkgr$STATEFP, blkgr$COUNTYFP, blkgr$TRACTCE)

## Manually selecting the tracts and block groups of interest

# Tracts in the urban ring for filtering:
urbanring <- c("51003010602", "51003010601", 
               "51003010700", "51003010901", "51003010902",
               "51003010903", "51003011302", "51003011303",
               "51003010602")

## Block groups
blkgr <- blkgr %>% 
  filter(blkgr$tract %in% urbanring | blkgr$COUNTYFP == "540")
View(blkgr[which(!blkgr$tract %in% urbanring),]) ## double checking that block groups not in urban ring are in Cville 

ggplot(blkgr) + geom_sf()

st_write(blkgr, "CvilleUrbanRing_blkgr.shp")
write_rds(blkgr, "CvilleUrbanRing_blkgr.RDS")

## Tract
tract <- tract %>% 
  filter(tract$GEOID %in% urbanring | tract$COUNTYFP == "540")
View(tract[which(!tract$GEOID %in% urbanring),])

ggplot(tract) + geom_sf()

st_write(tract, "CvilleUrbanRing_tract.shp")
write_rds(tract, "CvilleUrbanRing_tract.RDS")

## Below are attempts to figure out a way to creat a shape file with the urban ring using publicly available 
# shape files from Albemarle county. In the end, it was easier to create one manually using a list of 
# tract and block groups that are of interest

# Albemarle urban ring shapefiles?

# Download shapefiles ----
# comp plan areas
url <- "https://gisweb.albemarle.org/gisdata/CompPlan/Comp_Plan_Areas.zip"
destfile <- "~/Downloads/albco_compplanarea.zip" # you may have to change this to make this work for you
download.file(url, destfile)
unzip("~/Downloads/albco_compplanarea.zip", exdir = "~/Downloads/albco_compplanarea/")

comp <- st_read("~/Downloads/albco_compplanarea/COMP_PLAN_AREAS.shp")

# Comp plan files could work ----
# Comp plan boundaries could work -- not sure which ones these are
comp_neighborhood <- comp %>% 
  filter(Type == "Neighborhood")

comp_neighborhood <- st_transform(comp_neighborhood, crs = 4326)

ggplot(comp_neighborhood) + geom_sf()


# The neighborhood shape files from the county are not the same shapes 
# as census tracts or census block groups, so we need to filter the census shape files
# to those that contain the neighborhood shape files within it--but need to 
# retain the original shapes of the census tracts/block groups

# Trying to use st_intersect 
# testblkgr1 <- st_intersection(comp_neighborhood, blkgr)
# testblkgr2 <- st_intersection(blkgr, comp_neighborhood)
# ggplot(testblkgr1) + geom_sf()
# ggplot(testblkgr2) + geom_sf()

## It doesn't look like this will work, because I think st_intersection blends the 
# polygons together -- the resulting dataframe doesn't have the same boundaries as the 
# census tracts or block groups

# Instead, I think you can use st_join! 

# Block groups 
blkgrtest <- st_join(blkgr, comp_neighborhood, left = T, largest = TRUE)
# This gets us a data frame that still has the geometry of the block groups, 
# and it adds columns with which neighborhood row the block group intersects with
# so we can then filter by which rows overlap with a neighborhood polygon OR are 
# in Charlottesville City 

blkgrtestf <- blkgrtest %>%
  filter(Type == "Neighborhood" | COUNTYFP == "540")

ggplot(blkgrtestf) + geom_sf() + geom_sf(data = comp_neighborhood, color = "red") 
## blkgrtestf has all of the block groups within Charlottesville city,
# block groups that contained a neighborhood shape, and block groups that bordered a 
# neighborhood shape. If we want, I can figure out a way to filter out the bordering block groups--
# this might be too inclusive

st_write(blkgrtestf, "CvilleUrbanRing_blkgr.shp")
write_rds(blkgrtestf, "CvilleUrbanRing_blkgr.RDS")

# Now doing the same thing with census tracts
tracttest <- st_join(tract, comp_neighborhood, left = T, largest = TRUE)
tracttestf <- tracttest  %>%
  filter(Type == "Neighborhood" | COUNTYFP == "540")

ggplot(tracttestf) + geom_sf()

# st_write(tracttestf, "CvilleUrbanRing_tract.shp")
# write_rds(tracttestf, "CvilleUrbanRing_tract.RDS")

