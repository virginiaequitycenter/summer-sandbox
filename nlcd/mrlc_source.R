# NLCD explore
# MPC

# try downloading from mrlc

library(tidyverse)
library(sf)

# I've set up an R project so my working directory for this project is always this folder

# land cover ----
# download a data file (not sure if this is the right one)
url <- "https://s3-us-west-2.amazonaws.com/mrlc/NLCD_2016_Land_Cover_L48_20190424.zip"
download.file(url = url,
              destfile = paste(getwd(), "/2016/", "land2016.zip", sep = ""),
              mode = "wb")

# unzip
# unzip("2016/land2016.zip", exdir = "2016/land2016/")
# R's unzip can't handle this size of file reliably: https://stackoverflow.com/questions/42740206/r-possible-truncation-of-4gb-file
# unzipped manually...

# I don't fully understand what's here... and the only shapefile is in metadata, 
# which could be read in, but isn't especially helpful...
# land_sf = st_read(dsn = "2016/NLCD2016_spatial_metadata/NLCD2016_Spatial_metadata_20190205.shp")


# tree canopy ----
url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_treecanopy_2019_08_31.zip"
download.file(url = url,
              destfile = paste(getwd(), "/2016/", "tree2016.zip", sep = ""),
              mode = "wb")

# unzip
# unzip("2016/tree2016.zip", exdir = "2016/tree2016/")
# R's unzip can't handle this size of file reliably: https://stackoverflow.com/questions/42740206/r-possible-truncation-of-4gb-file
# unzipped manually...

# data appears to be in rrd and rde files; I'm not familiar with these

# deleted the downloaded files
