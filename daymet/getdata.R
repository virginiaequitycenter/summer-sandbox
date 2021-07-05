# Making sure I understand the DAYMET data a little bit

library(tidyverse)
library(FedData)
library(raster)
library(tigris)
library(sf)

cvillefips <- c("540", "003", "065", "079", "109", "125")
cville_blkgps <- block_groups(state = "51", county = cvillefips)

# 1. Download data ----
# Try 2010 monthly
cville_daymet_2010 <- get_daymet(
  template = cville_blkgps,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 2010, 
  tempo = "mon"
)

# Plot with raster::plot
plot(cville_daymet_2010$tmax$X2010.07.16)

# Try 2020 monthly (2020 doesn't work, but 2019 does)
cville_daymet_2020 <- get_daymet(
  template = cville_blkgps,
  label = "cvl",
  elements = c("prcp", "tmin", "tmax"),
  years = 2019, 
  tempo = "mon"
)

# Plot with raster::plot
plot(cville_daymet_2020$tmax$X2019.07.16)


# 1. Check the results ----
# check CRS
crs(cville_daymet_2010$tmax@crs) # check coordinate system: WGS84
st_crs(cville_blkgps) # check locality polygons for reference: NAD83

# reproject polygons to same CRS as cville_daymet_2010
cville_blkgps <- st_transform(cville_blkgps, projection(cville_daymet_2010$tmax@crs))

# Convert raster to df
cville_daymet_2010_df <- as.data.frame(cville_daymet_2010$tmax, xy = TRUE)

ggplot() +
  geom_raster(data = cville_daymet_2010_df , aes(x = x, y = y, fill = X2010.08.16)) +
  scale_fill_viridis_c() 

ggplot(cville_daymet_2010_df, aes(x = X2010.08.16)) + geom_histogram()

# plot raster with cvl boundaries
ggplot() +
  geom_raster(data = cville_daymet_2010_df, aes(x = x, y = y, fill = X2010.08.16)) + 
  scale_fill_viridis_c() +
  geom_sf(data = cville_blkgp, color = "red", fill = NA)


# 2. Extract and summarize tmax ----
# a. impervious surface
cville_daymet_2010_extract <- raster::extract(cville_daymet_2010$tmax, 
                                              cville_blkgps, df = TRUE)

# the above keeps all cell values within a polygon
# check variation within block groups units
cville_daymet_2010_extract %>% group_by(ID) %>% 
  summarize(cells = n(), 
            miss = sum(is.na(X2010.08.16)),
            mean = mean(X2010.08.16, na.rm = TRUE), 
            std = sd(X2010.08.16, na.rm = TRUE))

# use the mean as the summary function for the moment
cville_daymet_2010_mean <- raster::extract(cville_daymet_2010$tmax, 
                                           cville_blkgps, df = TRUE, 
                                           fun = mean, na.rm = TRUE,
                                           sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
cville_daymet_2010_blkgps <- st_as_sf(cville_daymet_2010_mean)

ggplot() + 
  geom_sf(data = cville_daymet_2010_blkgps, aes(fill = X2010.08.16)) +
  scale_fill_viridis_c()
# this produces the block group level data for the Cville region
# we could also extract monthly precipitation

# I think some key measures to generate as a starting point would be 
#   total annual precipitation (so summing across years)
#   max temps for summer months (so just Jun, Jul, Aug)
# for 1980, 1990, 2000, 2010, and 2020 (if that one ever works; we could do 2019 in the meantime)
# The csv file would have the GEOID info (the STATEFP through NAMELSAD columns), 
#   the total precipitation, the three summer months, and a variable for year
#   with the data for each year stacked

# Once a block group version for Cville region is done, we should do a 
# block and tract version
# Then repeat for the Eastern Shore!
