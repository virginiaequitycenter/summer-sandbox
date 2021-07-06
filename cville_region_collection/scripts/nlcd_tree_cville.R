# Get NLCD 2016 data for Charlottesville Region
# MPC
# 2021-07-05

library(FedData) 
library(sf)
library(raster)
library(tidyverse)
library(viridis)

# Cville region tracts, block groups, blocks
cville_tracts <- readRDS("data/cville_tracts.RDS")
cville_blkgps <- readRDS("data/cville_blkgps.RDS")
cville_blocks <- readRDS("data/cville_blocks.RDS")


# a. get tree canopy ----
tree <- get_nlcd(
  template = cville_tracts,
  label = "Charlottesville",
  dataset = "Tree_Canopy",
  year = 2016, 
  landmass = "L48"
)

plot(tree)

# # b. Check projections ----
# # Convert raster to df
# tree_df <- as.data.frame(tree, xy = TRUE)
# names(tree_df) <- c("x", "y", "tree_can")
# 
# crs(tree) # check coordinate system: WGS84
# st_crs(cville_tracts) # check locality polygons for reference: NAD83
# 
# # reproject polygons to same CRS as impervious
cville_tracts <- st_transform(cville_tracts, projection(tree))
cville_blkgps <- st_transform(cville_blkgps, projection(tree))
cville_blocks <- st_transform(cville_blocks, projection(tree))
# 
# # plot raster with cvl boundaries
# ggplot() +
#   geom_raster(data = tree_df, aes(x = x, y = y, fill = tree_can)) +
#   scale_fill_viridis_c() +
#   geom_sf(data = cville_blkgps, color = "red", fill = NA)


# c. extract tree canopy ----
# (not yet aggregated, check)
tree_extract <- raster::extract(tree, cville_tracts, df = TRUE)
names(tree_extract) <- c("tract", "tree_can")

# the above keeps all cell values within a polygon
# check variation within tracts/spatial units
tree_extract %>% group_by(tract) %>% 
  summarize(cells = n(), 
            miss = sum(is.na(tree_can)),
            mean = mean(tree_can, na.rm = TRUE), 
            std = sd(tree_can, na.rm = TRUE))

# replace NA with 0
# given equal grid sizes, omitting NAs that have no tree canopy will
# generate inflated estimates of percent tree canopy
tree0 <- tree
tree0[is.na(tree0[])] <- 0 

tree0_extract <- raster::extract(tree0, cville_tracts, df = TRUE)
names(tree0_extract) <- c("tract", "tree_can")

# use the mean as the summary function for the moment
tree_mean <- raster::extract(tree0, cville_tracts, df = TRUE, 
                             fun = mean, na.rm = TRUE,
                             sp = TRUE)

tree_mean_blkgps <- raster::extract(tree0, cville_blkgps, df = TRUE,
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

tree_mean_blocks <- raster::extract(tree0, cville_blocks, df = TRUE,
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
tree_tracts <- st_as_sf(tree_mean)
tree_tracts <- tree_tracts %>% 
  rename(tree_can = layer)

ggplot() + 
  geom_sf(data = tree_tracts, aes(fill = tree_can)) +
  scale_fill_viridis_c()
# this produces the tract level data for Cville

tree_blkgps <- st_as_sf(tree_mean_blkgps)
tree_blkgps <- tree_blkgps %>% 
  rename(tree_can = layer)

ggplot() + 
  geom_sf(data = tree_blkgps, aes(fill = tree_can)) +
  scale_fill_viridis_c()
# this produces the block group level data for Cville

tree_blocks <- st_as_sf(tree_mean_blocks)
tree_blocks <- tree_blocks %>% 
  rename(tree_can = layer)

ggplot() + 
  geom_sf(data = tree_blocks, aes(fill = tree_can)) +
  scale_fill_viridis_c()
# this produces the block level data for Cville


# d. write to csv file ----
# remove unncessary variables and geomstry
treet_tract_reduced <- tree_tracts %>% 
  select(STATEFP:NAMELSAD, tree_can) %>% 
  st_drop_geometry()

tree_blkgps_reduced <- tree_blkgps %>% 
  select(STATEFP:NAMELSAD, tree_can) %>% 
  st_drop_geometry()

tree_blocks_reduced <- tree_blocks %>% 
  select(STATEFP10:NAME10, tree_can) %>% 
  st_drop_geometry()

write_csv(treet_tract_reduced, "data/nlcd_tree_cville_tracts.csv")
write_csv(tree_blkgps_reduced, "data/nlcd_tree_cville_blkgps.csv")
write_csv(tree_blocks_reduced, "data/nlcd_tree_cville_blocks.csv")

# save everything for updates
save.image("data/nlcd_generate.RData")

# Question: should water have been clipped out first?
# This might be more central to the Eastern Shore area?
