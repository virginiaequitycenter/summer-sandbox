library(FedData) 
library(sf)
library(raster)
library(tidyverse)
library(viridis)

# I moved the tract simple features object to the nlcd/data folder for testing
cville_tracts <- readRDS("data/cville_tracts.RDS") # full cville region

# verify I am where I think I am in the directory
getwd() # should end in "/summer-sandbox/nlcd"

tree2 <- get_nlcd(
  template = cville_tracts,
  label = "Charlottesville",
  dataset = "Tree_Canopy",
  year = 2016,
  landmass = "L48"
)

plot(tree2)

# Convert raster to df
tree2_df <- as.data.frame(tree2, xy = TRUE)
names(tree2_df) <- c("x", "y", "tree_can")

ggplot() +
  geom_raster(data = tree2_df, aes(x = x, y = y, fill = tree_can)) +
  scale_fill_viridis_c() 

# crs(tree2) # check coordinate system: says NAD83
# st_crs(cville_tracts) # check locality polygons for reference: says NAD83
# but still not mapping together below... so reproject
cville_tracts <- st_transform(cville_tracts, projection(tree2))
 
# plot raster with cvl boundaries
ggplot() +
  geom_raster(data = tree2_df, aes(x = x, y = y, fill = tree_can)) +
  scale_fill_viridis_c() + 
  geom_sf(data = cville_tracts, color = "black", fill = NA)
