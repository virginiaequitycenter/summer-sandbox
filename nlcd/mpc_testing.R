library(FedData) 
library(tigris)
library(sf)
library(raster)
library(tidyverse)
library(viridis)

options(tigris_use_cache = TRUE)

# get tract polygons for Cville
cvltracts <- tracts(state = "VA", county = "540")
plot(cvltracts[1])

cville_tracts <- readRDS("data/cville_tracts.RDS")

# 1. Exploration and Understanding ----
# a. get impervious surface measures for Cville
imperv <- get_nlcd(
  template = cvltracts,
  label = "Charlottesville",
  dataset = "Impervious",
  year = 2019, 
  landmass = "L48"
)

plot(imperv)

summary(imperv)
# Q: what are missing pixels?

# check CRS
crs(imperv) # check coordinate system: WGS84
st_crs(cvltracts) # check locality polygons for reference: NAD83

# reproject polygons to same CRS as impervious
cvltracts <- st_transform(cvltracts, projection(imperv))
st_crs(cvltracts)

# check tracts
ggplot() + 
  geom_sf(data = cvltracts)

# Convert raster to df
# this section isn't necessary for extraction below, but useful for preliminary viz
imperv_df <- as.data.frame(imperv, xy = TRUE)
names(imperv_df) <- c("x", "y", "imp_surf")

ggplot() +
  geom_raster(data = imperv_df , aes(x = x, y = y, fill = imp_surf)) +
  scale_fill_viridis_c() 

ggplot(imperv_df, aes(x = imp_surf)) + geom_histogram()

# plot raster with cvl boundaries
ggplot() +
  geom_raster(data = imperv_df, aes(x = x, y = y, fill = imp_surf)) + 
  scale_fill_viridis_c() +
  geom_sf(data = cvltracts, color = "red", fill = NA)

# b. get tree canopy
tree <- get_nlcd(
  template = cvltracts,
  label = "Charlottesville",
  dataset = "Tree_Canopy",
  year = 2016, 
  landmass = "L48"
)

plot(tree)
# Convert raster to df
tree_df <- as.data.frame(tree, xy = TRUE)
names(tree_df) <- c("x", "y", "tree_can")

ggplot(tree_df, aes(x = tree_can)) + geom_histogram()

# check CRS
crs(tree) # check coordinate system: WGS84
st_crs(cvltracts) # check locality polygons for reference: NAD83

# reproject polygons to same CRS as impervious
cvltracts <- st_transform(cvltracts, projection(tree))
st_crs(cvltracts)

# plot raster with cvl boundaries
ggplot() +
  geom_raster(data = tree_df, aes(x = x, y = y, fill = tree_can)) + 
  scale_fill_viridis_c() +
  geom_sf(data = cvltracts, color = "red", fill = NA)


# c. get land cover designation
land <- get_nlcd(
  template = cvltracts,
  label = "Charlottesville",
  dataset = "Land_Cover",
  year = 2016, 
  landmass = "L48",
  extraction.dir = paste0(getwd(), "/data/FedData/")
)

# Convert raster to df
land_df <- as.data.frame(land, xy = TRUE)
names(land_df) <- c("x", "y", "land")

# plot raster with cvl boundaries
ggplot() +
  geom_raster(data = land_df, aes(x = x, y = y, fill = land)) + 
  scale_fill_viridis_d() +
  geom_sf(data = cvltracts, color = "red", fill = NA)

# d. evaluate missing in imp_surf and tree_can
df <- left_join(imperv_df, tree_df) %>% 
  left_join(land_df)

df %>% filter(is.na(imp_surf)) %>% count(land)
df %>% filter(is.na(tree_can)) %>% count(land)
# still want to understand what generates a missing cell, should these be 0s?
# the absence of 0 values makes this a possibility, and the land cover designation
# is plausible


# 2. Extract and summarize impervious surfaces, tree canopy ----
# a. impervious surface
imp_surf_extract <- raster::extract(imperv, cvltracts, df = TRUE)
names(imp_surf_extract) <- c("tract", "imp_surf")

# the above keeps all cell values within a polygon
# check variation within tracts/spatial units
imp_surf_extract %>% group_by(tract) %>% 
  summarize(cells = n(), 
            miss = sum(is.na(imp_surf)),
            mean = mean(imp_surf, na.rm = TRUE), 
            std = sd(imp_surf, na.rm = TRUE))

# is the mean a reasonable summary measure?
imp_surf_extract %>% 
  ggplot(aes(x = imp_surf)) + 
  geom_histogram() + 
  facet_wrap(~tract)

# use the mean as the summary function for the moment
imp_surf_mean <- raster::extract(imperv, cvltracts, df = TRUE, 
                                 fun = mean, na.rm = TRUE,
                                 sp = TRUE)
# THIS WILL NEED TO CHANGE IF WE DECIDE NA's ARE ZEROS

# the above returned a spatal polygon object; change it back to sf
imp_surf_tracts <- st_as_sf(imp_surf_mean)
imp_surf_tracts <- imp_surf_tracts %>% 
  rename(imp_surf = Charlottesville_NLCD_2016_Impervious_L48_nlcd)

ggplot() + 
  geom_sf(data = imp_surf_tracts, aes(fill = imp_surf)) +
  scale_fill_viridis_c()
# this produces the tract level data for Cville


# b. tree canopy
tree_extract <- raster::extract(tree, cvltracts, df = TRUE)
names(tree_extract) <- c("tract", "tree_can")

# the above keeps all cell values within a polygon
# check variation within tracts/spatial units
tree_extract %>% group_by(tract) %>% 
  summarize(cells = n(), 
            miss = sum(is.na(tree_can)),
            mean = mean(tree_can, na.rm = TRUE), 
            std = sd(tree_can, na.rm = TRUE))

# is the mean a reasonable summary measure?
tree_extract %>% 
  ggplot(aes(x = tree_can)) + 
  geom_histogram() + 
  facet_wrap(~tract)

# use the mean as the summary function for the moment
tree_mean <- raster::extract(tree, cvltracts, df = TRUE, 
                                 fun = mean, na.rm = TRUE,
                                 sp = TRUE)
# THIS WILL NEED TO CHANGE IF WE DECIDE NA's ARE ZEROS

# the above returned a spatal polygon object; change it back to sf
tree_tracts <- st_as_sf(tree_mean)
tree_tracts <- tree_tracts %>% 
  rename(tree_can = Charlottesville_NLCD_2016_Tree_Canopy_L48_nlcd)

ggplot() + 
  geom_sf(data = tree_tracts, aes(fill = tree_can)) +
  scale_fill_viridis_c()
# this produces the tract level data for Cville

# add in some tract labels
library(googlesheets4)
cvlnames <- read_sheet("https://docs.google.com/spreadsheets/d/1wURquMto0aqGccU7417v1JBsVqi7gm2PGOR-E_eYmoc/edit#gid=790108595",
                       sheet = "cville") %>% 
  mutate(GEOID = as.character(geoid))

# add centroid point to sf/dataframe and keypoints
tree_tracts <- tree_tracts %>% 
  mutate(center = st_centroid(tree_tracts)) %>% 
  left_join(cvlnames, by = "GEOID")

ggplot() + 
  geom_sf(data = tree_tracts, aes(fill = tree_can)) +
  geom_text()
  scale_fill_viridis_c()


# 3. Reduce and combine impervious surface and tree canopy estimates ----
#   into single dataframe for export to csv (can drop geometry)
imp_surf_tracts_drop <- st_drop_geometry(imp_surf_tracts)
tree_tracts_drop <- st_drop_geometry(tree_tracts)

nlcd_cville_tracts <- imp_surf_tracts_drop %>% 
  select(STATEFP:GEOID, imp_surf) %>% 
  left_join(tree_tracts_drop %>% select(STATEFP:GEOID, tree_can))

write_csv(nlcd_cville_tracts, "data/nlcd_cville_tracts.csv")

# we want to do this simultaneously for all tracts in the Cville region
#   and for all blkgps within the Cville region
#   and for all blocks in the Cville region
# Then repeat for the Eastern Shore localities

# Save this work
save.image("data/nlcd_testing.Rdata")
