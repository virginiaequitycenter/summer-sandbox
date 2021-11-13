# .............................................
# NLCD data from mrlc 
# Previous tree canopy used FedData package 
#    but FedData didn't work for impervious surfaces
# Last updated: 2021-09-09
# Michele Claibourn and Helena Lindsay
# .............................................
# 0. Load packages
# 1. Define Cville region
# 2. Download, land cover, impervious surfaces, tree canopy
# 3. Load land cover, impervious surfaces, tree canopy
# 3. Define region, align CRS, and crop 
# 4. Extract summaries
# 6. Reduce, Join, and Generate csv files
# .............................................

# 0. Load packages ----
library(raster)
library(sf)
library(sp)
library(tigris)
library(tidyverse)
options(tigris_use_cache = TRUE)


# 1. Download land cover, impervious surfaces, tree canopy ----
# a. Land Cover
# https://www.mrlc.gov/data?f%5B0%5D=category%3ALand%20Cover

# url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_land_cover_l48_20210604.zip"
# if (!dir.exists("dataraw")) {dir.create("dataraw")}
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "landcover2019.zip", sep = ""),
#               mode = "wb")

# Manually unzip in directory: dataraw/landcover2019/

# b. Impervious Surfaces
# https://www.mrlc.gov/data?f%5B0%5D=category%3AUrban%20Imperviousness

# url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2019_impervious_l48_20210604.zip"
# if (!dir.exists("dataraw")) {dir.create("dataraw")}
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "surface2019.zip", sep = ""),
#               mode = "wb")

# manually unzip in directory: dataraw/surface2019/

# c. Tree Canopy
# https://www.mrlc.gov/data?f%5B0%5D=category%3ATree%20Canopy
# NOTE: 2019 not up yet

# url <- "https://s3-us-west-2.amazonaws.com/mrlc/nlcd_2016_treecanopy_2019_08_31.zip"
# if (!dir.exists("dataraw")) {dir.create("dataraw")}
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "tree2016.zip", sep = ""),
#               mode = "wb")

# manually unzip in directory: dataraw/tree2016/


# 2. Load land cover, impervious surfaces, tree canopy ----
# a. Land Cover
nlcd_img <- raster("dataraw/landcover2019/nlcd_2019_land_cover_l48_20210604.img")
nlcd_img # WGS84
nlayers(nlcd_img)
plot(nlcd_img)
# image(nlcd_img)
# see legend: https://www.mrlc.gov/data/legends/national-land-cover-database-2019-nlcd2019-legend

# b. Impervious Surfaces
imperv_img <- raster("dataraw/surface2019/nlcd_2019_impervious_l48_20210604.img")
imperv_img # WGS84
plot(imperv_img)
nlayers(imperv_img)

# c. Tree Canopy
tree_img <- raster("dataraw/tree2016/nlcd_2016_treecanopy_2019_08_31.img")
tree_img # NAD83
plot(tree_img)
nlayers(tree_img)


# 3. Define region, align CRS, and crop ----
# define region (generated in spatial_units/get_spatial.R)
cville_tracts <- readRDS("data/cville_tracts.RDS")
crs(cville_tracts) # NAD83

# align crs in tract polygons and nlcd raster
cville_tracts2 <- st_transform(cville_tracts, proj4string(nlcd_img))

# a. Crop Land Cover
nlcd_img_cvl <- crop(nlcd_img, cville_tracts2)
plot(nlcd_img_cvl)

# Check: plot raster with cvl boundaries
nlcd_df <- as.data.frame(nlcd_img_cvl, xy = TRUE)
names(nlcd_df) <- c("x", "y", "land_cov")
nlcd_df <- nlcd_df %>% 
  mutate(land_cov = as.factor(land_cov))

ggplot() +
  geom_raster(data = nlcd_df, aes(x = x, y = y, fill = land_cov)) +
  scale_fill_viridis_d() +
  geom_sf(data = cville_tracts2, color = "red", fill = NA)

# b. Crop Impervious Surfaces
# imperv_img_cvl2 <- mask(imperv_img_cvl, cville_tracts2)
imperv_img_cvl <- crop(imperv_img, extent(cville_tracts2))
plot(imperv_img_cvl)

# Check: plot raster with cvl boundaries
imp_df <- as.data.frame(imperv_img_cvl, xy = TRUE)
names(imp_df) <- c("x", "y", "imp_surf")

ggplot() +
  geom_raster(data = imp_df, aes(x = x, y = y, fill = imp_surf)) +
  scale_fill_viridis_c() +
  geom_sf(data = cville_tracts2, color = "red", fill = NA)


# c. Crop Tree Canopy
# NOTE: tree says it's in NAD83, but extent is wrong
tree_img_cvl <- crop(tree_img, extent(cville_tracts2))
plot(tree_img_cvl)

# Check: plot raster with cvl boundaries
tree_df <- as.data.frame(tree_img_cvl, xy = TRUE)
names(tree_df) <- c("x", "y", "tree_can")

ggplot() +
  geom_raster(data = tree_df, aes(x = x, y = y, fill = tree_can)) +
  scale_fill_viridis_c() +
  geom_sf(data = cville_tracts2, color = "red", fill = NA)


# 4. Extract summares ----
# load block, block groups, census tract files
cville_tracts <- readRDS("data/cville_tracts.RDS")
cville_blkgps <- readRDS("data/cville_blkgps.RDS")
cville_blocks <- readRDS("data/cville_blocks.RDS")

# align crs in tract polygons and nlcd raster
cville_tracts2 <- st_transform(cville_tracts, proj4string(nlcd_img_cvl))
cville_blkgps2 <- st_transform(cville_blkgps, proj4string(nlcd_img_cvl))
cville_blocks2 <- st_transform(cville_blocks, proj4string(nlcd_img_cvl))

# Extract Land Cover
land_extract_tracts <- raster::extract(nlcd_img_cvl, cville_tracts2, df = TRUE)
names(land_extract_tracts) <- c("tract", "land_cov")

# the above keeps all cell values within a polygon
# check variation within tracts/spatial units
land_extract_tracts %>% group_by(tract) %>% 
  summarize(cells = n(), 
            openwater = sum(land_cov == 11),
            #icesnow = sum(land_cov == 12),
            devopen = sum(land_cov == 21),
            devlow = sum(land_cov ==22),
            devmed = sum(land_cov == 23),
            devhigh = sum(land_cov == 24),
            barren = sum(land_cov == 31),
            decidfor = sum(land_cov == 41),
            everfor = sum(land_cov == 42),
            mixfor = sum(land_cov == 43),
            #scrubdrawrf = sum(land_cov == 51),
            scrubshrub = sum(land_cov == 52),
            grassland = sum(land_cov == 71),
            pasture = sum(land_cov == 81),
            crops = sum(land_cov == 82),
            wetlandwood = sum(land_cov == 90),
            #wetlandherb = sum(land_cov == 05)
            ) %>% View()
# Let's get percent developed (open, low, med, high) and forest (deciduous, evergreen, mixed)
# by tract
land_summary_tracts <- land_extract_tracts %>% 
  group_by(tract) %>% 
  summarize(
    cells = n(),
    developed = sum(land_cov %in% c(21, 22, 23, 24)),
    forest = sum(land_cov %in% c(41, 42, 43))
  ) %>% 
  mutate(percent_dev = (developed/cells)*100,
         percent_for = (forest/cells)*100)

# HOW TO DO THIS FOR BLKGPS, BLOCKS?
# come back and do this with cville_blkgps2 and cville_blocks2 later
# block groups
land_extract_blkgps <- raster::extract(nlcd_img_cvl, cville_blkgps2, df = TRUE)
names(land_extract_blkgps) <- c("blkgps", "land_cov")

land_summary_blkgps <- land_extract_blkgps %>% 
  group_by(blkgps) %>% 
  summarize(
    cells = n(),
    developed = sum(land_cov %in% c(21, 22, 23, 24)),
    forest = sum(land_cov %in% c(41, 42, 43))
  ) %>% 
  mutate(percent_dev = (developed/cells)*100,
         percent_for = (forest/cells)*100)

land_extract_blocks <- raster::extract(nlcd_img_cvl, cville_blocks2, df = TRUE)
names(land_extract_blocks) <- c("blocks", "land_cov")

land_summary_blocks <- land_extract_blocks %>% 
  group_by(blocks) %>% 
  summarize(
    cells = n(),
    developed = sum(land_cov %in% c(21, 22, 23, 24)),
    forest = sum(land_cov %in% c(41, 42, 43))
  ) %>% 
  mutate(percent_dev = (developed/cells)*100,
         percent_for = (forest/cells)*100)


# Extract Impervious Surfaces
imperv_extract_tracts <- raster::extract(imperv_img_cvl, cville_tracts2, df = TRUE)
names(imperv_extract_tracts) <- c("tract", "imp_surf")

# the above keeps all cell values within a polygon
# check variation within tracts/spatial units
imperv_extract_tracts %>% group_by(tract) %>% 
  summarize(cells = n(), 
            miss = sum(is.na(imp_surf)),
            mean = mean(imp_surf, na.rm = TRUE), 
            std = sd(imp_surf, na.rm = TRUE),
            median = median(imp_surf, na.rm = TRUE)) %>% 
  View()

# distribution within tracts (check a few)
imperv_extract_tracts %>% filter(tract == 13) %>% 
  ggplot(aes(x = imp_surf)) + geom_histogram() 

# use the mean as the summary function for the moment
imperv_summary_tracts <- raster::extract(imperv_img_cvl, cville_tracts2, df = TRUE, 
                             fun = mean, na.rm = TRUE,
                             sp = TRUE)

imperv_summary_blkgps <- raster::extract(imperv_img_cvl, cville_blkgps2, df = TRUE, 
                                         fun = mean, na.rm = TRUE,
                                         sp = TRUE)

imperv_summary_blocks <- raster::extract(imperv_img_cvl, cville_blocks2, df = TRUE, 
                                         fun = mean, na.rm = TRUE,
                                         sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
imperv_tracts <- st_as_sf(imperv_summary_tracts)
imperv_blkgps <- st_as_sf(imperv_summary_blkgps)
imperv_blocks <- st_as_sf(imperv_summary_blocks)

# name and check
imperv_tracts <- imperv_tracts %>% 
  rename(imp_surf = nlcd_2019_impervious_l48_20210604)

ggplot(imperv_tracts, aes(x = imp_surf)) + geom_histogram() 

ggplot() + 
  geom_sf(data = imperv_tracts, aes(fill = imp_surf)) +
  scale_fill_viridis_c()

imperv_blkgps <- imperv_blkgps %>% 
  rename(imp_surf = nlcd_2019_impervious_l48_20210604)

ggplot(imperv_blkgps, aes(x = imp_surf)) + geom_histogram() 

ggplot() + 
  geom_sf(data = imperv_blkgps, aes(fill = imp_surf)) +
  scale_fill_viridis_c()

imperv_blocks <- imperv_blocks %>% 
  rename(imp_surf = nlcd_2019_impervious_l48_20210604)

ggplot(imperv_blocks, aes(x = imp_surf)) + geom_histogram() 

ggplot() + 
  geom_sf(data = imperv_blocks, aes(fill = imp_surf)) +
  scale_fill_viridis_c()

# Extract Tree Canopy
# (not yet aggregated, check)
tree_extract_tracts <- raster::extract(tree_img_cvl, cville_tracts2, df = TRUE)
names(tree_extract_tracts) <- c("tract", "tree_can")

# the above keeps all cell values within a polygon
# check variation within tracts/spatial units
tree_extract_tracts %>% group_by(tract) %>% 
  summarize(cells = n(), 
            miss = sum(is.na(tree_can)),
            mean = mean(tree_can, na.rm = TRUE), 
            std = sd(tree_can, na.rm = TRUE)) %>% 
  View()

# distribution within tracts (check a few)
tree_extract_tracts %>% filter(tract == 45) %>% 
  ggplot(aes(x = tree_can)) + geom_histogram() 


# use the mean as the summary function for the moment
tree_summary_tracts <- raster::extract(tree_img_cvl, cville_tracts2, df = TRUE, 
                             fun = mean, na.rm = TRUE,
                             sp = TRUE)

tree_summary_blkgps <- raster::extract(tree_img_cvl, cville_blkgps2, df = TRUE,
                                    fun = mean, na.rm = TRUE,
                                    sp = TRUE)

tree_summary_blocks <- raster::extract(tree_img_cvl, cville_blocks2, df = TRUE,
                                       fun = mean, na.rm = TRUE,
                                       sp = TRUE)

# the above returned a spatal polygon object; change it back to sf
tree_tracts <- st_as_sf(tree_summary_tracts)
tree_blkgps <- st_as_sf(tree_summary_blkgps)
tree_blocks <- st_as_sf(tree_summary_blocks)

# set CRS to WGS84
tree_tracts <- st_transform(tree_tracts, crs(imperv_tracts))
tree_blkgps <- st_transform(tree_blkgps, crs(imperv_blkgps))
tree_blocks <- st_transform(tree_blocks, crs(imperv_blocks))

# name and check
tree_tracts <- tree_tracts %>% 
  rename(tree_can = nlcd_2016_treecanopy_2019_08_31)

ggplot(tree_tracts, aes(x = tree_can)) + geom_histogram() 

ggplot() + 
  geom_sf(data = tree_tracts, aes(fill = tree_can)) +
  scale_fill_viridis_c()

tree_blkgps <- tree_blkgps %>% 
  rename(tree_can = nlcd_2016_treecanopy_2019_08_31)

ggplot(tree_blkgps, aes(x = tree_can)) + geom_histogram() 

ggplot() + 
  geom_sf(data = tree_blkgps, aes(fill = tree_can)) +
  scale_fill_viridis_c()

tree_blocks <- tree_blocks %>% 
  rename(tree_can = nlcd_2016_treecanopy_2019_08_31)

ggplot(tree_blocks, aes(x = tree_can)) + geom_histogram() 

ggplot() + 
  geom_sf(data = tree_blocks, aes(fill = tree_can)) +
  scale_fill_viridis_c()


# 6. Reduce, Join and Generate csv files ----
# Reduce, join tracts
tree_tracts2 <- tree_tracts %>% 
  select(STATEFP:NAMELSAD, tree_can) %>% 
  st_drop_geometry()

imperv_tracts2 <- imperv_tracts %>% 
  select(STATEFP:NAMELSAD, imp_surf) %>% 
  st_drop_geometry()

tracts <- left_join(tree_tracts2, imperv_tracts2)
tracts <- bind_cols(tracts, land_summary_tracts)

# Reduce, join block groups
tree_blkgps2 <- tree_blkgps %>% 
  select(STATEFP:NAMELSAD, tree_can) %>% 
  st_drop_geometry()

imperv_blkgps2 <- imperv_blkgps %>% 
  select(STATEFP:NAMELSAD, imp_surf) %>% 
  st_drop_geometry()

blkgps <- left_join(tree_blkgps2, imperv_blkgps2)
blkgps <- bind_cols(blkgps, land_summary_blkgps)

# Reduce, join blocks 
tree_blocks2 <- tree_blocks %>% 
  select(STATEFP, COUNTYFP, TRACTCE10:NAME10, tree_can) %>% 
  st_drop_geometry()

imperv_blocks2 <- imperv_blocks %>% 
  select(STATEFP, COUNTYFP, TRACTCE10:NAME10, imp_surf) %>% 
  st_drop_geometry()

blocks <- left_join(tree_blocks2, imperv_blocks2)
blocks <- bind_cols(blocks, land_summary_blocks)

# Write csv
write_csv(tracts, "data/nlcd_cville_tracts.csv")
write_csv(blkgps, "data/nlcd_cville_blkgps.csv")
write_csv(blocks, "data/nlcd_cville_blocks.csv")

# save everything for updates
save.image("dataraw/nlcd_generate.RData")


# useful tutorials
# https://www.neonscience.org/resources/learning-hub/tutorials/raster-data-r
# https://www.neonscience.org/resources/learning-hub/tutorials/dc-raster-data-r
