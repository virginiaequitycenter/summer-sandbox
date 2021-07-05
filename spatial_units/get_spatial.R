# Adding common spatial geometry files for use in data exploration
# Michele Claibourn
# 2021-06-29

library(tidyverse)
library(tigris)

options(tigris_use_cache = TRUE)

# cville region ---
cvillefips <- c("540", "003", "065", "079", "109", "125")

# county polygons
cville_counties <- counties(state = "51")
cville_counties <- cville_counties %>% 
  filter(COUNTYFP %in% cvillefips)

# tract polygons
cville_tracts <- tracts(state = "51", county = cvillefips)

# block group polygons
cville_blkgps <- block_groups(state = "51", county = cvillefips)

# block polygons
cville_blocks <- blocks(state = "51", county = cvillefips)

# save
saveRDS(cville_counties, file = "data/cville_counties.RDS")
saveRDS(cville_tracts, file = "data/cville_tracts.RDS")
saveRDS(cville_blkgps, file = "data/cville_blkgps.RDS")
saveRDS(cville_blocks, file = "data/cville_blocks.RDS")

# eastern shore ----
easternfips <- c("001", "131")

# county polygons
eastshore_counties <- counties(state = "51")
eastshore_counties <- eastshore_counties %>% 
  filter(COUNTYFP %in% easternfips)

# tract polygons
eastshore_tracts <- tracts(state = "51", county = easternfips)

# block group polygons
eastshore_blkgps <- block_groups(state = "51", county = easternfips)

# block polygons
eastshore_blocks <- blocks(state = "51", county = easternfips)

# save
saveRDS(eastshore_counties, file = "data/eastshore_counties.RDS")
saveRDS(eastshore_tracts, file = "data/eastshore_tracts.RDS")
saveRDS(eastshore_blkgps, file = "data/eastshore_blkgps.RDS")
saveRDS(eastshore_blocks, file = "data/eastshore_blocks.RDS")
