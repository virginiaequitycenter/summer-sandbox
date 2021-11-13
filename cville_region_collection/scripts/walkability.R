# Get EPA Walkability Scores
# Helena Lindsay, Michele Claibourn
# Created: 2021-07-01
# Last updated: 2021-11-12

library(tidyverse)


# Import data from source ----
url <- "https://edg.epa.gov/EPADataCommons/public/OA/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv"

df <- read_csv(url)


# Define area of interest ----
# Cville area
localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
# localfips <- c("001", "131")

# Define vars of interest ----
vars = c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE", "D3B", "D4A", "D2B_E8MIXA", "D2A_EPHHM", "D2A_Ranked", "D2B_Ranked", "D3B_Ranked", "D4A_Ranked", "NatWalkInd")

# Reduce data ----
local <- df %>% 
  select(vars) %>% 
  mutate(across(c("STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE"), as.character),
         COUNTYFP = str_pad(COUNTYFP, 3, side = "left", pad = "0"),
         TRACTCE = str_pad(TRACTCE, 6, side = "left", pad = "0"),
         FIPS_TRACT = paste0(STATEFP, COUNTYFP, TRACTCE),
         FIPS_BLKGP = paste0(FIPS_TRACT, BLKGRPCE)) %>% 
  filter(STATEFP == "51" & COUNTYFP %in% localfips) %>% 
  select(FIPS_TRACT, FIPS_BLKGP, everything())

# Rename, Recode variables
ggplot(local, aes(x = D4A)) + geom_histogram()
ggplot(local, aes(x = D4A_Ranked)) + geom_histogram()

local <- local %>% 
  mutate(D4A = ifelse(D4A == -99999.0, NA_integer_, D4A)) %>% 
  rename(intersection_density = D3B,
         int_density_rank = D3B_Ranked,
         proximity_transit = D4A,
         prox_transit_rank = D4A_Ranked,
         employment_mix = D2B_E8MIXA,
         emp_mix_rank = D2B_Ranked,
         emp_housing_mix = D2A_EPHHM,
         emp_hou_mix_rank = D2A_Ranked,
         walkability_index = NatWalkInd) %>% 
  mutate(walkability_bins = case_when(
    walkability_index <= 5.75 ~ "Least Walkable",
    walkability_index > 5.76 & walkability_index <= 10.5 ~ "Below Average Walkable",
    walkability_index > 10.5 & walkability_index <= 15.25 ~ "Above Average Walkable",
    walkability_index > 15.25 ~ "Most Walkable"),
    walkability_bins = factor(walkability_bins, levels = c("Least Walkable", "Below Average Walkable",
                                                           "Above Average Walkable", "Most Walkable")))



# Save data ----
# Cville area
write_csv(local, "data/cville_walk.csv")
# Eastern shore area
# write_csv(local, "data/eastern_walk.csv")

