# Get FCC Broadband Availability Data
# Author: Lee LeBoeuf, Michele Claibourn
# Last updated: 2021-11-18

# This script uses data downloaded from the FCC's website on broadband availability based on form 477 that all internet providers must fill out
# biannually. Data is available at the census block level for the entire state of Virginia. This script cleans it to focus on the Eastern Shore region, and 
# produces a csv file with the following variables 
# (1) The number of consumer internet providers in each su
# (2) The number of business internet providers in each su
# (3) The average maximum advertised download speed available in each su. (Note: this is based on what is advertised to consumers, not what is 
# actually contractually agreed upon/it could differ from what is actually provided.)
# (4) The average maximum advertised upload speed available in each su. (Note: this is based on what is advertised to consumers, not what is 
# actually contractually agreed upon/it could differ from what is actually provided.)
# (5) A binary variable indicating whether the su has "advanced telecommunications capability" which, based off the 
# FCC's benchmark, this means internet speeds of 25/3 Mbps. At the block group and tract level, this is represented 
# as the propotion of blocks within the SU that do not meet the threshhold. 

# Data downloaded from this link: https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477
# Downloaded VA data into dataraw 
# Ideally would download directly (in code)
# Also looked into using an API call here: https://opendata.fcc.gov/resource/hicn-aujz.json?stateabbr=VA
# but limit is 1000 and would need to loop through relevant fips block groups 

library(tidyverse)

# Areas included in these data:
# The Eastern Shore area: Accomack (51001), Northampton (51131)

# invisible(lapply(list('tidyverse', 'stargazer', 'janitor', 'tigris', 'sf', 'leaflet', 'rcartocolor', 
#                       'RColorBrewer', 'viridis', 'rgdal', 'lodes', 'psych', 'reshape2', 'googlesheets4', 
#                       'sp', 'geosphere', 'stringr'),
#                  function(pkg) library(pkg, character.only = TRUE)))

# FCC data is released biannually on a year and half delay from when it was collected. 
# New data can be downloaded from this link: https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477
# Reading in the current data and narrowing it to the Eastern Shore region
dat <- read.csv("dataraw/VA-Fixed-Dec2020-v1.csv")

dat <- dat %>%
  mutate(BlockCode = as.character(BlockCode),
         countycode = str_sub(BlockCode, 1, 5))

# Eastern Shore area 
eastfips <- c("51001", "51131")

eastdat <- dat %>%
  filter(countycode %in% eastfips)

# generate tract and block group codes; 
# generate indicators for >= 25/3
# filter to residential
eastdat <- eastdat %>%
  filter(Consumer == 1) %>% 
  mutate(Blkgr = str_sub(BlockCode, 1, 12),
         tract = str_sub(BlockCode, 1, 11),
         bb253 = ifelse(MaxAdDown >= 25 & MaxAdUp >= 3, 1, 0))

# Creating a block level summary
eastdatblock <- eastdat %>%
  group_by(BlockCode, Blkgr, tract) %>%
  summarize(resproviders = sum(Consumer),
            bb253_num = sum(bb253),
            bb253_per = round((bb253_num/n())*100, 1),
            bbmin_dl = min(MaxAdDown),
            bbmax_dl = max(MaxAdDown),
            bbmin_up = min(MaxAdUp),
            bbmax_up = max(MaxAdUp),
            avgMaxAdDown = mean(MaxAdDown),
            avgMaxAdUp = mean(MaxAdUp)) %>%
  select(BlockCode, Blkgr, tract, resproviders, bb253_num, bb253_per,
         bbmin_dl, bbmax_dl, bbmin_up, bbmax_up, avgMaxAdDown, avgMaxAdUp)

# Writing out the block level version to a csv
write.csv(eastdatblock, "data/fcc_broadband_eastern_block.csv", row.names = F)


# ------------------------------------------------------------------------------------------------------------------------------
# Forming block group summary version (need to account for the fact that some providers 
# are repeated within the block group)

# First creating a variable of the number of blocks in each block group so that we can calculate the proportion of blocks without 25/3 Mbps
eastdat <- eastdat %>% 
  group_by(Blkgr) %>%
  mutate(numberOfBlocks = length(unique(BlockCode))) %>%
  ungroup()

eastdatblkgr_b <- eastdat %>%
  group_by(Blkgr, tract, ProviderName) %>%
  summarize(blocks_number = first(numberOfBlocks),
            blocks_provided = n(),
            bbmax_dl = max(MaxAdDown),
            bbmax_up = max(MaxAdUp),
            bb253 = ifelse(bbmax_dl >= 25 & bbmax_up >= 3, 1, 0)) %>% 
  group_by(Blkgr, tract) %>% 
  summarize(resproviders = n(),
            bb253_num = sum(bb253)) %>% 
  mutate(bb253_per = round((bb253_num/resproviders)*100, 1))

eastdatblkgr_a <- eastdat %>% 
  group_by(Blkgr, tract) %>%
  summarize(resproviders = length(unique(ProviderName[which(Consumer == 1)])),
            bbmin_dl = min(MaxAdDown),
            bbmax_dl = max(MaxAdDown),
            bbmin_up = min(MaxAdUp),
            bbmax_up = max(MaxAdUp),
            avgMaxAdDown = mean(MaxAdDown),
            avgMaxAdUp = mean(MaxAdUp))

eastdatblkgr <- eastdatblkgr_b %>% 
  left_join(eastdatblkgr_a)

# # Writing out the block group level version to a csv
write.csv(eastdatblkgr, "data/fcc_broadband_eastern_blkgr.csv", row.names = F)


# ------------------------------------------------------------------------------------------------------------------------------
# Forming tract summary version (need to account for the fact that some providers 
# are repeated within the block group)

# First creating a variable of the number of blocks in each block group so that we can calculate the proportion of blocks without 25/3 Mbps
eastdat <- eastdat %>% 
  group_by(tract) %>%
  mutate(numberOfBlockstr = length(unique(BlockCode))) %>%
  ungroup()

eastdattr_b <- eastdat %>%
  group_by(tract, ProviderName) %>%
  summarize(blocks_number = first(numberOfBlockstr),
            blocks_provided = n(),
            bbmax_dl = max(MaxAdDown),
            bbmax_up = max(MaxAdUp),
            bb253 = ifelse(bbmax_dl >= 25 & bbmax_up >= 3, 1, 0)) %>% 
  group_by(tract) %>% 
  summarize(resproviders = n(),
            bb253_num = sum(bb253)) %>% 
  mutate(bb253_per = round((bb253_num/resproviders)*100, 1))

eastdattr_a <- eastdat %>% 
  group_by(tract) %>%
  summarize(resproviders = length(unique(ProviderName[which(Consumer == 1)])),
            bbmin_dl = min(MaxAdDown),
            bbmax_dl = max(MaxAdDown),
            bbmin_up = min(MaxAdUp),
            bbmax_up = max(MaxAdUp),
            avgMaxAdDown = mean(MaxAdDown),
            avgMaxAdUp = mean(MaxAdUp))

eastdattr <- eastdattr_b %>% 
  left_join(eastdattr_a)

# # Writing out the block group level version to a csv
write.csv(eastdattr, "data/fcc_broadband_eastern_tract.csv", row.names = F)
