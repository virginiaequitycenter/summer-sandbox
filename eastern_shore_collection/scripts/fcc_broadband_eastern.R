
# Author: Lee LeBoeuf
# Last updated: 07/28/2021

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

# Areas included in these data:
# The Eastern Shore area: Accomack (51001), Northampton (51131)

# invisible(lapply(list('tidyverse', 'stargazer', 'janitor', 'tigris', 'sf', 'leaflet', 'rcartocolor', 
#                       'RColorBrewer', 'viridis', 'rgdal', 'lodes', 'psych', 'reshape2', 'googlesheets4', 
#                       'sp', 'geosphere', 'stringr'),
#                  function(pkg) library(pkg, character.only = TRUE)))

# FCC data is released biannually on a year and half delay from when it was collected. 
# New data can be downloaded from this link: https://www.fcc.gov/general/broadband-deployment-data-fcc-form-477

# Reading in the current data and narrowing it to the Eastern Shore region
dat <- read.csv("VA-Fixed-Jun2020-v1.csv")
dat <- dat %>%
  mutate(countycode = str_sub(BlockCode, 1, 5))

# Eastern Shore area 
eastfips <- c("51001", "51131")

eastdat <- dat %>%
  filter(countycode %in% eastfips)

eastdat <- eastdat %>%
  mutate(Blkgr = str_sub(BlockCode, 1, 12),
         tract = str_sub(BlockCode, 1, 11))

# Creating a block level summary
eastdatblock <- eastdat %>%
  group_by(BlockCode) %>%
  summarize(consproviders = sum(Consumer),
            busproviders = sum(Business),
            avgMaxAdDown = mean(MaxAdDown),
            avgMaxAdUp = mean(MaxAdUp)) %>%
  mutate(Blkgr = str_sub(BlockCode, 1, 12),
         tract = str_sub(BlockCode, 1, 11)) %>%
  select(BlockCode, Blkgr, consproviders, busproviders, avgMaxAdDown, avgMaxAdUp, tract)

eastdatblock$under25.3mbps <- ifelse(eastdatblock$avgMaxAdDown <25 & eastdatblock$avgMaxAdUp <3, 1, 0)

# Writing out the block level version to a csv
write.csv(eastdatblock, "fcc_broadband_eastern_block.csv", row.names = F)

# ------------------------------------------------------------------------------------------------------------------------------
# Forming block group summary version (need to account for the fact that some providers 
# are repeated within the block group)

# First creating a variable of the number of blocks in each block group so that we can calculate the proportion of blocks without 25/3 Mbps
eastdat <- eastdat %>% 
  group_by(Blkgr) %>%
  mutate(numberOfBlocks = length(unique(BlockCode))) %>%
  ungroup()

# Next, finding the number of blocks without 25/3 Mbps by using the block level summaries calculated above and merging it back with the original data
eastdatblock2 <- eastdatblock %>%
  group_by(Blkgr) %>%
  mutate(Blocksunder25.3mbps = sum(under25.3mbps)) %>%
  select(Blocksunder25.3mbps, Blkgr) %>%
  ungroup()

eastdat <- left_join(eastdat, unique(eastdatblock2[c('Blocksunder25.3mbps', 'Blkgr')]), by = "Blkgr")

eastdatblkgr <- eastdat %>%
  group_by(Blkgr) %>%
  summarize(consproviders = length(unique(ProviderName[which(Consumer == 1)])),
            busproviders = length(unique(ProviderName[which(Business == 1)])),
            avgMaxAdDown = mean(MaxAdDown),
            avgMaxAdUp = mean(MaxAdUp),
            p_under25.3mbps = Blocksunder25.3mbps/numberOfBlocks) %>%
  distinct()

# # Writing out the block group level version to a csv
write.csv(eastdatblkgr, "fcc_broadband_eastern_blkgr.csv", row.names = F)

# ------------------------------------------------------------------------------------------------------------------------------
# Forming tract summary version (need to account for the fact that some providers 
# are repeated within the block group)

# First creating a variable of the number of blocks in each block group so that we can calculate the proportion of blocks without 25/3 Mbps
eastdat <- eastdat %>% 
  group_by(tract) %>%
  mutate(numberOfBlockstr = length(unique(BlockCode))) %>%
  ungroup()

# Next, finding the number of blocks without 25/3 Mbps by using the block level summaries calculated above and merging it back with the original data
eastdatblock3 <- eastdatblock %>%
  group_by(tract) %>%
  mutate(Blocksunder25.3mbpstr = sum(under25.3mbps)) %>%
  select(Blocksunder25.3mbpstr, tract) %>%
  ungroup()

eastdat <- left_join(eastdat, unique(eastdatblock3[c('Blocksunder25.3mbpstr', 'tract')], by = "tract"))

eastdattr <- eastdat %>%
  group_by(tract) %>%
  summarize(consproviders = length(unique(ProviderName[which(Consumer == 1)])),
            busproviders = length(unique(ProviderName[which(Business == 1)])),
            avgMaxAdDown = mean(MaxAdDown),
            avgMaxAdUp = mean(MaxAdUp),
            p_under25.3mbps = Blocksunder25.3mbpstr/numberOfBlockstr) %>%
  distinct()

# # Writing out the block group level version to a csv
write.csv(eastdattr, "fcc_broadband_eastern_tract.csv", row.names = F)
