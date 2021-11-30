# Get FEMA National Risk Index measures
# Michele Claibourn
# Created: 2021-06-23
# Last updated: 2021-11-29

library(tidyverse)

# Define localities of interest
# Cville area
# localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
localfips <- c("001", "131")


# Import data from source ----
# if (!dir.exists("dataraw")) {dir.create("dataraw")}
# 
# url <- "https://nri-data-downloads.s3.amazonaws.com/NRI_Table_CensusTracts.zip"
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "nri.zip", sep = ""),
#               mode = "wb")
# unzip("dataraw/nri.zip", exdir = paste(getwd(), "/dataraw/nri/", sep = ""))
# 

# Read in data ----
nri_table <- read_csv("dataraw/nri/NRI_Table_CensusTracts.csv")


# Reduce----
# Filter to variables of interest
cols <- c(1:15, 56:64, 93:103, 150:158, 167:175, 235:245, 255:265, 360)

local_nri <- nri_table %>% 
  filter(STCOFIPS %in% paste0("51",localfips)) %>% 
  select(all_of(cols))

# still too many... but okay for now


# Save to csv
# Cville area
# write_csv(local_nri, "data/fema_nri_cville_tract.csv")
# Eastern shore area
write_csv(local_nri, "data/fema_nri_eastern_tract.csv")
