# Get FEMA National Risk Index measures
# 2021-06-23
# MPC

library(tidyverse)

# Define localities of interest
cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")
eastfips <- c("51001", "51131")


# Pull National Risk Index
url <- "https://nri-data-downloads.s3.amazonaws.com/NRI_Table_CensusTracts.zip"
download.file(url = url,
              destfile = paste(getwd(), "/nri/", "tract.zip", sep = ""),
              mode = "wb")

# unzip and read
unzip("nri/tract.zip", exdir = "nri")
nri_table <- read_csv("nri/NRI_Table_CensusTracts.csv")


# Filter to relevant areas
cville_nri <- nri_table %>% 
  filter(STCOFIPS %in% cvillefips)
eastern_nri <- nri_table %>% 
  filter(STCOFIPS %in% eastfips)

# Filter to variables of interest
cols <- c(1:15, 56:64, 93:103, 150:158, 167:175, 235:245, 255:265, 360)

cville_nri <- cville_nri %>% 
  select(all_of(cols))

eastern_nri <- eastern_nri %>% 
  select(all_of(cols))

# still too many... but okay for now


# Save to csv
write_csv(cville_nri, path = "data/fema_nri_cville_tract.csv")
write_csv(eastern_nri, path = "data/fema_nri_eastern_tract.csv")
