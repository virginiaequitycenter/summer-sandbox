# Explore FEMA sources

library(tidyverse)

# Define localities of interest
cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")
eastfips <- c("51001", "51131")

# Insurance claims ----
# (takes a good while)
claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")

# filter to relevant areas
cville_claims <- claims %>% 
  filter(countyCode %in% cvillefips)
eastern_claims <- claims %>% 
  filter(countyCode %in% eastfips)


# National risk incidence ----
# download
url <- "https://nri-data-downloads.s3.amazonaws.com/NRI_Table_CensusTracts.zip"
download.file(url = url,
              destfile = paste(getwd(), "/nri/", "tract.zip", sep = ""),
              mode = "wb")

# unzip and read
unzip("nri/tract.zip", exdir = "nri")
nri_table <- read_csv("nri/NRI_Table_CensusTracts.csv")

# filter to relevant areas
cville_nri <- nri_table %>% 
  filter(STCOFIPS %in% cvillefips)
eastern_nri <- nri_table %>% 
  filter(STCOFIPS %in% eastfips)

# I'd save (much, much smaller) the filtered data to explore
