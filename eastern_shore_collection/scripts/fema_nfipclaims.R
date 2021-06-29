# Khalila Karefa-Kargbo
# 2021-06-29
# FEMA NFIP Claims (Insurance) Data 

library(tidyverse)

# Define localities of interest
cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")
eastfips <- c("51001", "51131")

# Get data
claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")

# Filter to relevant regions, years, and variables; group by spatial unit (tract)
cville_claims <- claims %>% 
  filter(countyCode %in% cvillefips,
         yearOfLoss >= 2010) %>%
  select(c('censusTract', 'countyCode', 'yearOfLoss', 'dateOfLoss', 'amountPaidOnBuildingClaim', 'amountPaidOnContentsClaim', 'totalBuildingInsuranceCoverage', 'totalContentsInsuranceCoverage')) %>%
  group_by(censusTract) 

eastern_claims <- claims %>% 
  filter(countyCode %in% eastfips,
         yearOfLoss >= 2010) %>%
  select(c('censusTract', 'countyCode', 'yearOfLoss', 'dateOfLoss', 'amountPaidOnBuildingClaim', 'amountPaidOnContentsClaim', 'totalBuildingInsuranceCoverage', 'totalContentsInsuranceCoverage')) %>%
  group_by(censusTract) 

# Save to csv
write_csv(cville_claims, path = "fema_nfip_cville_tract.csv")
write_csv(eastern_claims, path = "fema_nfip_eastern_tract.csv")
