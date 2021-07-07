# Khalila Karefa-Kargbo
# 2021-07-06
# FEMA NFIP Claims (Insurance) Data 

# Source link: https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims

library(tidyverse)
library(mosaic)

# Define localities of interest
cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")
eastfips <- c("51001", "51131")

# Get data
claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")

# Filter to relevant regions, years, and variables
cville_claims <- claims %>% 
  filter(countyCode %in% cvillefips,
         yearOfLoss >= 2010) %>%
  select(c('censusTract', 'countyCode', 'yearOfLoss', 'amountPaidOnBuildingClaim', 'amountPaidOnContentsClaim', 'totalBuildingInsuranceCoverage', 'totalContentsInsuranceCoverage')) 

eastern_claims <- claims %>% 
  filter(countyCode %in% eastfips,
         yearOfLoss >= 2010) %>%
  select(c('censusTract', 'countyCode', 'yearOfLoss', 'amountPaidOnBuildingClaim', 'amountPaidOnContentsClaim', 'totalBuildingInsuranceCoverage', 'totalContentsInsuranceCoverage'))

# Aggregate Data
cvilleAmountPaidByTract <- cville_claims %>%
  group_by(censusTract) %>%
  summarize(n = count(censusTract),
            sumAmountPaidOnBuildingClaim = sum(amountPaidOnBuildingClaim, na.rm = T),
            sumAmountPaidOnContentsClaim = sum(amountPaidOnContentsClaim, na.rm = T),
            sumBuildingInsuranceCoverage = sum(totalBuildingInsuranceCoverage, na.rm = T),
            sumContentsInsuranceCoverage = sum(totalContentsInsuranceCoverage, na.rm = T))

easternAmountPaidByTract <- eastern_claims %>%
  group_by(censusTract) %>%
  filter(!is.na(censusTract)) %>% # 9 NAs filtered out
  summarize(n = count(censusTract),
            sumAmountPaidOnBuildingClaim = sum(amountPaidOnBuildingClaim, na.rm = T),
            sumAmountPaidOnContentsClaim = sum(amountPaidOnContentsClaim, na.rm = T),
            sumBuildingInsuranceCoverage = sum(totalBuildingInsuranceCoverage, na.rm = T),
            sumContentsInsuranceCoverage = sum(totalContentsInsuranceCoverage, na.rm = T))

# Creating variable for total insurance amount (adding building + contents)
cvilleAmountPaidByTract$totalInsuranceCoverage <- cvilleAmountPaidByTract$sumBuildingInsuranceCoverage + cvilleAmountPaidByTract$sumContentsInsuranceCoverage
easternAmountPaidByTract$totalInsuranceCoverage <- easternAmountPaidByTract$sumBuildingInsuranceCoverage + easternAmountPaidByTract$sumContentsInsuranceCoverage

# Creating variable for total claim amount (adding building + contents)
cvilleAmountPaidByTract$totalAmountPaid <- cvilleAmountPaidByTract$sumAmountPaidOnBuildingClaim + cvilleAmountPaidByTract$sumAmountPaidOnContentsClaim
easternAmountPaidByTract$totalAmountPaid <- easternAmountPaidByTract$sumAmountPaidOnBuildingClaim + easternAmountPaidByTract$sumAmountPaidOnContentsClaim

# Save to csv
write_csv(cvilleAmountPaidByTract, path = "fema_nfip_cville_tract.csv")
write_csv(easternAmountPaidByTract, path = "fema_nfip_eastern_tract.csv")