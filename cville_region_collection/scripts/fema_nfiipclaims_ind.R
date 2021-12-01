#' Jordan House
#' 2021-10-10
#' FEMA NFIP Claims (Insurance) Data for the Charlottesville Area


# Import Libraries
library(tidyverse)

# Define localities of interest (county codes)
cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")


# Get data
claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")


# Filter to relevant regions and variables. Filters by county code
cville_individual_claims <- claims %>% 
  filter(countyCode %in% cvillefips) %>%
  select(c('id', 'asOfDate', 'dateOfLoss', 'yearOfLoss', 'censusTract', 'countyCode', 
           'state', 'reportedZipcode', 'latitude', 'longitude', 'floodZone', 'primaryResidence', 
           'occupancyType', 'elevationDifference', 'amountPaidOnBuildingClaim', 
           'amountPaidOnContentsClaim', 'amountPaidOnIncreasedCostOfComplianceClaim', 
           'originalConstructionDate', 'originalNBDate', 'totalBuildingInsuranceCoverage', 
           'totalContentsInsuranceCoverage'))

# Changing variables,"primaryResidence" & "occupancyType", to descriptive names & factors
cville_individual_claims <- cville_individual_claims %>% 
  mutate(primaryResidence = factor(primaryResidence,
                                   levels = c("1", "0"),
                                   labels = c("Yes", "No")),
         occupancyType = factor(occupancyType,
                                labels = c("Single family residence",
                                           "Residential building (2-4 units)",
                                           "Residential building (4+ units)",
                                           "Non-Residential building",
                                           "Non-Residential Business")))

# Add "amountPaid..." variables and treats NA as 0
amountPaid_sums <- rowSums(cville_individual_claims[,c("amountPaidOnBuildingClaim", "amountPaidOnContentsClaim", "amountPaidOnIncreasedCostOfComplianceClaim")], na.rm=TRUE)

#' NOTE: "amountPaidOnIncreasedCostOfComplianceClaim" variable is NA for all county codes in cvilleflips
#' I believe NA means that money was not given for that type of claim or no claims were filed, therefore I treated the variable as 0.

# Adds variable to dataframe
cville_individual_claims$totalAmountPaid <- amountPaid_sums

# Save to csv
write_csv(cville_individual_claims, file = "data/fema_nfipclaims_ind.csv")
