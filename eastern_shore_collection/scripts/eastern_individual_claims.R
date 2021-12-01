#' Jordan House
#' 2021-10-10
#' FEMA NFIP Claims (Insurance) Data for the Eastern Shore Area


# Import Libraries
library(tidyverse)
library(mosaic)

# Define localities of interest (county codes)
eastfips <- c("51001", "51131")

# Get data
claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")


# Filter to relevant regions and variables. Filters by county code
eastern_individual_claims <- claims %>% 
  filter(countyCode %in% eastfips)%>%
  select(c('id', 'asOfDate', 'dateOfLoss', 'yearOfLoss', 'censusTract', 'state', 'reportedZipcode', 'latitude', 'longitude', 'floodZone', 'primaryResidence', 'occupancyType', 'elevationDifference', 'amountPaidOnBuildingClaim', 'amountPaidOnContentsClaim', 'amountPaidOnIncreasedCostOfComplianceClaim', 'originalConstructionDate', 'originalNBDate', 'totalBuildingInsuranceCoverage', 'totalContentsInsuranceCoverage'))

# Changing variables,"primaryResidence" & "occupancyType", to descriptive names & factors
eastern_individual_claims <- eastern_individual_claims %>% 
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
amountPaid_sums <- rowSums(eastern_individual_claims[,c("amountPaidOnBuildingClaim", "amountPaidOnContentsClaim", "amountPaidOnIncreasedCostOfComplianceClaim")], na.rm=TRUE)


# Adds variable to dataframe
eastern_individual_claims$totalAmountPaid <- amountPaid_sums

# Save to csv
write_csv(eastern_individual_claims, file = "fema_nfip_eastern_indv.csv")