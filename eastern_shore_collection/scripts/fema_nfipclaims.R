# Khalila Karefa-Kargbo
# 2021-06-26
# FEMA NFIP Claims (Insurance) Data 

library(tidyverse)

# Define localities of interest
cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")
eastfips <- c("51001", "51131")

# Get data
claims <- read_csv("https://www.fema.gov/api/open/v1/FimaNfipClaims.csv")

# Filter to relevant years and regions; group by spatial unit (census tract #)
cville_claims <- claims %>% 
  filter(countyCode %in% cvillefips,
         yearOfLoss >= 2010) %>%
  group_by(censusTract)

eastern_claims <- claims %>% 
  filter(countyCode %in% eastfips,
         yearOfLoss >= 2010)%>%
  group_by(censusTract)


# Relevant Variables ( # of claims and $ amount)

# charlottesville 
amountPaidByTract_c <- cville_claims %>%
  group_by(censusTract) %>%
  summarize(n = count(censusTract),
            avgAmountPaid = mean(amountPaidOnBuildingClaim),
            avgInsuranceCoverage = mean(totalBuildingInsuranceCoverage),
            difference = avgInsuranceCoverage - avgAmountPaid,
            sumAmountPaid = sum(amountPaidOnBuildingClaim),
            sumInsurance = sum(totalBuildingInsuranceCoverage))
print(amountPaidByTract_c)

# eastern shore
amountPaidByTract_e <- eastern_claims %>%
  group_by(censusTract) %>%
  summarize(avgAmountPaid = mean(amountPaidOnBuildingClaim),
            avgInsuranceCoverage = mean(totalBuildingInsuranceCoverage),
            difference = avgInsuranceCoverage - avgAmountPaid,
            sumAmountPaid = sum(amountPaidOnBuildingClaim),
            sumInsurance = sum(totalBuildingInsuranceCoverage))
print(amountPaidByTract_e)

# Save to csv
write_csv(amountPaidByTract_c, path = "fema_nfip_cville_tract.csv")
write_csv(amountPaidByTract_e, path = "fema_nfip_eastern_tract.csv")
