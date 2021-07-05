# Pulled from femaclaims_cville.rmd

library(tidyverse)
library(googlesheets4)
library(mosaic)
library(stargazer)
library(sf)
library(leaflet)
library(viridis)

# not used here
# library(reticulate)
# library(tigris)
# library(rgdal)

# Loading data
cville_claims <- read_csv("data/fema_nfip_cville_tract.csv")
# In Rmd file, the path woud be "../data/fema_nfip_cville_tract.csv")
meta <- read_sheet("https://docs.google.com/spreadsheets/d/1nqm3DuVXD1ObbVe_deacvT7uSLdBXfQJo3mkbqDwrVo/edit#gid=5733069", sheet = 'fema_nfip')

# not used here
# cvillefips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# Variables
glimpse(cville_claims)

cville_claims %>% count(censusTract)

meta %>%
  select(c(varname, about)) %>% 
  as.list()

sum(is.na(cville_claims$amountPaidOnContentsClaim)) # 51 / 55  
sum(is.na(cville_claims$amountPaidOnBuildingClaim)) # 14 / 55
sum(is.na(cville_claims$totalBuildingInsuranceCoverage)) # 0
sum(is.na(cville_claims$totalContentsInsuranceCoverage))# 0

prop(is.na(cville_claims$amountPaidOnContentsClaim)) #  92.72% 
prop(is.na(cville_claims$amountPaidOnBuildingClaim)) # 25.45%
prop(is.na(cville_claims$totalBuildingInsuranceCoverage)) # 0
prop(is.na(cville_claims$totalContentsInsuranceCoverage)) # 0 

# Summaries
# 5-number summary of the $ amount paid
cvilleClaimSum <- cville_claims %>%
  group_by(censusTract) %>%
  summarize(n = count(censusTract),
            avg_amt = mean(amountPaidOnBuildingClaim, na.rm = TRUE),
            med_amt = median(amountPaidOnBuildingClaim, na.rm = TRUE),
            sd_amt = sd(amountPaidOnBuildingClaim, na.rm = TRUE),
            min_amt = min(amountPaidOnBuildingClaim),
            max_amt = max(amountPaidOnBuildingClaim))
print(cvilleClaimSum)

# 5-number summary of the total insurance amount
cvilleTotalSum <- cville_claims %>%
  group_by(censusTract) %>%
  summarize(n = count(censusTract),
            avg_amt = mean(totalBuildingInsuranceCoverage, na.rm = TRUE),
            med_amt = median(totalBuildingInsuranceCoverage, na.rm = TRUE),
            sd_amt = sd(totalBuildingInsuranceCoverage, na.rm = TRUE),
            min_amt = min(totalBuildingInsuranceCoverage),
            max_amt = max(totalBuildingInsuranceCoverage))
print(cvilleTotalSum)

# difference between amount covered and amount paid 
cville_claims %>% 
  mutate(diffmean = mean("totalBuildingInsuranceCoverage", na.rm = TRUE) - mean("amountPaidOnBuildingClaim", na.rm = TRUE))


# Visual distributions
# barplot of the average building amount paid by census tract
ggplot(cvilleClaimSum) + 
  geom_col(aes(x=as.factor(censusTract), y=avg_amt)) +
  coord_flip()

# barplot of the average building insurance coverage by census tract
ggplot(cvilleTotalSum) + 
  geom_col(aes(x=as.factor(censusTract), y=avg_amt)) +
  coord_flip()

# Maps
# One issue is that, by this point, you want the claims data to be aggregated
# by census tract -- ideally, that's the format of the csv file you originall
# read in: some combination of cvilleClaimSum and cvilleTotalSum from above
# I'll use CvilleClaimSum as an example

cville_tracts <- readRDS("data/cville_tracts.RDS")

cvilleClaimSum <- cvilleClaimSum %>% 
  mutate(TRACT = str_sub(censusTract, 6,11),
    TRACT = as.character(TRACT))

cville_claims_sum <- cville_tracts %>% 
  left_join(cvilleClaimSum, by = c("TRACTCE" = "TRACT"))

cville_claims_sum <- st_transform(cville_claims_sum, crs = 4326) # to WGS84, given error


#spatial intersection of amount paid on building claim over polygon
pal <- colorNumeric("plasma", reverse = TRUE, domain = cville_claims_sum$n) # viridis

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = cville_claims_sum,
              fillColor = ~pal(n),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T
              ),
              popup = paste0("Tract Number: ", cville_claims_sum$NAME, "<br>",
                             "Number: ", round(cville_claims_sum$n, 2))
  ) %>% 
  addLegend("bottomright", pal = pal, values = cville_claims_sum$n, 
            title = "# FEMA Claims", opacity = 0.7)
