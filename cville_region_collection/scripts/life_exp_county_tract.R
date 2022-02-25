# Get Life Expectancy Data
# Michele Claibourn
# Created: 2022-02-24

library(tidyverse)
library(readxl)

region <- c("51003", "51540", "51065", "51079", "51109", "51125")
region3 <- c("003", "540", "065", "079", "109", "125")

# ....................................................
# County Measures ----
# # Initial download
# # County Level Life Expectancy:  https://www.countyhealthrankings.org/app/virginia/2020/measure/outcomes/147/data
# url <- "https://www.countyhealthrankings.org/sites/default/files/media/document/2020%20County%20Health%20Rankings%20Virginia%20Data%20-%20v1_0.xlsx"
# destfile <- "data/county_health_rankings.xlsx"
# download.file(url, destfile)

county_life <- read_excel("data/county_health_rankings.xlsx", sheet = 5, skip = 1) 

region_life <- county_life %>% 
  filter(FIPS %in% region) %>% 
  select(FIPS, State, County, contains("Life Expectancy")) %>% 
  rename_with(~tolower(str_replace_all(.x, "\\.", ""))) %>%
  rename_with(~tolower(str_replace_all(.x, " ", "_"))) 

# Save
write.csv(region_life, "data/lifeexp_cville_county.csv", row.names = F) 


# ....................................................
# Tract Measures ----
# Tract Level Life Expectancy: https://www.cdc.gov/nchs/nvss/usaleep/usaleep.html <- use this for life expectancy for the tract disaggregated AHDI
url <- "https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/NVSS/USALEEP/XLSX/VA_A.XLSX"
destfile <- "data/tract_life_expectancy.xlsx"
download.file(url, destfile)

tract_expectancy <- read_excel("data/tract_life_expectancy.xlsx")

region_tract_life <- tract_expectancy %>% 
  filter(CNTY2KX %in% region3) %>% 
  rename_with(
    ~tolower(
      str_replace_all(.x, 
                      " ", "_")
    )    
  ) %>%
  rename(GEOID = tract_id, state_fips = state2kx, county_fips = cnty2kx, tract_fips = tract2kx, life_expectancy = `e(0)`, se = `se(e(0))` ) %>%
  select(-abridged_life_table_flag) %>%
  mutate(fips = paste0(state_fips, county_fips)) 

# Save
write.csv(region_tract_life, "data/lifeexp_cville_tract.csv", row.names = F) 
