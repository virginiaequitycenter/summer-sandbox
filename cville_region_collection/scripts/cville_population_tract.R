####################################################
# Greater Charlottesville Region Equity Profile
####################################################
# Acquire ACS data
# Last updated: 10/29/2020
# Metrics from ACS (in common with locality level): 
# * Total population
# * Poverty, child poverty 
# * Median HH Income, Gini income inequality index
# * Educational attainment: HS and more, BA and more, grad/prof and more
# * Unemployment 
# * Health insurance, and Public health insurance
# * Race/ethnicity: White (NH), Black, Asian, Hispanic, Indigenous, Multiracial, Other
# * Age groups: 0-17, 18-24, 25-64, 65 or more
# * Median personal earnings
# * Net school enrollment
# * Commute data: number of people with different average commute times. 

# Based on: ACS 2015-2019 
# Geography: Tracts in Localities in Charlottesville region
#     Charlottesville, Albemarle, Greene, Louisa, 
#     Fluvanna, Nelson
####################################################
# 1. Load libraries, provide api key (if needed), identify variables
# 2. Define variables, pull data
# 3. Reduce, derive calculated estimates, combine
# 4. Summarize/Examine
# 5. Save
####################################################


# ....................................................
# 1. Load libraries, provide api key (if needed), identify variables ----

# Load libraries
library(tidyverse)
library(tidycensus)


# Census api key
# census_api_key("", install = TRUE, overwrite = TRUE) # add key

# Variable view helper
# acs_var <- load_variables(2019, "acs5", cache = TRUE)
# acs_var1 <- load_variables(2019, "acs5/subject", cache = TRUE)
# acs_var3 <- load_variables(2019, "acs5/profile", cache = TRUE)
# dec_var <- load_variables(2010, "sf1", cache = TRUE)

# Variable of interest -
##  - Total population -- B01003_001
##  - Poverty rate -- S1701_C03_001 
##  - Child poverty rate -- S1701_C03_002
##  - Median HH Income -- S1901_C01_012	
##  - Gini Index of Income Inequality -- B19083_001
##  - Percent high school graduate or higher -- S1501_C02_014
##  - Percent bachelor's degree or higher -- S1501_C02_015
##  - Percent master's degree or higher -- S1501_C02_013
##  - Percent white alone -- DP05_0077P
##  - Percent black or African American alone -- DP05_0078P
##  - Percent American Indian and Alaska Native alone -- DP05_0079P
##  - Percent Asian alone -- DP05_0080P
##  - Percent Native Hawaiian and Other Pacific Islander alone -- DP05_0081P
##  - Percent Some other race alone -- DP05_0082P
##  - Percent Two or more races -- DP05_0083P
##  - Percent Hispanic or Latino -- DP05_0071P
##  - Percent unemployment (Population 16 and over) -- S2301_C04_001
##  - Percent with health insurance (Civilian noninstitutionalized population) -- S2701_C03_001	
##  - Percent with public health insurance (Civilian noninstitutionalized population) -- S2704_C03_001
##  - Age, population under 18 -- S0101_C02_022
##  - Age, population 18 to 24 -- S0101_C02_023	
##  - Age, 26 to 64 -- sum(S0101_C02_007, S0101_C02_008, S0101_C02_009, S0101_C02_010, S0101_C02_011, S0101_C02_012, S0101_C02_013, S0101_C02_014)
##  - Age, 65 and over --S0101_C02_030
##  - School enrollment for the population age 3 to 24 -- S1401_C02_014, S1401_C02_016, S1401_C02_018, S1401_C02_020, S1401_C02_022, S1401_C21_024
##  - Median personal earnings of all workers with earnings ages 16 and older -- S2001_C01_002
##  - Percent of cost-burdened renters -- B25070_007+B25070_008+B25070_009+B25070_010/B25070_001
##  - Home ownership rates -- B25003_002/B25003_002
##  - Housing vacant unitss -- B25002_003/B25002_001
##  - Estimated <20 minutes commute -- B08134_002 + B08134_003 + B08134_004
##  - Estimated 20 to 34 minutes commute -- B08134_005 + B08134_006 + B08134_008
##  - Estimated 35 to 59 minutes commute -- B08134_008 + B08134_009 
##  - Estimated >=60 minutes commute -- B08134_010


# ....................................................
# 2. Define localities, variables, pull data ----

# List of desired localities by FIPS
region <- c("003", "540", "065", "079", "109", "125")

# - 003 Albemarle County  
# - 540 Charlottesville
# - 065 Fluvanna County
# - 079 Greene County 
# - 109 Louisa County
# - 125 Nelson County


# Get Data
# variables: define varlist
varlist_s = c("S1701_C03_001", # povrate
              "S1701_C03_002",   # cpovrate
              "S1901_C01_012",   # hhinc
              "S1501_C02_014",   # hsmore
              "S1501_C02_015",   # bamore
              "S1501_C02_013",   # gradmore
              "S2301_C04_001",   # unemp
              "S2701_C03_001",   # hlthins
              "S2704_C03_001",   # pubins
              "S2001_C01_002")   # earn

varlist_b = c("B01003_001", # totalpop
              "B19083_001",  # gini
              "B25070_007",  # 30-35 rent burdened
              "B25070_008",  # 35-40 rent burdened
              "B25070_009",  # 40-50 rent burdened
              "B25070_010",  # 50+ rent burdened
              "B25070_001",  # all renters
              "B25003_002",  # owner-occupied housing units
              "B25003_001",  # occupied housing units
              "B25002_003",  # vacant housing units
              "B25002_001")  # housing units

# pull variables
tract_data_s <- get_acs(geography = "tract",
                        variables = varlist_s,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide")

tract_data_b <- get_acs(geography = "tract",
                        variables = varlist_b,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide")

# rename variables
names(tract_data_s) = c("GEOID", "NAME",
                        "povrateE", "povrateM",
                        "cpovrateE", "cpovrateM",
                        "hhincE", "hhincM",
                        "hsmoreE", "hsmoreM",
                        "bamoreE", "bamoreM",
                        "gradmoreE", "gradmoreM",
                        "unempE", "unempM",
                        "hlthinsE", "hlthinsM",
                        "pubinsE", "pubinsM",
                        "earnE", "earnM")

names(tract_data_b) = c("GEOID", "NAME",  
                        "totalpopE", "totalpopM",
                        "giniE", "giniM",
                        "rent30E", "rent30M",
                        "rent35E", "rent35M",
                        "rent40E", "rent40M",
                        "rent50E", "rent50M",
                        "rentallE", "rentallM",
                        "ownoccE", "ownoccM",
                        "occhseE", "occhseM",
                        "vachseE", "vachseM",
                        "allhseE", "allhseM")

# Derive some variables
tract_data_b <- tract_data_b %>% 
  mutate(rentersumE = (rent30E+rent35E+rent40E+rent50E),
         rentersumM = (rent30M/1.645)^2 + (rent35M/1.645)^2 + (rent40M/1.645)^2 + (rent50M/1.645)^2,
         rentersumM = sqrt(rentersumM)*1.645,
         renter30E = round((rentersumE/rentallE)*100,1),
         renter30M = moe_prop(rentersumE, rentallE, rentersumM, rentallM),
         renter30M = round(renter30M*100,1)) %>% 
  mutate(homeownE = round((ownoccE/occhseE)*100,1),
         homeownM = moe_prop(ownoccE, occhseE, ownoccM, occhseM),
         homeownM = round(homeownM*100, 1)) %>% 
  mutate(vacrateE = round((vachseE/allhseE)*100,1),
         vacrateM = moe_prop(vachseE, allhseE, vachseM, allhseM),
         vacrateM = round(vacrateM*100, 1)) %>% 
  select(-c(rentersumE, rentersumM,rent30E:occhseM))

# for commute variables  
varlist_c = c("B08134_002",  # < 10 min commute to work
              "B08134_003",  # 10 - 15 min comute to work (will become <20 min commute)
              "B08134_004",  # 15 - 19 min commute to work (will become <20 min commute)
              "B08134_005",  # 20 - 24 min commute to work (will become <20 min commute)
              "B08134_006",  # 25 - 29 min commute to work (will become 20 to 34 min commute)
              "B08134_007",  # 30 to 34 min commute to work (will become 20 to 34 min commute)
              "B08134_008",  # 35 to 44 min commute to work (will become 35 to 59 min commute)
              "B08134_009",  # 44 to 59 min commute to work (will become 35 to 59 min commute)
              "B08134_010")  # >=60 min commute 

# Pull variables
tract_data_c <- get_acs(geography = "tract",
                         variables = varlist_c,
                         show_call = T,
                         state = "VA", 
                         county = region, 
                         survey = "acs5",
                         year = 2019, 
                         output = "wide")

# rename variables -- these should be in the correct order given the download, but double check with future downloads 
# by matching the variable names above. 
names(tract_data_c) = c("GEOID", "NAME",
                         "c10minE", "c10minM",
                         "c10_15minE", "c10_15minM",
                         "c15_19minE", "c15_19minM",
                         "c20_24minE", "c20_24minM",
                         "c25_29minE", "c25_29minM",
                         "c30_34minE", "c30_34minM",
                         "c35_44minE", "c35_44minM",
                         "c44_59minE", "c44_59minM",
                         "cgreater_60minE", "cgreater_60minM")

# Derive some variables
tract_data_c <- tract_data_c %>% 
  mutate(cunder20minE = (c10minE + c10_15minE + c15_19minE),
         cunder20minM = (c10minM/1.645)^2 + (c10_15minM/1.645)^2 + (c15_19minM/1.645)^2,
         cunder20minM = round(sqrt(cunder20minM)*1.645, 2),
         c20to34minE = (c20_24minE + c25_29minE + c30_34minE),
         c20to34minM = (c20_24minM/1.645)^2 + (c25_29minM/1.645)^2 + (c30_34minM/1.645)^2,
         c20to34minM = round(sqrt(c20to34minM)*1.645, 2),
         c35to59minE = (c35_44minE + c44_59minE),
         c35to59minM = (c35_44minM/1.645)^2 + (c44_59minM/1.645)^2,
         c35to59minM = round(sqrt(c35to59minM)*1.645, 2)) %>%
  select(GEOID, NAME, cunder20minE, cunder20minM, c20to34minE, c20to34minM, c35to59minE, 
         c35to59minM, cgreater_60minE, cgreater_60minM)

# Get Data
# pull tables (easier to just pull tables separately)
tract_race <- get_acs(geography = "tract", 
                      table = "DP05", 
                      state = "VA", 
                      county = region, 
                      survey = "acs5",
                      year = 2019)

tract_age <- get_acs(geography = "tract", 
                     table = "S0101", 
                     state = "VA", 
                     county = region, 
                     survey = "acs5", 
                     year = 2019)

tract_enroll <- get_acs(geography = "tract", 
                        table = "S1401", 
                        state = "VA", 
                        county = region, 
                        survey = "acs5", 
                        year = 2019)


# ....................................................
# 3. Reduce and Combine data ----

# Reduce tables: tract_age, tract_race, tract_enroll
# tract_age: three age groups present as rows in the table,
#             one group must be summed
tract_age17 <- tract_age %>% 
  filter(variable == "S0101_C02_022") %>% 
  rename(age17E = estimate,
         age17M = moe) %>% 
  select(-variable)

tract_age24 <- tract_age %>% 
  filter(variable == "S0101_C02_023") %>% 
  rename(age24E = estimate,
         age24M = moe) %>% 
  select(-variable)

tract_age64 <- tract_age %>% 
  filter(variable %in% c("S0101_C02_007", "S0101_C02_008", "S0101_C02_009",
                         "S0101_C02_010", "S0101_C02_011", "S0101_C02_012",
                         "S0101_C02_013", "S0101_C02_014")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(age64E = sum(estimate),
            age64M = moe_sum(moe = moe, estimate = estimate))

tract_age65 <- tract_age %>% 
  filter(variable == "S0101_C02_030") %>% 
  rename(age65E = estimate,
         age65M = moe) %>% 
  select(-variable)

# tract_race: all groups present as rows in the table
#             but other race and native hawaiian/pacific islander combined
#             due to very small values
tract_white <- tract_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  select(-variable)

tract_black <- tract_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  select(-variable)

tract_indig <- tract_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  select(-variable)

tract_asian <- tract_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  select(-variable)

tract_othrace <- tract_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

tract_multi <- tract_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  select(-variable)

tract_ltnx <- tract_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  select(-variable)

# tract_schl: 6 groups (3-4, 5-9, 10-14, 15-17, 18-19, 20-24) must be summed
#             population and enrolled, and divided
tract_schl_num <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_014", "S1401_C01_016", "S1401_C01_018", "S1401_C01_020", "S1401_C01_022", "S1401_C01_024")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_num = sum(estimate), 
            schl_numM = moe_sum(moe = moe, estimate = estimate))

tract_schl_den <- tract_enroll %>% 
  filter(variable %in% c("S1401_C01_013", "S1401_C01_015", "S1401_C01_017", "S1401_C01_019", "S1401_C01_021", "S1401_C01_023")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(schl_den = sum(estimate), 
            schl_denM = moe_sum(moe = moe, estimate = estimate))

tract_schl_ratio <- left_join(tract_schl_num, tract_schl_den)

tract_schl <- tract_schl_ratio %>% 
  summarize(schlE = round((schl_num/schl_den)*100,1),
            schlM = moe_prop(schl_num, schl_den, schl_numM, schl_denM),
            schlM = round(schlM*100,1))


# Combine indicators
# joining columns
tract_data <- tract_data_s %>% 
  left_join(tract_data_b) %>% 
  left_join(tract_schl) %>% 
  left_join(tract_white) %>% 
  left_join(tract_black) %>% 
  left_join(tract_indig) %>% 
  left_join(tract_asian) %>% 
  left_join(tract_othrace) %>% 
  left_join(tract_multi) %>% 
  left_join(tract_ltnx) %>% 
  left_join(tract_age17) %>% 
  left_join(tract_age24) %>% 
  left_join(tract_age64) %>% 
  left_join(tract_age65) %>%
  left_join(tract_data_c)

tract_data <- tract_data %>% 
  mutate(year = "2019") %>% 
  select(GEOID, NAME, year, totalpopE, totalpopM, whiteE, whiteM, blackE, blackM, asianE, asianM, indigE, indigM, othraceE, othraceM, multiE, multiM, ltnxE, ltnxM, everything())

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

# ....................................................
# 4. Summarize/Examine indicators ----
tract_data %>% select_at(vars(ends_with("E"))) %>% summary()

# Ablemarle tract 109.03 is UVA -- 
#   highest poverty rate, and gini coefficient; missing child pov and hhinc
#   make this NA for all economic variables
tract_data <- tract_data %>% 
  mutate(povrateE = ifelse(GEOID == "51003010903", NA_integer_, povrateE),
         giniE = ifelse(GEOID == "51003010903", NA_integer_, giniE))

ggplot(tract_data, aes(x = bamoreE, y = giniE)) + 
  geom_point() + geom_smooth()


# ....................................................
# 5. Save ----
write.csv(tract_data, "acs_tract_cville.csv", row.names = F) 
# tract_data <- readRDS("data/tract_data.RDS")


# initailly added earnings and school enrollment in November/later
# but have since re-run with these included
# leaving this here for future additions

# # add year
# tract_earn <- tract_earn %>% 
#   mutate(year = "2018") 
# tract_schl <- tract_schl %>% 
#   mutate(year = "2018") 
# tract_grad <- tract_grad %>% 
#   mutate(year = "2018") 
# 
# # joining columns
# tract_data <- tract_data %>% 
#   left_join(tract_earn, by = c("GEOID", "NAME", "year"))
# 
# tract_data <- tract_data %>% 
#   left_join(tract_schl, by = c("GEOID", "year")) 
# 
# tract_data <- tract_data %>% 
#   left_join(tract_grad, by = c("GEOID", "year")) 
#
# # reorder
# tract_data <- tract_data %>% 
#   select(GEOID:bamoreM, gradmoreE, gradmoreM, unempE:tract)