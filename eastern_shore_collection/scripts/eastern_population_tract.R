####################################################
# Eastern Shore Population Data
####################################################
# Acquire ACS data
# Last updated: 04/21/2022
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
# * Broadband: percent of households with broadband and % with broadband and a computer 
# * Housing age: percent of housing units in a tract built within certain bands of time

# Based on: ACS 2015-2019 
# Geography: Blogk groups in Localities in Eastern Shore
#   Accomack (51001), Northampton (51131)
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
##  - Housing vacant units -- B25002_003/B25002_001
##  - Percent with <20 minutes commute -- B08134_002 + B08134_003 + B08134_004
##  - Percent with 20 to 34 minutes commute -- B08134_005 + B08134_006 + B08134_008
##  - Percent with 35 to 59 minutes commute -- B08134_008 + B08134_009 
##  - Percent with >=60 minutes commute -- B08134_010
##  - Percent that have a broadband subscription of any kind -- B28002_004
##  - Percent that have a computer and a broadband subscription -- B28003_004
##  - Percent of housing units built before 1940 -- B25034_011
##  - Percent of housing units biult between 1940 and 1959 -- B25034_010 + B25034_009
##  - Percent of housing units built between 1960 and 1979 -- B25034_008 + B25034_007
##  - Percent of housing units built between 1980 and 1999 -- B25034_006 + B25034_005
##  - Percent of housing units built after 2000 -- B25034_004 + B25034_003 + B25034_002
##  - Number of households who receive cash public assistance/SNAP benefits -- B19058_002
##  - Number of foreign-born residents -- B05002_013

# ....................................................
# 2. Define localities, variables, pull data ----

# List of desired localities by FIPS
region <- c("001", "131")

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
              "B25002_001",  # housing units
              "B28002_004", # Broadband of any type
              "B28003_004", # Have a computer and broadband
              "B19058_002", # SNAP
              "B05002_013") # Foreign-born

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
                        "allhseE", "allhseM",
                        "broadE", "broadM",
                        "compbroadE", "compbroadM",
                        "snapE", "snapM",
                        "foreignbE", "foreignbM")

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
  dplyr::select(-c(rentersumE, rentersumM,rent30E:occhseM))

# Derive broadband variables 
tract_data_b <- tract_data_b %>% 
  mutate(pbroadE = round(broadE/ allhseE* 100, 1),
         pbroadM = moe_prop(broadE, allhseE, broadM, allhseM),
         pbroadM = round(pbroadM*100, 1),
         pcompbroadE = round(compbroadE/ allhseE* 100, 1),
         pcompbroadM = moe_prop(compbroadE, allhseE, compbroadE, allhseM),
         pcompbroadM = round(pcompbroadM*100, 1)) %>% 
  dplyr::select(-c(broadE, broadM, compbroadE, compbroadM))

# Derive snap variables
tract_data_b <- tract_data_b %>% 
  mutate(perc_snaphseE = round((snapE / allhseE)*100,1),
         perc_snaphseM = round(moe_prop(snapE, allhseE, snapM, allhseM), 2),
         .keep = "all")

# Derive foreign born variables
tract_data_b <- tract_data_b %>% 
  mutate(perc_forbE = round((foreignbE / totalpopE)*100,1),
         perc_forbM = round(moe_prop(foreignbE, totalpopE, foreignbM, totalpopM), 2),
         .keep = "all")

# for commute variables  
varlist_c = c("B08134_001", # total workers
              "B08134_002",  # < 10 min commute to work
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
                        "totalworkersE", "totalworkersM",
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
  mutate(cunder20minsumE = (c10minE + c10_15minE + c15_19minE),
         cunder20minsumM = (c10minM/1.645)^2 + (c10_15minM/1.645)^2 + (c15_19minM/1.645)^2,
         cunder20minsumM = round(sqrt(cunder20minsumM)*1.645, 2),
         perc_cUnder20minE = round(cunder20minsumE / totalworkersE * 100, 1),
         perc_cUnder20minM = moe_prop(cunder20minsumE, totalworkersE, cunder20minsumM, totalworkersM),
         perc_cUnder20minM = round(perc_cUnder20minM*100,1),

         c20to34minsumE = (c20_24minE + c25_29minE + c30_34minE),
         c20to34minsumM = (c20_24minM/1.645)^2 + (c25_29minM/1.645)^2 + (c30_34minM/1.645)^2,
         c20to34minsumM = round(sqrt(c20to34minsumM)*1.645, 2),
         perc_c20to34minE = round(c20to34minsumE / totalworkersE * 100, 1),
         perc_c20to34minM = moe_prop(c20to34minsumE, totalworkersE, c20to34minsumM, totalworkersM),
         perc_c20to34minM = round(perc_c20to34minM*100,1),
         
         c35to59minsumE = (c35_44minE + c44_59minE),
         c35to59minsumM = (c35_44minM/1.645)^2 + (c44_59minM/1.645)^2,
         c35to59minsumM = round(sqrt(c35to59minsumM)*1.645, 2),
         perc_c35to59minE = round(c35to59minsumE / totalworkersE * 100, 1),
         perc_c35to59minM = moe_prop(c35to59minsumE, totalworkersE, c35to59minsumM, totalworkersM),
         perc_c35to59minM = round(perc_c35to59minM*100,1),
         
         perc_cgreater_60minE = round(cgreater_60minE / totalworkersE * 100, 1),
         perc_cgreater_60minM = moe_prop(cgreater_60minE, totalworkersE, cgreater_60minM, totalworkersM),
         perc_cgreater_60minM = round(perc_cgreater_60minM*100, 1)) %>%
  dplyr::select(GEOID, NAME, perc_cUnder20minE, perc_cUnder20minM, perc_c20to34minE, 
         perc_c20to34minM, perc_c35to59minE, perc_c35to59minM,
         perc_cgreater_60minE, perc_cgreater_60minM)

# For home age variables 
varlist_ha = c("B25034_001",  # Total housing 
               "B25034_002",  # House built 2014 or later
              "B25034_003",  # House built 2010 - 2013
              "B25034_004",  # House built 2000 - 2009
              "B25034_005",  # House built 1990 - 1999
              "B25034_006",  # House built 1980 - 1989
              "B25034_007",  # House built 1970 - 1979
              "B25034_008",  # House built 1960 - 1969
              "B25034_009",  # House built 1950 - 1959
              "B25034_010",  # House built 1940 - 1949
              "B25034_011")  # House built 1939 or earlier

# Pull variables
tract_data_ha <- get_acs(geography = "tract",
                        variables = varlist_ha,
                        show_call = T,
                        state = "VA", 
                        county = region, 
                        survey = "acs5",
                        year = 2019, 
                        output = "wide")

# rename variables -- these should be in the correct order given the download, but double check with future downloads 
# by matching the variable names above. 
names(tract_data_ha) = c("GEOID", "NAME",
                         "tothousinguE", "tothousinguM",
                        "b2014lE", "b2014lM",
                        "b2010_2013E", "b2010_2013M",
                        "b2000_2009E", "b2000_2009M",
                        "b1990_1999E", "b1990_1999M",
                        "b1980_1989E", "b1980_1989M",
                        "b1970_1979E", "b1970_1979M",
                        "b1960_1969E", "b1960_1969M",
                        "b1950_1959E", "b1950_1959M",
                        "b1940_1949E", "b1940_1949M",
                        "bbefore1940E", "bbefore1940M")

# Derive some variables
tract_data_ha <- tract_data_ha %>% 
  mutate(bbefore1940E = round(bbefore1940E/tothousinguE*100, 1),
         bbefore1940M = moe_prop(bbefore1940E, tothousinguE, bbefore1940M, tothousinguM),
         bbefore1940M = round(sqrt(bbefore1940M)*1.645, 2),
         
         bb1940_1959sumE = b1940_1949E + b1950_1959E,
         bb1940_1959sumM = (b1940_1949M / 1.645)^2 + (b1950_1959M / 1.645)^2,
         bb1940_1959sumM = sqrt(bb1940_1959sumM) * 1.645,
         bb1940_1959E = round((bb1940_1959sumE / tothousinguE)*100, 1),
         bb1940_1959M = moe_prop(bb1940_1959sumE, tothousinguE, bb1940_1959sumM, tothousinguM),
         bb1940_1959M = round(sqrt(bb1940_1959M)*1.645, 2),
         
         bb1960_1979sumE = b1960_1969E + b1970_1979E,
         bb1960_1979sumM = (b1960_1969M / 1.645)^2 + (b1970_1979M / 1.645)^2,
         bb1960_1979sumM = sqrt(bb1960_1979sumM) * 1.645,
         bb1960_1979E = round((bb1960_1979sumE / tothousinguE)*100, 1),
         bb1960_1979M = moe_prop(bb1960_1979sumE, tothousinguE, bb1960_1979sumM, tothousinguM),
         bb1960_1979M = round(sqrt(bb1960_1979M)*1.645, 2),
         
         bb1980_1999sumE = b1980_1989E + b1990_1999E,
         bb1980_1999sumM = (b1980_1989M / 1.645)^2 + (b1990_1999M / 1.645)^2,
         bb1980_1999sumM = sqrt(bb1980_1999sumM) * 1.645,
         bb1980_1999E = round((bb1980_1999sumE / tothousinguE)*100, 1),
         bb1980_1999M = moe_prop(bb1980_1999sumE, tothousinguE, bb1980_1999sumM, tothousinguM),
         bb1980_1999M = round(sqrt(bb1980_1999M)*1.645, 2),
         
         ba2000sumE = b2000_2009E + b2010_2013E + b2014lE,
         ba2000sumM = (b2000_2009M / 1.645)^2 + (b2010_2013M / 1.645)^2 + (b2014lM / 1.645)^2,
         ba2000sumM = sqrt(ba2000sumM) * 1.645,
         ba2000E = round((ba2000sumE / tothousinguE)*100, 1),
         ba2000M = moe_prop(ba2000sumE, tothousinguE, ba2000sumM, tothousinguM),
         ba2000M = round(sqrt(ba2000M)*1.645, 2)) %>%
  dplyr::select(GEOID, NAME, tothousinguE, tothousinguM, bbefore1940E, bbefore1940M, bb1940_1959E, bb1940_1959M, bb1960_1979E, 
         bb1960_1979M, bb1980_1999E, bb1980_1999M, ba2000E, ba2000M)


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
  dplyr::select(-variable)

tract_age24 <- tract_age %>% 
  filter(variable == "S0101_C02_023") %>% 
  rename(age24E = estimate,
         age24M = moe) %>% 
  dplyr::select(-variable)

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
  dplyr::select(-variable)

# tract_race: all groups present as rows in the table
#             but other race and native hawaiian/pacific islander combined
#             due to very small values
tract_white <- tract_race %>% 
  filter(variable == "DP05_0077P") %>% 
  rename(whiteE = estimate,
         whiteM = moe) %>% 
  dplyr::select(-variable)

tract_black <- tract_race %>% 
  filter(variable == "DP05_0078P") %>% 
  rename(blackE = estimate,
         blackM = moe) %>% 
  dplyr::select(-variable)

tract_indig <- tract_race %>% 
  filter(variable == "DP05_0079P") %>% 
  rename(indigE = estimate,
         indigM = moe) %>% 
  dplyr::select(-variable)

tract_asian <- tract_race %>% 
  filter(variable == "DP05_0080P") %>% 
  rename(asianE = estimate,
         asianM = moe) %>% 
  dplyr::select(-variable)

tract_othrace <- tract_race %>% 
  filter(variable %in% c("DP05_0081P", "DP05_0082P")) %>% 
  group_by(GEOID, NAME) %>% 
  summarize(othraceE = sum(estimate),
            othraceM = moe_sum(moe = moe, estimate = estimate))

tract_multi <- tract_race %>% 
  filter(variable == "DP05_0083P") %>% 
  rename(multiE = estimate,
         multiM = moe) %>% 
  dplyr::select(-variable)

tract_ltnx <- tract_race %>% 
  filter(variable == "DP05_0071P") %>% 
  rename(ltnxE = estimate,
         ltnxM = moe) %>% 
  dplyr::select(-variable)

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
  left_join(tract_data_c) %>%
  left_join(tract_data_ha)

tract_data <- tract_data %>% 
  mutate(year = "2019") %>% 
  dplyr::select(GEOID, NAME, year, totalpopE, totalpopM, whiteE, whiteM, blackE, blackM, asianE, asianM, indigE, indigM, othraceE, othraceM, multiE, multiM, ltnxE, ltnxM, everything())

tract_data <- tract_data %>% 
  mutate(geoid = GEOID) %>% 
  separate(geoid, into = c("state", "locality", "tract"), 
           sep = c(2,5)) 

# ....................................................
# 4. Summarize/Examine indicators ----
tract_data %>% select_at(vars(ends_with("E"))) %>% summary()

# ....................................................
# 5. Save ----
write.csv(tract_data, "acs_eastern_tract.csv", row.names = F) 
