
# Author: Lee LeBoeuf
# Last updated: 10/08/2021 

# This script cleans data that was manually downloaded from the Consumer Financial Protection Bureau and Federal Financial Institutions Examinations Council 
# All data were collected as part of the Home Mortgage Disclosure Act (HMDA), which requires financial institutions to maintain, report, 
# and publicly disclose information about mortgages. 
# New data is released annually (typically in the summer months -- 2020 data was released in June of 2021). 
# Starting in 2018, the data collection process mandated by the HMDA changed and expanded. 
# Some of the variable names and categories changed slightly from pre-2018 to 2018 and after, so 
# the majority of this script focuses on re-naming/re-coding variables such that they are consistent across time, and merging data sets.  
# The following variables are available pre-2017 and post-2018
#   state_code, county_code, census_tract, year (activity_year in post-2017 data, as_of_year in pre-2017 data)
#   action_taken, purchaser_type, preapproval, loan_type, loan_purpose, lien_status, rate_spread, hoepa_status
#   applicant annual income and loan amount 
# (however, these variables seem highly prone to error and unreliable--hundreds of the values 
# for these variables seem impossible--I don't recommend using them for analysis)
#   applicant_race_1/2/3/4/5 
# (Most values are NA's after applicant_race_3)
#   co_applicant_race_1/2/3/4/5, applicant_sex and co_applicant_sex, denial_reason_1/2/3
# Other population data available: 
#   % of population in the census tract that is a racial minority, the total population in the tract
#   the median family income for the MSA/MD in which the tract is located, the number of owner-occupied units in the tract, and 
#   the number of 1- to 4- family units in the tract. 
# With new data, this process can be repeated by re-running code for the post2017 data with the new data. 
# The process should never need to be repeated for data pre-2018.
# This script keeps data at the individual mortgage level--use the "hmda_cville_tract.R" script
# to aggregate the product of this script to tract-level summaries. 

# The first half of the script contains the cleaning process for any new data, the bottom half shows the process for all
# pre-2018 data. The code for pre-2018 data should not need to be run again.

# Data from 2016 and earlier can be downloaded via the Consumer Financial Protection Bureau
# here: https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=va&records=all-records&field_descriptions=labels
# Data from 2017 and onward can be downloaded via the Federal Financial Institutions Examinations Council 
# here: https://ffiec.cfpb.gov/data-browser/data/2018?category=states&items=VA 

# Areas included in these data:
# Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# Loading required packages
invisible(lapply(list('tidyverse','psych','stringr', 'DescTools', 'googlesheets4'),
                 function(pkg) library(pkg, character.only = TRUE)))

##### Information and process for recoding variables ######
### ALL OF THIS WILL NEED TO BE RUN TO CLEAN NEW DATA 
# Below I create two data frames that are then converted to character strings and used within the recode function 
# to recode columns for applicant race and denial reason. 
# The recode character strings can be used within the dyplr recode function by using a !!! operator, which
# effectively un-quotes the character strings so that they can be properly evaluated within the recode function. That is, it converts 
# the character strings to the correct format to be used as arguments within the recode function. There
# are multiple columns for applicant race and denial reasons, so this method allows for a simpler way to use the same information
# for multuple columns-worth of recodes, without having to type out the individual recodes each time. 
# The data values come from the data dictionary provided by the FFIEC (https://ffiec.cfpb.gov/documentation/2018/lar-data-fields/)
# If, in the future, they break down the categories for either variable to be more granular/specifc, you can add more values to the data frame created here--just make sure
# that in the resulting data frame, the correct values are paired with each other. 
# The old values are numeric and the new values are character strings 
# Be careful about running both sequentially because the column names in both the race recodes and denial reason recodes need to be 
# old and new

# Creating data frames with desired recodes for some of the variables below

# RACE recodes
old <- c(1, 2, 21, 22, 23, 24, 25, 26, 27, 3, 4, 41, 42, 43, 44, 5, 6, 7, 8)
new <- c("American Indian or Alaska Native", "Asian", "Asian", "Asian", "Asian", "Asian", "Asian", "Asian", "Asian", "Black",
         "Native Hawaiian or PI", "Native Hawaiian or PI", "Native Hawaiian or PI", "Native Hawaiian or PI", "Native Hawaiian or PI", "White",
         "Race not provided by applicant", "not applicable", "no co-applicant")
recodesrace <- data.frame(old, new)

# DENIAL reason recodes
old <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
new <- c("Debt-to-income-ratio", "Employment history", "Credit history", "Collateral", "Insufficient cash", "Unverifiable information", "credit app incomplete",
         "Mortgage insurance denied", "Other", "Not applicable")
recodesdenial <- data.frame(old, new)

# Writing a function for the later recoding using the recode data frames above
recodeparse <- function(x) {
  v <- setNames(x$new, x$old)
  v
}

# creating the character strings that will be used below
racerecodes <- recodeparse(recodesrace)
denialrecodes <- recodeparse(recodesdenial)

#####################################################################################################################################################
# Reading in the data and rbinding multiple years worth of data.                                                                                    # 
# The process in this box should never need to be repeated. When new data post-2020 is released, it can be run through the same                     # 
# data cleaning process below for the post-2017 data, and then rbinded to the dataframe containing all 2004-2020 data.                              #
                                                                                                                                                    #
# Reading in the 2017 and earlier data (This is for the entire state of VA for years 2007-2017, it will be a huge datafile that will                #
# need to be filtered to just Cville region and re-formatted slightly)                                                                              #                                                            #
filedr <- setwd("/Users/Carlson/Desktop/HMDA data/Pre-2017")                                                                                      #
file_names2017 <- dir(filedr)                                                                                                                     #
pre2018 <- do.call(rbind, lapply(file_names2017, read.csv))                                                                                       #
# # Filtering the pre-2017 data for just the Charlottesville region                                                                                 #
# # The pre-2018 data is missing leading zeros in the county/FIPs codes                                                                             #
# # which impacts Albemarle, Fluvanna, and Greene counties                                                                                          #
# # Below I input the leading zeros so that the data can be properly filtered by the fips codes                                                     #
# pre2018$county_code <- ifelse(pre2018$county_code == 3, paste0("00", 3), pre2018$county_code)                                                     #
# pre2018$county_code <- ifelse(pre2018$county_code == 65 | pre2018$county_code == 79, paste0("0", pre2018$county_code), pre2018$county_code)       #
#                                                                                                                                                   #
# # Reading in the post 2017 data--also for the entire state, for the years 2018-2020                                                               #
filedr <- setwd("/Users/Carlson/Desktop/HMDA data/Post-2017")                                                                                     #
file_names_post2017 <- dir(filedr)                                                                                                                #
post2017 <- do.call(rbind, lapply(file_names_post2017, read.csv))                                                                                 #
#####################################################################################################################################################

# Cleaning process for post-2017 data ###############################################################################################################
# This process can be repeated with single year datafiles from post-2020. The cleaned version can then be rbinded to the 
# datafile containing years 2004-2020, simply change the year names on the dataframes below. 
# More specifically, one could manually download state data from 2021, read it into R, and then replace 
# the cvlpost2017 dataframe below with whatever the new object is called. 

# Creating necessary filtering strings
cvlcounties <- c("51540", "51003", "51065", "51079", "51109", "51125") # Data post-2018 needs
# to be filtered using the full county codes 

cvlpost2017 <- post2017 %>%
  filter(county_code %in% cvlcounties)

# The naming conventions of certain variables changed slightly starting
# in 2018 such that periods were used where underscores had been used in previous years
# This line replaces the periods in the column names in the post-2017 data with underscores so 
# that it matches the pre-2018 data. 
colnames(cvlpost2017) <- str_replace_all(colnames(cvlpost2017), "(?<=\\w)\\.", "_")

# Changing column names and values so that they match the pre-2018 data, and selecting out the variables of interest
# Note, for much of these variables, I had to collapse some of the categories--for example, recoding Asian Indian, 
# Chinese, Filipino, Japanese, Korean, Vietnamese, and Other Asian all as Asian. The older data does not offer that 
# level of specificity, nor did it seem useful for our purposes, so the data were simplified. The original data 
# fields can be found here (https://ffiec.cfpb.gov/documentation/2018/lar-data-fields/). 
# Also, numeric values were converted to character strings to allow the user to immediately know what each 
# value represented, without having to refer to the data dictionary. 

cvlpost2017 <- cvlpost2017 %>%
  rename(year = activity_year,
         id = lei,
         minority_population = tract_minority_population_percent,
         median_family_income = ffiec_msa_md_median_family_income) %>%
  mutate(action_taken = recode(action_taken, "1" = "loan originated", "2" = "approved but not accepted", "3" = "denied", "4" = "withdrawn by applicant", "5" = "closed-incomplete", "6" = "purchased loan", "7" = "preapproval denied", "8" = "preapproval approved but not accepted"),
         purchaser_type = recode(purchaser_type, "0" = "not applicable", "1" = "FannieMae", "2" = "GinnieMae", "3" = "FreddieMac", "4" = "FarmerMac", "5" = "PrivateSecuritizer", "6" = "CommercialBank", "71" = "CreditUnion", "72" = "LifeInsuranceComp", "8" = "AffliateInstitution", "9" = "Other"),
         preapproval = recode(preapproval, "1" = "preapproval requested", "2" = "preapproval not requested"),
         loan_type = recode(loan_type, "1" = "Conventional", "2" = "FHA insured", "3" = "VA guaranteed", "4" = "USDA RHS or FSA guaranteed"),
         loan_purpose = recode(loan_purpose, "1" = "Home purchase", "2" = "Home improvement", "31" = "Refinancing", "32" = "Cash-out refinancing", "4" = "other", "5" = "not applicable"),
         lien_status = recode(lien_status, "1" = "secured by first lien", "2" = "secured by second lien"),
         hoepa_status = recode(hoepa_status, "1" = "high-cost mortgage", "2" = "not a high-cost mortgage", "3" = "not applicable"),
         applicant_race_1 = recode(applicant_race_1, !!!racerecodes),
         applicant_race_2 = recode(applicant_race_2, !!!racerecodes),
         applicant_race_3 = recode(applicant_race_3, !!!racerecodes),
         applicant_race_4 = recode(applicant_race_4, !!!racerecodes),
         applicant_race_5 = recode(applicant_race_5, !!!racerecodes),
         co_applicant_race_1 = recode(co_applicant_race_1, !!!racerecodes),
         co_applicant_race_2 = recode(co_applicant_race_2, !!!racerecodes),
         co_applicant_race_3 = recode(co_applicant_race_3, !!!racerecodes),
         co_applicant_race_4 = recode(co_applicant_race_4, !!!racerecodes),
         co_applicant_race_5 = recode(co_applicant_race_5, !!!racerecodes),
         applicant_sex = recode(applicant_sex, "1" = "Male", "2" = "Female", "3" = "sex not provided by applicant", "4" = "not applicable", "6" = "applicant selected both male and female"),
         co_applicant_sex = recode(co_applicant_sex, "1" = "Male", "2" = "Female", "3" = "sex not provided by applicant", "4" = "not applicable", 
                                   "5" = "no co-applicant", "6" = "applicant selected both male and female"),
         denial_reason_1 = recode(denial_reason_1, !!!denialrecodes),
         denial_reason_2 = recode(denial_reason_2, !!!denialrecodes),
         denial_reason_3 = recode(denial_reason_3, !!!denialrecodes),
         denial_reason_4 = recode(denial_reason_4, !!!denialrecodes),
         applicant_ethnicity_1 = recode(applicant_ethnicity_1, "1" = "Hispanic or Latino", "11" = "Hispanic or Latino", "12" = "Hispanic or Latino", "13" = "Hispanic or Latino", 
                                      "2" = "Not Hispanic or Latino", "3" = "Ethnicity not provided by applicant", "4" = "not applicable"),
         annual_income_000s = income,
         loan_amount_000s = loan_amount) %>%
  dplyr::select(year, id, county_code, census_tract, loan_amount, action_taken, purchaser_type, preapproval, loan_type, loan_purpose, lien_status, hoepa_status,
         applicant_race_1, applicant_race_2, applicant_race_3, applicant_race_4, applicant_race_5, applicant_ethnicity_1, co_applicant_race_1, co_applicant_race_2, co_applicant_race_3,
         co_applicant_race_4, co_applicant_race_5, applicant_sex, co_applicant_sex, denial_reason_1, denial_reason_2, denial_reason_3, denial_reason_4, 
         tract_population, minority_population, median_family_income, tract_owner_occupied_units, tract_one_to_four_family_homes, annual_income_000s, loan_amount_000s)

# Note 1: you will receive two warnings because the default for the recode function
# converts items not listed to NA's, but that is the desired action here, so they can be ignored. 
# Note 2: the id variable is formatted differently between the old and new data, but it should 
# still function as a unique identifier for each mortgage. 

# Binding all the data together! (The cvlpre2018 data frame was created below, but should never need to be created again)
# In the future, run the above code and simply r bind the new data frame to the product below (cvlall)
cvlall <- bind_rows(cvlpost2017, cvlpre2018)

# Writing out the data 
write.csv(cvlall, "data/hmda_cville_individual.csv", row.names = F)

##############################################################################################################################################################################################################################################################################################

# Cleaning process for pre-2018 data ###############################################################################################
# DO NOT REPEAT WITH DATA POST-2017  ###############################################################################################

cvlfips <- c("540", "003", "065", "079", "109", "125") # Data from pre-2018 needs to be filtered
# using the fips codes

cvlpre2018 <- pre2018 %>%
  filter(county_code %in% cvlfips)
unique(cvlpre2018$census_tract_number)
# The census tract numbers and county codes are coded differently pre- and post-2018
# In the data pre-2018, census tracts are indicated by their "tract name" which 
# varies in length by the locality and tract, so below I construct the 
# full 11 digit census tract codes for the 2015 data from the state and county codes and tract name

cvlpre2018$county_code <- paste0(cvlpre2018$state_code, cvlpre2018$county_code)
cvlpre2018$census_tract_number <- sprintf("%.2f", cvlpre2018$census_tract_number) # Using sprintf to convert to character string
# so that R keeps the trailing zeros for some of the census tract numbers 
cvlpre2018$census_tract_number <- gsub("\\.", cvlpre2018$census_tract_number, replacement = "") # Getting rid of 
# periods in some of the census tract numbers 

# Converting the "NA"'s back to actual NA's
cvlpre2018$census_tract_number <- ifelse(cvlpre2018$census_tract_number == "NA", NA, cvlpre2018$census_tract_number)

# Adding a leading zero the Albemarle, Fluvanna, and Greene counties' tract numbers 
cvlpre2018$census_tract_number <- ifelse(!is.na(cvlpre2018$census_tract_number) & (cvlpre2018$county_name == "Albemarle County" 
                                                                                   | cvlpre2018$county_name == "Fluvanna County" | cvlpre2018$county_name == "Greene County"),
                                         paste0("0", cvlpre2018$census_tract_number), cvlpre2018$census_tract_number)
# Adding three leading zeros to the Charlottesville City tract numbers 
cvlpre2018$census_tract_number <- ifelse(!is.na(cvlpre2018$census_tract_number) & cvlpre2018$county_name == "Charlottesville city",
                                         paste0("000", cvlpre2018$census_tract_number), cvlpre2018$census_tract_number)
cvlpre2018$census_tract_number <- ifelse(cvlpre2018$census_tract_number == "0001000" | cvlpre2018$census_tract_number == "000100",
                                         "001000", cvlpre2018$census_tract_number)

# Now creating a census tract column to match the post-2017 data 
cvlpre2018$census_tract <- ifelse(!is.na(cvlpre2018$county_code) & !is.na(cvlpre2018$census_tract_number), 
                                  paste0(cvlpre2018$county_code, cvlpre2018$census_tract_number), NA)

# Checking that all of the 2020 census tracts are in the 2015 data 
unique(cvlpre2018$census_tract[which(cvlpre2018$census_tract %in% cvlpost2017$census_tract == F)])

# Changing column names and values so that they match the post-2017 data, and selecting out the variables of interest

cvlpre2018 <- cvlpre2018 %>%
rename(year = as_of_year,
       id = respondent_id,
       tract_one_to_four_family_homes = number_of_1_to_4_family_units,
       median_family_income = hud_median_family_income,
       tract_owner_occupied_units = number_of_owner_occupied_units,
       tract_population = population) %>%
  mutate(action_taken = recode(action_taken, "1" = "loan originated", "2" = "approved but not accepted", "3" = "denied", "4" = "withdrawn by applicant", "5" = "closed-incomplete", "6" = "purchased loan", "7" = "preapproval denied", "8" = "preapproval approved but not accepted"),
         purchaser_type = recode(purchaser_type, "0" = "not applicable", "1" = "FannieMae", "2" = "GinnieMae", "3" = "FreddieMac", "4" = "FarmerMac", "5" = "PrivateSecuritizer", "6" = "CommercialBank", "7" = "CreditUnion", "8" = "AffliateInstitution", "9" = "Other"),
         preapproval = recode(preapproval, "1" = "preapproval requested", "2" = "preapproval not requested", "3" = "not applicable"),
         loan_type = recode(loan_type, "1" = "Conventional", "2" = "FHA insured", "3" = "VA guaranteed", "4" = "USDA RHS or FSA guaranteed"),
         loan_purpose = recode(loan_purpose, "1" = "Home purchase", "2" = "Home improvement", "3" = "Refinancing"),
         lien_status = recode(lien_status, "1" = "secured by first lien", "2" = "secured by second lien", "3" = "not secured by lien", "4" = "not applicable"),
         hoepa_status = recode(hoepa_status, "1" = "high-cost mortgage", "2" = "not a high-cost mortgage"),
         applicant_race_1 = recode(applicant_race_1, !!!racerecodes),
         applicant_race_2 = recode(applicant_race_2, !!!racerecodes),
         applicant_race_3 = recode(applicant_race_3, !!!racerecodes),
         applicant_race_4 = recode(applicant_race_4, !!!racerecodes),
         applicant_race_5 = recode(applicant_race_5, !!!racerecodes),
         co_applicant_race_1 = recode(co_applicant_race_1, !!!racerecodes),
         co_applicant_race_2 = recode(co_applicant_race_2, !!!racerecodes),
         co_applicant_race_3 = recode(co_applicant_race_3, !!!racerecodes),
         co_applicant_race_4 = recode(co_applicant_race_4, !!!racerecodes),
         co_applicant_race_5 = recode(co_applicant_race_5, !!!racerecodes),
         applicant_sex = recode(applicant_sex, "1" = "Male", "2" = "Female", "3" = "sex not provided by applicant", "4" = "not applicable"),
         co_applicant_sex = recode(co_applicant_sex, "1" = "Male", "2" = "Female", "3" = "sex not provided by applicant", "4" = "not applicable", "5" = "no co-applicant"),
         denial_reason_1 = recode(denial_reason_1, !!!denialrecodes),
         denial_reason_2 = recode(denial_reason_2, !!!denialrecodes),
         denial_reason_3 = recode(denial_reason_3, !!!denialrecodes),
         applicant_ethnicity_1 = recode(applicant_ethnicity, "1" = "Hispanic or Latino", "2" = "Not Hispanic or Latino", "3" = "Ethnicity not provided by applicant", "4" = "not applicable"),
         annual_income_000s = applicant_income_000s) %>%
  dplyr::select(year, id, county_code, census_tract, action_taken, purchaser_type, preapproval, loan_type, loan_purpose, lien_status, hoepa_status,
                applicant_race_1, applicant_race_2, applicant_race_3, applicant_race_4, applicant_race_5, applicant_ethnicity_1, co_applicant_race_1, co_applicant_race_2, co_applicant_race_3,
                co_applicant_race_4, co_applicant_race_5, applicant_sex, co_applicant_sex, denial_reason_1, denial_reason_2, denial_reason_3, 
                tract_population, minority_population, median_family_income, tract_owner_occupied_units, tract_one_to_four_family_homes, annual_income_000s, loan_amount_000s)

# Have to re-format a couple more variables to make them consistent with the old data before combining
cvlpre2018$county_code <- as.numeric(cvlpre2018$county_code)
cvlpre2018$census_tract <- as.numeric(cvlpre2018$census_tract)
