
# Author: Lee LeBoeuf
# Last updated: 08/25/2021 

# This script cleans manually downloaded data from the Consumer Financial Protection Bureau and 
# Federal Financial Institutions Examinations Council 
# All data were collected as part of the Home Mortgage Disclosure Act (HMDA). 
# The HMDA requires many financial institutions to maintain, report, and publicly disclose information
# about mortgages. 
# New data is released annually (typically in the summer months -- 2020 data was released in June of 2021). 
# Starting in 2018, the data collection process mandated by the HMDA changed and expanded. 
# Some of the variable names changed slightly from pre-2018 to 2018 and after, so 
# the majority of this script focuses on re-naming variables such that they are consistent across time,
# summarizing variables to the census tract level, and merging data sets. The 
# process can be repeated by downloaded data from any other combination of years, and 
# changing the year names of the data frames/re-using cleaning code where applicable. 

# The first half of the script contains the cleaning process for any new data, the bottom half shows the process for all
# pre-2018 data. The code for pre-2018 data should need need to be run again, unless users are interested
# in including more variables. In that case, each of the datafiles for 2007-2018 will need to be re-downloaded.

# Data from 2017 and earlier can be downloaded via the Consumer Financial Protection Bureau
# here: https://www.consumerfinance.gov/data-research/hmda/historic-data/?geo=va&records=all-records&field_descriptions=labels
# Data from 2018 and onward can be downloaded via the Federal Financial Institutions Examinations Council 
# here: https://ffiec.cfpb.gov/data-browser/data/2018?category=states&items=VA 

# Areas included in these data:
# Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# Loading required packages
invisible(lapply(list('tidyverse','psych','stringr', 'DescTools'),
                 function(pkg) library(pkg, character.only = TRUE)))

# Creating necessary filtering strings
cvlfips <- c("540", "003", "065", "079", "109", "125") # Data from pre-2018 needs to be filtered
# using the fips codes
cvlcounties <- c("51540", "51003", "51065", "51079", "51109", "51125") # Data post-2018 needs
# to be filtered using the full county codes 

#####################################################################################################################################
# Reading in the data and rbinding multiple years worth of data. 
# The process in this box should never need to be repeated. When new data post-2020 is released, it can be run through the same
# data cleaning process below for the post-2018 data, and then rbinded to the dataframe containing all 2004-2020 data. 

# Reading in the 2017 and earlier data (This is for the entire state of VA for years 2007-2017, it will be a huge datafile that will
# need to be filtered to just Cville region and re-formatted slightly)
# filedr <- setwd("/Users/Carlson/Desktop/HMDA data/Pre-2017")
# file_names2017 <- dir(filedr)
# pre2018 <- do.call(rbind, lapply(file_names2017, read.csv))
# # Filtering the pre-2017 data for just the Charlottesville region
# # The pre-2018 data is missing leading zeros in the county/FIPs codes 
# # which impacts Albemarle, Fluvanna, and Greene counties
# # Below I input the leading zeros so that the data can be properly filtered by the fips codes
# pre2018$county_code <- ifelse(pre2018$county_code == 3, paste0("00", 3), pre2018$county_code)
# pre2018$county_code <- ifelse(pre2018$county_code == 65 | pre2018$county_code == 79, paste0("0", pre2018$county_code), pre2018$county_code)
# 
# # Reading in the post 2017 data--also for the entire state, for the years 2018-2020
# filedr <- setwd("/Users/Carlson/Desktop/HMDA data/Post-2017")
# file_names_post2017 <- dir(filedr)
# post2017 <- do.call(rbind, lapply(file_names_post2017, read.csv))
####################################################################################################################################

# Cleaning process for post-2017 data ###############################################################################################
# This process can be repeated with single year datafiles from post-2020. The cleaned version can then be rbinded to the 
# datafile containing years 2004-2020, simply change the year names on the dataframes below. 

cvlpost2017 <- post2017 %>%
  filter(county_code %in% cvlcounties)

# The naming conventions of certain variables changed slightly starting
# in 2018 such that periods were used where underscores had been used in previous years
# This line replaces the periods in the column names in the post-2017 data with underscores so 
# that it matches the pre-2018 data. 
colnames(cvlpost2017) <- str_replace_all(colnames(cvlpost2017), "(?<=\\w)\\.", "_")

cvlpost2017$rate_spread <- as.numeric(cvlpost2017$rate_spread)

cvlnewtract <- cvlpost2017 %>%
  rename(year = activity_year) %>%
  group_by(census_tract, year) %>%
  summarise(conventional = sum(loan_type == 1),
            fha_insured = sum(loan_type == 2),
            va_guaranteed = sum(loan_type == 3),
            usda_guaranteed = sum(loan_type == 4),
            home_purchase = sum(loan_purpose == 1),
            home_improve = sum(loan_purpose == 2),
            refinance = sum(loan_purpose == 31),
            cash_out_refi = sum(loan_purpose == 32),
            other_purpose = sum(loan_purpose == 4),
            purposeNA = sum(loan_purpose == 5),
            req_preapproval = sum(preapproval == 1),
            noreq_preapproval = sum(preapproval == 2),
            originated_loan = sum(action_taken == 1),
            approvedApp_notAccepted = sum(action_taken == 2),
            app_denied = sum(action_taken == 3),
            app_withdrawn = sum(action_taken == 4),
            fileclosed_incomplete = sum(action_taken == 5),
            purchased_loan = sum(action_taken == 6),
            denied_preapproval = sum(action_taken == 7),
            approve_preapproval = sum(action_taken == 8),
            appRace_AIAN = sum(applicant_race_1 == 1),
            appRace_Asian = sum(applicant_race_1 == 2 | applicant_race_1 == 21 | applicant_race_1 == 22 | 
                                  applicant_race_1 == 23 | applicant_race_1 == 24 | applicant_race_1 == 25 |
                                  applicant_race_1 == 26 | applicant_race_1 == 27),
            appRace_Black = sum(applicant_race_1 == 3),
            appRace_HawPI = sum(applicant_race_1 == 4 | applicant_race_1 == 41 | applicant_race_1 == 42 |
                                  applicant_race_1 == 43 | applicant_race_1 == 44),
            appRace_White = sum(applicant_race_1 == 5),
            appRace_missing = sum(applicant_race_1 == 6),
            appRace_NA = sum(applicant_race_1 == 7),
            appRace_multiracial = sum(!is.na(applicant_race_1) & !is.na(applicant_race_2 | applicant_race_3 
                                                                        | applicant_race_4 | applicant_race_5)),
            appEth_HispLat = sum(applicant_ethnicity_1 == 1 | applicant_ethnicity_1 == 14),
            appMale = sum(applicant_sex == 1),
            appFemale = sum(applicant_sex == 2),
            appsex_missing = sum(applicant_sex == 3),
            avg_rateSpread = mean(na.omit(rate_spread) * 10),
            highCost_mortgages = sum(hoepa_status == 1),
            nonHighCost_mortgages = sum(hoepa_status == 2),
            firstlien_secured = sum(lien_status == 1),
            sublien_secured = sum(lien_status == 2),
            avg_app_income = mean(na.omit(income * 1000)),
            median_app_income = median(na.omit(income * 1000)),
            avg_loan_amount = mean(na.omit(loan_amount)),
            med_loan_amount = median(na.omit(loan_amount)),
            white_denial_rate = sum(action_taken == 3 & applicant_race_1 == 5) / sum(applicant_race_1 == 5),
            black_denial_rate = sum(action_taken == 3 & applicant_race_1 == 3) / sum(applicant_race_1 == 3),
            hislat_denial_rate = (sum(action_taken == 3 & (applicant_ethnicity_1 == 1 | applicant_ethnicity_1 == 14))) / (sum(applicant_ethnicity_1 == 1) + sum(applicant_ethnicity_1 == 14)),
            median_income_accepted_app = median(na.omit(income[which(action_taken == 1)] * 1000)),
            loans_per_units = sum(action_taken == 1) / tract_one_to_four_family_homes,
            perc_conventional = sum(loan_type == 1 & action_taken == 1) / sum(action_taken == 1) * 100,
            perc_govern_backed = sum(action_taken == 1 & (loan_type == 2 | loan_type == 3 | loan_type == 4)) / sum(action_taken == 1) * 100,
            sum_mortgage_dollars_in000s = sum(loan_amount[which(action_taken == 1)]),
            avg_homepurchase_loanamount = mean(na.omit(loan_amount[which(loan_purpose == 1)])),
            med_homepurchase_loanamount = median(na.omit(loan_amount[which(loan_purpose == 1)])),
            total_apps = length(loan_purpose),
            perc_app_missingRace = sum(applicant_race_1 == 6) / total_apps * 100,
            overall_denial_rate = sum(action_taken == 3) / total_apps,
            perc_white_apps = sum(applicant_race_1 == 5) / total_apps * 100,
            perc_black_apps = sum(applicant_race_1 == 3) / total_apps * 100,
            perc_hislat_apps = sum(applicant_ethnicity_1 == 1) + sum(applicant_ethnicity_1 == 14) / total_apps * 100)

cvlnewtract <- cvlnewtract %>%
  distinct()

popdatnew <- cvlpost2017 %>%
  rename(year = activity_year,
         population = tract_population,
         minority_population = tract_minority_population_percent,
         median_family_income = ffiec_msa_md_median_family_income,
         tract_to_msamd_income = tract_to_msa_income_percentage) %>%
  select(population, minority_population, census_tract, year, median_family_income, tract_to_msamd_income,
         tract_owner_occupied_units, tract_one_to_four_family_homes) %>%
  distinct()

cvlnewtract <- left_join(cvlnewtract, popdatnew, by = c("census_tract", "year"), keep = FALSE)
cvlnewtract <- cvlnewtract[-which(is.na(cvlnewtract$census_tract)),]

# Cleaning process for pre-2018 data ###############################################################################################
# DO NOT REPEAT WITH DATA POST-2018  ###############################################################################################
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

# Summarizing data to the census tract level--------------------------------------------------------------

cvloldtract <- cvlpre2018 %>%
  rename(year = as_of_year) %>%
  group_by(census_tract, year) %>%
  summarise(conventional = sum(loan_type == 1),
            fha_insured = sum(loan_type == 2),
            va_guaranteed = sum(loan_type == 3),
            usda_guaranteed = sum(loan_type == 4),
            home_purchase = sum(loan_purpose == 1),
            home_improve = sum(loan_purpose == 2),
            refinance = sum(loan_purpose == 31),
            cash_out_refi = sum(loan_purpose == 32),
            other_purpose = sum(loan_purpose == 4),
            purposeNA = sum(loan_purpose == 5),
            req_preapproval = sum(preapproval == 1),
            noreq_preapproval = sum(preapproval == 2),
            originated_loan = sum(action_taken == 1),
            approvedApp_notAccepted = sum(action_taken == 2),
            app_denied = sum(action_taken == 3),
            app_withdrawn = sum(action_taken == 4),
            fileclosed_incomplete = sum(action_taken == 5),
            purchased_loan = sum(action_taken == 6),
            denied_preapproval = sum(action_taken == 7),
            approve_preapproval = sum(action_taken == 8),
            appRace_AIAN = sum(applicant_race_1 == 1),
            appRace_Asian = sum(applicant_race_1 == 2 | applicant_race_1 == 21 | applicant_race_1 == 22 | 
                                  applicant_race_1 == 23 | applicant_race_1 == 24 | applicant_race_1 == 25 |
                                  applicant_race_1 == 26 | applicant_race_1 == 27),
            appRace_Black = sum(applicant_race_1 == 3),
            appRace_HawPI = sum(applicant_race_1 == 4 | applicant_race_1 == 41 | applicant_race_1 == 42 |
                                  applicant_race_1 == 43 | applicant_race_1 == 44),
            appRace_White = sum(applicant_race_1 == 5),
            appRace_missing = sum(applicant_race_1 == 6),
            appRace_NA = sum(applicant_race_1 == 7),
            appRace_multiracial = sum(!is.na(applicant_race_1) & !is.na(applicant_race_2 | applicant_race_3 
                                                                        | applicant_race_4 | applicant_race_5)),
            appEth_HispLat = sum(applicant_ethnicity == 1),
            appMale = sum(applicant_sex == 1),
            appFemale = sum(applicant_sex == 2),
            appsex_missing = sum(applicant_sex == 3),
            avg_rateSpread = mean(na.omit(rate_spread)),
            highCost_mortgages = sum(hoepa_status == 1),
            nonHighCost_mortgages = sum(hoepa_status == 2),
            firstlien_secured = sum(lien_status == 1),
            sublien_secured = sum(lien_status == 2),
            avg_loan_amount = mean(na.omit(loan_amount_000s * 1000)),
            med_loan_amount = median(na.omit(loan_amount_000s)),
            avg_app_income = mean(na.omit(applicant_income_000s * 1000)),
            median_app_income = median(na.omit(applicant_income_000s * 1000)),
            median_income_accepted_app = median(na.omit(applicant_income_000s[which(action_taken == 1)] * 1000)),
            white_denial_rate = sum(action_taken == 3 & applicant_race_1 == 5) / sum(applicant_race_1 == 5),
            black_denial_rate = sum(action_taken == 3 & applicant_race_1 == 3) / sum(applicant_race_1 == 3),
            hislat_denial_rate = (sum(action_taken == 3 & (applicant_ethnicity == 1))) / sum(applicant_ethnicity == 1),
            loans_per_units = sum(action_taken == 1) / sum(number_of_1_to_4_family_units),
            perc_conventional = sum(loan_type == 1 & action_taken == 1) / sum(action_taken == 1) * 100,
            perc_govern_backed = sum(action_taken == 1 & (loan_type == 2 | loan_type == 3 | loan_type == 4)) / sum(action_taken == 1) * 100,
            sum_mortgage_dollars_in000s = sum(loan_amount_000s[which(action_taken == 1)]),
            avg_homepurchase_loanamount = mean(na.omit(loan_amount_000s[which(loan_purpose == 1)])),
            med_homepurchase_loanamount = median(na.omit(loan_amount_000s[which(loan_purpose == 1)])),
            total_apps = length(loan_purpose),
            perc_app_missingRace = sum(applicant_race_1 == 6) / total_apps * 100,
            overall_denial_rate = sum(action_taken == 3) / total_apps,
            perc_white_apps = sum(applicant_race_1 == 5) / total_apps * 100,
            perc_black_apps = sum(applicant_race_1 == 3) / total_apps * 100,
            perc_hislat_apps = sum(applicant_ethnicity == 1) / total_apps * 100) 

cvloldtract <- cvloldtract %>%
  distinct()

popdat <- cvlpre2018 %>%
  rename(year = as_of_year,
         median_family_income = hud_median_family_income,
         tract_owner_occupied_units = number_of_owner_occupied_units,
         tract_one_to_four_family_homes = number_of_1_to_4_family_units) %>%
  select(population, minority_population, census_tract, year, median_family_income, tract_to_msamd_income,
         tract_owner_occupied_units, tract_one_to_four_family_homes) %>%
  distinct()

cvloldtract <- left_join(cvloldtract, popdat, by = c("census_tract", "year"), keep = FALSE)
cvloldtract <- cvloldtract[-which(is.na(cvloldtract$census_tract)),]


cvlnewtract$census_tract <- as.character(cvlnewtract$census_tract)
cvlall <- rbind(cvloldtract, cvlnewtract)

# Writing out tract-level data for 2007-2020
write.csv(cvlall, "hmda_cville_tract.csv", row.names = F)




