
# Author: Lee LeBoeuf
# Last updated: 10/11/2021 

# This script reads in already cleaned individual-level data for mortages and mortgage applications in the Eastern shore region, 
# and uses it to create tract-level summaries of mortage activity in the area. 
# Tract level summaries include variables like the raw counts of different loan types (conventional, FHA insured, etc.), 
# the counts of loan applications from different racial groups, the average and median loan amounts and applicant income 
# (however, these variables seem highly unreliable and prone to entry errors--many of the values seem impossible. 
# The FFEIC allows banks to submit data with an error threshold as high as 10% for community banks and credit unions 
# and it seems like annual income and loan amount are particularly prone to error due to misplaced decimals or accidental extra 0's).
# This script also calculates the denial rates for different racial groups and the number of applications from 
# each racial group. 
# Currently, the data file contains data from the years 2007 - 2020. When new data are released (which typically happens in the 
# summer on a year and a half delay), use the "hmda_eastern_individual.R" script to clean the new data and bind it with the old 
# data file. From there, the code below can be used to re-create tract level summaries for the full data set. 
# Any variables can also be added or deleted from the summarise function below if they are no longer of interest. 

# Reading in the individual-level data (created by the "hmda_cville_individual.R" script and available on the Equity Center github)
eastdat <- read.csv("hmda_eastern_individual.csv")

# Applicants' racial identities are listed in multiple columns. In order to indentify an applicant as 
# multiracial, we would need to identify those who have multiple racial identities listed. 
# This line creates a variable to code applicants as multiracial by counting the number of 
# applicants who had selected multiple racial identities (i.e., not just White, Black, etc.)
eastdat$multiracial <- ifelse(!is.na(eastdat$applicant_race_1) & !is.na(eastdat$applicant_race_2) | !is.na(eastdat$applicant_race_3) 
                             | !is.na(eastdat$applicant_race_4) | !is.na(eastdat$applicant_race_5), 1, 0)
# Creating tract level summaries
eastdattract <- eastdat %>%
  group_by(census_tract, year) %>%
  summarise(conventional = sum(loan_type == "Conventional"),
            fha_insured = sum(loan_type == "FHA insured"),
            va_guaranteed = sum(loan_type == "VA guranteed"),
            usda_guaranteed = sum(loan_type == "USDA RHS or FSA guaranteed"),
            home_purchase = sum(loan_purpose == "Home purchase"),
            home_improve = sum(loan_purpose == "Home improvement"),
            refinance = sum(loan_purpose == "Refinancing"),
            cash_out_refi = sum(loan_purpose == "Cash-out refinancing"),
            other_purpose = sum(loan_purpose == "other"),
            purposeNA = sum(loan_purpose == "not applicable"),
            req_preapproval = sum(preapproval == "preapproval requested"),
            noreq_preapproval = sum(preapproval == "preapproval not requested"),
            originated_loans = sum(action_taken == "loan originated"),
            approvedApp_notAccepted = sum(action_taken == "approved but not accepted"),
            app_denied = sum(action_taken == "denied"),
            app_withdrawn = sum(action_taken == "withdrawn by applicant"),
            fileclosed_incomplete = sum(action_taken == "closed-incomplete"),
            purchased_loan = sum(action_taken == "purchased loan"),
            denied_preapproval = sum(action_taken == "preapproval denied"),
            approve_preapproval = sum(action_taken == "preapproval approved but not accepted"),
            appRace_AIAN = sum(applicant_race_1 == "American Indian or Alaska Native"),
            appRace_Asian = sum(applicant_race_1 == "Asian"),
            appRace_Black = sum(applicant_race_1 == "Black"),
            appRace_HawPI = sum(applicant_race_1 == "Native Hawaiian or PI"),
            appRace_White = sum(applicant_race_1 == "White"),
            appRace_multiracial = sum(multiracial == 1),
            appRace_missing = sum(is.na(applicant_race_1)),
            appRace_NA = sum(applicant_race_1 == "not applicable"),
            appEth_HispLat = sum(applicant_ethnicity_1 == "Hispanic or Latino"),
            appMale = sum(applicant_sex == "Male"),
            appFemale = sum(applicant_sex == "Female"),
            appsex_missing = sum(applicant_sex == "sex not provided by applicant"),
            highCost_mortgages = sum(hoepa_status == "high-cost mortgage"),
            nonHighCost_mortgages = sum(hoepa_status == "not a high-cost mortgage"),
            firstlien_secured = sum(lien_status == "secured by first lien"),
            sublien_secured = sum(lien_status == "secured by second lien"),
            avg_income_000s = mean(na.omit(annual_income_000s)),
            median_income_000s = median(na.omit(annual_income_000s)),
            avg_loan_amount_000s = mean(na.omit(loan_amount_000s)),
            med_loan_amount_000s = median(na.omit(loan_amount_000s)),
            white_denial_rate = sum(action_taken == "denied" & applicant_race_1 == "White") / sum(applicant_race_1 == "White"),
            black_denial_rate = sum(action_taken == "denied" & applicant_race_1 == "Black") / sum(applicant_race_1 == "Black"),
            hislat_denial_rate = (sum(action_taken == "denied" & (applicant_ethnicity_1 == "Hispanic or Latino"))) / (sum(applicant_ethnicity_1 == "Hispanic or Latino")),
            median_income_accepted_app = median(na.omit(annual_income_000s[which(action_taken == "loan originated")])),
            loans_per_units = sum(action_taken == "loan originated") / tract_one_to_four_family_homes,
            perc_conventional = sum(loan_type == "Conventional" & action_taken == "loan originated") / sum(action_taken == "loan originated") * 100,
            perc_govern_backed = sum(action_taken == "loan originated" & (loan_type == "FHA insured" | loan_type == "VA guaranteed" | loan_type == "USDA RHS or FSA guaranteed")) / sum(action_taken == "loan originated") * 100,
            sum_mortgage_dollars_in000s = sum(loan_amount_000s[which(action_taken == "loan originated")]),
            avg_homepurchase_loanamount = mean(na.omit(loan_amount_000s[which(loan_purpose == "Home purchase")])),
            med_homepurchase_loanamount = median(na.omit(loan_amount_000s[which(loan_purpose == "Home purchase")])),
            total_apps = length(loan_purpose),
            perc_app_missingRace = sum(applicant_race_1 == "Race not provided by applicant") / total_apps * 100,
            overall_denial_rate = sum(action_taken == "denied") / total_apps,
            perc_white_apps = sum(applicant_race_1 == "White") / total_apps * 100,
            perc_black_apps = sum(applicant_race_1 == "Black") / total_apps * 100,
            perc_hislat_apps = sum(applicant_ethnicity_1 == "Hispanic or Latino") / total_apps * 100)

eastdattract <- eastdattract %>%
  distinct()

# Re-adding population data 
popdat <- eastdat %>%
  dplyr::select(year, census_tract, tract_population, minority_population, median_family_income, tract_owner_occupied_units, tract_one_to_four_family_homes) %>%
  distinct()
easttractf <- left_join(eastdattract, popdat, by = c("census_tract", "year"), keep = FALSE)

# Removing those with missing census tracts
easttractf <- easttractf[-which(is.na(easttractf$census_tract)),]

# Writing out tract-level data for 2007-2020
write.csv(easttractf, "hmda_eastern_tract.csv", row.names = F)

