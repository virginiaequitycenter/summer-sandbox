# Author: Lee LeBoeuf
# Last updated: 10/07/2021

# This script uses the CDC Places APA to download health problem and prevention prevalence data for the greater Eastern Shore
# Data are available at the tract level for the years 2017 and 2018. There are a number of health-related measures included, 
# and each measures has a % prevalence estimation associated with it. For example, "Current asthma among adults aged >=18 years". Each measure
# has 3 values: data_value (the prevalence estimate as a percent), low_confidence_limit and high_confidence_limit (the upper
# and lower limits of the percent estimate). The percent estimates come from a multilevel statistical modeling framework used 
# by the CDC (more information can be found here: https://www.cdc.gov/places/about/index.html). In the cleaning process, I just
# select the actaul estimate and exclude the lower and higher limits. 

invisible(lapply(list('tidyverse','jsonlite', 'stringr'),
                 function(pkg) library(pkg, character.only = TRUE)))

source(paste0("https://raw.githubusercontent.com/jacob-gg/manager/main/R/loch_missingness_monster.R"))

# The Eastern Shore: Accomack (51001) and Northampton (51131)

# Downloading data ----------------------------------------------------------------------------------------------------------------
# For some weird reason, when downloadind data for the entire state of VA at once, you end up with fewer observations for 
# each county than if you download data for each county at a time. (Or at least that's how it worked for the Charlottesville region). 
# So, below I download data for each county individually and then combine it into one data frame. 

accomack <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51001")
northampton <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51131")

eastdat <- bind_rows(accomack, northampton)

# Droppping unecessary columns

eastdat$geolocation <- NULL

eastdat <- eastdat %>%
  dplyr::select(year, countyname, countyfips, locationname, datasource, 
                category, measure, data_value, totalpopulation, short_question_text)

loch_missingness_monster(eastdat) # There is no missing data in the long format version of the data frame, but not every
# tract has every measure (i.e., missing data doesn't really show up in the current format because if a tract doesn't have data 
# for a certain measure, there just isn't a row for that measure for that tract)

# Getting rid of the spaces in the short answer version of the health measure
eastdat$short_question_text <- str_replace_all(eastdat$short_question_text, " ", "_")

# Data for, "High_Blood_Pressure","Cholesterol_Screening", "Taking_BP_Medication", and  "High_Cholesterol" all came form 2017
# Everything else is from 2018
# We will lose this information when re-formating to wide, so I'm adding it to the end of character strings that will
# become the column names
eastdat$short_question_text <- ifelse(eastdat$short_question_text == "High_Blood_Pressure" | eastdat$short_question_text == "Cholesterol_Screening" |
                                       eastdat$short_question_text == "Taking_BP_Medication" | eastdat$short_question_text == "High_Cholesterol",
                                     paste0(eastdat$short_question_text, 2017), paste0(eastdat$short_question_text, 2018))

unique(eastdat$short_question_text) # Checking that the above line worked

# Two more measure needs to be renamed before the data frame is converted to wide format
eastdat$short_question_text <- ifelse(eastdat$short_question_text == "Cancer_(except_skin)2018", "Cancer_except_skin2018", eastdat$short_question_text)
eastdat$short_question_text <- ifelse(eastdat$short_question_text == "Sleep_<7_hours2018", "less_than_sevenhr_sleep2018", eastdat$short_question_text)

# Converting to wide format such that there is a column for each health/behavior measures
eastdatwide <- pivot_wider(eastdat, id_cols = c(locationname, countyname, totalpopulation),
                          names_from = short_question_text,
                          values_from = data_value)

# Selecting columns we want (subject to change)
eastdatwidef <- eastdatwide %>%
  dplyr::select(locationname, countyname, totalpopulation,
                Coronary_Heart_Disease2018, Binge_Drinking2018, Mental_Health2018, High_Blood_Pressure2017,
                Physical_Inactivity2018, Diabetes2018, Current_Smoking2018, Cancer_except_skin2018, Current_Asthma2018,
                Dental_Visit2018, High_Cholesterol2017, COPD2018, Obesity2018, Physical_Health2018, Health_Insurance2018,
                less_than_sevenhr_sleep2018, Annual_Checkup2018)


## Writing out tract level summaries 
write.csv(eastdatwidef, "cdcplaces_eastern_tract.csv", row.names = F)
