# Get CDC Places 2020 Data
# Author: Lee LeBoeuf
# Last updated: 10/07/2021

# This script uses the CDC Places APA to download health problem and prevention prevalence data for the greater Charlottesville area
# Data are available at the tract level. The 2020 release uses 2018 BRFSS data for most (23) measures and 2017 BRFSS data for 4 measures 
# (high blood pressure, taking high blood pressure medication, high cholesterol, and cholesterol screening); 
# each measure has a % prevalence estimation associated with it. For example, "Current asthma among adults aged >=18 years". 
# Each measure has 3 values: data_value (the prevalence estimate as a percent), low_confidence_limit and high_confidence_limit 
# (the upper and lower limits of the percent estimate). The percent estimates come from a multilevel statistical modeling framework 
# used  by the CDC (more information can be found here: https://www.cdc.gov/places/about/index.html). In the cleaning process, I 
# select the actaul estimate and exclude the lower and higher limits. 

invisible(lapply(list('tidyverse','jsonlite', 'stringr'),
                 function(pkg) library(pkg, character.only = TRUE)))

source(paste0("https://raw.githubusercontent.com/jacob-gg/manager/main/R/loch_missingness_monster.R"))

# The larger Charlottesville area: Charlottesville City (51540), Albemarle County (51003), 
# Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# Downloading data ----------------------------------------------------------------------------------------------------------------
# I tried downloading data for the entire state at once by using both https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?stateabbr=VA
# AND https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?statedesc=Virginia and both work, but weirdly, the resulting dataframes have only
# 1,000 observations compared to the 1,397 observations I got when I downloaded data for each county individually and combined the data frames. 
# After some digging, I realized that the discrepancy is due to the fact that when you download data for the entire state of VA, whether by using 
# the full state name or just the abbreviation, it is missing data entirely for the counties 51540, 51065, 51079, 51109, and 51125
# (i.e., if you download the state data to a data frame named "vadat" and combine all the county data to a data frame named "cvldat" and then run the 
# following: unique(cvldat$countyfips[which(cvldat$countyfips %in% vadat1$countyfips == F)]), you get the county fips listed above). 
# SO, below I download the data for each county individually and then combine them. 

# The socrata API defaults to a limit of 1000 rows per page; it doesn't look like this can be changed in the call
# e.g., adding '$limit=10000' to the query returns null
# https://support.socrata.com/hc/en-us/articles/202949268-How-to-query-more-than-1000-rows-of-a-dataset

cvillecity <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51540")
albemarle <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51003")
fluvanna <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51065")
greene <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51079")
louisa <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51109")
nelson <- fromJSON("https://chronicdata.cdc.gov/resource/cwsq-ngmh.json?countyfips=51125")

cvldat <- bind_rows(cvillecity, albemarle, fluvanna, greene, louisa, nelson)

# Dropping unecessary columns

cvldat$geolocation <- NULL

cvldat <- cvldat %>%
  dplyr::select(year, countyname, countyfips, locationname, datasource, 
         category, measure, data_value, totalpopulation, short_question_text)

loch_missingness_monster(cvldat) # There is no missing data in the long format version of the data frame, but not every
# tract has every measure (i.e., missing data doesn't really show up in the current format because if a tract doesn't have data 
# for a certain measure, there just isn't a row for that measure for that tract)

# Getting rid of the spaces in the short answer version of the health measure
cvldat$short_question_text <- str_replace_all(cvldat$short_question_text, " ", "_")

# Data for, "High_Blood_Pressure","Cholesterol_Screening", "Taking_BP_Medication", and  "High_Cholesterol" all came form 2017
# Everything else is from 2018
# We will lose this information when re-formating to wide, so I'm adding it to the end of character strings that will
# become the column names
cvldat$short_question_text <- ifelse(cvldat$short_question_text == "High_Blood_Pressure" | cvldat$short_question_text == "Cholesterol_Screening" |
                                       cvldat$short_question_text == "Taking_BP_Medication" | cvldat$short_question_text == "High_Cholesterol",
                                     paste0(cvldat$short_question_text, 2017), paste0(cvldat$short_question_text, 2018))

unique(cvldat$short_question_text) # Checking that the above line worked

# Two more measure needs to be renamed before the data frame is converted to wide format
cvldat$short_question_text <- ifelse(cvldat$short_question_text == "Cancer_(except_skin)2018", "Cancer_except_skin2018", cvldat$short_question_text)
cvldat$short_question_text <- ifelse(cvldat$short_question_text == "Sleep_<7_hours2018", "less_than_sevenhr_sleep2018", cvldat$short_question_text)

# Converting to wide format such that there is a column for each health/behavior measures
cvldatwide <- pivot_wider(cvldat, id_cols = c(locationname, countyname, totalpopulation),
                          names_from = short_question_text,
                          values_from = data_value)

View(cvldat[which(cvldat$locationname == 51003010903),]) # This is the only tract I see with NA's in the wide format

# Selecting columns we want (subject to change)
cvldatwidef <- cvldatwide %>%
  dplyr::select(locationname, countyname, totalpopulation,
                Coronary_Heart_Disease2018, Binge_Drinking2018, Mental_Health2018, High_Blood_Pressure2017,
                Physical_Inactivity2018, Diabetes2018, Current_Smoking2018, Cancer_except_skin2018, Current_Asthma2018,
                Dental_Visit2018, High_Cholesterol2017, COPD2018, Obesity2018, Physical_Health2018, Health_Insurance2018,
                less_than_sevenhr_sleep2018, Annual_Checkup2018)


## Writing out tract level summaries 
write.csv(cvldatwidef, "data/cdcplaces_cville_tract.csv", row.names = F)


