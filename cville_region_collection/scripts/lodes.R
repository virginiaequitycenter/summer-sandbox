

# Author: Lee LeBoeuf
# Last updated: 06/24/2021

# This script uses the LODES function to pull LODES data for the Charlottesville and Eastern Shore areas and creates seperate datafiles with the following variables:
# Total number of jobs in each census block
# Numbers of high, mid, and low wage jobs in each census block
# Proportions of high, mid, and low wage jobs in each census block (relative to total number of jobs)
# Number of jobs in each census block disaggregated by racial identity of the worker
# Number of jobs in each census block disaggregated by the educational attainment of the worker

# Areas included in these data:
# (1) Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)
# (2) The Eastern Shore area: Accomack (51001), Northampton (51131)

# Loading required packages
# devtools::install_git("https://github.com/hrbrmstr/lodes.git")
# invisible(lapply(list('tidyverse', 'lodes'),
#                  function(pkg) library(pkg, character.only = TRUE)))

# LODES data is available for Virginia for the years 2002 to 2018. 
# In this script, I focus on data from 2018, though the process could be repeated for any year in that range by simply changing the year in the read_lodes argument. 
# However, LEHD does not recommend using LODES data for longitudinal analysis.
# Data including Race, Ethnicity, Education, and Sex are only available for the years 2009-2018 in the RAC and WAC files. 

# Reading in the Virginia LODES data for the year 2018
va_lodes_wac <- read_lodes(state = "VA", type = "workplace", year = 2018,
                           segment = "S000", job_type = "JT00")
# va_lodes_wac should contain 53,924 observations of 53 variables. 

# The type = "workplace" calls the Workplace Area Characteristics (WAC) data file which includes the number 
# of jobs in a census block dissaggregated by work and home census blocks, age groupings (<=29, 30-54, >55), 
# wage-earnings (low, mid, and high), sector, race, and education attainment

##### (1) Charlottesville Area #####------------------------------------------------------------------------------------------------------------------------------------------------------

# These are state number (51) + county code for the Charlottesville Area regions of interest
cvlfips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# Now, selecting out observations for Charlottesville region:
cvl_lodes_wac <- va_lodes_wac %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5)) %>% 
  filter(w_county %in% cvlfips)
# cvl_lodes_wac should contain 2,028 obs. of 54 variables 

# Creating/re-naming variables for the following:
# Number of low-, mid-, and high-wage jobs in each block (Low-wage: <= $1250/month, mid-wage: $1251/month-$3333/month, high-wage: >$3333/month)
# Proportions of each of those wage groups relative to all the jobs in the block
# Number of jobs for different racial groups in each block (racial groups: White, Black, American Indian/Alaskan Native, Asian, Native Hawaiian/PI, and Two or more race groups)
# Number of jobs for different educational attainment levels (<high school, high school, some college/associates degree, Bachelor's/advanced degree)

cvl_jobs <- cvl_lodes_wac %>% 
  rename(lowwage = CE01,
            midwage = CE02,
            hiwage = CE03,
            alljobs = C000) %>%
  mutate(lowwage_p = lowwage/alljobs,
         midwage_p = midwage/alljobs,
         higwage_p = hiwage/alljobs) %>%
  rename(White_workers = CR01,
            Black_workers = CR02,
            AI.Na_workers = CR03,
            Asian_workers = CR04,
            NaH.PI_workers = CR05,
            Multi.racial_workers = CR07) %>%
  rename(lessThanHS = CD01,
            HSnoCollege = CD02,
            SomeColl.Associates = CD03,
            Bach.AdvDeg = CD04) %>% 
  select(w_geocode, lowwage, midwage, hiwage, alljobs, lowwage_p, midwage_p, higwage_p, White_workers, Black_workers, AI.Na_workers, Asian_workers, NaH.PI_workers,
         Multi.racial_workers, lessThanHS, HSnoCollege, SomeColl.Associates, Bach.AdvDeg, w_county)

# Creating a variable of county names for ease of interpretation
cvl_jobs$countyName <- ifelse(cvl_jobs$w_county == "51540", "Charlottesville",
                              ifelse(cvl_jobs$w_county == "51003", "Albemarle",
                                     ifelse(cvl_jobs$w_county == "51065", "Fluvanna",
                                            ifelse(cvl_jobs$w_county == "51079", "Greene",
                                                   ifelse(cvl_jobs$w_county == "51125", "Nelson",
                                                          ifelse(cvl_jobs$w_county == "51109", "Louisa", NA))))))

# Writing out the datafile:
write.csv(cvl_jobs, "lodes_employment_cville_block.csv", row.names = F)

##### (2) Eastern Shore #####------------------------------------------------------------------------------------------------------------------------------------------------------
# The Eastern Shore area: Accomack (51001), Northampton (51131)
# These are state number (51) + county code for the Eastern Shore regions of interest
eastfips <- c("51001", "51131")

# Now, selecting out observations for Charlottesville region:
east_lodes_wac <- va_lodes_wac %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5)) %>% 
  filter(w_county %in% eastfips)
# cvl_lodes_wac should contain 2,028 obs. of 54 variables 

# Creating/re-naming variables for the following:
# Number of low-, mid-, and high-wage jobs in each block (Low-wage: <= $1250/month, mid-wage: $1251/month-$3333/month, high-wage: >$3333/month)
# Proportions of each of those wage groups relative to all the jobs in the block
# Number of jobs for different racial groups in each block (racial groups: White, Black, American Indian/Alaskan Native, Asian, Native Hawaiian/PI, and Two or more race groups)
# Number of jobs for different educational attainment levels (<high school, high school, some college/associates degree, Bachelor's/advanced degree)

east_jobs <- east_lodes_wac %>% 
  rename(lowwage = CE01,
         midwage = CE02,
         hiwage = CE03,
         alljobs = C000) %>%
  mutate(lowwage_p = lowwage/alljobs,
         midwage_p = midwage/alljobs,
         higwage_p = hiwage/alljobs) %>%
  rename(White_workers = CR01,
         Black_workers = CR02,
         AI.Na_workers = CR03,
         Asian_workers = CR04,
         NaH.PI_workers = CR05,
         Multi.racial_workers = CR07) %>%
  rename(lessThanHS = CD01,
         HSnoCollege = CD02,
         SomeColl.Associates = CD03,
         Bach.AdvDeg = CD04) %>% 
  select(w_geocode, lowwage, midwage, hiwage, alljobs, lowwage_p, midwage_p, higwage_p, White_workers, Black_workers, AI.Na_workers, Asian_workers, NaH.PI_workers,
         Multi.racial_workers, lessThanHS, HSnoCollege, SomeColl.Associates, Bach.AdvDeg, w_county)

# Creating a variable of county names for ease of interpretation
east_jobs$countyName <- ifelse(east_jobs$w_county == "51001", "Accomack",
                              ifelse(east_jobs$w_county == "51131", "Northampton", NA))

# Writing out the datafile:
write.csv(east_jobs, "lodes_employment_eastern_block.csv", row.names = F)






