# Get LEHD Origin-Destination data
# Regional Employment Data
# Author: Lee LeBoeuf
# Last updated: 07/01/2021

# This script uses the LODES function to pull LODES data for the Charlottesville area, and creates separate 
# data files with the following variables (1 for census blocks, 1 for census block groups, and 1 for census tracts):
# Total number of jobs in each su (spatial unit)
# Numbers of high, mid, and low wage jobs in each su
# Proportions of high, mid, and low wage jobs in each su (relative to total number of jobs)
# Number of jobs in each su by racial identity of the worker
# Number of jobs in each su by the educational attainment of the worker
# Ratio of workers to residents in each su

# Areas included in these data:
# Charlottesville City (51540), Albemarle County (51003), Fluvanna (51065), Greene (51079), Louisa (51109), Nelson (51125)

# Loading required packages
# ndevtools::install_git("https://github.com/hrbrmstr/lodes.git")
invisible(lapply(list('tidyverse', 'lodes'),
                 function(pkg) library(pkg, character.only = TRUE)))

# LODES data is available for Virginia for the years 2002 to 2018. 
# In this script, I focus on data from 2018, though the process could be repeated for any year in that range by simply changing the year in the read_lodes argument. 
# LEHD does not recommend using LODES data for longitudinal analysis.
# Data including Race, Ethnicity, Education, and Sex are only available for the years 2009-2018 in the RAC and WAC files. 

# Reading in the Virginia LODES data for the year 2018
# Will need both the wac and rac data files in order to calculate the ratio of workers to residents in each su

# wac
va_lodes_wac <- read_lodes(state = "VA", type = "workplace", year = 2018,
                           segment = "S000", job_type = "JT00")
# The type = "workplace" calls the Workplace Area Characteristics (WAC) data file which includes the number 
# of jobs in a census block dissaggregated by work and home census blocks, age groupings (<=29, 30-54, >55), 
# wage-earnings (low, mid, and high), sector, race, and education attainment

# rac
va_lodes_rac <- read_lodes(state = "VA", type = "residence", year = 2018,
                           segment = "S000", job_type = "JT00")
# The type = "residence" calls the Residence Area Characteristics (RAC) data file which is in the same structure
# as the RAC data, but represents the number of residence instead of workers. 

##### Charlottesville Area #####------------------------------------------------------------------------------------------------------------------------------------------------------

# These are state number (51) + county code for the Charlottesville Area regions of interest
cvlfips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# Selecting out observations for Charlottesville region:
cvl_lodes_wac <- va_lodes_wac %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         w_tract = str_sub(w_geocode, 1, 11),
         w_blkgroup = str_sub(w_geocode, 1, 12)) %>% 
  filter(w_county %in% cvlfips)

# Will only need the total number of residents in each su, and the w_geocode from the RAC
cvl_lodes_rac <- va_lodes_rac %>% 
  mutate(w_county = str_sub(h_geocode, 1, 5)) %>% 
  filter(w_county %in% cvlfips) %>%
  rename(w_geocode = h_geocode,
         residents = C000) %>%
  select(w_geocode, residents)

# Binding the # of residents in each w_geocode (from the RAC data) to the wac data
cvl_lodes <- merge(cvl_lodes_wac, cvl_lodes_rac, by = "w_geocode", all.x = T)

# Creating/re-naming variables for the following:
# Number of low-, mid-, and high-wage jobs in each su (Low-wage: <= $1250/month, mid-wage: $1251/month-$3333/month, high-wage: >$3333/month)
# Proportions of each of those wage groups relative to all the jobs in the su
# Number of jobs for different racial groups in each su (racial groups: White, Black, American Indian/Alaskan Native, Asian, Native Hawaiian/PI, and Two or more race groups)
# Number of jobs for different educational attainment levels in each su (<high school, high school, some college/associates degree, Bachelor's/advanced degree)

# 1. BLOCKS
cvl_jobs_blocks <- cvl_lodes %>% 
  rename(lowwage_jobs = CE01,
         midwage_jobs = CE02,
         higwage_jobs = CE03,
         alljobs = C000) %>%
  mutate(lowwage_p = lowwage_jobs/alljobs,
         midwage_p = midwage_jobs/alljobs,
         higwage_p = higwage_jobs/alljobs,
         resid_jobs_ratio = residents/alljobs) %>%
  rename(White_workers = CR01,
         Black_workers = CR02,
         AI_Na_workers = CR03,
         Asian_workers = CR04,
         NaH_PI_workers = CR05,
         Multiracial_workers = CR07,
         lessThanHS_jobs = CD01,
         HSnoCollege_jobs = CD02,
         SomeColl_Associates_jobs = CD03,
         Bach_AdvDeg_jobs = CD04) %>% 
  select(w_geocode, lowwage_jobs, midwage_jobs, higwage_jobs, alljobs, lowwage_p, midwage_p, higwage_p, 
         White_workers, Black_workers, AI_Na_workers, Asian_workers, NaH_PI_workers,Multiracial_workers, 
         lessThanHS_jobs, HSnoCollege_jobs, SomeColl_Associates_jobs, Bach_AdvDeg_jobs, residents, resid_jobs_ratio,
         w_county, w_tract, w_blkgroup)

# Creating a variable of county names for ease of interpretation
cvl_jobs_blocks$countyName <- ifelse(cvl_jobs_blocks$w_county == "51540", "Charlottesville",
                                     ifelse(cvl_jobs_blocks$w_county == "51003", "Albemarle",
                                            ifelse(cvl_jobs_blocks$w_county == "51065", "Fluvanna",
                                                   ifelse(cvl_jobs_blocks$w_county == "51079", "Greene",
                                                          ifelse(cvl_jobs_blocks$w_county == "51125", "Nelson",
                                                                 ifelse(cvl_jobs_blocks$w_county == "51109", "Louisa", NA))))))

# Creating a categorical variable for block type
cvl_jobs_blocks$blocktype <- ifelse(cvl_jobs_blocks$resid_jobs_ratio <= 0.05, "employment",
                                    ifelse(cvl_jobs_blocks$resid_jobs_ratio >= 2.0, "residential", 
                                           ifelse(cvl_jobs_blocks$resid_jobs_ratio > 0.05 & cvl_jobs_blocks$resid_jobs_ratio < 2.0, "mixed", cvl_jobs_blocks$resid_jobs_ratio)))

# Note: some of the census blocks are missing from the RAC datafile, so there are missing values for residents and 
# the residents to jobs ratios. For this reason, those variables are not included in the block group and tracts files

#### Writing out the block version ####
write.csv(cvl_jobs_blocks, "data/lodes_employment_cville_block.csv", row.names = F)

# 2. BLOCK GROUPS
cvl_jobs_blkgr <- cvl_lodes %>% 
  group_by(w_blkgroup) %>%
  summarize(lowwage_jobs = sum(CE01),
         midwage_jobs = sum(CE02),
         higwage_jobs = sum(CE03),
         alljobs = sum(C000),
         White_workers = sum(CR01),
         Black_workers = sum(CR02),
         AI_Na_workers = sum(CR03),
         Asian_workers = sum(CR04),
         NaH_PI_workers = sum(CR05),
         Multiracial_workers = sum(CR07),
         lessThanHS_jobs = sum(CD01),
         HSnoCollege_jobs = sum(CD02),
         SomeColl_Associates_jobs = sum(CD03),
         Bach_AdvDeg_jobs = sum(CD04)) %>%
  mutate(lowwage_p = lowwage_jobs/alljobs,
         midwage_p = midwage_jobs/alljobs,
         higwage_p = higwage_jobs/alljobs) %>%
  select(w_blkgroup, lowwage_jobs, midwage_jobs, higwage_jobs, alljobs, lowwage_p, midwage_p, higwage_p, 
         White_workers, Black_workers, AI_Na_workers, Asian_workers, NaH_PI_workers,Multiracial_workers, 
         lessThanHS_jobs, HSnoCollege_jobs, SomeColl_Associates_jobs, Bach_AdvDeg_jobs)

# Re-adding a county code column 
cvl_jobs_blkgr <- cvl_jobs_blkgr %>% 
  mutate(w_county = str_sub(w_blkgroup, 1, 5))

# Creating a variable of county names for ease of interpretation
cvl_jobs_blkgr$countyName <- ifelse(cvl_jobs_blkgr$w_county == "51540", "Charlottesville",
                                    ifelse(cvl_jobs_blkgr$w_county == "51003", "Albemarle",
                                           ifelse(cvl_jobs_blkgr$w_county == "51065", "Fluvanna",
                                                  ifelse(cvl_jobs_blkgr$w_county == "51079", "Greene",
                                                         ifelse(cvl_jobs_blkgr$w_county == "51125", "Nelson",
                                                                ifelse(cvl_jobs_blkgr$w_county == "51109", "Louisa", NA))))))

#### Writing out the block group version ####
write.csv(cvl_jobs_blkgr, "data/lodes_employment_cville_blkgr.csv", row.names = F)


# 3. TRACTS
# Aggregating the census blocks to census tracts:
cvl_jobs_tract <- cvl_lodes %>% 
  group_by(w_tract) %>%
  summarize(lowwage_jobs = sum(CE01),
            midwage_jobs = sum(CE02),
            higwage_jobs = sum(CE03),
            alljobs = sum(C000),
            White_workers = sum(CR01),
            Black_workers = sum(CR02),
            AI_Na_workers = sum(CR03),
            Asian_workers = sum(CR04),
            NaH_PI_workers = sum(CR05),
            Multiracial_workers = sum(CR07),
            lessThanHS_jobs = sum(CD01),
            HSnoCollege_jobs = sum(CD02),
            SomeColl_Associates_jobs = sum(CD03),
            Bach_AdvDeg_jobs = sum(CD04)) %>%
  mutate(lowwage_p = lowwage_jobs/alljobs,
         midwage_p = midwage_jobs/alljobs,
         higwage_p = higwage_jobs/alljobs) %>%
  select(w_tract, lowwage_jobs, midwage_jobs, higwage_jobs, alljobs, lowwage_p, midwage_p, higwage_p, 
         White_workers, Black_workers, AI_Na_workers, Asian_workers, NaH_PI_workers,Multiracial_workers, 
         lessThanHS_jobs, HSnoCollege_jobs, SomeColl_Associates_jobs, Bach_AdvDeg_jobs)

# Re-adding a county code column 
cvl_jobs_tract <- cvl_jobs_tract %>% 
  mutate(w_county = str_sub(w_tract, 1, 5)) 

# Creating a variable of county names for ease of interpretation
cvl_jobs_tract$countyName <- ifelse(cvl_jobs_tract$w_county == "51540", "Charlottesville",
                                     ifelse(cvl_jobs_tract$w_county == "51003", "Albemarle",
                                            ifelse(cvl_jobs_tract$w_county == "51065", "Fluvanna",
                                                   ifelse(cvl_jobs_tract$w_county == "51079", "Greene",
                                                          ifelse(cvl_jobs_tract$w_county == "51125", "Nelson",
                                                                 ifelse(cvl_jobs_tract$w_county == "51109", "Louisa", NA))))))

#### Writing out the tract version ####
write.csv(cvl_jobs_tract, "data/lodes_employment_cville_tracts.csv", row.names = F)


