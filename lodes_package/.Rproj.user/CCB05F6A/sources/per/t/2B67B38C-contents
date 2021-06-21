library(tidyverse)

# .................................................
# https://github.com/hrbrmstr/lodes
# devtools::install_git("https://github.com/hrbrmstr/lodes.git")
library(lodes)

cvlfips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# codebook: https://lehd.ces.census.gov/data/lodes/LODES7/LODESTechDoc7.5.pdf

# Arguments
# year: 2002-2018
# type: od (origin-destination), residence (aka rac), workplace (aka wac)
# job_type: JT00=All Jobs, JT01=Primary Jobs, JT02=All Private Jobs, JT03=Private Primary Jobs, JT04=All Federal Jobs, JT05=Federal Primary Jobs 
# NOTE: not entirely sure what the job_type distinctions mean...

# segment (od): main=jobs with both workplace and residence in the state, aux=jobs with the workplace in the state and the residence outside of the state
# segment (residence, workplace): S000=total jobs, SA01=29 or younger jobs, SA02=30-54 jobs, SA03=55+ jobs, 
# segment (residence, workplace): SE01=1250-/month jobs, SE02=1251-3333/month jobs, SE03=3333+/month jobs, 
# segment (residence, workplace): SI01=goods producing industry jobs, SI02=trade/transportation/utilities jobs, SI03=other services jobs

# od ----
va_lodes_od <- read_lodes(state = "VA", type = "od", year = 2018, 
                          segment = "main", job_type = "JT00")

# Workers who work or live in Cville region
cvl_lodes_od <- va_lodes_od %>% 
  mutate(w_county = str_sub(w_geocode, 1, 5),
         h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(w_county %in% cvlfips | h_county %in% cvlfips)

summary(cvl_lodes_od) 

# Number/proportion of folks who work in Cville but live in other localities
cvl_lodes_od %>% 
  filter(w_county == "51540") %>% 
  group_by(h_county) %>% 
  summarize(jobs = sum(S000)) %>% 
  ungroup() %>% 
  mutate(alljobs = sum(jobs),
         prop = jobs/alljobs) %>% 
  arrange(desc(jobs)) %>% 
  View()

# It looks like we can look at this by age groups, wage groups, and crude industry


# residence ----
va_lodes_rac <- read_lodes(state = "VA", type = "residence", year = 2018,
                           segment = "S000", job_type = "JT00")

cvl_lodes_rac <- va_lodes_rac %>% 
  mutate(h_county = str_sub(h_geocode, 1,5)) %>% 
  filter(h_county %in% cvlfips)

# Resident workers in wage groups by county 
# (i.e., where do higher/lower wage workers in the region live?)
cvl_lodes_rac %>% 
  group_by(h_county) %>% 
  summarize(lowwage = sum(CE01),
            midwage = sum(CE02),
            hiwage = sum(CE03),
            allwage = sum(C000)) %>% 
  mutate(lowwage_p = lowwage/allwage,
         midwage_p = midwage/allwage,
         higwage_p = hiwage/allwage)

# it looks like we can look at this (and workplace) by granular industry, race of worker, plus
# age groups, wage groups


# workplace ----
va_lodes_wac <- read_lodes(state = "VA", type = "workplace", year = 2018,
                           segment = "S000", job_type = "JT00")

cvl_lodes_wac <- va_lodes_wac %>% 
  mutate(w_county = str_sub(w_geocode, 1,5)) %>% 
  filter(w_county %in% cvlfips)

# Jobs in wage groups by locality
# (i.e., where are the higher/lower paying jobs located?)
cvl_lodes_wac %>% 
  group_by(w_county) %>% 
  summarize(lowwage = sum(CE01),
            midwage = sum(CE02),
            hiwage = sum(CE03),
            allwage = sum(C000)) %>% 
  mutate(lowwage_p = lowwage/allwage,
         midwage_p = midwage/allwage,
         higwage_p = hiwage/allwage)

  
# And we can look at these thing by block group (or aggregate to census tract)
# Or pull in different years (as in LODES.R) for over time trends...

# .................................................
# https://github.com/jamgreen/lehdr
# devtools::install_github("jamgreen/lehdr")
library(lehdr)
# not yet sure if this provides any advantages over lodes...
