# Low-Income Energy Affordability 2018/Energy Burden
# 6/25/21
# Marisa Lemma

library(tidyverse)
# Calling in data from source
leadurl <- "https://data.openei.org/files/573/VA-2018-LEAD-data.zip"
download.file(url = leadurl,
              destfile = paste(getwd(), "/", "va2018lead.zip", sep = ""))
unzip("va2018lead.zip", exdir = paste(getwd(), "/data/", sep = ""))

ami_census_tracts <- read_csv("data/VA AMI Census Tracts 2018.csv")


# Filter by area
eastern_shore = filter(ami_census_tracts, (FIP%in%51001090100:51001990200 | 
                                             FIP%in%51131930100:51131990100) & HINCP!="NA")

# Calculate average energy burden for each census tract
energy_burden <- eastern_shore %>% 
  group_by(FIP) %>% 
  summarize(totalinc = sum(HINCP*UNITS, na.rm = TRUE),
            totalelep = sum(ELEP*UNITS, na.rm = TRUE),
            totalgas = sum(GASP*UNITS, na.rm = TRUE),
            totalother = sum(FULP*UNITS, na.rm = TRUE)) %>% 
  mutate(averageburden = ((totalelep+totalgas+totalother)/totalinc)*100)

# Calculate number of energy burdened households in each census tract
hh_energy_burden <- eastern_shore %>%
  group_by(FIP) %>% 
  mutate(hh_energy_exp = ELEP+GASP+FULP) %>% 
  mutate(hh_burden = (hh_energy_exp/HINCP)*100)

hh_energy_burden <- hh_energy_burden %>% 
  mutate(burden = ifelse(hh_burden>=6 & hh_burden<=10, "high", 
                         ifelse(hh_burden>10 & hh_burden<=20, "very high", 
                                ifelse(hh_burden>20, "extremely high", no="low"))))

hh_energy_burden <- hh_energy_burden %>%
  mutate(high = ifelse(burden=="high", (UNITS), no=0),
         veryhigh = ifelse(burden=="very high", (UNITS), no=0),
         extremelyhigh = ifelse(burden=="extremely high", (UNITS), no=0),
         low = ifelse(burden=="low", (UNITS), no=0))

householdburden <- hh_energy_burden %>% 
  group_by(FIP) %>%   
  summarize(highburden = sum(high, na.rm = TRUE),
            veryhighburden = sum(veryhigh, na.rm = TRUE),
            extremelyhighburden = sum(extremelyhigh, na.rm = TRUE),
            lowburden = sum(low, na.rm = TRUE),
            totalunits = sum(high+veryhigh+extremelyhigh+low, na.rm = TRUE))

# Calculate number of households in each AMI group
# and number of burdened households in each AMI group
burden_by_ami <- hh_energy_burden %>% 
  group_by(FIP) %>% 
  mutate(total_0_30 = ifelse(AMI68=="0-30%", (high+veryhigh+extremelyhigh+low), no=0),
         total_30_60 = ifelse(AMI68=="30-60%", (high+veryhigh+extremelyhigh+low), no=0),
         total_60_80 = ifelse(AMI68=="60-80%", (high+veryhigh+extremelyhigh+low), no=0),
         total_80_100 = ifelse(AMI68=="80-100%", (high+veryhigh+extremelyhigh+low), no=0),
         total_over_100 = ifelse(AMI68=="100%+", (high+veryhigh+extremelyhigh+low), no=0),
         burdened_0_30 = ifelse(AMI68=="0-30%", (high+veryhigh+extremelyhigh), no=0),
         burdened_30_60 = ifelse(AMI68=="30-60%", (high+veryhigh+extremelyhigh), no=0),
         burdened_60_80 = ifelse(AMI68=="60-80%", (high+veryhigh+extremelyhigh), no=0),
         burdened_80_100 = ifelse(AMI68=="80-100%", (high+veryhigh+extremelyhigh), no=0),
         burdened_over_100 = ifelse(AMI68=="100%+", (high+veryhigh+extremelyhigh), no=0),) 
burden_by_ami <- burden_by_ami %>%  
  summarize(total_0_30 = sum(total_0_30, na.rm = TRUE),
            total_30_60 = sum(total_30_60, na.rm = TRUE),
            total_60_80 = sum(total_60_80, na.rm = TRUE),
            total_80_100 = sum(total_80_100, na.rm = TRUE),
            total_over_100 = sum(total_over_100, na.rm = TRUE),
            burdened_0_30 = sum(burdened_0_30, na.rm = TRUE),
            burdened_30_60 = sum(burdened_30_60, na.rm = TRUE),
            burdened_60_80 = sum(burdened_60_80, na.rm = TRUE),
            burdened_80_100 = sum(burdened_80_100, na.rm = TRUE),
            burdened_over_100 = sum(burdened_over_100, na.rm = TRUE))

# Calculate percent of households in each AMI group that are energy burdened
# (e.g. 100 in the 0_30 category means that 100% of households 
# in the 0-30% AMI range are energy burdened)
burden_by_ami <- burden_by_ami %>% 
  mutate(percent_0_30 = (burdened_0_30/total_0_30)*100,
         percent_30_60 = (burdened_30_60/total_30_60)*100,
         percent_60_80 = (burdened_60_80/total_60_80)*100,
         percent_80_100 = (burdened_80_100/total_80_100)*100,
         percent_over_100 = (burdened_over_100/total_over_100)*100)

# Calculate number of owners and renters who are energy burdened
burden_by_ten <- hh_energy_burden %>% 
  group_by(FIP) %>% 
  mutate(total_owners = ifelse(TEN=="OWNER", (UNITS), no=0),
         total_renters = ifelse(TEN=="RENTER", (UNITS), no=0),
         burdened_owners = ifelse(TEN=="OWNER", (high+veryhigh+extremelyhigh), no=0),
         burdened_renters = ifelse(TEN=="RENTER", (high+veryhigh+extremelyhigh), no=0))
burden_by_ten <- burden_by_ten %>% 
  summarize(total_owners = sum(total_owners, na.rm = TRUE),
            total_renters = sum(total_renters, na.rm = TRUE),
            burdened_owners = sum(burdened_owners, na.rm = TRUE),
            burdened_renters = sum(burdened_renters, na.rm = TRUE))

# Calculate percentage of owners and renters in each tract that are energy burdened
burden_by_ten <- burden_by_ten %>% 
  mutate(percent_burdened_owners = (burdened_owners/total_owners)*100,
         percent_burdened_renters = (burdened_renters/total_renters)*100)

# Join the average energy burden with the household burden
burden <- inner_join(energy_burden, householdburden)
burden <- inner_join(burden, burden_by_ami)
burden <- inner_join(burden, burden_by_ten)

# Number of households with high energy burden or above
burden <- burden %>% 
  mutate(numberburdened = highburden +
           veryhighburden +
           extremelyhighburden)

# Percentage of households that are energy burdened
burden <- burden %>% 
  mutate(percentburdened = (numberburdened/totalunits)*100)

# Add column for county name
burden <- burden %>% 
  mutate(county = ifelse(FIP%in%51001090100:51001990200, "Accomack", no="Northampton"))

# Create new variable, `tract`, that is labeled with tract numbers
burden <- burden %>% 
  mutate(tract = ifelse(FIP==51001090100, 901,
                  ifelse(FIP==51001090200, 902,
                  ifelse(FIP==51001090300, 903,
                  ifelse(FIP==51001090400, 904,
                  ifelse(FIP==51001090500, 905,
                  ifelse(FIP==51001090600, 906,
                  ifelse(FIP==51001090700, 907,
                  ifelse(FIP==51001090800, 908,
                  ifelse(FIP==51001980100, 9801,
                  ifelse(FIP==51001980200, 9802,
                  ifelse(FIP==51001990100, 9901,
                  ifelse(FIP==51001990200, 9902,
                  ifelse(FIP==51131930100, 9301,
                  ifelse(FIP==51131930200, 9302,
                  ifelse(FIP==51131930300, 9303,
                  no=9901))))))))))))))))
# Note: Census tracts 9801, 9802, 9901, and 9902 (from Accomack County) are not in this dataset

# Change the order of the columns so that they make more sense
burden <- burden %>%
  select(FIP, county, tract, totalinc, totalelep, totalgas, totalother,
         averageburden, lowburden, highburden, veryhighburden,
         extremelyhighburden, totalunits, numberburdened,
         percentburdened, everything())

# Export to CSV
write.csv(burden, "/Users/marisalemma/Desktop/Equity Center/summer-sandbox/eastern_shore_collection/data//lead_easternshore_tract.csv", row.names = F)
