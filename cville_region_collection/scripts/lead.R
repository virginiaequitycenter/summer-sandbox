# Get Low-Income Energy Affordability 2018/Energy Burden
# Marisa Lemma, Michele Claibourn
# Created: 2021-06-25
# Last updated: 2021-11-15


library(tidyverse)


# Import data from source ----
# if (!dir.exists("dataraw")) {dir.create("dataraw")}
# 
# url <- "https://data.openei.org/files/573/VA-2018-LEAD-data.zip"
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "va2018lead.zip", sep = ""))
# unzip("dataraw/va2018lead.zip", exdir = paste(getwd(), "/dataraw/va2018lead/", sep = ""))


# Read in data ----
ami_census_tracts <- read_csv("dataraw/va2018lead/VA AMI Census Tracts 2018.csv")


# Define area of interest ----
# Cville area
localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
# localfips <- c("001", "131")


# Data preparation ----
# Filter by area
cville_ami_tracts <- ami_census_tracts %>% 
  mutate(state = str_sub(FIP, 1, 2),
         county = str_sub(FIP, 3,5),
         tract = str_sub(FIP, 6, 11)) %>% 
  filter(state == "51" & county %in% localfips) %>% 
  filter(HINCP != "NA")

# # Calculate average energy burden for each census tract
energy_burden <- cville_ami_tracts %>%
  group_by(FIP) %>%
  summarize(totalinc = sum(HINCP*UNITS),
         totalelep = sum(ELEP*UNITS, na.rm = TRUE),
         totalgas = sum(GASP*UNITS, na.rm = TRUE),
         totalother = sum(FULP*UNITS, na.rm = TRUE)) %>%
  mutate(averageburden = ((totalelep+totalgas+totalother)/totalinc)*100)

# Calculate number of energy burdened households in each census tract
hh_energy_burden <- cville_ami_tracts %>%
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

# Average yearly household energy expenditure
avg_household_exp <- hh_energy_burden %>%
  group_by(FIP) %>% 
  summarize(avg_hh_exp = mean(hh_energy_exp, na.rm = TRUE))

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
burden <- inner_join(burden, avg_household_exp)

# Number of households with high energy burden or above
burden <- burden %>% 
  mutate(numberburdened = highburden +
                          veryhighburden +
                          extremelyhighburden)

# Percentage of households that are energy burdened
burden <- burden %>% 
  mutate(percentburdened = (numberburdened/totalunits)*100)

# Add column for county fips and name
burden <- burden %>% 
  mutate(state_fip = str_sub(FIP, 1, 2),
         county_fip = str_sub(FIP, 3,5),
         tract_fip = str_sub(FIP, 6, 11)) %>% 
  mutate(county = case_when(
    county_fip == "540" ~ "Charlottesville city",
    county_fip == "003" ~ "Albemarle",
    county_fip == "065" ~ "Fluvanna",
    county_fip == "079" ~ "Greene",
    county_fip == "109" ~ "Louisa",
    county_fip == "125" ~ "Nelson"
  ))

# Change the order of the columns so that they make more sense
burden <- burden %>%
  select(FIP, state_fip, county_fip, tract_fip, county, totalinc, totalelep, totalgas, totalother,
         averageburden, avg_hh_exp, lowburden, highburden, veryhighburden,
         extremelyhighburden, totalunits, numberburdened,
         percentburdened, everything())


# Save data ----
# Cville area
write_csv(burden, "data/lead_cville_tract.csv")
# Eastern shore area
# write_csv(burden, "data/lead_eastern_tract.csv")

