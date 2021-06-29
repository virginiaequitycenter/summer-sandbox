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
cville_area = filter(ami_census_tracts, (FIP%in%51540000201:51540001000 | 
                                        FIP%in%51003010100:51003011400 | 
                                        FIP%in%51065020101:51065020300 | 
                                        FIP%in%51079030101:51079030200 | 
                                        FIP%in%51109950100:51109950500 | 
                                        FIP%in%51125950100:51125950300) & HINCP!="NA")

# Calculate average energy burden for each census tract
energy_burden <- cville_area %>% 
  group_by(FIP) %>% 
  summarize(totalinc = sum(HINCP*UNITS),
         totalelep = sum(ELEP*UNITS, na.rm = TRUE),
         totalgas = sum(GASP*UNITS, na.rm = TRUE),
         totalother = sum(FULP*UNITS, na.rm = TRUE)) %>% 
  mutate(averageburden = ((totalelep+totalgas+totalother)/totalinc)*100)

# Calculate number of energy burdened households in each census tract
hh_energy_burden <- cville_area %>%
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
  mutate(county = ifelse(FIP%in%51540000201:51540001000, "Charlottesville city",
                  ifelse(FIP%in%51003010100:51003011400, "Albemarle",
                  ifelse(FIP%in%51065020101:51065020300, "Fluvanna",
                  ifelse(FIP%in%51079030101:51079030200, "Greene",
                  ifelse(FIP%in%51109950100:51109950500, "Louisa",
                  no = "Nelson"))))))

# Generate new variable, `tract`, that is labeled with tract numbers
burden <- burden %>%
  mutate(tract = ifelse(FIP==51540000201, 2.01,
                  ifelse(FIP==51540000202, 2.02,
                  ifelse(FIP==51540000302, 3.02,
                  ifelse(FIP==51540000401, 4.01,
                  ifelse(FIP==51540000402, 4.02,
                  ifelse(FIP==51540000501, 5.01,
                  ifelse(FIP==51540000502, 5.02,
                  ifelse(FIP==51540000600, 6,
                  ifelse(FIP==51540000700, 7,
                  ifelse(FIP==51540000800, 8,
                  ifelse(FIP==51540000900, 9,
                  ifelse(FIP==51540001000, 10,
                  ifelse(FIP==51003010100, 101,
                  ifelse(FIP==51003010201, 102.01,
                  ifelse(FIP==51003010202, 102.02,
                  ifelse(FIP==51003010300, 103,
                  ifelse(FIP==51003010401, 104.01,
                  ifelse(FIP==51003010402, 104.02,
                  ifelse(FIP==51003010500, 105,
                  ifelse(FIP==51003010601, 106.01,
                  ifelse(FIP==51003010602, 106.02,
                  ifelse(FIP==51003010700, 107,
                  ifelse(FIP==51003010800, 108,
                  ifelse(FIP==51003010901, 109.01,
                  ifelse(FIP==51003010902, 109.02,
                  ifelse(FIP==51003010903, 109.03,
                  ifelse(FIP==51003011000, 110,
                  ifelse(FIP==51003011100, 111,
                  ifelse(FIP==51003011201, 112.01,
                  ifelse(FIP==51003011202, 112.02,
                  ifelse(FIP==51003011301, 113.01,
                  ifelse(FIP==51003011302, 113.02,
                  ifelse(FIP==51003011303, 113.03,
                  ifelse(FIP==51003011400, 114,
                  ifelse(FIP==51065020101, 201.01,
                  ifelse(FIP==51065020102, 201.02,
                  ifelse(FIP==51065020200, 202,
                  ifelse(FIP==51065020300, 203,
                  ifelse(FIP==51079030101, 301.01,
                  ifelse(FIP==51079030102, 301.02,
                  ifelse(FIP==51079030200, 302,
                  ifelse(FIP==51109950100, 9501,
                  ifelse(FIP==51109950201, 9502.01,
                  ifelse(FIP==51109950202, 9502.02,
                  ifelse(FIP==51109950300, 9503,
                  ifelse(FIP==51109950400, 9504,
                  ifelse(FIP==51109950500, 9505,
                  ifelse(FIP==51125950100, 9501,
                  ifelse(FIP==51125950200, 9502,
                  no=9503))))))))))))))))))))))))))))))))))))))))))))))))))

# Change the order of the columns so that they make more sense
burden <- burden %>%
  select(FIP, county, tract, totalinc, totalelep, totalgas, totalother,
         averageburden, lowburden, highburden, veryhighburden,
         extremelyhighburden, totalunits, numberburdened,
         percentburdened, everything())

# Export to CSV
write.csv(burden, "/Users/marisalemma/Desktop/Equity Center/summer-sandbox/cville_region_collection/data//lead_cville_tract.csv", row.names = F)
