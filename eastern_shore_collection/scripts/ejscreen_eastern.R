# Get EJSCREEN data
# Marisa Lemma and Lee LeBoeuf
# Last updated: 2021-11-19

library(tidyverse)
library(stats)

# # Import data from source ----
# url <- "https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_USPR.csv.zip"
# download.file(url = url,
#               destfile = paste(getwd(), "/dataraw/", "ejscreen.zip", sep = ""),
#               mode = "wb")
# 
# # unzip and read
# unzip("dataraw/ejscreen.zip", exdir = paste(getwd(), "/dataraw/ejscreen/", sep = ""))


# Read in data ----
ejscreen <- read_csv("dataraw/ejscreen/EJSCREEN_2020_USPR.csv")

# Define localities of interest
# Cville area
# localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
localfips <- c("001", "131")


# Reduce----
# Get rid of demographic variables (since we don't need those)
ejscreen_clean <- ejscreen %>%
  select(-c("OBJECTID", "ACSTOTPOP":"PRE1960", "VULEOPCT":"DISPEO", "D_LDPNT_2":"D_PM25_2",
            "ST_ABBREV":"P_OVR64PCT", "P_VULEOPCT", "P_LDPNT_D2":"T_VULEOPCT",
            "T_LDPNT_D2", "T_DSLPM_D2", "T_CANCR_D2", "T_RESP_D2", "T_PTRAF_D2", "T_PWDIS_D2",
            "T_PNPL_D2", "T_PRMP_D2", "T_PTSDF_D2", "T_OZONE_D2", "T_PM25_D2", "Shape_Length", 
            "Shape_Area"))

# Filter to areas of interest
eastern_area <- ejscreen_clean %>%
  mutate(statefips = str_sub(ID, 1,2),
         countyfips = str_sub(ID, 3,5),
         tractfips = str_sub(ID, 6, 11),
         blkgpfips = str_sub(ID, 12,12)) %>% 
  filter(statefips == "51" & countyfips %in% localfips) %>% 
  select(ID, statefips, countyfips, tractfips, blkgpfips, everything(),
         -c(STATE_NAME, NPL_CNT, TSDF_CNT, AREALAND, AREAWATER))


# Export to .csv
write.csv(eastern_area, "data/ejscreen_eastern_blkgps.csv", row.names = F)

# Aggregating to tract level
# PM2.5 (PM25), ozone (OZONE), and NATA indicators (cancer risk [CANCER], respiratory hazard index [RESP], and diesel particulate matter [DSLPM]) 
# are measured at the census tract level, and the same value is assigned to each block group 
# within that tract. So for those variables, the values are the same. 
# For proximity variables, I copied the EJSCREEN procedure for aggregating their original 
# proximity estimates based on blocks to block groups: taking a population-weighted average 
# (this requires reading in the population estimates for the block groups)
# For the remaining variables, I took the average across the block groups. 

# reading in the population data
pop <- read_csv("acs_eastern_tract.csv") %>%
  mutate(geoid = as.character(GEOID)) %>%
  select(geoid, totalpopE)

# reading in block group data
blkgr <- read_csv("../data/ejscreen_eastern_blkgps.csv")

blkgr <- blkgr %>%
  mutate(geoid = as.character(ID)) %>%
  left_join(pop)

blkgr$tract <- paste0(blkgr$statefips, blkgr$countyfips, blkgr$tractfips)

east_tract <- blkgr %>%
  group_by(tract) %>%
  summarise(PM25 = PM25,
            OZONE = OZONE,
            CANCER = CANCER,
            RESP = RESP,
            DSLPM = DSLPM,
            PTRAF = weighted.mean(PTRAF, totalpopE, na.rm = T),
            Avg_PRE1960PCT = mean(PRE1960PCT),
            PWDIS = weighted.mean(PWDIS, totalpopE, na.rm = T),
            PNPL = weighted.mean(PNPL, totalpopE, na.rm = T),
            PRMP = weighted.mean(PRMP, totalpopE, na.rm = T),
            PTSDF = weighted.mean(PTSDF, totalpopE, na.rm = T),
            P_PM25 = P_PM25,
            P_OZONE = P_OZONE,
            P_CANCR = P_CANCR,
            P_RESP = P_RESP,
            P_DSLPM = P_DSLPM,
            T_PM25 = T_PM25,
            T_OZONE = T_OZONE,
            T_CANCR = T_CANCR,
            T_RESP, T_RESP,
            T_DSLPM = T_DSLPM) %>%
  distinct()

# Export to .csv
write.csv(east_tract, "../data/ejscreen_eastern_tract.csv", row.names = F)
