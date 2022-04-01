# Get EJSCREEN data
# Marisa Lemma and Lee LeBoeuf
# Last updated: 2022-04-01

library(tidyverse)

# Import data from source ----
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
localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
# localfips <- c("001", "131")


# Reduce----
# Get rid of demographic variables (since we don't need those)
ejscreen_clean <- ejscreen %>%
  select(-c("OBJECTID", "ACSTOTPOP":"PRE1960", "VULEOPCT":"DISPEO", "D_LDPNT_2":"D_PM25_2",
            "ST_ABBREV":"P_OVR64PCT", "P_VULEOPCT", "P_LDPNT_D2":"T_VULEOPCT",
            "T_LDPNT_D2", "T_DSLPM_D2", "T_CANCR_D2", "T_RESP_D2", "T_PTRAF_D2", "T_PWDIS_D2",
            "T_PNPL_D2", "T_PRMP_D2", "T_PTSDF_D2", "T_OZONE_D2", "T_PM25_D2", "Shape_Length", 
            "Shape_Area"))

# Filter to areas of interest
cville_area <- ejscreen_clean %>%
  mutate(statefips = str_sub(ID, 1,2),
         countyfips = str_sub(ID, 3,5),
         tractfips = str_sub(ID, 6, 11),
         blkgpfips = str_sub(ID, 12,12)) %>% 
  filter(statefips == "51" & countyfips %in% localfips) %>% 
  select(ID, statefips, countyfips, tractfips, blkgpfips, everything(),
         -c(STATE_NAME, NPL_CNT, TSDF_CNT, AREALAND, AREAWATER))


# Export to .csv
write.csv(cville_area, "data/ejscreen_cville_blkgps.csv", row.names = F)


# Aggregating to tract level
# PM2.5 (PM25), ozone (OZONE), and NATA indicators (cancer risk [CANCER], respiratory hazard index [RESP], and diesel particulate matter [DSLPM]) 
# are measured at the census tract level, and the same value is assigned to each block group 
# within that tract. So for those variables, the values are the same. 
# For other variables, I took the average across the block groups. 

cville_area$tract <- paste0(cville_area$statefips, cville_area$countyfips, cville_area$tractfips)

cville_tract <- cville_area %>%
  group_by(tract) %>%
  summarise(PM25 = PM25,
            OZONE = OZONE,
            CANCER = CANCER,
            RESP = RESP,
            DSLPM = DSLPM,
            Avg_PRE1960PCT = mean(PRE1960PCT),
            Avg_PTRAF = mean(PTRAF, na.rm = T),
            Avg_PWDIS = mean(PWDIS),
            Avg_PNPL = mean(PNPL),
            Avg_PRMP = mean(PRMP),
            Avg_PTSDF = mean(PTSDF),
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
write.csv(cville_tract, "data/ejscreen_cville_tract.csv", row.names = F)
