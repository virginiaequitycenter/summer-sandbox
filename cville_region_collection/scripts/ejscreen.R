# Get EJSCREEN data
# Marisa Lemma
# Last updated: 2021-11-19

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
