# EJSCREEN generative file

library(tidyverse)

# Pull EJSCREEN
url <- "https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_USPR.csv.zip"
download.file(url = url,
              destfile = paste(getwd(), "/", "ejscreen.zip", sep = ""),
              mode = "wb")

# unzip and read
unzip("ejscreen.zip", exdir = getwd())

# Load data
ejscreen <- read_csv("EJSCREEN_2020_USPR.csv")

# Get rid of demographic variables (since we don't need those)
ejscreen_clean <- ejscreen %>%
  select(-c("OBJECTID", "ACSTOTPOP":"PRE1960", "VULEOPCT":"DISPEO", "D_LDPNT_2":"D_PM25_2",
            "ST_ABBREV":"P_OVR64PCT", "P_VULEOPCT", "P_LDPNT_D2":"T_VULEOPCT",
            "T_LDPNT_D2", "T_DSLPM_D2", "T_CANCR_D2", "T_RESP_D2", "T_PTRAF_D2", "T_PWDIS_D2",
            "T_PNPL_D2", "T_PRMP_D2", "T_PTSDF_D2", "T_OZONE_D2", "T_PM25_D2", "Shape_Length", 
            "Shape_Area"))

# Filter to just areas of interest
virginia <- ejscreen_clean %>%
  filter(STATE_NAME=="Virginia")
eastern_shore <- virginia %>%
  filter(ID%in%510010901001:510019902000 |
           ID%in%511319301001:511319901000)


# Get rid of unnecessary state name variable
eastern_shore <- eastern_shore %>% 
  select(-STATE_NAME, -NPL_CNT, -TSDF_CNT)

# Export to .csv
write.csv(eastern_shore, "eastern_shore_collection/data//ejscreen_eastern_blkgps.csv", row.names = F)
