
url <- ("https://edg.epa.gov/EPADataCommons/public/OA/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")

cvillefips <- c("540", "003", "065", "079", "109", "125", "001", "131")
vars = c("D3B", "D4A", "D2B_E8MIXA", "D2A_EPHHM", "D2A_Ranked", "D2B_Ranked", "D3B_Ranked", "D4A_Ranked", "NatWalkInd", "GEOID10", "GEOID20", "STATEFP", "COUNTYFP", "TRACTCE", "BLKGRPCE")


cville_walk <- read_csv(url)%>%
  filter(STATEFP == 51) 

cville_walk$COUNTYFP <- cville_walk$COUNTYFP%>%
  str_pad(3, pad = "0")


cville_walk <- cville_walk %>%
  filter(COUNTYFP %in% cvillefips)%>%
  dplyr::select(vars)

cville_walk <- na_if(cville_walk, -99999.00)

write_csv(cville_walk, "cville_walk.csv")
