# Get NOAA Minimum Temperature Data 
# 2021-07-03
# Tolu Odukoya

library(tidyverse)
library(sf)
library(stringi)
library(leaflet)
library(naniar)

##Minimum Temperature

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmincy-v1.0.0-20210604"
download.file(url = url,
              destfile = paste(getwd(), "/", "climdiv-tmincy-v1.0.0-20210604.txt", sep = ""))

va_noaa <- read.table(file = "climdiv-tmincy-v1.0.0-20210604.txt", header = FALSE)
head(va_noaa)

cville_noaa <- rename(va_noaa, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                      Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)
eastern_noaa <- rename(va_noaa, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                       Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)

# Filter to relevant areas in temp data
cville_noaa <- cville_noaa %>% filter(Fipsyear %in% c(44540281895:44540282021, 44003281895:44003282021, 
                                                      44065281895:44065282021, 44079281895:44079282021, 
                                                      44109281895:44109282021, 44125281895:44125282021))
eastern_noaa <- eastern_noaa %>% filter(Fipsyear %in% c(44001281895:44001282021, 44131281895:44131282021))

# Create a country fips that matches the SF data
cville_noaa <- cville_noaa %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7), TempType = "Minimum")
eastern_noaa <- eastern_noaa %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7), TempType = "Minimum")


# Arrange and Select: This part can be ignored. I thought I needed to reshape the data.
cville_noaa <- cville_noaa %>%
  dplyr::select(CNTY_FIPS, Fipsyear, Year, everything())

eastern_noaa <- eastern_noaa %>%
  dplyr::select(CNTY_FIPS, Fipsyear, Year, everything())

cville_noaa1 <- gather(cville_noaa, key="Months", value ="Temperature", Jan:Dec)
eastern_noaa1 <- gather(eastern_noaa, key="Months", value ="Temperature", Jan:Dec)

cville_noaa1 <- cville_noaa1 %>% replace_with_na(replace = list(Temperature = -99.9))
eastern_noaa1 <- eastern_noaa1 %>% replace_with_na(replace = list(Temperature = -99.9))

cville_noaa1 <- cville_noaa1 %>% arrange(Fipsyear) %>% group_by(Year) %>% 
  mutate(row = row_number())
eastern_noaa1 <- eastern_noaa1 %>% arrange(Fipsyear) %>% group_by(Year) %>% 
  mutate(row = row_number())

cville_noaa1 <- cville_noaa1 %>% group_by(Year) %>%  
  select(-Fipsyear) %>% spread(key = Year, value = Temperature) %>% 
  select(-row)
eastern_noaa1 <- eastern_noaa1 %>% group_by(Year) %>%  
  select(-Fipsyear) %>% spread(key = Year, value = Temperature) %>% 
  select(-row)

cville_noaa1 <- cville_noaa1 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 
eastern_noaa1 <- eastern_noaa1 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 

# Remove -99.9 and replace with NA

cville_noaa <- gather(cville_noaa, key="Months", value ="Temperature", Jan:Dec)
eastern_noaa <- gather(eastern_noaa, key="Months", value ="Temperature", Jan:Dec)

cville_noaa <- cville_noaa %>% arrange(CNTY_FIPS, match(Months, month.abb)) 
eastern_noaa <- eastern_noaa %>% arrange(CNTY_FIPS, match(Months, month.abb)) 

cville_noaa <- cville_noaa %>% replace_with_na(replace = list(Temperature = -99.9))
eastern_noaa <- eastern_noaa %>% replace_with_na(replace = list(Temperature = -99.9))

cville_noaa <- spread(cville_noaa, key="Months", value ="Temperature")
eastern_noaa <- spread(eastern_noaa, key="Months", value ="Temperature")

# Create Yearly Mean
cville_noaa <- cville_noaa %>%
  dplyr::select(CNTY_FIPS, Year,  TempType, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)
eastern_noaa <- eastern_noaa %>%
  dplyr::select(CNTY_FIPS, Year, TempType, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)

cville_noaa$Avg_Tempmin = rowMeans(cville_noaa[,c(4:15)], na.rm = TRUE)
eastern_noaa$Avg_Tempmin = rowMeans(eastern_noaa[,c(4:15)], na.rm = TRUE)


cville_noaa <- cville_noaa %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Tempmin, Fipsyear)

eastern_noaa <- eastern_noaa %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Tempmin, Fipsyear)

cville_noaa$Year = as.numeric(cville_noaa$Year)
eastern_noaa$Year = as.numeric(eastern_noaa$Year)


# Rename variables of interest
cville_noaa <- rename(cville_noaa, Fipsyear=Fipsyear, Janmin=Jan, Febmin=Feb, Marmin=Mar, Aprmin=Apr, Maymin=May, Junmin=Jun, 
                      Julmin=Jul, Augmin=Aug, Sepmin=Sep, Octmin=Oct, Novmin=Nov, Decmin=Dec)
eastern_noaa <- rename(eastern_noaa, Fipsyear=Fipsyear, Janmin=Jan, Febmin=Feb, Marmin=Mar, Aprmin=Apr, Maymin=May, Junmin=Jun, 
                       Julmin=Jul, Augmin=Aug, Sepmin=Sep, Octmin=Oct, Novmin=Nov, Decmin=Dec)

## Maximum Temperature

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20210604"
download.file(url = url,
              destfile = paste(getwd(), "/", "climdiv-tmaxcy-v1.0.0-20210604.txt", sep = ""))

va_noaa1 <- read.table(file = "climdiv-tmaxcy-v1.0.0-20210604.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
cville_noaa2 <- rename(va_noaa1, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                      Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)
eastern_noaa2 <- rename(va_noaa1, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                       Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)

# Filter to relevant areas in temp data
cville_noaa2 <- cville_noaa2 %>% filter(Fipsyear %in% c(44540271895:44540272021, 44003271895:44003272021, 
                                                      44065271895:44065272021, 44079271895:44079272021, 
                                                      44109271895:44109272021, 44125271895:44125272021))
eastern_noaa2 <- eastern_noaa2 %>% filter(Fipsyear %in% c(44001271895:44001272021, 44131271895:44131272021))

# Create a country fips that matches the SF data
cville_noaa2 <- cville_noaa2 %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7), TempType = "Maximum")
eastern_noaa2 <- eastern_noaa2 %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7), TempType = "Maximum")


# Arrange and Select: This part can be ignored. I thought I needed to reshape the data.
cville_noaa2 <- cville_noaa2 %>%
  dplyr::select(CNTY_FIPS, Fipsyear, Year, everything())

eastern_noaa2 <- eastern_noaa2 %>%
  dplyr::select(CNTY_FIPS, Fipsyear, Year, everything())

cville_noaa3 <- gather(cville_noaa2, key="Months", value ="Temperature", Jan:Dec)
eastern_noaa3 <- gather(eastern_noaa2, key="Months", value ="Temperature", Jan:Dec)

cville_noaa3 <- cville_noaa3 %>% replace_with_na(replace = list(Temperature = -99.9))
eastern_noaa3 <- eastern_noaa3 %>% replace_with_na(replace = list(Temperature = -99.9))

cville_noaa3 <- cville_noaa3 %>% arrange(Fipsyear) %>% group_by(Year) %>% 
  mutate(row = row_number())
eastern_noaa3 <- eastern_noaa3 %>% arrange(Fipsyear) %>% group_by(Year) %>% 
  mutate(row = row_number())

cville_noaa3 <- cville_noaa3 %>% group_by(Year) %>%  
  select(-Fipsyear) %>% spread(key = Year, value = Temperature) %>% 
  select(-row)
eastern_noaa3 <- eastern_noaa3 %>% group_by(Year) %>%  
  select(-Fipsyear) %>% spread(key = Year, value = Temperature) %>% 
  select(-row)

cville_noaa3 <- cville_noaa3 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 
eastern_noaa3 <- eastern_noaa3 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 

# Remove -99.9 and replace with NA

cville_noaa2 <- gather(cville_noaa2, key="Months", value ="Temperature", Jan:Dec)
eastern_noaa2 <- gather(eastern_noaa2, key="Months", value ="Temperature", Jan:Dec)

cville_noaa2 <- cville_noaa2 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 
eastern_noaa2 <- eastern_noaa2 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 

cville_noaa2 <- cville_noaa2 %>% replace_with_na(replace = list(Temperature = -99.9))
eastern_noaa2 <- eastern_noaa2 %>% replace_with_na(replace = list(Temperature = -99.9))

cville_noaa2 <- spread(cville_noaa2, key="Months", value ="Temperature")
eastern_noaa2 <- spread(eastern_noaa2, key="Months", value ="Temperature")

# Create Yearly Mean
cville_noaa2 <- cville_noaa2 %>%
  dplyr::select(CNTY_FIPS, Year, TempType, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)
eastern_noaa2 <- eastern_noaa2 %>%
  dplyr::select(CNTY_FIPS, Year, TempType, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)

cville_noaa2$Avg_Tempmax = rowMeans(cville_noaa2[,c(4:15)], na.rm = TRUE)
eastern_noaa2$Avg_Tempmax = rowMeans(eastern_noaa2[,c(4:15)], na.rm = TRUE)


cville_noaa2 <- cville_noaa2 %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Tempmax, Fipsyear)

eastern_noaa2 <- eastern_noaa2 %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Tempmax, Fipsyear)

cville_noaa2$Year = as.numeric(cville_noaa2$Year)
eastern_noaa2$Year = as.numeric(eastern_noaa2$Year)

# Rename variables of interest
cville_noaa2 <- rename(cville_noaa2, Fipsyear=Fipsyear, Janmax=Jan, Febmax=Feb, Marmax=Mar, Aprmax=Apr, Maymax=May, Junmax=Jun, 
                      Julmax=Jul, Augmax=Aug, Sepmax=Sep, Octmax=Oct, Novmax=Nov, Decmax=Dec)
eastern_noaa2 <- rename(eastern_noaa2, Fipsyear=Fipsyear, Janmax=Jan, Febmax=Feb, Marmax=Mar, Aprmax=Apr, Maymax=May, Junmax=Jun, 
                       Julmax=Jul, Augmax=Aug, Sepmax=Sep, Octmax=Oct, Novmax=Nov, Decmax=Dec)

## Precipitation

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpncy-v1.0.0-20210604"
download.file(url = url,
              destfile = paste(getwd(), "/", "climdiv-pcpncy-v1.0.0-20210604.txt", sep = ""))

va_noaa2 <- read.table(file = "climdiv-pcpncy-v1.0.0-20210604.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
cville_noaa4 <- rename(va_noaa2, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                      Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)
eastern_noaa4 <- rename(va_noaa2, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                       Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)

# Filter to relevant areas in temp data
cville_noaa4 <- cville_noaa4 %>% filter(Fipsyear %in% c(44540011895:44540012021, 44003011895:44003012021, 
                                                      44065011895:44065012021, 44079011895:44079012021, 
                                                      44109011895:44109012021, 44125011895:44125012021))
eastern_noaa4 <- eastern_noaa4 %>% filter(Fipsyear %in% c(44001011895:44001012021, 44131011895:44131012021))

# Create a country fips that matches the SF data
cville_noaa4 <- cville_noaa4 %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7), TempType = "Precipitation")
eastern_noaa4 <- eastern_noaa4 %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7), TempType = "Precipitation")


# Arrange and Select: This part can be ignored. I thought I needed to reshape the data.
cville_noaa4 <- cville_noaa4 %>%
  dplyr::select(CNTY_FIPS, Fipsyear, Year, everything())

eastern_noaa4 <- eastern_noaa4 %>%
  dplyr::select(CNTY_FIPS, Fipsyear, Year, everything())

cville_noaa5 <- gather(cville_noaa4, key="Months", value ="Temperature", Jan:Dec)
eastern_noaa5 <- gather(eastern_noaa4, key="Months", value ="Temperature", Jan:Dec)

cville_noaa5 <- cville_noaa5 %>% replace_with_na(replace = list(Temperature = -99.9))
eastern_noaa5 <- eastern_noaa5 %>% replace_with_na(replace = list(Temperature = -99.9))

cville_noaa5 <- cville_noaa5 %>% arrange(Fipsyear) %>% group_by(Year) %>% 
  mutate(row = row_number())
eastern_noaa5 <- eastern_noaa5 %>% arrange(Fipsyear) %>% group_by(Year) %>% 
  mutate(row = row_number())

cville_noaa5 <- cville_noaa5 %>% group_by(Year) %>%  
  select(-Fipsyear) %>% spread(key = Year, value = Temperature) %>% 
  select(-row)
eastern_noaa5 <- eastern_noaa5 %>% group_by(Year) %>%  
  select(-Fipsyear) %>% spread(key = Year, value = Temperature) %>% 
  select(-row)

cville_noaa5 <- cville_noaa5 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 
eastern_noaa5 <- eastern_noaa5 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 

# Remove -99.9 and replace with NA

cville_noaa4 <- gather(cville_noaa4, key="Months", value ="Temperature", Jan:Dec)
eastern_noaa4 <- gather(eastern_noaa4, key="Months", value ="Temperature", Jan:Dec)

cville_noaa4 <- cville_noaa4 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 
eastern_noaa4 <- eastern_noaa4 %>% arrange(CNTY_FIPS, match(Months, month.abb)) 

cville_noaa4 <- cville_noaa4 %>% replace_with_na(replace = list(Temperature = -99.9))
eastern_noaa4 <- eastern_noaa4 %>% replace_with_na(replace = list(Temperature = -99.9))

cville_noaa4 <- spread(cville_noaa4, key="Months", value ="Temperature")
eastern_noaa4 <- spread(eastern_noaa4, key="Months", value ="Temperature")

# Create Yearly Mean
cville_noaa4 <- cville_noaa4 %>%
  dplyr::select(CNTY_FIPS, Year, TempType, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)
eastern_noaa4 <- eastern_noaa4 %>%
  dplyr::select(CNTY_FIPS, Year, TempType, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)

cville_noaa4$Avg_Temppcp = rowMeans(cville_noaa4[,c(4:15)], na.rm = TRUE)
eastern_noaa4$Avg_Temppcp = rowMeans(eastern_noaa4[,c(4:15)], na.rm = TRUE)


cville_noaa4 <- cville_noaa4 %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Temppcp, Fipsyear)

eastern_noaa4 <- eastern_noaa4 %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Temppcp, Fipsyear)

cville_noaa4$Year = as.numeric(cville_noaa4$Year)
eastern_noaa4$Year = as.numeric(eastern_noaa4$Year)

# Rename variables of interest
cville_noaa4 <- rename(cville_noaa4, Fipsyear=Fipsyear, Janpcp=Jan, Febpcp=Feb, Marpcp=Mar, Aprpcp=Apr, Maypcp=May, Junpcp=Jun, 
                       Julpcp=Jul, Augpcp=Aug, Seppcp=Sep, Octpcp=Oct, Novpcp=Nov, Decpcp=Dec)
eastern_noaa4 <- rename(eastern_noaa4, Fipsyear=Fipsyear, Janpcp=Jan, Febpcp=Feb, Marpcp=Mar, Aprpcp=Apr, Maypcp=May, Junpcp=Jun, 
                        Julpcp=Jul, Augpcp=Aug, Seppcp=Sep, Octpcp=Oct, Novpcp=Nov, Decpcp=Dec)


# Merging the files and deleting two Fipsyear
noaa_cville <- merge(cville_noaa, cville_noaa2, by = c('CNTY_FIPS', 'Year'))
noaa_cville1 <- merge(noaa_cville, cville_noaa4, by = c('CNTY_FIPS', 'Year'))
noaa_cville1 <- noaa_cville1[, c( -16, -30)]
noaa_eastern <- merge(eastern_noaa, eastern_noaa2, by = c('CNTY_FIPS', 'Year'))
noaa_eastern1 <- merge(noaa_eastern, eastern_noaa4, by = c('CNTY_FIPS', 'Year'))
noaa_eastern1 <- noaa_eastern1[, c( -16, -30)]

# Merging the transposed datasets
noaa_cvillet <- rbind(cville_noaa1, cville_noaa3)
noaa_cvillet1 <- rbind(noaa_cvillet, cville_noaa5)
noaa_easternt <- rbind(eastern_noaa1, eastern_noaa3)
noaa_easternt1 <- rbind(noaa_easternt, eastern_noaa5)


# Save to csv
write_csv(noaa_cville1, file = "noaa_cville_county.csv")
write_csv(noaa_eastern1, file = "noaa_eastern_county.csv")
write_csv(noaa_cvillet1, file = "noaa_cville_countyt.csv")
write_csv(noaa_easternt1, file = "noaa_eastern_countyt.csv")
