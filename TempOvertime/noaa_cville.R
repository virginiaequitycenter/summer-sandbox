# Get NOAA Maximum Temperature Data 
# 2021-07-01
# Tolu Odukoya

library(tidyverse)
library(sf)
library(stringi)
library(leaflet)
library(naniar)

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
va_noaa <- read.table(file = "climdiv-tmaxcy-v1.0.0-20210604.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
cville_noaa <- rename(va_noaa, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                     Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)
eastern_noaa <- rename(va_noaa, Fipsyear=V1, Jan=V2, Feb=V3, Mar=V4, Apr=V5, May=V6, Jun=V7, 
                      Jul=V8, Aug=V9, Sep=V10, Oct=V11, Nov=V12, Dec=V13)

# Filter to relevant areas in temp data
cville_noaa <- cville_noaa %>% filter(Fipsyear %in% c(44540271895:44540272021, 44003271895:44003272021, 
                                                       44065271895:44065272021, 44079271895:44079272021, 
                                                       44109271895:44109272021, 44125271895:44125272021))
eastern_noaa <- eastern_noaa %>% filter(Fipsyear %in% c(44001271895:44001272021, 44131271895:44131272021))

# Create a country fips that matches the SF data
cville_noaa <- cville_noaa %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7))
eastern_noaa <- eastern_noaa %>%
  group_by(Fipsyear) %>% mutate(Year = as.numeric(str_sub(Fipsyear, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyear, 3, -7))


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
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)
eastern_noaa <- eastern_noaa %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Fipsyear)

cville_noaa$Avg_Temp = rowMeans(cville_noaa[,c(3:14)], na.rm = TRUE)
eastern_noaa$Avg_Temp = rowMeans(eastern_noaa[,c(3:14)], na.rm = TRUE)


cville_noaa <- cville_noaa %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Temp, Fipsyear)

eastern_noaa <- eastern_noaa %>%
  dplyr::select(CNTY_FIPS, Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, 
                Sep, Oct, Nov, Dec, Avg_Temp, Fipsyear)

cville_noaa$Year = as.numeric(cville_noaa$Year)
eastern_noaa$Year = as.numeric(eastern_noaa$Year)

# Save to csv
write_csv(cville_noaa, path = "noaa_cville_county.csv")
write_csv(eastern_noaa, path = "noaa_eastern_county.csv")
write_csv(cville_noaa1, path = "noaa_cville_countyt.csv")
write_csv(eastern_noaa1, path = "noaa_eastern_countyt.csv")
