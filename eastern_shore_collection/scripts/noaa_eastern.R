# Get NOAA Temperature Data 
# 2021-07-03
# Tolu Odukoya and Lee LeBoeuf
# Last updated: 10/08/2021

# Loading required packages
invisible(lapply(list('tidyverse','sf','stringr', 'leaflet', 'naniar'),
                 function(pkg) library(pkg, character.only = TRUE)))

## Minimum Temperature ----------------------------------------------------------------------------

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"
# To check what the right ending is, go to the link above and look at the file names. 

url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmincy-v1.0.0-20211006"

download.file(url = url, destfile = paste(getwd(), "/", "climdiv-tmincy-v1.0.0-20211006.txt", sep = ""))

va_noaa <- read.table(file = "climdiv-tmincy-v1.0.0-20211006.txt", header = FALSE)
head(va_noaa)

eastern_noaa <- rename(va_noaa, Fipsyearmin=V1, Janmin=V2, Febmin=V3, Marmin=V4, Aprmin=V5, Maymin=V6, Junmin=V7, 
                      Julmin=V8, Augmin=V9, Sepmin=V10, Octmin=V11, Novmin=V12, Decmin=V13)

# Filter to relevant areas in temp data
eastern_noaa <- eastern_noaa %>% filter(Fipsyearmin %in% c(44001281895:44001282021, 44131281895:44131282021))

# Create a country fips that matches the SF data
eastern_noaa <- eastern_noaa %>%
  group_by(Fipsyearmin) %>% 
  mutate(Year = as.numeric(str_sub(Fipsyearmin, 8, )),CNTY_FIPS = str_sub(Fipsyearmin, 3, -7))

# Remove -99.9 and replace with NA
eastern_noaa <- eastern_noaa %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean
eastern_noaa$Avg_Tempmin = round(rowMeans(eastern_noaa[,c(2:13)], na.rm = TRUE), 2) # These are the columns with temperature values

## Maximum Temperature

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"
# To check what the right ending is, go to the link above and look at the file names. 

url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20211006"
download.file(url = url, destfile = paste(getwd(), "/", "climdiv-tmaxcy-v1.0.0-20211006.txt", sep = ""))

va_noaa2 <- read.table(file = "climdiv-tmaxcy-v1.0.0-20211006.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
eastern_noaa2 <- rename(va_noaa2, Fipsyearmax=V1, Janmax=V2, Febmax=V3, Marmax=V4, Aprmax=V5, Maymax=V6, Junmax=V7, 
                       Julmax=V8, Augmax=V9, Sepmax=V10, Octmax=V11, Novmax=V12, Decmax=V13)

# Filter to relevant areas in temp data
eastern_noaa2 <- eastern_noaa2 %>% filter(Fipsyearmax %in% c(44001271895:44001272021, 44131271895:44131272021))

# Create a country fips that matches the SF data
eastern_noaa2 <- eastern_noaa2 %>%
  group_by(Fipsyearmax) %>% mutate(Year = as.numeric(str_sub(Fipsyearmax, 8, )), 
                                   CNTY_FIPS = str_sub(Fipsyearmax, 3, -7))

# Remove -99.9 and replace with NA
eastern_noaa2 <- eastern_noaa2 %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean
eastern_noaa2$Avg_Tempmax = round(rowMeans(eastern_noaa2[,c(2:13)], na.rm = TRUE), 2) # These column numbers are all the columns
# with temperature values 

## Precipitation ----------------------------------------------------------------------------

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"

url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpncy-v1.0.0-20211006"
download.file(url = url, destfile = paste(getwd(), "/", "climdiv-pcpncy-v1.0.0-20211006.txt", sep = ""))

va_noaa3 <- read.table(file = "climdiv-pcpncy-v1.0.0-20211006.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
eastern_noaa3 <- rename(va_noaa3, Fipsyearpcp=V1, Janpcp=V2, Febpcp=V3, Marpcp=V4, Aprpcp=V5, Maypcp=V6, Junpcp=V7, 
                       Julpcp=V8, Augpcp=V9, Seppcp=V10, Octpcp=V11, Novpcp=V12, Decpcp=V13)

# Filter to relevant areas in temp data
eastern_noaa3 <- eastern_noaa3 %>% filter(Fipsyearpcp %in% c(44001011895:44001012021, 44131011895:44131012021))

# Create a country fips that matches the SF data
eastern_noaa3 <- eastern_noaa3 %>%
  group_by(Fipsyearpcp) %>% mutate(Year = as.numeric(str_sub(Fipsyearpcp, 8, )), 
                                   CNTY_FIPS = str_sub(Fipsyearpcp, 3, -7))

# Remove -99.9 and replace with NA
eastern_noaa3 <- eastern_noaa3 %>% replace_with_na_all(condition = ~.x == -9.99)

# Create Yearly Mean and Sum
eastern_noaa3$Avg_monthlypcp = round(rowMeans(eastern_noaa3[,c(2:13)], na.rm = TRUE), 2) # These are just the column numbers that have precipitation values

eastern_noaa3$Tot_yearlypcp = rowSums(eastern_noaa3[,c(2:13)], na.rm = TRUE)

# Merging the files and deleting two Fipsyear
noaa_eastern <- merge(eastern_noaa, eastern_noaa2, by = c('CNTY_FIPS', 'Year'))
noaa_easternfull <- merge(noaa_eastern, eastern_noaa3, by = c('CNTY_FIPS', 'Year'))

# Save to csv
write_csv(noaa_easternfull, file = "noaa_eastern_county.csv")
