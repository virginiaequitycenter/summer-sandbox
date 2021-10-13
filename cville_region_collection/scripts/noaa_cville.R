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

cville_noaa <- rename(va_noaa, Fipsyearmin=V1, Janmin=V2, Febmin=V3, Marmin=V4, Aprmin=V5, Maymin=V6, Junmin=V7, 
                      Julmin=V8, Augmin=V9, Sepmin=V10, Octmin=V11, Novmin=V12, Decmin=V13)

# Filter to relevant areas in temp data
cville_noaa <- cville_noaa %>% filter(Fipsyearmin %in% c(44540281895:44540282021, 44003281895:44003282021, 
                                                      44065281895:44065282021, 44079281895:44079282021, 
                                                      44109281895:44109282021, 44125281895:44125282021))

# Create a country fips that matches the SF data
cville_noaa <- cville_noaa %>%
  group_by(Fipsyearmin) %>% 
  mutate(Year = as.numeric(str_sub(Fipsyearmin, 8, )),CNTY_FIPS = str_sub(Fipsyearmin, 3, -7))

# Remove -99.9 and replace with NA
cville_noaa <- cville_noaa %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean
cville_noaa$Avg_Tempmin = round(rowMeans(cville_noaa[,c(2:13)], na.rm = TRUE), 2) # These are the columns with temperature values

## Maximum Temperature ----------------------------------------------------------------------------

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"
# To check what the right ending is, go to the link above and look at the file names. 

url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20211006"
download.file(url = url, destfile = paste(getwd(), "/", "climdiv-tmaxcy-v1.0.0-20211006.txt", sep = ""))

va_noaa2 <- read.table(file = "climdiv-tmaxcy-v1.0.0-20211006.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
cville_noaa2 <- rename(va_noaa2, Fipsyearmax=V1, Janmax=V2, Febmax=V3, Marmax=V4, Aprmax=V5, Maymax=V6, Junmax=V7, 
                      Julmax=V8, Augmax=V9, Sepmax=V10, Octmax=V11, Novmax=V12, Decmax=V13)

# Filter to relevant areas in temp max data
cville_noaa2 <- cville_noaa2 %>% filter(Fipsyearmax %in% c(44540271895:44540272021, 44003271895:44003272021, 
                                                      44065271895:44065272021, 44079271895:44079272021, 
                                                      44109271895:44109272021, 44125271895:44125272021))

# Create a country fips that matches the SF data
cville_noaa2 <- cville_noaa2 %>%
  group_by(Fipsyearmax) %>% mutate(Year = as.numeric(str_sub(Fipsyearmax, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyearmax, 3, -7))

# Remove -99.9 and replace with NA
cville_noaa2 <- cville_noaa2 %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean
cville_noaa2$Avg_Tempmax = round(rowMeans(cville_noaa2[,c(2:13)], na.rm = TRUE), 2) # These column numbers are all the columns
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
cville_noaa3 <- rename(va_noaa3, Fipsyearpcp=V1, Janpcp=V2, Febpcp=V3, Marpcp=V4, Aprpcp=V5, Maypcp=V6, Junpcp=V7, 
                      Julpcp=V8, Augpcp=V9, Seppcp=V10, Octpcp=V11, Novpcp=V12, Decpcp=V13)

# Filter to relevant areas in precipitation data
cville_noaa3 <- cville_noaa3 %>% filter(Fipsyearpcp %in% c(44540011895:44540012021, 44003011895:44003012021, 
                                                      44065011895:44065012021, 44079011895:44079012021, 
                                                      44109011895:44109012021, 44125011895:44125012021))

# Create a country fips that matches the SF data
cville_noaa3 <- cville_noaa3 %>%
  group_by(Fipsyearpcp) %>% mutate(Year = as.numeric(str_sub(Fipsyearpcp, 8, )), 
                                CNTY_FIPS = str_sub(Fipsyearpcp, 3, -7))

# Remove -99.9 and replace with NA
cville_noaa3 <- cville_noaa3 %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean and Sum
cville_noaa3$Avg_monthlypcp = round(rowMeans(cville_noaa3[,c(2:13)], na.rm = TRUE), 2) # These are just the column numbers that have precipitation values

cville_noaa3$Tot_yearlypcp = rowSums(cville_noaa3[,c(2:13)], na.rm = TRUE)

# Merging the files and deleting two Fipsyear
noaa_cville <- merge(cville_noaa, cville_noaa2, by = c('CNTY_FIPS', 'Year'))
noaa_cvillefull <- merge(noaa_cville, cville_noaa3, by = c('CNTY_FIPS', 'Year'))

# Save to csv
write_csv(noaa_cvillefull, file = "noaa_cville_county.csv")

