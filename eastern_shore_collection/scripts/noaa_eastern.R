# Get NOAA Temperature Data 
# 2021-07-03
# Tolu Odukoya and Lee LeBoeuf
# Last updated: 10/08/2021

# Loading required packages
invisible(lapply(list('tidyverse','sf','stringr', 'leaflet', 'naniar'),
                 function(pkg) library(pkg, character.only = TRUE)))

# Define localities of interest
# Cville area
# localfips <- c("540", "003", "065", "079", "109", "125")
# Eastern shore area
localfips <- c("001", "131")


## Minimum Temperature ----------------------------------------------------------------------------

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"
# To check what the right ending is, go to the link above and look at the file names. 

# url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20211104"
# download.file(url = url, destfile = paste(getwd(), "/dataraw/", "climdiv-tmaxcy.txt", sep = ""))

va_noaa <- read.table(file = "dataraw/climdiv-tmincy.txt", header = FALSE)
head(va_noaa)

eastern_noaa <- rename(va_noaa, fipsdivelemyear=V1, Janmin=V2, Febmin=V3, Marmin=V4, Aprmin=V5, Maymin=V6, Junmin=V7, 
                       Julmin=V8, Augmin=V9, Sepmin=V10, Octmin=V11, Novmin=V12, Decmin=V13)

# Filter to relevant areas in temp data
# Create year and state/county fips
eastern_noaa <- eastern_noaa %>% 
  mutate(fipsdivelemyear = as.character(fipsdivelemyear),
         fipsdivelemyear = str_pad(fipsdivelemyear, width = 11, side = "left", pad = "0"),
         state = str_sub(fipsdivelemyear, 1,2),
         county = str_sub(fipsdivelemyear, 3,5),
         elem = str_sub(fipsdivelemyear, 6,7),
         year = str_sub(fipsdivelemyear, 8,11)) %>% 
  filter(state == "44" & county %in% localfips)

# Remove -99.9 and replace with NA
eastern_noaa <- eastern_noaa %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean
eastern_noaa <- eastern_noaa %>% 
  mutate(Avg_Tempmin = round(rowMeans(.[c(2:13)])))
# 2:13 are the columns with temperature values


## Maximum Temperature ----------------------------------------------------------------------------

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"
# To check what the right ending is, go to the link above and look at the file names. 

# url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20211006"
# download.file(url = url, destfile = paste(getwd(), "/", "climdiv-tmaxcy.txt", sep = ""))

va_noaa2 <- read.table(file = "climdiv-tmaxcy.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
eastern_noaa2 <- rename(va_noaa2, fipsdivelemyear=V1, Janmax=V2, Febmax=V3, Marmax=V4, Aprmax=V5, Maymax=V6, Junmax=V7, 
                       Julmax=V8, Augmax=V9, Sepmax=V10, Octmax=V11, Novmax=V12, Decmax=V13)

# Filter to relevant areas in temp data
# Create year and state/county fips
eastern_noaa2 <- eastern_noaa2 %>% 
  mutate(fipsdivelemyear = as.character(fipsdivelemyear),
         fipsdivelemyear = str_pad(fipsdivelemyear, width = 11, side = "left", pad = "0"),
         state = str_sub(fipsdivelemyear, 1,2),
         county = str_sub(fipsdivelemyear, 3,5),
         elem = str_sub(fipsdivelemyear, 6,7),
         year = str_sub(fipsdivelemyear, 8,11)) %>% 
  filter(state == "44" & county %in% localfips)

# Remove -99.9 and replace with NA
eastern_noaa2 <- eastern_noaa2 %>% replace_with_na_all(condition = ~.x == -99.9)

# Create Yearly Mean
eastern_noaa2 <- eastern_noaa2 %>% 
  mutate(Avg_Tempmax = round(rowMeans(.[c(2:13)])))
# 2:13 are the columns with temperature values


## Precipitation ----------------------------------------------------------------------------

# Pull NOAA County Climate Data: Found at (https://www.ncei.noaa.gov/pub/data/cirs/climdiv/)
# Note: The date at the end of the url below may need to be updated if data are downloaded at a later date. When Tolu initially downloaded data 
# in July of 2021, the url ended with "0.0-20210707". When I downloaded data in October of 2021, I had to update the date to "0.0-20211006"

# url <- "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pcpncy-v1.0.0-20211104"
# download.file(url = url, destfile = paste(getwd(), "/dataraw/", "climdiv-pcpncy.txt", sep = ""))

va_noaa3 <- read.table(file = "dataraw/climdiv-pcpncy.txt", header = FALSE)
head(va_noaa)

# Rename variables of interest
eastern_noaa3 <- rename(va_noaa3, fipsdivelemyear=V1, Janpcp=V2, Febpcp=V3, Marpcp=V4, Aprpcp=V5, Maypcp=V6, Junpcp=V7, 
                       Julpcp=V8, Augpcp=V9, Seppcp=V10, Octpcp=V11, Novpcp=V12, Decpcp=V13)

# Filter to relevant areas in temp data
# Create year and state/county fips
eastern_noaa3 <- eastern_noaa3 %>% 
  mutate(fipsdivelemyear = as.character(fipsdivelemyear),
         fipsdivelemyear = str_pad(fipsdivelemyear, width = 11, side = "left", pad = "0"),
         state = str_sub(fipsdivelemyear, 1,2),
         county = str_sub(fipsdivelemyear, 3,5),
         elem = str_sub(fipsdivelemyear, 6,7),
         year = str_sub(fipsdivelemyear, 8,11)) %>% 
  filter(state == "44" & county %in% localfips)

# Remove -99.9 and replace with NA
eastern_noaa3 <- eastern_noaa3 %>% replace_with_na_all(condition = ~.x == -9.99)

# Create Yearly Mean and Sum
eastern_noaa3 <- eastern_noaa3 %>% 
  mutate(Avg_monthlypcp = round(rowMeans(.[c(2:13)])),
         Tot_yearlypcp = rowSums(.[c(2:13)]))
# 2:13 are the columns with temperature values

# Merging the files and deleting two Fipsyear
noaa_eastern <- select(eastern_noaa, -c(state, fipsdivelemyear, elem)) %>% 
  left_join(select(eastern_noaa2, -c(state, fipsdivelemyear, elem)), by = c("county", "year")) %>% 
  left_join(select(eastern_noaa3, -c(state, fipsdivelemyear, elem)), by = c("county", "year")) %>% 
  select(county, year, Avg_Tempmin, Avg_Tempmax, Avg_monthlypcp, Tot_yearlypcp,
         everything())

# Save to csv
write_csv(noaa_eastern, file = "data/noaa_eastern_county.csv")
