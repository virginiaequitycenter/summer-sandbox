library(tidyverse)

# run the first time
leadurl <- "https://data.openei.org/files/573/VA-2018-LEAD-data.zip"
download.file(url = leadurl,
              destfile = paste(getwd(), "/", "va2018lead.zip", sep = ""))
unzip("va2018lead.zip", exdir = paste(getwd(), "/data/", sep = ""))

ami <- read_csv("data/VA AMI State, Counties, Cities 2018.csv")
cvlfips <- c("51540", "51003", "51065", "51079", "51109", "51125")

# Create new datasets for the whole region and each county individually
cville_area = filter(ami, FIP %in% cvlfips & HINCP!="NA")
cville = filter(cville_area, FIP==51540 & HINCP!="NA")

# do I understand what ELEP.UNITS (etc.) are?
cville <- cville %>% 
  mutate(elepunit_check = ELEP*UNITS)

ggplot(cville, aes(x = elepunit_check, y = ELEP*UNITS)) + geom_point()
# yes, basically

# Total cville income
i <- cville %>% summarize(totalinc = sum(HINCP*UNITS))

# Total cville energy expenditures
e <- cville %>% summarize(totalelep = sum(ELEP*UNITS, na.rm = TRUE))
g <- cville %>% summarize(totalgas = sum(GASP*UNITS, na.rm = TRUE))
f <- cville %>% summarize(totalother = sum(FULP*UNITS, na.rm = TRUE))
(e + g + f)/i # average energy burden
# so tract level should be same calculation done on tract level data, grouped by tract?
# e.g., in Cville to reproduce C3's Chart 8?

# Test by doing it by county using cville_area
cvl_area_burden <- cville_area %>% 
  group_by(FIP) %>% 
  summarize(totalinc = sum(HINCP*UNITS, na.rm = TRUE),
            totalelep = sum(ELEP*UNITS, na.rm = TRUE),
            totalgas = sum(GASP*UNITS, na.rm = TRUE),
            totalother = sum(FULP*UNITS, na.rm = TRUE)) %>% 
  mutate(totalexp = totalelep + totalgas + totalother,
         avgburden = totalexp/totalinc)
# Yes! Let's do this with the tract data -- generate a tract-level energy burden value

# Next, how would we add number of households with high burden (6% or more) overall in city
# and number of households with "very high burden" (10%)
# and number of households with "extremely high burden" (20%)

# And once we figure that out, ideas about how we might elaborate this
# e.g., can we use this to do energy burden by AMI, by TEN/ownership, 
#   by BLD/type of unit (probably collapsed into 1 attached, 1 detached, mobile, 2 & 3-4 unit, 5-9 & 10-19 unit, 20-49 & 50+; or something),
#   by HFL/fuel type (I don't actually understand this one yet -- is it primary fuel?)

tractdata <- read_csv("data/VA AMI Census Tracts 2018.csv")

tractdata <- tractdata %>% 
  mutate(countyfip = str_sub(FIP, 1,5),
         tractfip = str_sub(FIP, 6,11))

cville_area_tracts = filter(tractdata, countyfip %in% cvlfips & HINCP!="NA")
cville_tracts = filter(cville_area_tracts, countyfip==51540 & HINCP!="NA")

cvl_tract_burden <- cville_tracts %>% 
  group_by(tractfip) %>% 
  summarize(totalinc = sum(HINCP*UNITS, na.rm = TRUE),
            totalelep = sum(ELEP*UNITS, na.rm = TRUE),
            totalgas = sum(GASP*UNITS, na.rm = TRUE),
            totalother = sum(FULP*UNITS, na.rm = TRUE)) %>% 
  mutate(totalexp = totalelep + totalgas + totalother,
         avgburden = totalexp/totalinc)
