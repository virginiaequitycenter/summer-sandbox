library(tidyverse)

# smart location database: https://www.epa.gov/smartgrowth/smart-location-mapping#SLD

url <- ("https://edg.epa.gov/EPADataCommons/public/OA/EPA_SmartLocationDatabase_V3_Jan_2021_Final.csv")
# I found the csv link here: https://edg.epa.gov/metadata/catalog/search/resource/details.page?uuid=%7B33514B4C-54F2-464A-BCC7-35F441B7E21A%7D

sld <- read_csv(url)

# National Walkability documentation: https://www.epa.gov/smartgrowth/national-walkability-index-user-guide-and-methodology
