library(tidyverse)
library(sf)

# download
url <- "https://coast.noaa.gov/htdata/Inundation/SLR/SLRdata/VA/VA_EasternShore_slr_data_dist.zip"
download.file(url = url,
              destfile = "easternshore.zip",
              mode = "wb")

unzip("easternshore.zip", exdir = "easternshore")

# read (this seems to take awhile)
st_layers(dsn = "easternshore/VA_EasternShore_slr_final_dist.gdb/") # the available layers
es_slr <- st_read(dsn = "easternshore/VA_EasternShore_slr_final_dist.gdb/", layer = "VA_ES_slr_1ft")

# reading was really slow... but eventually loaded