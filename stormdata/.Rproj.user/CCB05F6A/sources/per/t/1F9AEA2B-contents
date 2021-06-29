library(tidyverse)
library(rvest)

# Pull from website ----
# 1. Get list of files to download
# Scrape filenames
url = "https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
dl <- read_html(url)
filenames <- dl %>% 
  html_elements("a") %>% 
  html_attr("href")

# just the "details" files (others are "locations" and "fatalities")
filenames <- filenames[str_detect(filenames, "details")]

# 2. Download
# try one at a time
download.file(url = url = paste(url, filenames[72], sep = ""),
              destfile = paste(getwd(), "/details/", filenames[72], sep = ""),
              mode = "wb")
# did this manually for records 69-72 to test reading the data below (sometimes didn't work, though)

# multiple at a time (select only a few)
# to make this work, I had to go to 
# Tools > Global Options > Packages, and unselect "Use Internet Explorer library/proxy for HTTP".
filenames3 <- filenames[69:71]

# download 
for (f in filenames3) {
  download.file(url = paste(url, f, sep = ""), 
                destfile = paste(getwd(), "/details/", f, sep=""),
                mode = "wb")
}

# 3. Unzip and read
# one at a time
test2021 <- read_csv("details/StormEvents_details-ftp_v1.0_d2021_c20210607.csv.gz")

# test on a few (having downloaded several individually above)
files <- list.files("details/", pattern = ".gz$")
test <- map_df(paste0("details/", files), read_csv)
# this assumes the column names are the same for each file; not sure that's true... 
# (but a problem for another day)

# Use noaastormevents package!!!
# install.packages("noaastormevents")
library(noaastormevents)

# CRAN: https://cran.r-project.org/web/packages/noaastormevents/index.html
# Vignettes: https://cran.r-project.org/web/packages/noaastormevents/vignettes/noaastormevents.html
# Vignettes: https://cran.r-project.org/web/packages/noaastormevents/vignettes/details.html

