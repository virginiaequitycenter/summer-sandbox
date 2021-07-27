# Exploration of the 2020 EJSCREEN data
# The 2020 data uses 2016 estimates for PM2.5, which makes it an okay comparison for the replication data

library(tidyverse)
library(leaflet)
library(sf)
library(stargazer)
library(corrplot) #for correlation matrix


# Pull EJSCREEN
url <- "https://gaftp.epa.gov/EJSCREEN/2020/EJSCREEN_2020_USPR.csv.zip"
download.file(url = url,
               destfile = paste(getwd(), "/", "ejscreen.zip", sep = ""),
               mode = "wb")

# unzip and read
unzip("ejscreen.zip", exdir = getwd())

# Load data
ejscreen <- read_csv("EJSCREEN_2020_USPR.csv")

# Get rid of demographic variables (since we don't need those)
ejscreen_clean <- ejscreen %>%
  select(-c("OBJECTID", "ACSTOTPOP":"PRE1960", "VULEOPCT":"DISPEO", "D_LDPNT_2":"D_PM25_2",
            "ST_ABBREV":"P_OVR64PCT", "P_VULEOPCT", "P_LDPNT_D2":"B_VULEOPCT", "B_LDPNT_D2":"T_VULEOPCT",
            "T_LDPNT_D2", "T_DSLPM_D2", "T_CANCR_D2", "T_RESP_D2", "T_PTRAF_D2", "T_PWDIS_D2",
            "T_PNPL_D2", "T_PRMP_D2", "T_PTSDF_D2", "T_OZONE_D2", "T_PM25_D2", "Shape_Length", "Shape_Area"))

# Filter to just areas of interest
virginia <- ejscreen_clean %>%
  filter(STATE_NAME=="Virginia")
cville_area <- virginia %>%
  filter(ID%in%510030101001:510030114004 |
           ID%in%510650201011:510650203003 |
           ID%in%510790301011:510790302004 |
           ID%in%511099501001:511099505003 |
           ID%in%511259501001:511259503004 |
           ID%in%515400002011:515400010003)

# Some spatial explorations
# Load shapefile
cville_blkgps <- readRDS("../cville_region_collection/data/cville_blkgps.RDS")
cville_area <- cville_area %>% rename(GEOID = ID)

cvilleshapes <- merge(cville_blkgps, cville_area, by = 'GEOID', all.x = T)
cvilleshapes <- st_transform(cvilleshapes, crs = 4326) # to WGS84, given error

# Spatial distribution for proximity to treatment storage and disposal facilities (PTSDF)
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvilleshapes$PTSDF)
leaflet(cvilleshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvilleshapes,
              fillColor = ~pal(PTSDF),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", cvilleshapes$GEOID, "<br>",
                             "Proximity to TSDF: ", cvilleshapes$T_PTSDF)) %>%
  addLegend("bottomright", pal = pal, values = cvilleshapes$PTSDF,
            title = "Proximity to TSDF", opacity = 0.7)


# Spatial distribution for proximity to traffic (PTRAF)
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvilleshapes$PTRAF)
leaflet(cvilleshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvilleshapes,
              fillColor = ~pal(PTRAF),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", cvilleshapes$GEOID, "<br>",
                             "Proximity to traffic: ", cvilleshapes$T_PTRAF)) %>%
  addLegend("bottomright", pal = pal, values = cvilleshapes$PTRAF,
            title = "Traffic Proximity", opacity = 0.7)

# Spatial distribution of PM2.5 percentile
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvilleshapes$P_PM25)
leaflet(cvilleshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvilleshapes,
              fillColor = ~pal(P_PM25),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", cvilleshapes$GEOID, "<br>",
                             "PM2.5 Percentile: ", cvilleshapes$P_PM25)) %>%
  addLegend("bottomright", pal = pal, values = cvilleshapes$P_PM25,
            title = "PM2.5 Percentiles", opacity = 0.7)
# I compared these percentiles to the ones from the air quality replication data and they are very different
  # The trends are not even the same - the Cville tracts are the highest percentile in the replication data,
    # but in this data the highest percentile is in Louisa County

# Spatial distribution of PM2.5
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvilleshapes$PM25)
leaflet(cvilleshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvilleshapes,
              fillColor = ~pal(PM25),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", cvilleshapes$GEOID, "<br>",
                             "PM2.5 Level: ", cvilleshapes$PM25)) %>%
  addLegend("bottomright", pal = pal, values = cvilleshapes$PM25,
            title = "PM2.5 Concentrations", opacity = 0.7)
# The trends on this metric are more similar to the replication data than on the percentile map
  # However, PM2.5 concentrations are still much higher in Louisa County on this map than on the replication

# Summary Table
cville_area %>% select(-c(GEOID, STATE_NAME, B_DSLPM:T_PM25, NPL_CNT, TSDF_CNT)) %>%
  select(where(~is.numeric(.x) && !is.na(.x))) %>%
  as.data.frame() %>%
  stargazer(., type = "text", title = "Summary Statistics", digits = 0,
            summary.stat = c("mean", "sd", "min", "median", "max"))

# Visual Distributions
# Correlation Matrix
correlation <- cville_area %>%
  select(PRE1960PCT:PM25)
num_correlation <- cor(correlation, use = "complete.obs")
num_correlation <- round(num_correlation, digits = 2)
corrplot(num_correlation, type = {"upper"}, method = "shade", shade.col = NA, tl.col = "black", diag = F, addCoef.col = "black")

# Ozone vs. PM25
cville_area %>%
  ggplot() +
  geom_point(aes(x=OZONE, y=PM25))

cville_area %>%
  ggplot() +
  geom_point(aes(x=P_OZONE, y=P_PM25))

# Proximity to traffic vs. air toxics cancer risk
cville_area %>%
  ggplot() +
  geom_point(aes(x=PTRAF, y=CANCER))

cville_area %>%
  ggplot() +
  geom_point(aes(x=P_PTRAF, y=P_CANCR))
# Interestingly, there is a correlation in the percentiles, but not on the actual levels

# Proximity to traffic vs. diesel particulate matter level
cville_area %>%
  ggplot() +
  geom_point(aes(x=PTRAF, y=DSLPM))

cville_area %>%
  ggplot() +
  geom_point(aes(x=P_PTRAF, y=P_DSLPM))
# As above, there is a correlation in the percentiles, but not on the actual levels

# PM2.5 vs. diesel particulate matter level
cville_area %>%
  ggplot() +
  geom_point(aes(x=PM25, y=DSLPM))

cville_area %>%
  ggplot() +
  geom_point(aes(x=P_PM25, y=P_DSLPM))
# These metrics are not super correlated, either on actual levels or on percentiles

# PM2.5, ozone, and NATA indicators are measured at the census tract level
  # So each block group within that census tract is assigned the same value
  # That means this may not be super useful for a lot of these measures (especially PM2.5)


# Comparison to PM2.5 data
# Create tract variable
cville_area <- cville_area %>%
  mutate(tract = str_sub(GEOID, 1, 11))

# Load data
airquality <- read_csv("/Users/marisalemma/Desktop/Equity Center/summer-sandbox/cville_region_collection/data/airquality_cville_tract.csv")
# airquality <- read_csv("../cville_region_collection/data/airquality_cville_tract.csv")
airquality_eastern <- read_csv("/Users/marisalemma/Desktop/Equity Center/summer-sandbox/eastern_shore_collection/data/airquality_eastern_tract.csv")

# Merge datasets
airquality <- airquality %>% rename(tract = trtid10)

pm25 <- cville_area %>%
  group_by(tract) %>%
  summarize(pm25_ejscreen = mean(PM25),
            pctile_pm25_ejscreen = mean(P_PM25))

pm25 <- merge(airquality, pm25, by = 'tract', all.x = T)

# Scatterplots to see if trends between the two metrics are consistent
pm25 %>%
  ggplot() +
  geom_point(aes(x = pm2_5_2016, y = pm25_ejscreen))
# There is very little correlation between these two metrics

pm25 %>%
  ggplot() +
  geom_point(aes(x = percentile_2016, y = pctile_pm25_ejscreen))
# There is also very little correlation between these two metrics. I am not sure why this is the case

# Looking at differences between the two sets of values
pm25 <- pm25 %>%
  mutate(pmdifference = pm25_ejscreen - pm2_5_2016,
         pctiledifference = pctile_pm25_ejscreen - percentile_2016)
# Both variables are all over the place (some are positive, some are negative)
  # Some of the values for pctiledifference are very large - I'm not sure why this difference exists


# Some questions that all this exploration raises:
  # What do the NAs represent? Are they 0s?
    # NAs only exist for PTRAF and PWDIS
    # In the case of traffic proximity, would that mean that there are no roads in the block group?
    # I think NAs are probably 0s but I'm not completely sure
  # Why are there such large differences between the replication data estimates and the EJSCREEN estimates?
    # Specifically for PM2.5, there are very large differences and I don't understand why
