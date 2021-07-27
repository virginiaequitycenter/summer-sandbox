# Exploration of the 2020 EJSCREEN data for the eastern shore region

library(tidyverse)
library(leaflet)
library(sf)
library(stargazer)


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
eastern_shore <- virginia %>%
 filter(ID%in%510010901001:510019902000 |
        ID%in%511319301001:511319901000)

# Some spatial explorations
# Load shapefile
eastshore_blkgps <- readRDS("../eastern_shore_collection/data/eastshore_blkgps.RDS")
eastern_shore <- eastern_shore %>% rename(GEOID = ID)

eastshore_shapes <- merge(eastshore_blkgps, eastern_shore, by = 'GEOID', all.x = T)
eastshore_shapes <- st_transform(eastshore_shapes, crs = 4326) # to WGS84, given error

# Spatial distribution for proximity to treatment storage and disposal facilities (PTSDF)
pal <- colorNumeric("plasma", reverse = TRUE, domain = eastshore_shapes$PTSDF)
leaflet(eastshore_shapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = eastshore_shapes,
              fillColor = ~pal(PTSDF),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", eastshore_shapes$GEOID, "<br>",
                             "Proximity to TSDF: ", eastshore_shapes$T_PTSDF)) %>%
  addLegend("bottomright", pal = pal, values = eastshore_shapes$PTSDF,
            title = "Proximity to TSDF", opacity = 0.7)


# Spatial distribution for proximity to traffic (PTRAF)
pal <- colorNumeric("plasma", reverse = TRUE, domain = eastshore_shapes$PTRAF)
leaflet(eastshore_shapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = eastshore_shapes,
              fillColor = ~pal(PTRAF),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", eastshore_shapes$GEOID, "<br>",
                             "Proximity to traffic: ", eastshore_shapes$T_PTRAF)) %>%
  addLegend("bottomright", pal = pal, values = eastshore_shapes$PTRAF,
            title = "Traffic Proximity", opacity = 0.7)

# Spatial distribution of PM2.5 percentile
pal <- colorNumeric("plasma", reverse = TRUE, domain = eastshore_shapes$P_PM25)
leaflet(eastshore_shapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = eastshore_shapes,
              fillColor = ~pal(P_PM25),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", eastshore_shapes$GEOID, "<br>",
                             "PM2.5 Percentile: ", eastshore_shapes$P_PM25)) %>%
  addLegend("bottomright", pal = pal, values = eastshore_shapes$P_PM25,
            title = "PM2.5 Percentiles", opacity = 0.7)

# Spatial distribution of PM2.5
pal <- colorNumeric("plasma", reverse = TRUE, domain = eastshore_shapes$PM25)
leaflet(eastshore_shapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = eastshore_shapes,
              fillColor = ~pal(PM25),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("FIPS Code: ", eastshore_shapes$GEOID, "<br>",
                             "PM2.5 Level: ", eastshore_shapes$PM25)) %>%
  addLegend("bottomright", pal = pal, values = eastshore_shapes$PM25,
            title = "PM2.5 Concentrations", opacity = 0.7)

# Summary Table
eastern_shore %>% select(-c(GEOID, STATE_NAME, B_DSLPM:T_PM25, NPL_CNT, TSDF_CNT)) %>%
  select(where(~is.numeric(.x) && !is.na(.x))) %>%
  as.data.frame() %>%
  stargazer(., type = "text", title = "Summary Statistics", digits = 0,
            summary.stat = c("mean", "sd", "min", "median", "max"))

# Visual Distributions
# Correlation matrix
correlation <- eastern_shore %>% 
  select(PRE1960PCT:PM25)
num_correlation <- cor(correlation, use = "complete.obs")
num_correlation <- round(num_correlation, digits = 2)
corrplot(num_correlation, type = {"upper"}, method = "shade", shade.col = NA, tl.col = "black", diag = F, addCoef.col = "black")

# Ozone vs. PM25
eastern_shore %>%
  ggplot() +
  geom_point(aes(x=OZONE, y=PM25))

eastern_shore %>%
  ggplot() +
  geom_point(aes(x=P_OZONE, y=P_PM25))

# Proximity to traffic vs. air toxics cancer risk
eastern_shore %>%
  ggplot() +
  geom_point(aes(x=PTRAF, y=CANCER))

eastern_shore %>%
  ggplot() +
  geom_point(aes(x=P_PTRAF, y=P_CANCR))

# Proximity to traffic vs. diesel particulate matter level
eastern_shore %>%
  ggplot() +
  geom_point(aes(x=PTRAF, y=DSLPM))

eastern_shore %>%
  ggplot() +
  geom_point(aes(x=P_PTRAF, y=P_DSLPM))

# PM2.5 vs. diesel particulate matter level
eastern_shore %>%
  ggplot() +
  geom_point(aes(x=PM25, y=DSLPM))

eastern_shore %>%
  ggplot() +
  geom_point(aes(x=P_PM25, y=P_DSLPM))

# Interestingly, there is not really a correlation between any of these metrics

# Comparison to PM2.5 data
# Create tract variable
eastern_shore <- eastern_shore %>%
  mutate(tract = str_sub(GEOID, 1, 11))

# Load data
airquality_eastern <- read_csv("../eastern_shore_collection/data/airquality_eastern_tract.csv")

# Merge datasets
airquality_eastern <- airquality_eastern %>% rename(tract = trtid10)

pm25 <- eastern_shore %>%
  group_by(tract) %>%
  summarize(pm25_ejscreen = mean(PM25),
            pctile_pm25_ejscreen = mean(P_PM25))

pm25 <- merge(airquality_eastern, pm25, by = 'tract', all.x = T)

# Scatterplots to see if trends between the two metrics are consistent
pm25 %>%
  ggplot() +
  geom_point(aes(x = pm2_5_2016, y = pm25_ejscreen))

pm25 %>%
  ggplot() +
  geom_point(aes(x = percentile_2016, y = pctile_pm25_ejscreen))

# Looking at differences between the two sets of values
pm25 <- pm25 %>%
  mutate(pmdifference = pm25_ejscreen - pm2_5_2016,
         pctiledifference = pctile_pm25_ejscreen - percentile_2016)
# Interestingly, the eastern shore PM2.5 values are much closer together than the Cville ones
  # The percentages are still way different though