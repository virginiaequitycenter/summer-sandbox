# Pulled from lead_cville.Rmd

# Load libraries and data
library(tidyverse)
library(googlesheets4)
library(stargazer)
library(sf)
library(leaflet)

# library(mosaic)
# library(viridis)
# library(rgdal)

lead <- read_csv("data/lead_cville_tract.csv")

meta <- read_sheet("https://docs.google.com/spreadsheets/d/1nqm3DuVXD1ObbVe_deacvT7uSLdBXfQJo3mkbqDwrVo/edit#gid=1573436636", 
                   sheet="lead", gs4_deauth())


# Variables

glimpse(lead)

# Summary
lead %>% select(-c(FIP:tract)) %>% 
  select(where(~is.numeric(.x) && !is.na(.x))) %>% 
  as.data.frame() %>% 
  stargazer(., type = "text", title = "Summary Statistics", digits = 0,
            summary.stat = c("mean", "sd", "min", "median", "max"))

# Visuals
lead %>% select(FIP, averageburden) %>% 
  pivot_longer(-FIP, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() 

lead %>% select(FIP, percentburdened) %>% 
  pivot_longer(-FIP, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram(binwidth=2) 

lead %>%
  ggplot() +
  geom_point(aes(x=extremelyhighburden, y=percentburdened))

lead %>%
  ggplot() +
  geom_point(aes(x=lowburden, y=percentburdened))


# By AMI
lead %>% select(FIP, percent_0_30:percent_over_100) %>% 
  pivot_longer(-FIP, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram() + 
  facet_wrap(~measure, scales = "free")

# By own/rent
lead %>% select(FIP, percent_burdened_owners, percent_burdened_renters) %>% 
  pivot_longer(-FIP, names_to = "measure", values_to = "value") %>% 
  ggplot(aes(x = value, fill = measure)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~measure, scales = "free")

# Maps
cville_tracts <- readRDS("data/cville_tracts.RDS")

lead <- lead %>% dplyr::rename(GEOID = FIP)

cvlshapes <- merge(cville_tracts, lead, by = 'GEOID', all.x = T)

cvlshapes <- st_transform(cvlshapes, crs = 4326) # to WGS84, given error

# average burden
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$averageburden)

leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(averageburden),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$tract, "<br>",
                             "Avg. Burden: ", round(cvlshapes$averageburden, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$averageburden,
            title = "Average Energy Burden", opacity = 0.7)


# average expenditure
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$avg_hh_exp)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(avg_hh_exp),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$tract, "<br>",
                             "Avg. Household Energy Expenditures: ", round(cvlshapes$avg_hh_exp, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$avg_hh_exp,
            title = "Average Yearly Household Energy Expenditures", opacity = 0.7)

# Number of energy burdened
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$numberburdened)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(numberburdened),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$tract, "<br>",
                             "Number Burdened: ", round(cvlshapes$numberburdened, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$numberburdened,
            title = "Number of Energy Burdened Households", opacity = 0.7)

# Percent of Energy Burdened Households
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$percentburdened)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(percentburdened),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$tract, "<br>",
                             "Pct. Burdened: ", round(cvlshapes$percentburdened, 2), "<br>",
                             "Pct. Burdened - 0-30% AMI: ", round(cvlshapes$percent_0_30, 2), "<br>",
                             "Pct. Burdened - 30-60% AMI: ", round(cvlshapes$percent_30_60, 2), "<br>",
                             "Pct. Burdened - 60-80% AMI: ", round(cvlshapes$percent_60_80, 2), "<br>",
                             "Pct. Burdened - 80-100% AMI: ", round(cvlshapes$percent_80_100, 2), "<br>",
                             "Pct. Burdened - 100%+ AMI: ", round(cvlshapes$percent_over_100, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$percentburdened,
            title = "Percent of Energy Burdened Households", opacity = 0.7)

# Percent of Energy Burdened Rented Households
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$percent_burdened_renters)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(percent_burdened_renters),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$tract, "<br>",
                             "Pct. of Renters Burdened: ", round(cvlshapes$percent_burdened_renters, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$percent_burdened_renters,
            title = "Percent of Energy Burdened Rented Households", opacity = 0.7)

# Percent of Energy Burdened Owned Households
pal <- colorNumeric("plasma", reverse = TRUE, domain = cvlshapes$percent_burdened_owners)
leaflet(cvlshapes) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = cvlshapes,
              fillColor = ~pal(percent_burdened_owners),
              weight = 1,
              opacity = 1,
              color = "white",
              fillOpacity = 0.6,
              highlight = highlightOptions(weight = 2, fillOpacity = 0.8, bringToFront = T),
              popup = paste0("Tract Number: ", cvlshapes$tract, "<br>",
                             "Pct. of Owners Burdened: ", round(cvlshapes$percent_burdened_owners, 2))) %>%
  addLegend("bottomright", pal = pal, values = cvlshapes$percent_burdened_owners,
            title = "Percent of Energy Burdened Owned Households", opacity = 0.7)

# Important Notes
