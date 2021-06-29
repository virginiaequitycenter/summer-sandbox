library(tidyverse)
# devtools::install_github("ropensci/FedData")
library(FedData) # https://docs.ropensci.org/FedData/index.html
library(tigris)

# Get region to downlaod using tigris shapefiles
vacounties <- counties(state="VA") # download VA counties

cvl_alb <- vacounties %>%  # keep Cville and AlbCo as tests (ultimatley want more)
  filter(COUNTYFP %in% c("003", "540"))

sp::plot(cvl_alb[1], col="firebrick", border=NA) # the resulting polygon

# download land cover
land <-
  get_nlcd(
    template = cvl_alb,
    dataset = "Land_Cover",
    label = "Albemarle",
    year = 2016,
    landmass = "L48"
  )

sp::plot(land) # the resulting raster layer

# downlaod tree canopy
tree <- get_nlcd(
  template = cvl_alb,
  label = "Albemarle",
  dataset = "Tree_Canopy",
  year = 2016, 
  landmass = "L48"
)

sp::plot(tree)

# download impervious surfaces
imperv <- get_nlcd(
  template = cvl_alb,
  label = "Albemarle",
  dataset = "Impervious",
  year = 2016, 
  landmass = "L48"
)

sp::plot(imperv)



