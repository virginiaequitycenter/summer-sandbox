library(tidyverse)

#' Cleaning up the data, restricting the sample to only local areas
wac = wac_all %>% 
  filter(cty==51540 | cty==51003 | cty==51065 | cty==51079 | cty==51109 | cty==51125)

#' Breaking the sample down even further to create datasets for each area
wac_cville = wac %>% 
  filter(cty==51540)

wac_albemarle = wac %>% 
  filter(cty==51003)

wac_fluvanna = wac %>% 
  filter(cty==51065)

wac_greene = wac %>% 
  filter(cty==51079)

wac_louisa = wac %>% 
  filter (cty==51109)

wac_nelson = wac %>% 
  filter(cty==51125)


#' Some interesting summary statistics
wac %>% 
  group_by(ctyname) %>% 
  summarize(mean(ce01), mean(ce02), mean(ce03))

wac %>% 
  group_by(ctyname) %>% 
  summarize(mean(ca01), mean(ca02), mean(ca03))


#' Graphs to show trends over time (how has the number of jobs changed?)
ggplot(wac_cville) +
  geom_line(aes(x=year, y=c000)) +
  facet_wrap(~trctname)

wac_cville %>% 
  filter(year>2008) %>%
  group_by(trctname) %>% 
  ggplot() +
    geom_line(aes(x=year, y=cr02, color=trctname))

#' Graph to see how the number of jobs has varied by race and by year 
#' within one census tract
wac_cville %>% 
  filter(year>2008, trctname=="2.01 (Charlottesville city, VA)") %>%
ggplot() +
  geom_line(aes(x=year, y=cr01, color='white')) +
  geom_line(aes(x=year, y=cr02, color='black')) + 
  geom_line(aes(x=year, y=cr03, color='american indian or alaska native')) + 
  geom_line(aes(x=year, y=cr04, color='asian')) +
  geom_line(aes(x=year, y=cr05, color='native hawaiian or other pacific islander')) + 
  geom_line(aes(x=year, y=cr07, color='two or more race groups')) +
  labs(x="year", y="number of jobs", 
  title="Number of jobs by race (Charlottesville 2.01)")

#' Cleaning up the previous graph a little bit so it's easier to read
wac_cville %>% 
  filter(year>2008, trctname=="2.01 (Charlottesville city, VA)") %>%
  ggplot() +
  geom_line(aes(x=year, y=cr01, color='white')) +
  geom_line(aes(x=year, y=cr02, color='black')) + 
  geom_line(aes(x=year, y=cr04, color='asian')) + 
  geom_line(aes(x=year, y=(cr03+cr05+cr07), color='other')) +
  labs(x="year", y="number of jobs", 
  title="Number of jobs by race (Charlottesville 2.01)")

#' This could easily be repeated using any other census tract
#' However, the problem is that we don't know what the makeup of these tracts are, 
#' so maybe there is a disparity because the census tract is overwhelmingly white.