library(tidyverse)
library(mosaic)

ami <- read.csv("VA AMI State, Counties, Cities 2018.csv")

#' Create new datasets for the whole region and each county individually
cville_area = filter(ami, (FIP==51540 | FIP==51003 | FIP==51065 | FIP==51079 
                           | FIP==51109 | FIP==51125) & HINCP!="NA")
cville = filter(ami, FIP==51540 & HINCP!="NA")
albemarle = filter(ami, FIP==51003 & HINCP!="NA")
fluvanna = filter(ami, FIP==51065 & HINCP!="NA")
greene = filter(ami, FIP==51079 & HINCP!="NA")
louisa = filter(ami, FIP==51109 & HINCP!="NA")
nelson = filter(ami, FIP==51125 & HINCP!="NA")

#' Average household income in Charlottesville by income range
cville %>% 
  group_by(AMI68) %>% 
    summarize(mean(HINCP))

#' Average expenditures on each type of energy by income range
cville %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_elep = mean(ELEP), 
            mean_gasp = mean(GASP),
            mean_fulp = mean(FULP))

#' Average expenditures for all energy by income range
#' Then a bar graph depicting this
cville %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_all_energy = mean(ELEP+GASP+FULP)) %>% 
  ggplot() +
    geom_col(aes(x=(AMI68), y=(mean_all_energy))) +
    labs(x="Income range (compared to area median income)",
        y="average energy expenditures",
        title="Average energy expenditures by income range (Charlottesville)")

albemarle %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_all_energy = mean(ELEP+GASP+FULP)) %>% 
  ggplot() +
  geom_col(aes(x=(AMI68), y=(mean_all_energy))) +
  labs(x="Income range (compared to area median income)",
       y="average energy expenditures",
       title="Average energy expenditures by income range (Albemarle County)")


fluvanna %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_all_energy = mean(ELEP+GASP+FULP)) %>% 
  ggplot() +
  geom_col(aes(x=(AMI68), y=(mean_all_energy))) +
  labs(x="Income range (compared to area median income)",
       y="average energy expenditures",
       title="Average energy expenditures by income range (Fluvanna)")

greene %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_all_energy = mean(ELEP+GASP+FULP)) %>% 
  ggplot() +
  geom_col(aes(x=(AMI68), y=(mean_all_energy))) +
  labs(x="Income range (compared to area median income)",
       y="average energy expenditures",
       title="Average energy expenditures by income range (Greene)")

louisa %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_all_energy = mean(ELEP+GASP+FULP)) %>% 
  ggplot() +
  geom_col(aes(x=(AMI68), y=(mean_all_energy))) +
  labs(x="Income range (compared to area median income)",
       y="average energy expenditures",
       title="Average energy expenditures by income range (Louisa)")

nelson %>% 
  group_by(AMI68) %>% 
  filter(ELEP!="NA" & GASP!="NA" & FULP!="NA") %>% 
  summarize(mean_all_energy = mean(ELEP+GASP+FULP)) %>% 
  ggplot() +
  geom_col(aes(x=(AMI68), y=(mean_all_energy))) +
  labs(x="Income range (compared to area median income)",
       y="average energy expenditures",
       title="Average energy expenditures by income range (Nelson)")

#' Scatterplot of the relationship between household income and energy expenditure
cville %>% 
  ggplot() +
    geom_point(aes(x=HINCP, y=(ELEP+GASP+FULP), color=AMI68)) +
    labs(x="Household income",
         y="Total energy expenditures",
         title="Energy expenditures by income (Charlottesville)")

cville_area %>% 
  ggplot() +
  geom_point(aes(x=HINCP, y=(ELEP+GASP+FULP), color=AMI68)) +
  facet_wrap(~LOCATION) +
  labs(x="Household income",
       y="Total energy expenditures",
       title="Energy expenditures by income")
#' Both these scatterplots are hard to read because there are so many points.
#' Is there some equivalent for Stata's binscatter function on R?

#' Same graph as above, but faceting by AMI68 instead of location
cville_area %>% 
  ggplot() +
  geom_point(aes(x=HINCP, y=(ELEP+GASP+FULP), color=LOCATION)) +
  facet_wrap(~AMI68) +
  labs(x="Household income",
       y="Total energy expenditures",
       title="Energy expenditures by income")

cville %>% 
  ggplot() +
  geom_point(aes(x=HINCP, y=(ELEP+GASP+FULP), color=AMI68)) +
  facet_wrap(~AMI68) +
  labs(x="Household income",
       y="Total energy expenditures",
       title="Energy expenditures by income (Charlottesville)")
#' These graphs are not quite as helpful
#' They show much less of a relationship between income and energy expenditures
#' This is likely because they are broken up by income already

#' Graphing energy expenditure by year built
cville_area %>% 
  ggplot() +
  geom_col(aes(x=YBL6, y=(ELEP+GASP+FULP))) +
  facet_wrap(~LOCATION) +
  labs(x="Year built",
       y="Total energy expenditures",
       title="Energy expenditures by year built") +
  theme(axis.text = element_text(size=6))
