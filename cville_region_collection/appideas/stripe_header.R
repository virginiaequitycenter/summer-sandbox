# could warming stripes with different palette make a good header
#   for the app?


library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(viridis)

noaa <- read_csv("data/noaa_cville_county.csv")

cville_yr <- select(noaa, county, year, Avg_Tempmax) %>% 
  mutate(date = str_c(year, "01-01", sep = "-") %>% ymd()) %>% 
  filter(county == "540")

theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(vjust = 3, size = 10),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, face = "bold"),
        plot.caption = element_text(size = 10) 
        
  )


col_strip1 <- brewer.pal(11, "RdBu")
col_strip2 <- viridis_pal(option = "inferno")(9)

ggplot(cville_yr,
       aes(x = date, y = 1, fill = Avg_Tempmax))+
  geom_tile()+
  scale_x_date(date_breaks = "5 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip2))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Greene County Average Yearly Maximum Temperature 1895-2020",
       caption = "Data: NOAA Surface Temperature Analysis")+
  theme_strip


pal <- function(col, border = "light grey", ...){
  n <- length(col)
  plot(0, 0, type="n", xlim = c(0, 1), ylim = c(0, 1),
       axes = FALSE, xlab = "", ylab = "", ...)
  rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
}

pal(viridis_pal(option = "turbo")(15)) # sequential
