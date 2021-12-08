# try bivariate choropleth

library(tidyverse)
library(sf)
library(leaflet)
library(leafem)
# install.packages("biscale")
library(biscale)
# install.packages("cowplot")
library(cowplot)
library(plotly)
library(viridis)
library(scales)
library(patchwork)

# DATA ----
places <- read_csv("data/cdcplaces_cville_tract.csv")
places <- places %>% 
  mutate(GEOID = as.character(locationname))
airquality <- read_csv("data/airquality_cville_tract.csv")
airquality <- airquality %>% 
  mutate(GEOID = as.character(gid))

tracts <- readRDS("data/cville_tracts.RDS")

df <- left_join(places, airquality, by = "GEOID")
df <- left_join(tracts, df)

# check
ggplot(df, aes(x = pm2_5_2016, y = Current_Asthma2018, alpha = 1/10, color = countyname)) +
  geom_point() +
  labs(x = "PM2.5 Percentile, 2016", y = "Percent with Asthma") +
  guides(size = "none", alpha = "none") +
  theme(legend.position = "bottom")

ggplot(df, aes(fill = Current_Asthma2018)) +
  geom_sf()


# USING BISCALE ----
# https://slu-opengis.github.io/biscale/articles/biscale.html
# create classes
df_bs <- bi_class(df, x = pm2_5_2016, y = Current_Asthma2018, style = "quantile", dim = 3)

map <- ggplot() +
  geom_sf(data = df_bs, 
          mapping = aes(fill = bi_class), color = "white", size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  labs(
    title = "Air Quality and Asthma"
  ) +
  bi_theme()

# legend
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "PM ",
                    ylab = "Asthma  ",
                    size = 8)

finalPlot <- ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.2, .55, 0.2, 0.2)
finalPlot

# viewing biscale palettes?

show_col(pal_grpink(3))

# MANUALLY WITH VIRIDIS ----
# http://lenkiefer.com/2017/04/24/bivariate-map/
d <- expand.grid(x=1:100,y=1:100)
ggplot(d, aes(x,y,fill=atan(y/x),alpha=x+y)) +
  geom_tile() +
  scale_fill_viridis() +
  theme(legend.position="none",
        panel.background=element_blank())

d <- expand.grid(x=1:3,y=1:3)
d<-merge(d,data.frame(x=1:3,xlabel=c("X low", "X middle","X high")),by="x")
d<-merge(d,data.frame(y=1:3,ylabel=c("Y low", "Y middle","Y high")),by="y")

g.legend <-
  ggplot(d, aes(x,y,fill=atan(y/x),alpha=x+y,label=paste0(xlabel,"\n",ylabel))) +
  geom_tile() +
  geom_text(alpha=1) +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position="none",
        panel.background=element_blank(),
        plot.margin=margin(t=10,b=10,l=10)) +
  geom_segment(aes(x=1, xend = 3 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=1, yend = 3), size=1.5,
               arrow = arrow(length = unit(0.6,"cm"))) 

# compute quantiles (dividing into 3rds) 
as <- quantile(df$Current_Asthma2018, c(0.33,0.66,1))
pm <- quantile(df$pm2_5_2016, c(0.33,0.66,1))

df <- df %>% 
  mutate(y = ifelse(Current_Asthma2018 < as[1], 1,
                  ifelse(Current_Asthma2018 < as[2], 2, 3)),
        x = ifelse(pm2_5_2016 < pm[1], 1,
                   ifelse(pm2_5_2016 < pm[2], 2, 3))
        )  

ggplot(df, aes(x = pm2_5_2016, y = Current_Asthma2018,
                    color = atan(y/x))) +
  geom_point(size=1) +  
  guides(alpha = "none", color = "none") +
  geom_hline(yintercept = as, color = "gray20", linetype = 2) +
  geom_vline(xintercept = pm, color = "gray20", linetype = 2) +
  scale_color_viridis(name = "Color scale") +
  theme_minimal() +
  theme(plot.caption=element_text(size = 9, hjust=0),
        panel.grid=element_blank()) +
  labs(x = "Particulate Matter",
       y = "Asthma Prevalence") 

# map 
map2 <- ggplot(df) +
  geom_sf(aes(fill = atan(y/x), alpha = x+y)) +
  scale_fill_viridis() +
  guides(alpha = "none", fill = "none") +
  theme_void()

g.legend2 <- 
  ggplot(d, aes(x, y, fill = atan(y/x), alpha = x+y)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_void() +
  theme(legend.position = "none",
        axis.title = element_text(size=10),
        axis.title.y = element_text(angle = 90),
        panel.background = element_blank(),
        plot.margin = margin(t=10,b=10,l=10)) +
  theme(axis.title = element_text(color="black") )+ 
  labs(x = "Particulate Matter",
       y = "Asthma Prevalence") +
  geom_segment(aes(x = 1, xend = 3 , y = 0, yend = 0), size = 1,
               arrow = arrow(length = unit(0.6,"cm"))) +
  geom_segment(aes(x = 0, xend = 0 , y = 1, yend = 3), size = 1,
               arrow = arrow(length = unit(0.6,"cm"))) 

ggdraw() +
   draw_plot(map2, 0, 0, 1, 1) +
   draw_plot(g.legend2, 0.7, .7, 0.2, 0.2)
 

# MANUALLY WITH DEFINED PALETTE ----
bipal <- c("#e8e8e8", "#dfd0d6", "#be64ac", # A-1, A-2, A-3,
           "#ace4e4", "#a5add3", "#8c62aa", # B-1, B-2, B-3
           "#5ac8c8", "#5698b9", "#3b4994") # C-1, C-2, C-2

bipal2 <- c("#be64ac", "#8c62aa", "#3b4994",
            "#dfd0d6", "#a5add3", "#5698b9",
            "#e8e8e8", "#ace4e4", "#5ac8c8") 
            # A-3, B-3, C-3
            # A-2, B-2, C-2
            # A-1, B-1, C-1
# https://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/

show_col(bipal2)

# create class
df <- df %>%
  mutate(asthma = ntile(Current_Asthma2018, 3),
         pm25_2016 = ntile(pm2_5_2016, 3)) %>%
  mutate(pm25_2016 = if_else(pm25_2016 == 1, 'A', 
                          if_else(pm25_2016 == 2, 'B', 'C')),
         biclass = paste0(pm25_2016, asthma)) 

count(df, biclass)

ggplot(df) + 
  geom_sf(aes(fill = biclass)) + 
  scale_fill_manual(values = bipal)

# add biclass labels to verify
tract_points <- st_centroid(df)
tract_points <- tract_points %>% 
  mutate(lat = st_coordinates(.)[,1],
         lon = st_coordinates(.)[,2])

ggplot(df) + 
  geom_sf(aes(fill = biclass)) + 
  scale_fill_manual(values = bipal) +
  geom_text(data = tract_points,
            aes(x = lat, y = lon, label = biclass), 
            color = "white")
# Yes
# Manually colored map with biscale-generated legend
map3 <- ggplot(df) + 
  geom_sf(aes(fill = biclass)) + 
  scale_fill_manual(values = bipal, guide = "none") +
  theme_void()

ggdraw() +
  draw_plot(map3, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, 0.7, 0.3, 0.3)

# BUT SEE HERE for better palette choices
# https://www.r-bloggers.com/2020/08/how-to-choose-a-bivariate-color-palette/

# MANUALLY PRODUCED LEGEND ----
# https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/
# possibly originally here: https://bluegreenlabs.org/post/map-building-3/
bivariate_color_scale <- tibble(
  "3-3" = "#3b4994", # high inequality, high income
  "2-3" = "#8c62aa",
  "1-3" = "#be64ac", # low inequality, high income
  "3-2" = "#5698b9",
  "2-2" = "#a5add3", # medium inequality, medium income
  "1-2" = "#dfd0d6",
  "3-1" = "#5ac8c8", # high inequality, low income
  "2-1" = "#ace4e4",
  "1-1" = "#e8e8e8" # low inequality, low income
) %>%
  gather("group", "fill")

bivariate_color_scale %<>%
  separate(group, into = c("pm2.5", "asthma"), sep = "-") %>%
  mutate(pm2.5 = as.integer(pm2.5),
         asthma = as.integer(asthma))

legend4 <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = pm2.5,
      y = asthma,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = expression("Higher pm2.5" %->%  ""),
       y = expression("Higher asthma" %->% "")) +
  theme_void() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6),
    axis.title.y = element_text(angle = 90)
  ) +
  # quadratic tiles
  coord_fixed()
legend4

ggdraw() +
  draw_plot(map3, 0, 0, 1, 1) +
  draw_plot(legend4, 0.65, 0.7, 0.2, 0.2)

# different approach? https://stackoverflow.com/questions/32749889/make-a-2d-legend-for-a-plot-bi-variate-choropleth-maps

# And more continuous color scale approach here: https://www.datalorax.com/post/creating-bivariate-color-palettes/

# WITH PATCHWORK ----
map3 + inset_element(legend, left = 0.7, bottom = 0.7, right = 1, top = 1)
map3 + inset_element(legend4, left = 0.7, bottom = 0.7, right = 1, top = 1)


# LEAFLET CHOROPLETH ----
df_4326 <- st_transform(df, 4326)
pal <- colorNumeric("Blues", domain = df_4326$Current_Asthma2018)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = df_4326,
              fillColor = ~pal(Current_Asthma2018),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.6,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", df_4326$GEOID, "<br>",
                             "Percent with asthma: ", 
                             df_4326$Current_Asthma2018)) %>% 
  leaflet::addLegend("bottomright", pal = pal, values = (df_4326$Current_Asthma2018), 
                     title = "Prevalence <br> of Asthma", opacity = 0.7)

# of interest: https://www.r-bloggers.com/2021/02/bivariate-dasymetric-map/

# LEAFLET BICHOROPLETH ----
factpal <- colorFactor(bipal, domain = df_4326$biclass)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = df_4326,
              fillColor = ~factpal(biclass),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", df_4326$GEOID, "<br>",
                             "Percent with asthma: ", 
                             df_4326$Current_Asthma2018, "<br>",
                             "PM2.5 rate: ", 
                             round(df_4326$pm2_5_2016,1)))

# make legend an image
ggsave(plot = legend4, filename = "images/bivariate_legend.svg",
       width = 1, height = 1)

leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = df_4326,
              fillColor = ~factpal(biclass),
              weight = 1,
              opacity = 1,
              color = "white", 
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 2,
                fillOpacity = 0.8,
                bringToFront = T),
              popup = paste0("GEOID: ", df_4326$GEOID, "<br>",
                             "Percent with asthma: ", 
                             df_4326$Current_Asthma2018, "<br>",
                             "PM2.5 rate: ", 
                             round(df_4326$pm2_5_2016,1))) %>% 
  addLogo("images/bivariate_legend.svg", src = "local",
          position = "topleft", width = 100, height = 100,
          alpha = 0.8)

