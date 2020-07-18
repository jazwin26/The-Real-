#Mapping Points of Urban Butterfly Sightings
#Jazmyn Winzer 
#jiwinzer@email.arizona.edu 
#2020/6/22

#Clean up workspace and remove unwanted variables from previous projects
rm(list = ls())

#loading in data
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggsn)
library(mapproj)

eBut <- read_csv("eBut.Data.csv")
view(eBut)
iNat <- read_csv("iNaturalist.csv")
view(iNat)

#making data frames that only show unique lat/longs for every site
latlongs <- unique(eBut[, c("Longitude", "Latitude")])

latlongs_iNat <- unique(iNat[, c("Longitude", "Latitude")])

latlongs_TohonoChul <- data.frame("Longitude" = -110.9817, "Latitude" = 32.3393, "Source" = "Tohono Chul")

#making a single data frame from both sites
#Add a source column to each data set

latlongs$Source <- "eButterfly"
latlongs_iNat$Source <- "iNaturalist"

#combine the two data sets 
latlong_both <- rbind(latlongs, latlongs_iNat, latlongs_TohonoChul)

#making the square for map bounds
map_bounds <- c(floor(min(latlong_both$Longitude)),
                floor(min(latlong_both$Latitude)),
                ceiling(max(latlong_both$Longitude)),
                ceiling(max(latlong_both$Latitude)))
names(map_bounds) <- c("left", "bottom", "right", "top")

#we are pulling a map template from R that will have our box on it
tu_map <- get_map(location = map_bounds,
                  source = "stamen",
                  maptype = "terrain-lines",
                  color = "bw")

bounds <- data.frame(x = c(rep(min(latlong_both$Longitude), times = 2), 
                           rep(max(latlong_both$Longitude), times = 2)),
                     y = c(min(latlong_both$Latitude), 
                           max(latlong_both$Latitude), max(latlong_both$Latitude), min(latlong_both$Latitude)))

#so here we have dummy data because that is how ggsn works
latlong_scale <- latlong_both[, 1:2]
colnames(latlong_scale) <- c("long", "lat")
latlong_scale[nrow(latlong_scale) + 1, ] <- list("long" = -111.235, "lat" = 32.041)

#now we can finally visualize the map
both_map <- ggmap(tu_map) +
  geom_polygon(data = bounds,
               mapping = aes(x = x, y = y),
               fill = "#ffffff",
               color = "#dd2222",
               alpha = 0.45) +
  geom_point(data = latlong_both,
             mapping = aes(x = Longitude, y = Latitude, shape = Source, color = Source, fill = Source),
             size = 3) +
  scale_shape_manual(values = c(4, 21, 18)) +
  scale_color_manual(values = c("#1133ff", "#000000", "#008000")) +
  scale_fill_manual(values = c("#ffffff", "#ff8c1a", "#808080")) +
  scalebar(data = latlong_scale,
           transform = TRUE,
           dist = 10,
           dist_unit = "km",
           location = "bottomleft",
           st.bottom = TRUE,
           st.dist = 0.05,
           st.size = 4.5,
           model = "WGS84") +
  theme(legend.position = c(0.80, 0.15)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(xlim = c(min(latlong_both$Longitude) - 0.1, max(latlong_both$Longitude) + 0.1),
            ylim = c(min(latlong_both$Latitude) - 0.1, max(latlong_both$Latitude) + 0.1))
print(both_map)

