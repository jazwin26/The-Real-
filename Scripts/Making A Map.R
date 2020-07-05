#Mapping Points of Urban Butterfly Sightings
#Jazmyn Winzer 
#jiwinzer@email.arizona.edu 
#2020/6/22

#Clean up workspace, removing variables from memory
rm(list = ls())

#loading in data
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggsn)
library(mapproj)

eBut <- read_csv("Data/eBut.Data.csv")
# view(eBut)
iNat <- read_csv("Data/iNaturalist.csv")
# view(iNat)

#making data frames that only show unique lat/longs for every site
latlongs <- unique(eBut[, c("Longitude", "Latitude")])

latlongs_iNat <- unique(iNat[, c("Longitude", "Latitude")])

#making a single data frame from both sites
# Start by adding a Source column to each data set
latlongs$Source <- "Butterfly Walk"
latlongs_iNat$Source <- "inaturalist"
# Then combine the two data sets
latlong_both <- rbind(latlongs, latlongs_iNat)

#making the square for map bounds
map_bounds <- c(floor(min(latlong_both$Longitude)),
                floor(min(latlong_both$Latitude)),
                ceiling(max(latlong_both$Longitude)),
                ceiling(max(latlong_both$Latitude)))
names(map_bounds) <- c("left", "bottom", "right", "top")

# Download a map of the greater tucson area, based on the bounds of our data
tucson_map <- get_map(location = map_bounds,
                  source = "stamen",
                  maptype = "terrain-lines",
                  color = "bw")

# Create a data frame with x and y limits of the map, the data frame will have
# four rows and two columns:
# Row 1: bottom left limit
# Row 2: top left limit
# Row 3: bottom right limit
# Row 4: top right limit
bounds <- data.frame(x = c(rep(min(latlong_both$Longitude), times = 2), 
                           rep(max(latlong_both$Longitude), times = 2)),
                     y = c(min(latlong_both$Latitude), max(latlong_both$Latitude), 
                           max(latlong_both$Latitude), min(latlong_both$Latitude)))

# For the scale bar, we need to pass ggsn::scalebar a data frame with the data
# we used. However, we do not want the scale bar to cover up any of the points, 
# so we add one extra point (which will not be plotted) to "trick" 
# ggsn::scalebar into thinking our map is larger. In this new data frame, we 
# we have to set the Latitude and Longitude column names to "lat" and "long", 
# respectively, because those are required by ggsn::scalebar
latlong_scale <- latlong_both[, 1:2]
colnames(latlong_scale) <- c("long", "lat")
latlong_scale[nrow(latlong_scale) + 1, ] <- list(long = -111.253,
                                                 lat = 32.041)

city_labels <- data.frame(names = c("Tucson", "Tohono Chul"),
                          longitude = c(-110.9747, -110.9817),
                          latitude = c(32.2226, 32.3393),
                          sizes = c(7,7))

#now we can finally visualize the map
both_map <- ggmap(tucson_map) +
  geom_polygon(data = bounds,
               mapping = aes(x = x, y = y),
               fill = "#ffffff",
               color = "#dd2222",
               alpha = 0.45) +
  annotate("text",
           label = city_labels$names,
           x = city_labels$longitude,
           y = city_labels$latitude,
           size = city_labels$sizes,
           color = "black") +
  geom_point(data = latlong_both,
             mapping = aes(x = Longitude, 
                           y = Latitude, 
                           shape = Source, 
                           color = Source, 
                           fill = Source),
             size = 3) +
  scale_shape_manual(values = c(4, 21)) +
  scale_color_manual(values = c("#1133ff", "#000000")) +
  scale_fill_manual(values = c("#ffffff", "#ff8c1a")) +
  scalebar(data = latlong_scale,
           transform = TRUE,
           dist = 10, # units of 10 on the scale bar
           dist_unit = "km",
           location = "bottomleft",
           st.size = 4.5,
           st.bottom = TRUE,
           st.dist = 0.05,
           model = "WGS84") +
  theme(legend.position = c(0.80, 0.15)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(xlim = c(min(latlong_both$Longitude) - 0.1, max(latlong_both$Longitude) + 0.1),
            ylim = c(min(latlong_both$Latitude) - 0.1, max(latlong_both$Latitude) + 0.1))

print(both_map)
