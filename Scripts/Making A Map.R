#Mapping Points of Urban Butterfly Sightings
#Jazmyn Winzer 
#jiwinzer@email.arizona.edu 
#2020/6/22

rm(list = ls())

#loading in data
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggsn)

eBut <- read_csv("eBut.Data.csv")
view(eBut)
iNat <- read_csv("iNaturalist.csv")
view(iNat)

#making data frames that only show unique lat/longs for every site
latlongs <- unique(eBut[, c("Longitude", "Latitude")])

latlongs_iNat <- unique(iNat[, c("Longitude", "Latitude")])

#making a single data frame from both sites
latlong_both <- rbind(latlongs, latlongs_iNat)
latlong_both$Source <- "inaturalist"
latlong_both$Source[1:nrow(latlongs)] <- "Butterfly Walk"

#making the square for map bounds
map_bounds <- c(floor(min(eBut$Longitude)),
                floor(min(eBut$Latitude)),
                ceiling(max(eBut$Longitude)),
                ceiling(max(eBut$Latitude)))
names(map_bounds) <- c("left", "bottom", "right", "top")

#truthfully I have no idea what this does which may be part of the problem 
#i think what is happening is we are pulling a map template from R that will have our box on it
la_map <- get_map(location = map_bounds,
                  source = "stamen",
                  maptype = "terrain-lines",
                  color = "bw")

bounds <- data.frame(x = c(rep(min(latlongs$Longitude), times = 2), rep(max(latlongs$Longitude), times = 2)),
                     y = c(min(latlongs$Latitude), max(latlongs$Latitude), max(latlongs$Latitude), min(latlongs$Latitude)))

#so here we have dummy data because that is how ggsn works
latlong_scale <- latlongs
colnames(latlong_scale) <- c("long", "lat")
latlong_scale[nrow(latlong_scale) + 1, ] <- list("long" = -118.425, "lat" = 33.84)

city_labels <- data.frame(names = c("Tucson", "Tohono Chul"),
                          longitude = c(-110.9747, -110.9817),
                          latitude = c(32.2226, 32.3393),
                          sizes = c(7,7))

#now we can finally visualize the map
both_map <- ggmap(la_map) +
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
             mapping = aes(x = Longitude, y = Latitude, shape = Source, color = Source, fill = Source),
             size = 3) +
  scale_shape_manual(values = c(4, 21)) +
  scale_color_manual(values = c("#1133ff", "#000000")) +
  scale_fill_manual(values = c("#ffffff", "#ff8c1a")) +
  scalebar(data = latlong_scale,
           dist = 5,
           location = "bottomleft",
           dd2km = TRUE,
           height = 0.03,
           st.dist = 0.04,
           st.size = 4.5,
           model = "WGS84") +
  theme(legend.position = c(0.80, 0.11)) +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map(xlim = c(min(eBut$Longitude) - 0.1, max(eBut$Longitude) + 0.1),
            ylim = c(min(eBut$Latitude) - 0.1, max(eBut$Latitude) + 0.1))
print(both_map)

#line 75 gives me an error that says "transform should be logical error in r"
#when you remove the scale bar part the cide works up until line 89 where i get a warning that says 
# "Coordinate system already present. Adding new coordinate system, which will replace the existing one"
#then when I go to print the map I get an error that says "Error in loadNamespace(name) : there is no package called ‘mapproj’"
#also overall for come reason the map is really small like the code makes my polygon but doesnt zoom in
#to that section of the map so then the dots are all over the place