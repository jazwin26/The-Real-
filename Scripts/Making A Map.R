rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggsn)

eBut <- read_csv("eBut.Data.csv")
view(eBut)
iNat <- read_csv("iNaturalist.csv")
view(iNat)

latlongs <- unique(eBut[, c("Longitude", "Latitude")])

latlongs_iNat <- unique(iNat[, c("Longitude", "Latitude")])

latlong_both <- rbind(latlongs, latlongs_iNat)
latlong_both$Source <- "inaturalist"
latlong_both$Source[1:nrow(latlongs)] <- "Butterfly Walk"

map_bounds <- c(floor(min(eBut$Longitude)),
                floor(min(eBut$Latitude)),
                ceiling(max(eBut$Longitude)),
                ceiling(max(eBut$Latitude)))
names(map_bounds) <- c("left", "bottom", "right", "top")

la_map <- get_map(location = map_bounds,
                  source = "stamen",
                  maptype = "terrain-lines",
                  color = "bw")

bounds <- data.frame(x = c(rep(min(latlongs$Longitude), times = 2), rep(max(latlongs$Longitude), times = 2)),
                     y = c(min(latlongs$Latitude), max(latlongs$Latitude), max(latlongs$Latitude), min(latlongs$Latitude)))

latlong_scale <- latlongs
colnames(latlong_scale) <- c("long", "lat")
latlong_scale[nrow(latlong_scale) + 1, ] <- list("long" = -118.425, "lat" = 33.84)

city_labels <- data.frame(names = c("Tucson", "Tohono Chul"),
                          longitude = c(-110.9747, -110.9817),
                          latitude = c(32.2226, 32.3393),
                          sizes = c(7,7))

both_map <- ggmap(la_map) +
  geom_polygon(data = bounds,
               mapping = aes(x = x, y =y),
               fill = "#ffffff",
               color = "dd2222",
               alpha = 0.45) +
  annotate("text",
           label = city_labels$names,
           x = city_labels$longitude,
           y = city_labels$latitude,
           size = city_labels$sizes,
           color = "black") +
  geom_point(data = latlong_both,
             mapping = aes(x = Longitude, y = Latitude, shape = Source, fill = Source),
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
