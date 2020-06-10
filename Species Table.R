rm(list = ls())

library(tidyverse)

wide <- read_csv("eBut.Data.csv")
iNat <- read_csv("iNaturalist.csv")

species.cols <- c(6:88)

site.column <- which(colnames(wide) == "Site")

counts <- wide[, c(site.column, species.cols)] %>%
  gather(key = "Species", value = "Count", -Site)

counts <- counts[counts$Count > 0,]

inaturalist.counts <- data.frame(Site = "iNaturalist",
                                 Species = iNat$Species,
                                 Count = 1)

counts <- rbind(counts, inaturalist.counts)

long <- as.data.frame(table(counts[, c("Species", "Site")]))

species.table <- spread(data = long, key = Site, value = Freq)

view(species.table)

write.csv(x = species.table,
          file = "species-site.txt",
          row.names = FALSE,
          quote = FALSE)
