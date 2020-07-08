#Building a Tabel the Descrpies Unigue Species from Each Data Set
#Jazmyn Winzer 
#jiwinzer@email.arizona.edu
#2020-7-8

#This does a bit of cleaning so that we are working in a fresh directory
rm(list = ls())

#Tehn we loaded in the needed files and program for data wrangling
library(tidyverse)

wide <- read_csv("eBut.Data.csv")
iNat <- read_csv("iNaturalist.csv")

#We distingusih the columns in our wide data set that holds the species count
species.cols <- c(6:88)

#We then build a column that distinguishes which data set we are using
site.column <- which(colnames(wide) == "Site")

#Still working in the eBut data we clean up our data frame
counts <- wide[, c(site.column, species.cols)] %>%
  gather(key = "Species", value = "Count", -Site)

counts <- counts[counts$Count > 0,]

#Then we begin working in our iNaturalist data frame
#We are just attaching the species reported in iNaturalist to those seen in eButterfly
inaturalist.counts <- data.frame(Site = "iNaturalist",
                                 Species = iNat$Species,
                                 Count = 1)

counts <- rbind(counts, inaturalist.counts)

#Then the table counts the unique species for us 
long <- as.data.frame(table(counts[, c("Species", "Site")]))

species.table <- spread(data = long, key = Site, value = Freq)

view(species.table)

write.csv(x = species.table,
          file = "species-site.txt",
          row.names = FALSE,
          quote = FALSE)
