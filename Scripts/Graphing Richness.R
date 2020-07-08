#Graphgin Richness
#Jazmyn Winzer 
#jiwinzer@email.arizona.edu 
#2020-7-8

#Here we are loading in that data and needed packages for graphing and such
library(tidyverse)
library(dplyr)
library(ggplot2)

iNat <- read_csv("iNaturalist.csv")
Andy <- read_csv("eBut_ah.csv")
Az <- read_csv("eBut_az.csv")

#Now we have to determine the different types of species we see across all sites
#This is divided by our data sources
unique_iNat <- unique(iNat$Species, incomparables = FALSE)
unique_Andy<- unique(Andy$Species, incomparables = FALSE)
unique_AZ <- unique(Az$Species, incomparables = FALSE)

#Above we had data frame/list of all the unique species
#Then we counted how many names appeared in each list
iNat_total <- length(unique_iNat)
Tohono_Chul_total <- length(unique_Andy)
Tucson_total <- length(unique_AZ)

#Then we created a new data frame of one variable (total number of names per list)
#Then we cleaned up the data frame we made 
Richness <- data.frame(iNat_total, Tohono_Chul_total, Tucson_total)

Richness_Data_Frame <- 
  Richness %>%
  rename(
    "iNat" = iNat_total, 
    "Tohono Chul" = Tohono_Chul_total,
    "eBut" = Tucson_total)

#Richness_Data_Frame

#We then converted our single row data frame into a three row data frame
#Then we made a bar chart out of our new data frame 
working_df <- data.frame(Site = c("iNat","Tohono Chul","eBut"),
                         Total = c(79, 64, 61))

Richness_plot <- ggplot(working_df, aes(x = Site, y = Total)) +
  geom_bar(stat = "identity", width = 0.2) +
  theme_minimal() +
  xlab(NULL) +
  ylab("Total Number of Species")

print(Richness_plot)

ggsave(filename = "Richness Visual.png",
       plot = Richness_plot,
       width = 4.75,
       height = 3)

