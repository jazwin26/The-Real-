library(tidyverse)
library(dplyr)
library(ggplot2)

iNat <- read_csv("iNaturalist.csv")
Andy <- read_csv("eBut_ah.csv")
Az <- read_csv("eBut_az.csv")

unique_iNat <- unique(iNat$Species, incomparables = FALSE)
unique_Andy<- unique(Andy$Species, incomparables = FALSE)
unique_AZ <- unique(Az$Species, incomparables = FALSE)

iNat_total <- length(unique_iNat)
Tohono_Chul_total <- length(unique_Andy)
Tucson_total <- length(unique_AZ)

Richness <- data.frame(iNat_total, Tohono_Chul_total, Tucson_total)

Richness_Data_Frame <- 
  Richness %>%
  rename(
    "iNat" = iNat_total, 
    "Tohono Chul" = Tohono_Chul_total,
    "eBut" = Tucson_total)

Richness_Data_Frame


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

