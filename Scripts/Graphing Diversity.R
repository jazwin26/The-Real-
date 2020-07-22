#Graphing Butterfly Diversity in Tucson
#Jazmyn Winzer 
#jiwinzer@email.arizona.edu
#2020-6-28

#Lodaing in data and checking for data frame structure
library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)

all_data <- read_csv("Data/eBut.Data.csv")

# One column (rita) has *only* missing data; remove that column; also, X1 is
# uninformative (it looks to be rownames written by a save), so we can drop 
# that one, too
all_data <- all_data %>%
  select(-rita, -X1)

#view(all_data)

#Calling the columns that hold the species counts
# We use this to subset the data frame for only columns that have abundance 
# data in them (because this is the input vegan is looking for)
species.cols <- c(6:86)

#Removing the NA fills and exchnaging them for zeros
species_data <- all_data[,species.cols]
species_data[is.na(species_data)] <- 0
all_data[,species.cols] <- species_data

#We are adding a column to the end of ths all_data data frame
# This column is filled with the shannon diversity for that row (Shannon's 
# diversity for each site for each day)
all_data$Diversity <- apply(X = all_data[, species.cols],
                            MARGIN = 1,
                            FUN = function(x) {
                              vegan::diversity(x = x, index = "shannon")
                            })

#Pull out only the site and the diversity score for t-test
eBut_Diversity <- all_data %>%
  select(Site, Diversity)

#Here we are running a t-test that will compare the means of the butterflies 
# seen at our two sites
deBut.t.test <- t.test(x = eBut_Diversity$Diversity[eBut_Diversity$Site == "eBut"],
                       y = eBut_Diversity$Diversity[eBut_Diversity$Site == "Tohono Chul"],
                       paired = FALSE)

#Store the t value from the t-test as a makeshift data frame
deBut.t <- round(deBut.t.test$statistic, 3)

#Account for rounding error in calculation for values greater than zero
deBut.p <- "< 0.001"

if(deBut.t.test$p.value > 0.001) {
  deBut.p <- round(deBut.t.test$p.value, 3)
}

#Store the difference in means that was calculated in the t-test
deBut.estimate <- round(deBut.t.test$estimate, 3)

#Store the means for each site that were calculated in the t-test
deBut.means <- all_data %>%
  group_by(Site) %>%
  summarise(means = round(mean(Diversity), 3))

# deBut.means

# Create a boxplot of diversity scores; an alternative approach would be a
# violin plot (replace geom_boxplot() with geom_violin())
Diversity_plot <- ggplot(data = eBut_Diversity,
                         mapping = aes(x = Site, y = Diversity)) +
  geom_boxplot() +
  ylab(NULL) +
  xlab(NULL) +
  theme_minimal()

print(Diversity_plot)

ggsave(filename = "Diversity Visual.png",
       plot = Diversity_plot,
       width = 4.75,
       height = 3)
