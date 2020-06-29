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

view(all_data)

#truthfully I have no idea what this does
knitr::opts_chunk$set(echo = TRUE)

#Calling the columns that hold the species counts
species.cols <- c(6:88)

#Removing the NA fills and exchnaging them for zeros
species_data <- all_data[,species.cols]
species_data[is.na(species_data)] <- 0
all_data[,species.cols] <- species_data

#We are adding a column to the end of ths all_data data frame
#This column is filled with the average number of times a count comes up at each site
all_data$Diversity <- apply(X = all_data[, species.cols],
                            MARGIN = 1,
                            FUN = function(x) {
                              vegan::diversity(x = x, index = "shannon")
                            })

#This makes a data frame that we will later fill with our t-test data
eBut_Diversity <- all_data[, c("X1", "Site", "Diversity")]
eBut_Diversity <- eBut_Diversity %>%
  spread(Site, Diversity)

#Here we are running a t-test that will compare the means of the butterflies seen at our two sites
deBut.t.test <- t.test(x = eBut_Diversity$eBut,
                       y = eBut_Diversity$`Tohono Chul`,
                       paired = FALSE)

#Store the t value from the t-test as a makeshift data frame
deBut.t <- round(deBut.t.test$statistic, 3)

#Account for rounding error in calculation for values greater than zero
deBut.p <- "< 0.001"

if(deBut.t.test$p.value > 0.001) {
  deBut.p <- round(deBut.t.test$p.value, 3)
}

#Store the difference in means that was calculated in the t-tes
deBut.estimate <- round(deBut.t.test$estimate, 3)

#Store the means for each site that were calculated in the t-test
deBut.means <- all_data %>%
  group_by(Site) %>%
  summarise(means = round(mean(Diversity), 3))

deBut.means

#Here we need to be reformated to we can use it to make a graph in ggplot
all_data_long <- all_data[, c("Site", "Diversity")] %>%
  gather(key = "statistic",
         value = "value",
         -Site)

#Reorder your data for better visulatization 
all_data_long$Site <- factor(all_data_long$Site,
                             levels = c("eBut", "Tohono Chul"))

all_data_long$statistic <- factor(all_data_long$statistic,
                                  levels = c("Diversity"))

#Make the bar chart with two columns
Diversity_plot <- ggplot(all_data_long, 
                         mapping = aes(x = Site, y = value)) +
  geom_bar(stat = "identity")+
#so geom_bar() works but it doesnt graph the values from the t-test it graphs the counts for each site which is not what I want
#geom_col() does the same thing as geom_bar
#geom_bar(stat = "identity") also does the same thing
#so ya see there is no error just for some reason my visual isnt working
  facet_wrap(~ statistic,
             scales = "free_y",
             strip.position = "left",
             labeller = as_labeller(c(Diversity = "Diversity~(italic(H))"),
                                    label_parsed)) +
  theme_bw() +
  ylab(NULL) +
  xlab(NULL) +
  theme_minimal()

print(Diversity_plot)

ggsave(filename = "Diversity Visual.png",
       plot = Diversity_plot,
       width = 4.75,
       height = 3)
