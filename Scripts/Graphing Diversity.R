library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vegan)

all_data <- read_csv("eBut.Data.csv")

view(all_data)

knitr::opts_chunk$set(echo = TRUE)

species.cols <- c(6:88)

species_data <- all_data[,species.cols]
species_data[is.na(species_data)] <- 0
all_data[,species.cols] <- species_data

all_data$Diversity <- apply(X = all_data[, species.cols],
                            MARGIN = 1,
                            FUN = function(x) {
                              vegan::diversity(x = x, index = "shannon")
                            })

eBut_Diversity <- all_data[, c("X1", "Site", "Diversity")]
eBut_Diversity <- eBut_Diversity %>%
  spread(Site, Diversity)

deBut.t.test <- t.test(x = eBut_Diversity$eBut,
                       y = eBut_Diversity$`Tohono Chul`,
                       paired = FALSE)

deBut.t <- round(deBut.t.test$statistic, 3)

deBut.p <- "< 0.001"

if(deBut.t.test$p.value > 0.001) {
  deBut.p <- round(deBut.t.test$p.value, 3)
}

deBut.estimate <- round(deBut.t.test$estimate, 3)

deBut.means <- all_data %>%
  group_by(Site) %>%
  summarise(means = round(mean(Diversity), 3))

deBut.means

all_data_long <- all_data[, c("Site", "Diversity")] %>%
  gather(key = "statistic",
         value = "value",
         -Site)

all_data_long$Site <- factor(all_data_long$Site,
                             levels = c("eBut", "Tohono Chul"))

all_data_long$statistic <- factor(all_data_long$statistic,
                                  levels = c("Diversity"))

Diversity_plot <- ggplot(data = all_data_long,
                    mapping = aes(x = Site, y = value)) +
  geom_bar(stat = "identity", width = 0.2) +
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
