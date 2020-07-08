#Combining Our Data Frames
#Jazmyn Winzer
#jiwinzer@email.arizona.edu 
#2020/5/25

#loading in our data
#we were using tidyverse to use the filter function 
library(tidyverse)
library(dplyr)

iNat <- read_csv("iNat Data.csv")
Andy <- read_csv("eBut_ah.csv")
Az <- read_csv("eBut_az.csv")

#there were additional columns of data that we did not need 
#there were some differences between the data collected in iNat and eBut so we reformated the columns so they would match up better 
#we added an additional column to help with the analysis later on as we are comparing across "sites"
iNat <- iNat %>% 
  select(latitude, longitude, taxon_species_name, observed_on) %>%
  dplyr::rename(Latitude = latitude,
    Longitude = longitude,
    Species = taxon_species_name,
    `Date Observed` = observed_on)%>%
  cbind(Site = "iNat")

glimpse(Andy)

new_Andy <- Andy %>% 
  select(Latitude, Longitude, Species, `Date Observed`, `Number of individuals`) %>%
    cbind(Site = "Tohono Chul")

new_Andy_wide <- new_Andy %>%
  pivot_wider(names_from = Species, values_from = `Number of individuals`)

view(new_Andy_wide)

new_Az <- Az %>%
  select(Latitude, Longitude, Species, `Date Observed`, `Number of individuals`) %>%
  cbind(Site = "eBut")

new_Az_wide <- new_Az %>%
  group_by(Species) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = Species, values_from = `Number of individuals`) %>%
  select(-row)

view(new_Az_wide)

#we moved the data into wide format to make calculations easier in the future
#then we combined the data that came from eButterfly so we dont have to load in some many files
#this also makes it easier to run calcultaions later 
view(full_data <- full_join(new_Andy_wide, new_Az_wide))

full <- (full_data)

#we made csv files from our data frames so we could work with them in R 
write.csv(full_data, "eBut.Data.csv")

write.csv(new_Andy_wide, "Andy.csv")

write.csv(new_Az_wide, "Tucson.csv")

write.csv(iNat, "iNaturalist.csv")


