library(tidyverse)

# working directory of project folder
setwd("C:/Users/Laura/Desktop/final")

# species_birds.rds: data set obtained from Ikedichi (script: BBS_data_EDITED.Rmd)
data <- readRDS("data/species_birds.rds")
str(data)

# select relevant features
data <- data %>%
  select(aou, y, Year, route_id, full_species, sp.bbs)

# select only observations where y was larger than 0
y_non_zero <- data[which(data$y > 0),]

# save as csv file
write.csv(y_non_zero, "data/non_zero.csv")
