#load required packages
library(vroom)
library(tidyverse)
library(plyr)
library(comprehenr)


# environmental data
full_routes <- vroom("data/Full_routes.csv")

full_routes <- full_routes %>% 
  select(route_id, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, Year, Latitude, Longitude, land_area, countrynum, elevation)


# survey data (only non-zero y's)
y_non_zero <- vroom("data/non_zero.csv")

# final dataframe will consist of one entry for each combination of species, location and time
spec <- unique(y_non_zero$sp.bbs)
new <- full_routes[rep(seq_len(nrow(full_routes)), each = length(spec)),]
new$sp.bbs <- rep(spec, nrow(full_routes))

data <- y_non_zero %>% 
  select(sp.bbs, route_id, Year, y)

# join survey data with environmental data
new2 <- dplyr::full_join(new, data)

# fill zeros
y_zero <- which(is.na(new2$y))
new2$y[y_zero] <- 0


#write.csv(new2, "data/BBS_all_data.csv")
saveRDS(new2, file = "data/BBS_all_data.rds")
