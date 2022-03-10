# load required packages
library(vroom)
library(tidyverse)
library(caret)
library(sf)


# load environmental data
data <- read.csv("data/Full_routes.csv")


cols <- rainbow(100)

# grids for cross validation
# grids were manually created (in a way that each rectangle had a similar data density)
grids <- data.frame("long_min" = c(-175, -130, -130, -126, -125, -116, -116, -116, -125,
                                   -112, -105, -105, -107, -112, -94, -83, -70.5, -104,
                                   -104, -96, -85.5, -92, -83.5, -92, -98, -94, -82,
                                   -73.5, -90, -90, -80, -80, -83.5),
                    
                    "long_max" = c(-130, -90, -116, -116, -116, -105, -105, -107, -112,
                                   -104, -94, -94, -98, -96, -83, -70.5, -50, -92,
                                   -92, -85.5, -79, -83.5, -74, -83.5, -90, -82, -73.5,
                                   -70.5, -80, -80, -73.5, -73.5, -75),
                    
                    "lat_min" = c(50, 52.5, 47, 42.5, 38, 47, 42.5, 38, 31,
                                  32, 47, 42.5, 38, 25, 45, 45, 41, 32,
                                  35, 27, 24, 32, 35.5, 35, 38, 42.5, 42.5,
                                  40, 38, 40, 40, 38, 32),
                    
                    "lat_max"= c(71.5, 71.5, 52.5, 47, 42.5, 52.5, 47, 42.5, 38,
                                 38, 52.5, 47, 42.5, 32, 52.5, 53.5, 55, 35,
                                 38, 32, 32, 35, 38, 38, 42.5, 45, 45,
                                 45, 40, 42.5, 42.5, 40, 35.5))


# unique locations
data <- data %>% 
  drop_na(PC1, Longitude, Latitude, Year)

locations <- data %>% 
  group_by(route_id, Longitude, Latitude) %>% 
  summarise() %>% 
  ungroup()


# assign folds for cross-validation to locations
folds <- 5
set.seed(10)
ind <- sample(c(rep(1:5, 6), 1:3), size = nrow(grids), replace = FALSE)

fold <- rep(NA, nrow(locations))
grids$class <- ind

for(i in 1:nrow(locations)){
  for(j in 1:nrow(grids)){
    if(locations$Longitude[i] >= grids$long_min[j] & locations$Longitude[i] < grids$long_max[j]){
      if(locations$Latitude[i] >= grids$lat_min[j] & locations$Latitude[i] < grids$lat_max[j]){
        fold[i] <- grids$class[j]
      }
    }
  }
}

locations$fold <- fold
locations <- locations %>% 
  dplyr::select(route_id, fold)


# locations as sf objects
data_sf <- st_as_sf(data, coords = c(15, 14), crs = 4326)

# map of USA and Canada
worldmap <- rnaturalearth::ne_download(scale = 110,
                                       type = "countries",
                                       destdir = tempdir(),
                                       load = TRUE,
                                       returnclass = "sf")

ind <- which(worldmap$NAME == "United States of America" | worldmap$NAME == "Canada")
North_America <- st_union(worldmap[ind,])

# plot the spatial blocks
set.seed(500)

cols <- sample(cols, nrow(grids))

ggplot() +
  geom_sf(data = North_America) +
  geom_sf(data = data_sf, size = 0.05) +
  coord_sf(expand = F) +
  annotate(geom="rect", xmin = grids[,1], xmax = grids[,2], ymin = grids[,3], 
           ymax = grids[,4], colour = "black", fill = grids$class,
           alpha = 0.3)


# check number of data points in each rectangle
data_points <- function(min_lon, max_lon, min_lat, max_lat){
  num <- nrow(data[which(data$Longitude >= min_lon
                         & data$Longitude < max_lon
                         & data$Latitude >= min_lat
                         & data$Latitude < max_lat),])
  return(num)
}

# calculate for every block
num_points <- c()

for(i in 1:nrow(grids)){
  num <- data_points(grids[i,1], grids[i,2], grids[i,3], grids[i,4])
  num_points <- append(num_points, num)
}

# distribution of data points per block
ggplot() +
  geom_histogram(aes(x = num_points), bins = 21) +
  xlim(0, 3000)

# there is still a spread, but the distribution is much more homogeneous than with equally sized grids


# save results for cross validation
write.csv(grids, "data/spatial_grids_cross_validation.csv")


