# load required packages
library(vroom)
library(tidyverse)
library(ranger)
library(parglm)


# both models (glm and gradient boosted tree) were analysed separately to avoid that R crashes
# however they use exactly the same data

# increase memory limit to avoid error messages
memory.limit(size = 100000)

setwd("C:/Users/Laura/Desktop/final")


##### load data #####
# BBS data
data <- readRDS("data/BBS_all_data.rds")
# grids for cross validation
grids <- read.csv("data/spatial_grids_cross_validation.csv")
# environmental data
full_routes <- vroom("data/Full_routes.csv")
# weights for spatial balancing
sampling <- read.csv("data/areas.csv")


##### data pre-processing #####
# unique locations (route_id is a unique combination of Longitude and Latitude)
full_routes <- full_routes %>% 
  drop_na(PC1, Longitude, Latitude, Year)

locations <- full_routes %>% 
  group_by(route_id, Longitude, Latitude) %>% 
  summarise() %>% 
  ungroup()


# assign folds for cross-validation to locations
folds <- 5
set.seed(100)
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


# assign sampling probabilities to locations
sampling <- sampling %>% 
  dplyr::select(route_id, area)

locations <- full_join(locations, sampling)


# BBS data: select 400 species randomly
set.seed(20)
species <- sample(unique(data$sp.bbs), size = 400)

data <- data %>% 
  filter(sp.bbs %in% species) %>% 
  drop_na(PC1, sp.bbs, Year)


# join location data with survey data (assigns folds and areas of voronoi polygons to the survey data)
data_new <- full_join(data, locations, by = "route_id")
data <- data_new %>% 
  drop_na(PC1, Year, Longitude, Latitude, fold)

# standardize sampling probabilities, depending on number of occurrencies of location in the dataset
weights <- data %>% 
  group_by(route_id, area) %>% 
  summarise(n = n()) %>% 
  mutate(weight = area/n)

# assign final sampling probabilities for single points
data <- left_join(data, weights)


### models, evaluated in a 5-fold cross validation
RMSE_glm <- c()
RMSE_glm_int <- c()

for(k in 1:1){
  
  # assign train and test data
  train <- data[data$fold != k,]
  test <- data[data$fold == k,]
  
  set.seed(k)
  ind_test <- sample(1:nrow(test), size = 10000, replace = T,
                     prob = test$weight/sum(test$weight))
  
  test_bal <- test[ind_test,]
  
  # prepare data frame for integration of random effect
  test2 <- test_bal %>% 
    dplyr::select(PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, sp.bbs, Year, y)
  
  # locations of training data
  long_lat_train <- train %>% 
    group_by(Longitude, Latitude) %>% 
    summarise() %>% 
    ungroup()
  
  test_long <- test2[rep(seq_len(nrow(test2)), each = nrow(long_lat_train)),] %>% 
    mutate(Longitude = rep(long_lat_train$Longitude, nrow(test2)),
           Latitude = rep(long_lat_train$Latitude, nrow(test2)))
  
  ### models
  # assumption: location is unknown in test data
  # approach 1: ignore location
  glm <- glm(sqrt(y) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + sp.bbs + Year,
                data = train)
  glm_preds <- predict(glm, newdata = test2)
  
  # approach 2: use sampling probabilities and integrate over random effect
  glm2 <- glm(sqrt(y) ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + sp.bbs + Year,
                 data = train, weights = train$weight)
  glm2_preds <- predict(glm2, newdata = test_long)
  glm2_preds_final <- .colMeans(glm2_preds, nrow(long_lat_train),
                                length(glm2_preds) / nrow(long_lat_train))
  
  # calculate RMSE for each method
  RMSE_glm <- append(RMSE_glm, sqrt(mean((glm_preds^2 - test2$y)^2)))
  RMSE_glm_int <- append(RMSE_glm_int, sqrt(mean((glm2_preds_final^2 - test2$y)^2)))
  
}


# combine results
results <- data.frame("RMSE" = c(RMSE_glm, RMSE_glm_int),
                      "method" = c(rep("glm ign", 5), rep("glm int", 5)))

ggplot() +
  geom_boxplot(data = results, aes(x = RMSE, col = method)) +
  coord_flip() +
  xlim(0, max(results$RMSE) + 0.1)


# save results
write.csv(results, "data/RMSE_glm_400_spec.csv")

