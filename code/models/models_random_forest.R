# load required packages
library(vroom)
library(tidyverse)
library(ranger)
library(parglm)
library(xgboost)
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
  drop_na(PC1, Longitude, Latitude)

locations <- full_routes %>% 
  group_by(route_id, Longitude, Latitude) %>% 
  summarise() %>% 
  ungroup()

# map of all locations
ggplot() +
  geom_point(data = locations, aes(x = Longitude, y = Latitude))

set.seed(20)
species <- sample(unique(data$sp.bbs), size = 5)

# remove entries with missing data in PCA axes, species or year
data <- data %>% 
  #filter(sp.bbs %in% species) %>% 
  drop_na(PC1, sp.bbs, Year)


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


# then join with data (assigns folds and sampling probabilities to the data)
data_new <- full_join(data, locations, by = "route_id")
data <- data_new %>% 
  drop_na(PC1, Year, Longitude, Latitude, fold)
data$fold
data$area


# plot folds for all locations
all_folds <- data %>% 
  drop_na(Longitude, Latitude) %>% 
  group_by(Longitude, Latitude, fold) %>% 
  summarise() %>% 
  ungroup()

ggplot() +
  geom_point(data = all_folds, aes(x = Longitude, y = Latitude, col = as.factor(fold)))


# standardize sampling probabilities, depending on number of occurrencies of location in data
weights <- data %>% 
  group_by(route_id, area) %>% 
  summarise(n = n()) %>% 
  mutate(weight = area/n)

data <- left_join(data, weights)



RMSE_rf <- c()
RMSE_rf_int <- c()

for(k in 1:2){
  
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
  rf <- ranger(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + sp.bbs + Year, data = train)
  rf_preds <- predict(rf, data = test2)$prediction
  
  # approach 2: use sampling probabilities and integrate over random effect
  rf2 <- ranger(y ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + sp.bbs + Year, data = train,
                case.weights = train$weight)
  
  rf2_preds <- predict(rf2, data = test_long)$prediction
  rf2_preds_final <- .colMeans(rf2_preds, nrow(long_lat_train),
                               length(rf2_preds) / nrow(long_lat_train))
  
  # calculate RMSE for each method
  RMSE_rf <- append(RMSE_rf, sqrt(mean((rf_preds - test2$y)^2)))
  RMSE_rf_int <- append(RMSE_rf_int, sqrt(mean((rf2_preds_final - test2$y)^2)))
  
}


# combine results
results_rf <- data.frame("RMSE" = c(RMSE_rf, RMSE_rf_int),
                         "method" = c(rep("rf ign", 5), rep("rf int", 5)))
ggplot() +
  geom_boxplot(data = results_rf, aes(x = RMSE, col = method)) +
  coord_flip() +
  xlim(0, max(results_rf$RMSE) + 0.1)

# compare results of both approaches
t.test(RMSE_rf, RMSE_rf_int)
