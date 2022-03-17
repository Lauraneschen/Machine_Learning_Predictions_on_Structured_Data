### description
# simulate structured data
# random effect = groups
# assumption: impact of groups on y is normally distributed
# unbalanced data -> one/some groups are overrepresented
# test the influence of group effect on the performance of both approaches
# other parameters that could be varied: effect of x (= climate variable) on y, random error


### load required packages
library(tidyverse)
library(ranger)
library(gridExtra)
library(groupdata2)
library(comprehenr)


### functions
# generate randomly distributed group effect (random intercept) for 10 groups
# returns a data frame of groups and their random intercepts
rand_eff <- function(re_sd){
  groups <- paste(letters)[1:10]
  group_eff <- rnorm(length(groups), 0, re_sd)
  group_effect <- data.frame(groups, group_eff)
  return(group_effect)
}


# simulate relationship between x and y, with grouping as random effect
sim_data <- function(param1 = 5, ran_error = 10, balanced = F, group_effect){
  
  # predictor variables
  x <- runif(1000, 0, 100)
  intercept <- 100
  
  # random error
  error <- rnorm(1000, 0, ran_error)

  # balanced = data points per group are equal
  n_bal <- rep(2000, 10)
  
  # unbalanced = one group is strongly overrepresented
  n_unbal <- c(1550, rep(50, 9))
  
  # assign number of observations per groups
  if(balanced == T){
    n <- n_bal
  } else{
    n <- n_unbal
  }
  
  # all groups and their random intercepts
  groups <- group_effect$groups
  group_eff <- group_effect$group_eff
  
  # sample 10 groups
  group_final <- c()
  group_eff_final <- c()
  
  # collect group and random intercept for all observations of the final data set
  for(i in 1:length(n)){
    group_final <- append(group_final, rep(groups[i], n[i]))
    group_eff_final <- append(group_eff_final, rep(group_eff[i], n[i]))
  }
  
  # simualte relationship
  y <- param1*x + group_eff_final + intercept + error
  data <- data.frame(x, group_final, y)
  
  return(data)
}


# integration over random effect
pred_int <- function(model, data_int, len, type){
  if(type == "rf"){
    preds <- predict(model, data = data_int)$prediction
  }
  else if(type == "glm"){
    preds <- predict(model, newdata = data_int)
  }
  preds_final <- .colMeans(preds, len, length(preds) / len)
  return(preds_final)
}


### main script

# parameters
param1 <- 5
error <- 50
list_group_effect <- c(1, 10, 20, 30, 50)
rand_intercept <- to_list(for(i in list_group_effect) rand_eff(i))


RMSE_ign <- vector(mode = "list", length = length(list_group_effect))
RMSE_int <- vector(mode = "list", length = length(list_group_effect))
RMSE_glm_ign <- vector(mode = "list", length = length(list_group_effect))
RMSE_glm_int <- vector(mode = "list", length = length(list_group_effect))


# iterate over varying group effect
# each repeated 10 times
for(i in 1:length(list_group_effect)){
  for(j in 1:10){
    
    train <- sim_data(param1 = param1, ran_error = error, balanced = F, group_effect = rand_intercept[[i]])
    train_balanced <- upsample(train, cat_col = "group_final")
    test <- sim_data(param1 = param1, ran_error = error, balanced = T, group_effect = rand_intercept[[i]])
    
    # model without random effect
    model_ign <- ranger(y ~ x, data = train)
    preds_ign <- predict(model_ign, data = test)$prediction
    glm_ign <- glm(y ~ x, data = train)
    preds_glm_ign <- predict(glm_ign, newdata = test)
    
    # model with random effect
    model_int <- ranger(y ~ x + group_final, data = train_balanced, importance = "impurity")
    glm_int <- glm(y ~ x + group_final, data = train_balanced)

    groups <- unique(train$group_final)
    test_int <- data.frame("x" = rep(test$x, each = length(groups)))
    test_int <- test_int %>% 
    mutate(group_final = rep(groups, nrow(test)))
    
    preds_int <- pred_int(model_int, test_int, length(groups), type = "rf")
    preds_glm_int <- pred_int(glm_int, test_int, length(groups), type = "glm")
    
    # results
    RMSE_ign[[i]] <- append(RMSE_ign[[i]], sqrt(mean((preds_ign - test$y)^2)))
    RMSE_int[[i]] <- append(RMSE_int[[i]], sqrt(mean((preds_int - test$y)^2)))
    RMSE_glm_ign[[i]] <- append(RMSE_glm_ign[[i]], sqrt(mean((preds_glm_ign - test$y)^2)))
    RMSE_glm_int[[i]] <- append(RMSE_glm_int[[i]], sqrt(mean((preds_glm_int - test$y)^2)))
  
  }
}


# RMSE of glm models
results_glm <- data.frame("list_group_effect" = as.factor(rep(rep(list_group_effect, each = 10), 2)),
                         "RMSE" = c(unlist(RMSE_glm_ign), c(unlist(RMSE_glm_int))),
                         "method" = c(rep("ign", 50), rep("int", 50)))

# RMSE of random forest
results_rf <- data.frame("list_group_effect" = as.factor(rep(rep(list_group_effect, each = 10), 2)),
                         "RMSE" = c(unlist(RMSE_ign), c(unlist(RMSE_int))),
                         "method" = c(rep("ign", 50), rep("int", 50)))

# assign model
results_glm$model <- "glm"
results_rf$model <- "rf"

# combine results
results_all <- rbind(results_glm, results_rf)


# plot RMSE against variance of random intercept
ggplot() +
  geom_boxplot(data = results_all, aes(x = list_group_effect, y = RMSE, fill = method),
               size = 0.2, outlier.size = 0.5) +
  facet_wrap(~model, ncol = 2) +
  xlab("Standandard deviation of random intercept") +
  theme(aspect.ratio = 1,
        strip.text.x = element_text(size = 15),
        text = element_text(size = 30),
        legend.key.size = unit(1.5, 'cm'))


# average RMSE's for both models, separately for ignore and integrate

# glm errors
results_glm_int <- results_glm %>% 
  group_by(list_group_effect, method) %>% 
  summarise(RMSE = mean(RMSE)) %>% 
  filter(method == "int")

results_glm_ign <- results_glm %>% 
  group_by(list_group_effect, method) %>% 
  summarise(RMSE = mean(RMSE)) %>% 
  filter(method == "ign")

# random forest errors
results_rf_int <- results_rf %>% 
  group_by(list_group_effect, method) %>% 
  summarise(RMSE = mean(RMSE)) %>% 
  filter(method == "int")

results_rf_ign <- results_rf %>% 
  group_by(list_group_effect, method) %>% 
  summarise(RMSE = mean(RMSE)) %>% 
  filter(method == "ign")


# percentages of improvement
1 - results_glm_int$RMSE/results_glm_ign$RMSE
1 - results_rf_int$RMSE/results_rf_ign$RMSE


