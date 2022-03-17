# load RMSE data for glm and xgboost models
glm <- read.csv("data/RMSE_glm_400_spec.csv")
xgboost <- read.csv("data/RMSE_xgboost_400_spec_50_rounds.csv")

# t test of RMSE (ingore random effect vs. integrate)
t.test(glm$RMSE[glm$method == "glm ign"], glm$RMSE[glm$method == "glm int"])
t.test(xgboost$RMSE[xgboost$method == "xgboost ign"], xgboost$RMSE[xgboost$method == "xgboost int"])
