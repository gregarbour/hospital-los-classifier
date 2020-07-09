library(xgboost)
library(caret)
library(dplyr)
library(pROC)
library(ggplot2)


setwd("~/Desktop/Data Science/Project/Data Files/")
load('train_final.R')
load('test_final.R')
outcome_numeric <- test$long_los
train$long_los <- ifelse(train$long_los == 1, 'long', 'short')
test$long_los <- ifelse(test$long_los == 1, 'long', 'short')



#With XGBoost package
xg_model1 <- xgboost(data = data, label = label,
                     max.depth = 10, #Default 2. More depth if more complex relationships between vars
                     eta = 1,
                     nrounds = 10, # num of passes on the data
                     eval_metric = 'auc',
                     objective = "binary:logistic")


#With Caret Package
tune_grid <- expand.grid(
        nrounds = seq(from = 25, to = 100, by = 25),
        eta = c(0.025, 0.05, 0.1, 0.3),
        max_depth = c(2, 3, 4, 5, 6),
        gamma = 0,
        colsample_bytree = 1,
        min_child_weight = 1,
        subsample = 1)


tune_control <- caret::trainControl(
        method = "cv",
        number = 10,
        classProbs = TRUE)

xg_model <- caret::train(
  long_los ~., 
  data = train,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE)

save(xg_model, file = "~/Desktop/Data Science/Project/Model Files/xg_model.R")
load("~/Desktop/Data Science/Project/Model Files/xg_model.R")


## Predict on test dataset
xg_pred_prob <- predict(xg_model, newdata = test, type = 'prob')
xg_pred_class <- predict(xg_model, newdata = test, type = 'raw')
save(xg_pred_prob, file = '~/Desktop/Data Science/Project/Model Files/xg_pred_prob.R')
save(xg_pred_class, file = '~/Desktop/Data Science/Project/Model Files/xg_pred_class.R')

##### Model Evaluation #####

# Confusion matrix
confusionMatrix(data = xg_pred_class, as.factor(test$long_los))


#ROC curve
roc_object_XG <- roc(outcome_numeric, xg_pred_prob$short, smoothed = TRUE)
roc_object_XG
plot(roc_object_XG)

# Histogram of probabilities, faceted by outcome (0 or 1)
data.frame(predicted_probability = xg_pred_prob$long, actual_class = outcome_numeric) %>% 
  ggplot(aes(x = predicted_probability)) + geom_histogram() + facet_grid(~actual_class)



#Feature Importanc
importance <- varImp(xg_model)
head(importance)

# Gain is the improvement in accuracy brought by a feature to the branches it is on. The idea is that before 
# adding a new split on a feature X to the branch there was some wrongly classified elements, after adding 
# the split on this feature, there are two new branches, and each of these branch is more accurate 
# (one branch saying if your observation is on this branch then it should be classified as 1, and the other 
#         branch saying the exact opposite).
# 
# Cover measures the relative quantity of observations concerned by a feature.
# 
# Frequency is a simpler way to measure the Gain. It just counts the number of times a feature is used in 
# all generated trees. You should not use it (unless you know why you want to use it).


