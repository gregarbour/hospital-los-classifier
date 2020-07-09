library(glmnet)
library(caret)
library(dplyr)
library(pROC)
library(ggplot2)

setwd("~/Desktop/Data Science/Project/Data Files/")
load('train_final.R')
load('test_final.R')
train$long_los <- ifelse(train$long_los == 1, 'long', 'short')
outcome_numeric <- test$long_los
test$long_los <- as.factor(ifelse(test$long_los == 1, 'long', 'short'))

#Set training control parameters
Control <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 3,
                        classProbs = TRUE,
                        verboseIter =TRUE)

#Elastic net (using a tuning grid for alpha & lambda)

LR_model <- train(long_los ~., data = train,
                     method = "glmnet",
                     trControl = Control,
                     metric = 'accuracy',
                     tuneGrid = expand.grid(alpha = 0:10/10,
                                           lambda = seq(0.0001, 1, length = 20)))
save(LR_model, file = "~/Desktop/Data Science/Project/Model Files/LR_model.R")




## Model Evaluation
load("~/Desktop/Data Science/Project/Model Files/LR_model.R")
LR_pred_prob <- predict(LR_model, newdata = test, type = 'prob')
LR_pred_class <- predict(LR_model, newdata = test, type = 'raw')
save(LR_pred_prob, file = '~/Desktop/Data Science/Project/Model Files/LR_pred_prob.R')
save(LR_pred_class, file = '~/Desktop/Data Science/Project/Model Files/LR_pred_class.R')

# Confusion matrix
confusionMatrix(data = LR_pred_class, test$long_los)

#ROC curve
roc_object_LR <- roc(outcome_numeric, LR_pred_prob$short, smoothed = TRUE)
roc_object_LR
plot(roc_object_LR)

# Histogram of probabilities, faceted by outcome (0 or 1)
data.frame(predicted_probability = LR_pred_prob$long, actual_class = outcome_numeric) %>% 
  ggplot(aes(x = predicted_probability)) + geom_histogram() + facet_grid(~actual_class)


#Feature Importance
varImp(LR_model)

#Visualize Accuracy for diff tuning parameters
plot(LR_model) 



