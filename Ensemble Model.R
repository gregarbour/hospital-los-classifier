library(corrplot)
library(randomForest)
library(dplyr)
library(ggplot2)

#Load data files
setwd("~/Desktop/Data Science/Project/Data Files/")
load('test_final.R')
load('validation_final.R')
outcome_numeric <- validation$long_los
validation$long_los <- as.factor(ifelse(validation$long_los == 1, 'long', 'short'))

# Several values in validation dataset have different factor level than appeared in training data set.
# Impute the values to most similar factor level
validation$chlorpropamide <- ifelse(validation$chlorpropamide == 'Down', 'Steady',
                                    validation$chlorpropamide)
validation$tolazamide <- ifelse(validation$tolazamide == 'Up', 'Steady',
                                    validation$tolazamide)
#Load predictions from each model
setwd("~/Desktop/Data Science/Project/Model Files/")
load("gam_pred_prob.R")
load("gam_pred_class.R")
load("LR_pred_class.R")
load("LR_pred_prob.R")
load("xg_pred_prob.R")
load("xg_pred_class.R")
load('LR_model.R')
load('xg_model.R')

prob_test <- data.frame(xg = xg_pred_prob$long, lr = LR_pred_prob$long, gam = as.vector(gam_pred_prob),
                   outcome = test$long_los)

#Check correlation betweeb models
r <- cor(prob_test[,-4])
r
# GAM and Logistic Regression models have near perfect correlation. Almost nothing will be gained by including
# both in the ensemble model. Remove GAM predictors and create ensemble with LR and XG predictions

#Predictions from Logistic Regression and XG Boost models have more separation, though still very high.
# Correlation greater than 0.75 implies likely not much improvement to be gained from ensembling.
prob_test %>% ggplot(aes(x = gam, y = xg)) + 
  geom_point(col = 'coral') + 
  geom_jitter(alpha = 0.1)

table(LR_pred_class, xg_pred_class)

#Remove GAM predictions from ensemble model variables
prob_test <- select(prob_test, -gam)

#Train a random forest on the predicted probabilities
rf <- randomForest(
  outcome ~ .,
  data=prob_test)


##### Evaluate the ensemble model on the Validation data set ######

#First have to create predictions on validation data using LR and XG models
LR_pred_prob <- predict(LR_model, newdata = validation, type = 'prob')
LR_pred_class <- predict(LR_model, newdata = validation, type = 'raw')
xg_pred_prob <- predict(xg_model, newdata = validation, type = 'prob')
xg_pred_class <- predict(xg_model, newdata = validation, type = 'raw')

prob_valid <- data.frame(xg = xg_pred_prob$long, lr = LR_pred_prob$long)

ensemble_pred_prob <- predict(rf, newdata = prob_valid, type = 'prob')[,2]
ensemble_pred_class <- predict(rf, newdata = prob_valid, type = 'response')


##### Model Evaluation #####

# Confusion matrix
confusionMatrix(data = ensemble_pred_class, outcome_numeric)


#ROC curve
roc_object_ensemble <- roc(outcome_numeric, ensemble_pred_prob, smoothed = TRUE)
roc_object_ensemble
plot(roc_object_ensemble)

# Histogram of probabilities, faceted by outcome (0 or 1)
data.frame(predicted_probability = xg_pred_prob$long, actual_class = outcome_numeric) %>% 
  ggplot(aes(x = predicted_probability)) + geom_histogram() + facet_grid(~actual_class)

