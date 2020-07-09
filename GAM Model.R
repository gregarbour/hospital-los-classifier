library(caret)
library(dplyr)
library(mgcv)
library(ggplot2)
library(pROC)

##### NEXT STEPS:  
# variable importance for GAM
# some plot to compare predictions between three diff models (corr plots?)
# retrain gam with diff # of predictors (removing unimportant vars) OR diff method
# Calibration plot code for all three models

setwd("~/Desktop/Data Science/Project/Data Files/")
load('train_final.R')
load('test_final.R')
outcome_numeric <- test$long_los
test$long_los <- as.factor(ifelse(test$long_los == 1, 'long', 'short'))

# Write regression formula
fm <- paste('s(', names(train)[4:7], ', k = 3)', sep = "", collapse = ' + ')
fm <- paste(fm, '+', 
            paste(names(train)[c(1:3, 8:28, 30:71)], sep = '', collapse = ' + '))
fm <- as.formula(paste('long_los ~', fm))

# Train model
gam_model <- gam(fm, data=train, family=binomial) #REML method recommended but failed
save(gam_model, file = "~/Desktop/Data Science/Project/Model Files/gam_model.R")

## Model Evaluation
load("~/Desktop/Data Science/Project/Model Files/gam_model.R")
gam_pred_prob <- predict(gam_model, newdata = test, type = 'response')
gam_pred_class <- as.factor(ifelse(gam_pred_prob > 0.5, 'long', 'short'))
save(gam_pred_prob, file = '~/Desktop/Data Science/Project/Model Files/gam_pred_prob.R')
save(gam_pred_class, file = '~/Desktop/Data Science/Project/Model Files/gam_pred_class.R')


# Confusion matrix
confusionMatrix(data = gam_pred_class, test$long_los)

#ROC curve
roc_object_gam <- roc(outcome_numeric, gam_pred_prob, smoothed = TRUE)
roc_object_gam
plot(roc_object_gam)

# Histogram of probabilities, faceted by outcome (0 or 1)
data.frame(predicted_probability = gam_pred_prob, actual_class = outcome_numeric) %>% 
  ggplot(aes(x = predicted_probability)) + geom_histogram() + facet_grid(~actual_class)


#Feature Importance
