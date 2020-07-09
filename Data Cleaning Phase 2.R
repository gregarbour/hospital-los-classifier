# After completion of data exploration, we now do a final data cleaning to prepare the dataset for modelling
# Remove variables for one of the following reasons:
# - Duplicate or redundant (now that data exploration is complete)
# - Variables that are especially sparse (less than 10 occurences)

library(dplyr)
setwd("~/Desktop/Data Science/Project/Data Files/")
load('train_phase1.R')
load('test_phase1.R')
load('validation_phase1.R')


#Define function to perform final cleaning (i.e. removing variables, formating long_los variable)
final_clean <- function(df){
  df <- select(df,
          -encounter_id,
          -patient_nbr,
          -medical_specialty, # using medical specialty groups instead
          -diag_1, # Using diagnosis group variables instead
          -diag_2,
          -diag_3,
          -time_in_hospital, # will use "long_los" as primary outcome variable
          -acetohexamide, #one occurences
          -troglitazone, #two occurences
          -examide, #zero occurences
          -citoglipton, #zero occurences
          -glipizide_metformin, #10 occurences
          -glimepiride_pioglitazone, # one occurence
          -metformin_rosiglitazone, # one occurences
          -metformin_pioglitazone, # one occurence
          -HTN, #zero occurences
          -HTNcx, #zero occurences
          -Hypothyroid, # one occurence
          -Liver, #zero occurences
          -PUD, #zero occurences
          -Obesity, #zero occurences
          -BloodLoss)  #zero occurences
  df$long_los <- as.factor(df$long_los)
  return(df)
}



train <- final_clean(train)
test <- final_clean(test)
validation <- final_clean(validation)


save(train, file = "train_final.R")
save(test, file = "test_final.R")
save(validation, file = "validation_final.R")