library(ggplot2)
library(corrplot)
library(scales)
library(dplyr)
setwd("~/Desktop/Data Science/Project/Data Files/")
load('train_phase1.R')

##### Table 1 #####
table1 <- list()
vars <- names(train)[c(3,4,15:43, 45:94)]
#vars <- names(train)[c(45:50)]
count_total <- nrow(train)
list_index <- 1
for(i in vars){
  levels <- unique(train[[i]])

  for(j in levels){
    los_df <- train %>% select(long_los, time_in_hospital, i)
    los_df <- los_df[los_df[[i]]==j,]
    los_df[,3] <- as.character(los_df[,3])

    temp_df <- data.frame(var_name = i,
                          level = as.character(j),
                          count = nrow(los_df),
                          percent_of_total = percent(nrow(los_df)/count_total),
                          mean = round(mean(los_df$time_in_hospital), 2),
                          median = quantile(los_df$time_in_hospital, 0.5, na.rm = T, type = 4),
                          p25 = quantile(los_df$time_in_hospital, 0.25, na.rm = T, type = 4),
                          p75 = quantile(los_df$time_in_hospital, 0.75, na.rm = T, type = 4),
                          p90 = quantile(los_df$time_in_hospital, 0.9, na.rm = T, type = 4),
                          plong_los = percent(sum(los_df$long_los)/nrow(los_df)))
    table1[[list_index]] <- temp_df
    list_index <- list_index + 1
  }
}
table1 <- do.call(rbind.data.frame, table1) ###Need to fix the two 0's in medical_spec_grouped
rownames(table1) <- NULL
save(file = 'table1.R', table1)

##### Descriptive Plots #####

#Correlation Plot
M <- cor(select(train, num_procedures, num_medications,number_outpatient, number_emergency,
                number_inpatient, num_lab_procedures, time_in_hospital))
corrplot(M, method = 'number')



#Medical Speciality
med_spec_prop <- as.data.frame(sort(prop.table(table(train$medical_specialty))))
med_spec_group_prop <- as.data.frame(sort(prop.table(table(train$medical_specialty_grouped))))
# Could possibly optimize this variable further, though there is decent separation among the groups



#How unique are ICD codes?
icd_all<- as.data.frame(table(c(train$diag_1, train$diag_2, train$diag_3)))
summary(icd_all$Freq)
hist(icd_all$Freq)
hist(log1p(icd_all$Freq))
#800 unique ICD codes. Median # of values 18 (half of distinct ICD codes have 18 or fewer cases)
# Grouping by rounding numeric ICD codes with decimals only reduces the number
# of unique values from 883 to 849


#Results from Table1
#Comment on decision to leave repeated patient enc untouched though with a binary variable included
# indicating if they had a other encounters or not


