library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(scales)
library(icd)
library(stringr)
setwd("~/Desktop/Data Science/Project/Data Files/")
full_dataset <- read_csv("diabetic_data.csv")
id_map <- read_csv("IDs_mapping.csv")
admit_type_map <- id_map[1:8,]
admit_type_map$admission_type_id <- as.numeric(admit_type_map$admission_type_id)
names(admit_type_map)[2] <- 'admit_type_desc'

admit_source_map <- id_map[43:67,]
names(admit_source_map) <- c('admission_source_id', 'admit_source_desc')
admit_source_map$admission_source_id <- as.numeric(admit_source_map$admission_source_id)

#Randomly select a single encounter from patients that have more than one
# IMPORTANT NOTE: The decision to remove multiple encounters from the same patient was done AFTER the vast majority of data exploration
# and nearly all of the data cleaning. However, this is unlikely to have a significant impact on the validity of the data preparation/modelling
# decisions made.
full_dataset <- distinct(full_dataset[sample(1:nrow(full_dataset)), ], 
                         patient_nbr, 
                         .keep_all = TRUE)

##### Split into training, validation, and test sets #####
fractionTraining   <- 0.70
fractionValidation <- 0.15
fractionTest       <- 0.15

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(full_dataset))
sampleSizeValidation <- floor(fractionValidation * nrow(full_dataset))
sampleSizeTest       <- floor(fractionTest       * nrow(full_dataset))

# Create the randomly-sampled indices for the dataframe.
indicesTraining    <- sort(sample(seq_len(nrow(full_dataset)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(full_dataset)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test. Exploratory analysis will only be
# performed on training set
train   <- full_dataset[indicesTraining, ]
validation <- full_dataset[indicesValidation, ]
test   <- full_dataset[indicesTest, ]



##### Feature creation and clustering of existing variables #####

#Define function for grouping ICD codes
group_icd <- function(icd){
        if(grepl(pattern = 'V|E', icd)) return("external_cause")
        else if (icd == "?") return(NA)
        
        icd <- floor(as.numeric(as.character(icd)))
        if(icd < 140) return("infectious_parasitic")
        else if (icd >= 140 && icd < 240) return ("neoplasms")
        else if (icd >= 240 && icd < 280 && icd != 250) return('endocrine_nutritional_immunity')
        else if (icd == 250) return("diabetes_mellitus")
        else if (icd >= 280 & icd < 290) return("blood")
        else if (icd >= 290 & icd < 320) return("mental_disorder")
        else if (icd >= 320 & icd < 390) return("nervous_system")
        else if (icd >= 390 & icd < 460 || icd == 785) return("circulatory")
        else if (icd >= 460 & icd < 520 || icd == 786) return("respitory")
        else if (icd >= 520 & icd < 580 || icd == 787) return("digestive")
        else if (icd >= 580 & icd < 630 || icd == 788) return("genitourinary")
        else if (icd >= 630 & icd < 680) return("pregnancy")
        else if (icd >= 680 & icd < 710 || icd == 782) return("skin")
        else if (icd >= 710 & icd < 740) return("musculoskeletal")
        else if (icd >= 740 & icd < 760) return("congenital_anomalies")
        else if (icd >= 760 & icd < 780) return("perinatal_related")
        else if (icd >= 789 & icd < 800 || icd %in% c(780, 781, 783, 784)) return("ill_defined")
        else if (icd >= 800 & icd < 1000) return("injury_or_poisoning")
}


#Definte '%notin%' function for use in grouping Admit sources
'%notin%' <-  Negate('%in%') 



#Define function for completing remainder of cleaning tasks
clean_df <- function(df){

  #Replace admission type and source codes with their respective descriptions
  df <- left_join(df, admit_type_map, by = 'admission_type_id')
  df <- left_join(df, admit_source_map, by = 'admission_source_id')
  
  #Group everything not "Emergency", "Elective" or "Urgent" together
  df$admit_type_desc <- ifelse(df$admit_type_desc %notin% c("Emergency", "Elective", "Urgent"),'Other', df$admit_type_desc)
  
  #Group everything not "ER or MD Referral together
  df$admit_source_desc <- ifelse(df$admit_source_desc %notin% c('Physician Referral', 'Emergency Room'),
                                 'Other', df$admit_source_desc)
  df <- select(df, -admission_type_id,  -admission_source_id) #Remove old ID variable
  
  
  
  #Initialize primary outcome measure: whether a patient remained in hospital one week or longer
  df$long_los <- ifelse(df$time_in_hospital > 6, 1, 0)
  
  
  
  #Remove weight variable as 97% missingness
  df <- select(df, - weight)
  
  
  
  #Remove payer code variable as 52% missing and not relevant to outcome variable
  df <- select(df, - payer_code)
  
  
  
  #Remove num_lab_procedures, num_procedures, num_medications, discharge_disposition_id as they are different
  # expressions of the primary outcome variables (LOS)
  df <- select(df, -num_lab_procedures, -num_procedures, -num_medications, -discharge_disposition_id, -readmitted)
  
  
  
  #Group Medical Speciality into the following categories (based on my own domain knowledge)
  #Internal Medecine, Cardiology, Surgery, Family/general practice, Missing or unknown, Other
  med_spec <- read_excel("med_specialty_groups.xlsx")
  df <- left_join(df, med_spec, by = 'medical_specialty')
  df$medical_specialty_grouped[df$medical_specialty_grouped=='0'] <- 'Missing'
  
  
  
  #Group races together that comprise less than 5% of total
  df$race <- ifelse(df$race %in% c('Other', "?", "Asian", "Hispanic"), "Other", df$race)
  
  
  
  #Randomly impute a gender (only 2 cases out of 50K records)
  df$gender <- ifelse(df$gender == 'Unknown/Invalid',
                     sample(sum(df$gender=='Unknown/Invalid'), x = c("Male", "Female"), replace = T),
                     df$gender)
  
  
  
  #Group ICD codes
  df$diag_1_group <- do.call(rbind, lapply(X = df$diag_1, FUN = group_icd))
  df$diag_2_group <- do.call(rbind, lapply(X = df$diag_2, FUN = group_icd))
  df$diag_3_group <- do.call(rbind, lapply(X = df$diag_3, FUN = group_icd))
  
  # Spread each diagnoses group into its own binary variable
  var_names <- unique(c(df$diag_1_group, df$diag_2_group, df$diag_3_group))
  var_names <- var_names[!is.na(var_names)]
  
  for(i in var_names){
    df[[i]] <- 0
    for (j in 1:nrow(df)){
      df[[i]][j] <- ifelse(df$diag_1_group[j] == i || df$diag_2_group[j] == i || df$diag_3_group[j] == i, 1, 0)
    }
  }
  df <- mutate_at(df, 45:ncol(df), ~replace(., is.na(.), 0)) #Fix NA's created from blank diag_2 or diag_3 field
  
  #Remove original diagnosis group variables
  df <- select(df, -diag_1_group, -diag_2_group, -diag_3_group)
  
  
  
  # Fix column names with "-" in them
  names(df) <- str_replace(names(df), pattern = "-", replacement = "_")
  
  
  
  # Create comorbidity features using AHRQ Comorbidity Map
  df <- bind_cols(df, as_tibble(icd9_comorbid(df, map = icd9_map_ahrq, return_binary = T)))
  
  
  
  # Add "other encounters" flag to identify patients with multiple encounters
  ## DEPRECATED since decision to remove multiple encounters for each unique patient ##
  # readmit_encs <- df %>%
  #   group_by(patient_nbr) %>%
  #   summarize(count = n()) %>%
  #   filter(count > 1) %>%
  #   ungroup()
  # df$multiple_enc <- ifelse(train$patient_nbr %in% readmit_encs$patient_nbr, 1, 0)
  
  return(df)
}

train <- clean_df(train)
test <- clean_df(test)
validation <- clean_df(validation)



save(train, file = "train_phase1.R")
save(test, file = "test_phase1.R")
save(validation, file = "validation_phase1.R")
