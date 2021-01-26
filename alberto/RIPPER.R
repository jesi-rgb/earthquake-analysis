rm(list=ls())

# set working directory correctly
setwd("C:/Users/alver/OneDrive/Documentos/repos/TrabajoPreprocesamiento")

# select memory space for RWeka processing
options(java.parameters = "-Xmx8000m")

# Load libraries
library(tidyverse)
library(RWeka)

# load preprocessed data
train_preprocessed <- read_csv("data/train_initial_preprocessing.csv")
test_values  <- read_csv("data/test_values.csv")

# get names
names <- colnames(train_preprocessed)

# convert into factors
factores <- c(1:3,8:26,28:38)

for (i in factores) {
  train_preprocessed[[names[i]]] <- train_preprocessed[[names[i]]] %>% as.factor()
  test_values[[names[i]]]  <- test_values[[names[i]]] %>% as.factor()
}

# Labels into ordered factor
train_preprocessed$damage_grade <- train_preprocessed$damage_grade %>% as.ordered()

set.seed(1)

# select % of instances from the original dataset for RIPPER run (random selection of instances)
proportion_total_train <- 0.3
total_train_indexes <- sample(1:nrow(train_preprocessed), round(proportion_total_train*nrow(train_preprocessed)))
train_ini <- train_preprocessed %>% slice(total_train_indexes)

# select train and test sets (90% train, 10% test, random selection)
proportion_train <- 0.9
train_indexes <- sort(sample(1:nrow(train_ini), round(proportion_train*nrow(train_ini))))

RIPPER <- function(seed_ripper, data, train_idx, selected_variables, num_folds){
  
  # select variables
  data <- data %>% select(selected_variables)
  
  # build the model using training data
  model.Ripper = JRip(damage_grade~., data, subset=train_idx)
  
  # get test indexes and test data
  test_indexes <- which(!(c(1:nrow(train_ini)) %in% train_idx))
  test_data <- train_ini %>% slice(test_indexes)
  
  # get the prediction and the accuracy of it
  model.Ripper.pred = predict(model.Ripper, newdata = test_data)
  accuracy <- (sum(as.integer(model.Ripper.pred) == as.integer(test_data %>% pull(damage_grade)))/nrow(test_data)*100)
  
  # Cross validation
  model.Ripper = JRip(damage_grade~., train_ini)
  cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=num_folds)
  
  # return the model, the accuracy with test data and the cross validation results
  list(model.Ripper, accuracy, cv_JRip)
  
}

# set parameters
seed_ripper <- 1
data <- train_ini
train_idx <- train_indexes
selected_variables <- names(train_ini)
num_folds <- 10

result <- RIPPER(seed, data, train_idx, selected_variables, num_folds)

# print results
print("RULES: ")
print(result[[1]])
print("ACCURACY OF THE INITIAL TEST DATA: ")
print(result[[2]])
print("CROSS VALIDATION RESULTS: ")
print(result[[3]])

# load test data

# use the model to predict test data
final_model <- result[[1]]
final_labels = predict(final_model, newdata = test_values %>% select(-building_id))

# build the file to upload to drivendata
final_test_labels <- data.frame(building_id = test_values$building_id, damage_grade=final_labels)
write_csv(final_test_labels, "data/test_labels_03_nopreproc.csv")

