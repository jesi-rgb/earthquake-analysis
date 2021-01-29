rm(list=ls())

# select memory space for RWeka processing
options(java.parameters = "-Xmx12000m")

# Load libraries
library(tidyverse)
library(RWeka)

# load preprocessed data
train_name <- "data/preprocessing/v13 - preprocessed_train.csv"
test_name <- "data/preprocessing/v13 - preprocessed_test.csv"
#train_labels_name <- "data/train_labels.csv"
train_labels_name <- "data/preprocessing/v13 - preprocessed_train_labels.csv"

# load test ids
test_values  <- read_csv("data/test_values.csv")
test_ids <- test_values$building_id

# load the train and test sets
train_preprocessed <- read_csv(train_name)
test_values <- read_csv(test_name)
train_labels <- read_csv(train_labels_name)
train_labels <- train_labels %>% select(damage_grade)

# get names
names <- colnames(train_preprocessed)

# convert into factors
# factores <- c(1:3,8:26,28:38) # v0
factores <- c(1,5:21, 23:33) #v6, v10, v12, v13, v13b

# convert training data into factors
for (i in factores) {
  train_preprocessed[[names[i]]] <- train_preprocessed[[names[i]]] %>% as.factor()
  test_values[[names[i]]] <- test_values[[names[i]]] %>% as.factor()
}

# add the building ids to the test values
test_values <- test_values %>% mutate(building_id = test_ids)

# Add train labels and convert them into ordered factor
train_preprocessed <- train_preprocessed %>% mutate(damage_grade = train_labels$damage_grade)
train_preprocessed$damage_grade <- train_preprocessed$damage_grade %>% as.ordered()

# function that runs the RIPPPER Algorithm
RIPPER <- function(train_data_p, num_folds){
  
  # build the model using training data
  model.Ripper = JRip(damage_grade~., train_data_p, control = Weka_control(F=4, O=3))
  
  # Cross validation
  cv_JRip = evaluate_Weka_classifier(model.Ripper,numFolds=num_folds)
  
  # return the model, the accuracy with test data and the cross validation results
  list(model.Ripper, cv_JRip)
  
}

# set parameters
num_folds <- 5

# run the algorithm
result <- RIPPER(train_preprocessed, num_folds)

# print results
print("RULES: ")
print(result[[1]])
print("CROSS VALIDATION RESULTS: ")
print(result[[2]])

# get F1 Score of the best model obtained using CV

# use the model to predict test data
final_model <- result[[1]]
final_labels = predict(final_model, newdata = test_values %>% select(-building_id))

# build the file to upload to drivendata
final_test_labels <- data.frame(building_id = test_values$building_id, damage_grade=final_labels)
write_csv(final_test_labels, "data/v13b - test_labels.csv")

# function for getting the F1 score
f1_score <- function(confusion_matrix, positive.class="1") {
  
  res <- list()
  
  cm = confusion_matrix
  
  tp <- diag(cm)
  fp <- cm[lower.tri(cm)]
  fn <- cm[upper.tri(cm)]
  
  res$precision <- tp / (tp + fp)
  res$recall <- tp / (tp + fn)
  
  f1 <-  ifelse(res$precision + res$recall == 0, 0,
                2 * res$precision * res$recall / (res$precision + res$recall))
  
  #Assuming that F1 is zero when it's not possible compute it
  f1[is.na(f1)] <- 0
  
  #Binary F1 or Multi-class macro-averaged F1
  res$f1 <- ifelse(ncol(cm) == 2, f1[positive.class], mean(f1))
  
  res
}

# Function for showing the F1 Score
compare_pred <- function(confusion_matrix) {
  confusion_matrix %>% print()
  
  f1_score(confusion_matrix) %>% print()
}

# get the R1 score of the best CV confusion matrix of the models

# === Confusion Matrix === modelo 0
#   
#   a     b     c   <-- classified as
# 2741  4672   117 |     a = 1
# 1525 36550  6298 |     b = 2
# 80 12589 13608 |     c = 3

cm0 <- matrix(c(2741,1525,80,4672,36550,12589,117,6298,13608),nrow=3,ncol=3)
compare_pred(cm0)

# === Confusion Matrix === modelo 3
#   
# a     b     c   <-- classified as
# 17498  7374   252 |     a = 1
# 1573 41851    38 |     b = 2
# 11    59 12464 |     c = 3

cm3 <- matrix(c(17498,1573,11,7374,41851,59,252,38,12464),nrow=3,ncol=3)
compare_pred(cm3)

# === Confusion Matrix === modelo 4
#   
#   a     b     c   <-- classified as
# 17307  7445   372 |     a = 1
# 2678 35123   103 |     b = 2
# 23   293 13576 |     c = 3

cm4 <- matrix(c(17307,2678,23,7445,35123,293,372,103,13576),nrow=3,ncol=3)
compare_pred(cm4)

# === Confusion Matrix === modelo 6
#   
#   a     b     c   <-- classified as
# 6821 13859   296 |     a = 1
# 3970 82883 14048 |     b = 2
# 213 31807 25378 |     c = 3

cm6 <- matrix(c(6821,3970,213,13859,82883,31807,296,14048,25378),nrow=3,ncol=3)
compare_pred(cm6)

# === Confusion Matrix === modelo 10
#   
# a     b     c   <-- classified as
# 5106  9936   174 |     a = 1
# 3257 50306  7480 |     b = 2
# 181 20132 12039 |     c = 3

cm10 <- matrix(c(5106,3257,181,9936,50306,20132,174,7480,12039),nrow=3,ncol=3)
compare_pred(cm10)

# === Confusion Matrix === modelo 12
#   
#   a      b      c   <-- classified as
# 5364    768      0 |      a = 1
# 78 135342   2567 |      b = 2
# 0   9679   7771 |      c = 3

cm12 <- matrix(c(5364,78,0,768,135342,9679,0,2567,7771),nrow=3,ncol=3)
compare_pred(cm12)

# === Confusion Matrix === modelo 13
#   
#   a      b      c   <-- classified as
# 8174   6876    122 |      a = 1
# 4033 117507  13439 |      b = 2
# 102  19678  41733 |      c = 3

cm13 <- matrix(c(8174,4033,102,6876,117507,19678,122,13439,41733),nrow=3,ncol=3)
compare_pred(cm13)

# === Confusion Matrix === modelo 13 b
#   
#   a      b      c   <-- classified as
# 8821   6243    108 |      a = 1
# 4609 117313  13057 |      b = 2
# 132  19751  41630 |      c = 3

cm13b <- matrix(c(8821,4609,132,6243,117313,19751,108,13057,41630),nrow=3,ncol=3)
compare_pred(cm13b)