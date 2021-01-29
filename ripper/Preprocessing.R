rm(list=ls())

options(java.parameters = "-Xmx12000m")
memory.limit(size=56000)

# Load libraries
library(tidyverse)

# Load data

train_values <- read_csv("data/train_values.csv")
train_labels <- read_csv("data/train_labels.csv")
test_values  <- read_csv("data/test_values.csv")

# Nacho's Initial Preprocessing - remove ID, set colums to factors, add labels to training data

# Remove ID from rows and building_id variables
train_values.ids <- train_values$building_id
train_values$building_id <- NULL

# Store column names
names <- colnames(train_values)

#  select categorical data for turning it into a factor
factores <- c(1:3,8:26,28:38)

for (i in factores) {
  train_values[[names[i]]] <- train_values[[names[i]]] %>% as.factor()
}

# Labels into ordered factor
train_labels$damage_grade <- train_labels$damage_grade %>% as.ordered()

# add labels to training set
train_complete <- train_values %>% mutate(damage_grade=(train_labels %>% select(damage_grade) %>% pull(damage_grade)))
train_complete <- train_complete %>% mutate(damage_grade=as.character(damage_grade))
train_complete <- train_complete %>% mutate(damage_grade=as.factor(damage_grade))

# libraries for using NoiseFiltersR
library(NoiseFiltersR)
library(RWeka)
library(kknn)
library(nnet)
library(caret)
library(e1071)
library(rpart)
library(randomForest)
library(MASS)
library(rJava)
library(stats)
library(utils)

# IPF
# We fix a seed since there exists a random folds partition for the ensemble
set.seed(1)
out_IPF <- NoiseFiltersR::IPF(damage_grade~., data = train_complete, s = 2)
summary(out_IPF)
out_IPF$cleanData

# select variables to build the model number 12 and 13
train_name <- "data/preprocessing_nacho/v6 - preprocessed_train.csv"
train_for_vars <- read_csv(train_name)

# select instances for model 12 - IPF
train_IPF_final <- out_IPF$cleanData %>% select(names(train_for_vars))
write_csv(train_IPF_final, "data/preprocessing_nacho/v12 - preprocessed_train.csv")
write_csv(out_IPF$cleanData %>% select(damage_grade), "data/preprocessing_nacho/v12 - preprocessed_train_labels.csv")

# select instances for model 13 - CVCF
out_CVCF <- NoiseFiltersR::CVCF(damage_grade~., data = train_complete) 
write_csv(train_CVCF_final, "data/preprocessing_nacho/v13 - preprocessed_train.csv")
write_csv(out_CVCF$cleanData %>% select(damage_grade), "data/preprocessing_nacho/v13 - preprocessed_train_labels.csv")

# partitioning over the training_data for model using INFFC
train_complete_1 <- train_complete %>% slice(1:round(nrow(train_complete)/4))
train_complete_2 <- train_complete %>% slice(round(nrow(train_complete)/4)+1:round(nrow(train_complete)/4)*2)
train_complete_3 <- train_complete %>% slice(round(nrow(train_complete)/4)*2+1:round(nrow(train_complete)/4)*3)
train_complete_4 <- train_complete %>% slice(round(nrow(train_complete)/4)*3+1:nrow(train_complete))

# apply the filtering and conserve only clean data
out_INFFC1 <- NoiseFiltersR::INFFC(damage_grade~., data = train_complete_1)
cd1 <- out_INFFC1$cleanData
rm(out_INFFC1)
out_INFFC2 <- NoiseFiltersR::INFFC(damage_grade~., data = train_complete_2)
cd2 <- out_INFFC2$cleanData
rm(out_INFFC2)
out_INFFC3 <- NoiseFiltersR::INFFC(damage_grade~., data = train_complete_3)
cd3 <- out_INFFC3$cleanData
rm(out_INFFC3)
out_INFFC4 <- NoiseFiltersR::INFFC(damage_grade~., data = train_complete_4)
cd4 <- out_INFFC4$cleanData
rm(out_INFFC4)

out_INFFC <- rbind(cd1, cd2, cd3, cd4)

# select variables to build the model number 13-14
train_name <- "data/preprocessing_nacho/v6 - preprocessed_train.csv"
train_for_vars <- read_csv(train_name)

# run INFFC - Inviable computacionalmente - +4 horas de ejecución sin resultados
train_INFFC_final <- out_INFFC$cleanData %>% select(names(train_for_vars))
write_csv(train_INFFC_final, "data/preprocessing_nacho/v14 - preprocessed_train.csv")
write_csv(out_INFFC %>% select(damage_grade), "data/preprocessing_nacho/v14 - preprocessed_train_labels.csv")

# F selector

library(rpart)
library(FSelector)
library(mlbench)

names <- names(train_complete)

# evaluator for the wrapper
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(train_complete))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- train_complete[test.idx, , drop=FALSE]
    train <- train_complete[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "damage_grade"), train) # we use classification trees to make the classification model
    error.rate = sum(test$damage_grade != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

# no son viables computacionalmente
subset <- forward.search(names(train_complete)[-which(names == "damage_grade")], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("forward.search:")
print(f)

subset <- backward.search(names(train_complete)[-which(names == "damage_grade")], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("backward.search:")
print(f)

subset <- hill.climbing.search(names(train_complete)[-which(names == "damage_grade")], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("hill.climbing.search:")
print(f)

subset <- best.first.search(names(train_complete)[-which(names == "damage_grade")], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("best.first.search:")
print(f)

# random forest

train_complete_1<- train_complete %>% slice(c(1:20000))
train_complete_2 <- train_complete %>% slice(c(1:40000))
train_complete_3 <- train_complete %>% slice(c(1:60000))
train_complete_4 <- train_complete %>% slice(c(1:80000))
train_complete_5 <- train_complete %>% slice(c(1:100000))
train_complete_6 <- train_complete %>% slice(c(1:120000))
train_complete_7 <- train_complete %>% slice(c(1:140000))
train_complete_8 <- train_complete %>% slice(c(1:160000))
train_complete_9 <- train_complete %>% slice(c(1:180000))
train_complete_10 <- train_complete %>% slice(c(1:200000))
train_complete_11 <- train_complete %>% slice(c(1:220000))
train_complete_12 <- train_complete %>% slice(c(1:240000))
train_complete_13 <- train_complete %>% slice(c(1:260000))

weights_1 <- random.forest.importance(damage_grade~., train_complete_1 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_2 <- random.forest.importance(damage_grade~., train_complete_2 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_3 <- random.forest.importance(damage_grade~., train_complete_3 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_4 <- random.forest.importance(damage_grade~., train_complete_4 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_5 <- random.forest.importance(damage_grade~., train_complete_5 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_6 <- random.forest.importance(damage_grade~., train_complete_6 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_7 <- random.forest.importance(damage_grade~., train_complete_7 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_8 <- random.forest.importance(damage_grade~., train_complete_8 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_9 <- random.forest.importance(damage_grade~., train_complete_9 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_10 <- random.forest.importance(damage_grade~., train_complete_10 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_11 <- random.forest.importance(damage_grade~., train_complete_11 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_12 <- random.forest.importance(damage_grade~., train_complete_12 %>% select(-c(geo_level_2_id, geo_level_3_id)))
weights_13 <- random.forest.importance(damage_grade~., train_complete_13 %>% select(-c(geo_level_2_id, geo_level_3_id)))

# oneR

weights <- oneR(damage_grade~., train_complete)
print(weights)
subset <- cutoff.k(weights, 5)
f <- as.simple.formula(subset, "damage_grade")
print(f)


