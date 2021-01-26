rm(list=ls())

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
train_values <- train_values %>% mutate(damage_grade=(train_labels %>% select(damage_grade) %>% pull(damage_grade)))

# create initial training preprocessed data
write_csv(train_values, "data/train_initial_preprocessing.csv")

# cleaning noisy data - EF, CVCF...

# selección de caracteristicas, al tener tantos elementos mejor uno de selección hacia atrás

# comprobar Fselector, Caret y Boruta

# F selector

# backward search - wrapper

# iris
library(rpart)
library(FSelector)
data(iris)
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(iris))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- iris[test.idx, , drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "Species"), train)
    error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
subset <- forward.search(names(iris)[-5], evaluator)
f <- as.simple.formula(subset, "Species")
print(f)

# with earthquake dataset
library(rpart)
library(FSelector)
train_complete <- train_values %>% mutate(damage_grade=(train_labels %>% select(damage_grade) %>% pull(damage_grade))) %>% select(-c(geo_level_1_id,  geo_level_2_id, geo_level_3_id)) %>% slice(1:500)
train_complete <- train_complete %>% mutate(damage_grade=as.character.factor(damage_grade))
evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(train_complete))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- train_complete[test.idx, , drop=FALSE]
    train <- train_complete[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "damage_grade"), train)
    error.rate = sum(test$damage_grade != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}
subset <- forward.search(names(train_complete)[-36], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("forward.search:")
print(f)
subset <- backward.search(names(train_complete)[-36], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("backward.search:")
print(f)
subset <- hill.climbing.search(names(train_complete)[-36], evaluator)
f <- as.simple.formula(subset, "damage_grade")
print("hill.climbing.search:")
print(f)
