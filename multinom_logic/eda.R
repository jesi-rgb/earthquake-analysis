library(tidyverse)

train_values <- read.csv(
  "data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Values.csv"
  )
train_labels <- read.csv(
  "data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Labels.csv"
  )
head(train_labels)

dim(data)

data %>%
  keep(is.numeric) %>%
  lapply(summary)

data %>%
  keep(is.character) %>%
  lapply(unique) %>%
  lapply(sort)

attach(data)
str(data)


  