library(tidyverse)
library(nnet)
library(scales)


train_values <- read.csv(
  "data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Values.csv"
)
train_labels <- read.csv(
  "data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Labels.csv"
)
head(train_labels)

full_data = train_values
full_data$damage_grade = train_labels$damage_grade
full_data$damage_grade = factor(full_data$damage_grade, levels = c(1, 2, 3), labels=c("low damage", "medium damage", "almost destructed"))
full_data$damage_grade = relevel(full_data$damage_grade, ref=1)
str(full_data)



prediction = function(n, formula){
  #build model
  model = multinom(formula, data = full_data[1:n,])
  
  #error calculation
  confMat = table(predict(model), full_data[1:n,]$damage_grade)
  
  
  total_error = sum(diag(confMat))/sum(confMat)
  print("______________")
  print(total_error)
  
  z = summary(model)$coefficients/summary(model)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  print(p)
}

attach(full_data)
n = 100000 #260601
formula = damage_grade ~ .
prediction(n, formula)


full_data %>% ggplot(aes(x=plan_configuration)) + geom_bar() + scale_y_continuous(trans = "log10")
full_data %>% count(ground_floor_type)

full_data %>% count(position)

