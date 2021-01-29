library(e1071)
library(parallelSVM)
library(fastDummies)
library(ggplot2)
library(tidyr)
library(dplyr)
library(glm2)
library(readr)
library(parallel)


##### ParallelSVM #####

train_values <- read_csv("data/preprocessed/v6 - preprocessed_train.csv")

train_labels <- read_csv("data/preprocessed/v6 - preprocessed_train_labels.csv")

test_values  <- read_csv("data/preprocessed/v6 - preprocessed_test.csv")


x <- train_values

#one-hot encoding para las columnas dummy
x_dummy <- dummy_cols(x)
x_dummy <- x

#Eliminamos las columnas character
x_dummy <- x_dummy[,-which(sapply(x_dummy, class) == "character")]


y <- factor(train_labels$damage_grade, c(1,2,3), c("low damage", "medium damage", "almost destructed"))








############### INI - Determinamos el mejor valor para Cost y Gamma ############
############### 
set.seed(1)

datos_svm = tibble::as_tibble(
  data.frame(
    x_dummy,
    y
  )
)

#Tamaño del dataset para hacer el tune
tama_ds_modeloCV = 2000
index = as.integer(runif(tama_ds_modeloCV, min = 1, max = nrow(x_dummy)))

system.time(
  modeloCV <- tune("svm", y~., data = datos_svm[index,], kernel = "radial",
                   ranges = list(cost = c(0.01, 0.1, 1, 2, 10),
                                 gamma = c(0.1, 0.5, 1, 2, 5))
  )
)

summary(modeloCV)

##### (Last) Parameters:
#####    SVM-Type:  eps-regression 
#####  SVM-Kernel:  radial 
#####        cost:  2 
#####       gamma:  0.1 
#####     epsilon:  0.1
modeloCV$best.model
############### 
############### FIN - Determinamos el mejor valor para Cost y Gamma ############







# Size of your data or of x you will take in each sample
x_samplingSize = 0.1

# Create a model
system.time(
  model <- parallelSVM(x_dummy, y, numberCores=8, kernel="radial",
                       samplingSize=x_samplingSize, cost=2, gamma=0.1)
)

# Get prediction
system.time(
  predictions <- predict(model, x_dummy)
)


# Check the quality
xtab <- as.data.frame.matrix(table(predictions,y))

xtab <- xtab %>% select(sort(names(.)))
xtab

quality <- (xtab[1,1]+xtab[2,2]+xtab[3,3])/nrow(x_dummy)

sprintf("Tamaño de dataset para crear el modelo: %s", x_samplingSize)
sprintf("Cost del modelo: %s", model[[1]]$cost)
sprintf("Gamma del modelo: %s", model[[1]]$gamma)
sprintf("Precisión del modelo: %s", quality)







################################################################################
################################################################################
################################################################################
#######################   Get prediction of TEST_VALUES   ######################
################################################################################
################################################################################
################################################################################

t_values <- test_values

#one-hot encoding para las columnas dummy
t_values_dummy <- dummy_cols(t_values)
t_values_dummy <- t_values

#Eliminamos las columnas character
t_values_dummy <- t_values_dummy[,-which(sapply(t_values_dummy, class) == "character")]

# Get prediction
system.time(
  predictions_test <- predict(model, t_values_dummy)
)

levels(predictions_test) <- c(3,1,2)

test_labels <- data.frame(read_csv("data/test_values.csv")$building_id , predictions_test)
test_labels <- unite(test_labels, "building_id,damage_grade",c(1:2),  sep = ",", remove = TRUE)
str(test_labels)

write.csv(test_labels, file = 'data/test_labels.csv', row.names=FALSE)


## End(Not run)