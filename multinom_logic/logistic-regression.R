library(nnet)
# library(mgcv)


train_values <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Values.csv"
)
train_labels <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Labels.csv"
)




full_data = train_values
full_data$damage_grade = train_labels$damage_grade
full_data$damage_grade = factor(full_data$damage_grade, levels = c(1, 2, 3), labels=c("low damage", "medium damage", "almost destructed"))
full_data$damage_grade = relevel(full_data$damage_grade, ref=1)

char_to_num = function(data){
  data[sapply(data, is.character)] = lapply(data[sapply(data, is.character)], as.factor)
  data[sapply(data, is.factor)] = lapply(data[sapply(data, is.factor)], function(x) {as.numeric(x)})
  return(data)
}

full_data = char_to_num(full_data)

norm_data = data.frame(apply(full_data, 2, scale))
head(norm_data)

prediction = function(n, formula){
  #build model
  model = multinom(formula, data = norm_data[1:n,])
  
  #error calculation
  confMat = table(predict(model), norm_data[1:n,]$damage_grade)
  
  
  total_error = sum(diag(confMat))/sum(confMat)
  print(total_error)
}

attach(full_data)
n = 260601 #260601
formula = damage_grade ~ .

prediction(n, formula)



#### POLR ####
library(MASS)
full_data$damage_grade = factor(full_data$damage_grade, ordered = T, levels = c(1, 2, 3), labels=c("low damage", "medium damage", "almost destructed"))
str(full_data)
full

model = polr(damage_grade ~ ., full_data)
mean(predict(model) == damage_grade)

predict(model, char_to_num(test_values))



#### CREACIÓN DEL ARCHIVO DE TEST ####
test_values <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Test_Values.csv"
)

# El mejor modelo resultó ser el no procesado, con todas las variables.
# Construimos el archivo para subirlo a DrivenData.
model = multinom(damage_grade ~ ., data = full_data)

# Comprobamos que sigue obteniendo ese score
mean(predict(model) == damage_grade)

# Predecimos sobre test y creamos un dataset con solo la prediccion y el id
output = data.frame(test_values$building_id, as.integer(predict(model, test_values)))

# Renaming por si acaso
names(output)[1] <- "building_id"
names(output)[2] <- "damage_grade"

# Comprobamos que todo está en orden
str(output)

# Escribimos a disco, ready para enviar!
write.csv(output, file="submission_logistic_reg.csv", row.names = F)
