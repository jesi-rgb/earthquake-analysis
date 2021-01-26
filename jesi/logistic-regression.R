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




prediction = function(n, formula){
  #build model
  model = multinom(formula, data = full_data[1:n,])
  
  #error calculation
  confMat = table(predict(model), full_data[1:n,]$damage_grade)
  
  
  total_error = sum(diag(confMat))/sum(confMat)
  print(total_error)
  
  z = summary(model)$coefficients/summary(model)$standard.errors
  p <- (1 - pnorm(abs(z), 0, 1)) * 2
  print(p)
}

attach(full_data)
n = 100000 #260601
formula = damage_grade ~ . -building_id - geo_level_2_id - 
                            geo_level_3_id - 
                            area_percentage - 
                            height_percentage - 
                            has_superstructure_rc_non_engineered -
                            legal_ownership_status -
                            plan_configuration -
                            has_secondary_use_institution -
                            has_secondary_use_health_post -
                            has_secondary_use_use_police -
                            has_secondary_use_other -
                            has_secondary_use_rental -
                            has_secondary_use_industry -
                            has_secondary_use_gov_office -
                            has_superstructure_cement_mortar_brick -
                            has_superstructure_cement_mortar_stone

prediction(n, formula)



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
