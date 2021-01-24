library(regtools)

train_values <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Values.csv"
)
train_labels <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Labels.csv"
)

# in order for the data to work with the regtools library, we must convert all columns
# to numeric or int variables. So, we factor the target variable
full_data = train_values
full_data$damage_grade = train_labels$damage_grade
full_data$damage_grade = factor(full_data$damage_grade, levels = c(1, 2, 3), labels=c("low damage", "medium damage", "almost destructed"))
full_data$damage_grade = as.numeric(full_data$damage_grade) - 1 
head(full_data)


full_data[sapply(full_data, is.character)] = lapply(full_data[sapply(full_data, is.character)], as.factor)
full_data[sapply(full_data, is.factor)] = lapply(full_data[sapply(full_data, is.factor)], function(x) {as.numeric(x)-1})
head(full_data)

# TRY OVO
# for it to work, we must set the target column as the last one in the dataframe
ovotrn = ovalogtrn(3, full_data[ ,c(2,5:39, 40)])
ovoypred <- ovalogpred(ovotrn, full_data[,c(2,5:39)])

mean(ovoypred == full_data$damage_grade)
# [1] 0.581832

# TRY QUADRATIC DATA
quadratic_data = full_data[,c(2,5:39)]^2

qovodata = ovalogtrn(3, cbind(quadratic_data, full_data$damage_grade))
qovoypred <- ovalogpred(qovodata, quadratic_data)

mean(qovoypred == full_data$damage_grade)
# [1] 0.5794606


# TRY AVA
data_matrix = data.matrix(full_data[ ,c(2,5:39, 40)])
avatrn = avalogtrn(3, data_matrix)
avaypred <- avalogpred(3, avatrn, full_data[,c(2,5:39)])

mean(avaypred == full_data$damage_grade)
# [1] 0.5822656

# TRY QUADRATIC DATA
#quadratic data defined before
data_matrix = data.matrix(cbind(quadratic_data, full_data$damage_grade))
avatrn = avalogtrn(3, data_matrix)
avaypred <- avalogpred(3, avatrn, full_data[,c(2,5:39)])

mean(avaypred == full_data$damage_grade)
# [1] 0.5746448





