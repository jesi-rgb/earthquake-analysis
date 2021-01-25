library(regtools)

# Import data as usual...
train_values <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Values.csv"
)
train_labels <- read.csv(
  "../data/Richters_Predictor_Modeling_Earthquake_Damage_-_Train_Labels.csv"
)

# In order for the data to work with the regtools library, we must convert all columns
# to numeric or int variables. So, we factor the target variable and then parse it as numeric.
full_data = train_values
full_data$damage_grade = train_labels$damage_grade
full_data$damage_grade = factor(full_data$damage_grade, levels = c(1, 2, 3), labels=c("low damage", "medium damage", "almost destructed"))
full_data$damage_grade = as.numeric(full_data$damage_grade) - 1 
head(full_data)

# We do the same with all non-numeric variables
full_data[sapply(full_data, is.character)] = lapply(full_data[sapply(full_data, is.character)], as.factor)
full_data[sapply(full_data, is.factor)] = lapply(full_data[sapply(full_data, is.factor)], function(x) {as.numeric(x)-1})
head(full_data)

# Let's start with the classifiers.

#### One Vs All ####
# For it to work, we must set the target column as the last one in the dataframe,
# so we can just make a subset of the dataset in the order we need. Since the target
# was already the last one, its index is 40, but when making that selection it is
# necessary.
ovatrn = ovalogtrn(3, full_data[ ,c(2,5:39, 40)])


# Finally, to predict, we get rid of the target column and use the 
# predict helper function.
ovaypred <- ovalogpred(ovatrn, full_data[,c(2,5:39)])

# The mean function over a boolean vector gives us the proportion of true/all
# values in the vector. Essentially: the accuracy.
mean(ovaypred == full_data$damage_grade)
# [1] 0.581832 Not a great result, let's keep trying.

# Let us try with quadratic data, which may exaggerate some of the features in
# the variables and, possibly, make it a bit easier for the algorithm.

# Take all the columns we are interested in and square them. Save that subset.
quadratic_data = full_data[,c(2,5:39)]^2

# Now the subset lacks the target variable, so we append it before it is passed
# to the function via cbind (column bind).
qovadata = ovalogtrn(3, cbind(quadratic_data, full_data$damage_grade))

# We predict as usual and get our boolean vector.
qovaypred <- ovalogpred(qovadata, quadratic_data)

mean(qovaypred == full_data$damage_grade)
# [1] 0.5794606 Was even worse, but not by much. 


#### All vs All ####
# For it to work, we must pass in a matrix, not a dataframe. Let us convert 
# the data into matrix form, again, with the subset we are interested in, and 
# the target value last.
data_matrix = data.matrix(full_data[ ,c(2,5:39, 40)])

# Call ava train function and classify.
avatrn = avalogtrn(3, data_matrix)

# Predict as usual, but keeping the target variable out. No need for matrix form
# in this function.
avaypred <- avalogpred(3, avatrn, full_data[,c(2,5:39)])

mean(avaypred == full_data$damage_grade)
# [1] 0.5822656 We did not improve much, if at all.

# Let us try again with quadratic data.
# The quadratic version was defined before, so we can just use it.
data_matrix = data.matrix(cbind(quadratic_data, full_data$damage_grade))

# Train...
avatrn = avalogtrn(3, data_matrix)

# Predict...
avaypred <- avalogpred(3, avatrn, full_data[,c(2,5:39)])

mean(avaypred == full_data$damage_grade)
# [1] 0.5746448 And it seems like the quadratic strategy did not help much.





