library(e1071)
library(parallelSVM)
library(fastDummies)
library(ggplot2)
library(tidyr)
library(dplyr)
library(glm2)
library(readr)
library(parallel)


train_values <- subset(read_csv("data/preprocessed/v6 - preprocessed_train.csv"), select = c(geo_level_1_id, area_percentage))

train_labels <- read_csv("data/preprocessed/v6 - preprocessed_train_labels.csv")

y <- factor(train_labels$damage_grade, c(1,2,3), c("low damage", "medium damage", "almost destructed"))


data_plot <- train_values[1:10000,]
y_plot <- y[1:10000]
y_plot <- y_plot[data_plot[,2] < 70]
data_plot <- data_plot[data_plot[,2] < 70,] #Eliminación de dato fuera de rango


ggplot2::ggplot(data = data_plot, aes(x = geo_level_1_id, y = area_percentage, color = as.factor(y_plot))) +
  geom_point(size = 2) +
  labs(title = "Clases no separables linealmente") +
  theme( legend.position = "none",
         plot.title = element_text(hjust = 0.5, size = 11)
  ) + scale_color_manual(values=c("springgreen3", "orange", "purple4"))







############### INI - Determinamos el mejor valor para Cost y Gamma ############
############### 
set.seed(1)

datos_svm = tibble::as_tibble(
  data.frame(
    data_plot,
    y_plot
  )
)

#Tamaño del dataset para hacer el tune
tama_ds_modeloCV = 2000
index = as.integer(runif(tama_ds_modeloCV, min = 1, max = nrow(data_plot)))

system.time(
  modeloCV <- tune("svm", y_plot~., data = datos_svm[index,], kernel = "radial",
                   ranges = list(cost = c(0.01, 0.1, 1, 2, 10, 30),
                                 gamma = c(0.1, 0.5, 1, 2, 5))
  )
)

summary(modeloCV)

##### (Last) Parameters:
#####    SVM-Type:  eps-regression 
#####  SVM-Kernel:  radial 
#####        cost:  30
#####       gamma:  5
#####     epsilon:  0.1
modeloCV$best.model
############### 
############### FIN - Determinamos el mejor valor para Cost y Gamma ############







# Create a model
system.time(
  model <- parallelSVM(data_plot, y_plot, numberCores=2, kernel="radial",
                       cost=30, gamma=5)
)


# interpolamos de nuevo puntos para ver la frontera de decision
# se utilizan nuevos puntos para predecir segun el modelo y asi poder
# colorear el espacio del grafico
rangoX1 = range(data_plot$geo_level_1_id)
rangoX2 = range(data_plot$area_percentage)

# generacion de puntos (100 x 100 como combinacion de cada uno
# de X1 con todos los de X2)
valoresX1 = seq(from = rangoX1[1], to = rangoX1[2], length=400)
valoresX2 = seq(from = rangoX2[1], to = rangoX2[2], length=400)
nuevosPuntos = tibble::as_tibble(expand.grid(geo_level_1_id=valoresX1, area_percentage = valoresX2))
str(nuevosPuntos)

# Get prediction
system.time(
  predictions_points <- predict(model, nuevosPuntos)
)

# se almacenan los puntos predichos para dar color a las
# regiones
colorRegiones <- data.frame(nuevosPuntos, y = predictions_points)

# se extrae la ecuacion del hiperplano y del margen
# se obtienen los coeficientes
coeficientes <- model[[1]]$coefs





ggplot() +
  # Representación de las 2 regiones empleando los puntos y coloreándolos
  # según la clase predicha por el modelo
  geom_point(data = colorRegiones, aes(x = geo_level_1_id, y = area_percentage, color = as.factor(y)),
             size = 0.5, alpha=0.1) +
  # Se añaden las observaciones
  geom_point(data = data_plot, aes(x = geo_level_1_id, y = area_percentage, color = as.factor(y_plot)),
             size = 2.5) +
  # Se identifican aquellas observaciones que son vectores soporte
  geom_point(data = data_plot[model[[1]]$index, ],
             aes(x = geo_level_1_id, y = area_percentage, color = as.factor(y_plot)),
             shape = 21, colour = "black",
             size = 2.5) +
  theme_bw() +
  theme(legend.position = "none") + 
  scale_color_manual(values=c("springgreen3", "orange", "purple4"))






# Check the quality


system.time(
  predictions <- predict(model, data_plot)
)


xtab <- as.data.frame.matrix(table(predictions,y_plot))

xtab <- xtab %>% select(sort(names(.)))
xtab

quality <- (xtab[1,1]+xtab[2,2]+xtab[3,3])/nrow(data_plot)

sprintf("Tamaño de dataset para crear el modelo: %s", nrow(data_plot))
sprintf("Cost del modelo: %s", model[[1]]$cost)
sprintf("Gamma del modelo: %s", model[[1]]$gamma)
sprintf("Precisión del modelo: %s", quality)

