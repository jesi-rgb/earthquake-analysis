library(ggplot2)
library(tidyr)
library(dplyr)
library(e1071)
library(glm2)
library(readr)



train_values <- read_csv("data/train_values.csv")

cols(
  .default = col_double(),
  land_surface_condition = col_character(),
  foundation_type = col_character(),
  roof_type = col_character(),
  ground_floor_type = col_character(),
  other_floor_type = col_character(),
  position = col_character(),
  plan_configuration = col_character(),
  legal_ownership_status = col_character()
)


train_labels <- read_csv("data/train_labels.csv")

cols(
  building_id = col_double(),
  damage_grade = col_double()
)



test_values  <- read_csv("data/test_values.csv")

cols(
  .default = col_double(),
  land_surface_condition = col_character(),
  foundation_type = col_character(),
  roof_type = col_character(),
  ground_floor_type = col_character(),
  other_floor_type = col_character(),
  position = col_character(),
  plan_configuration = col_character(),
  legal_ownership_status = col_character()
)





############  Variables: geo_level_1_id + area_percentage ############


datos <- tibble::as_tibble(data.frame(train_values$geo_level_1_id, train_values$area_percentage,
                                      damage_grade = factor(train_labels$damage_grade, c(1,2,3), c("1", "2", "3"))))

colnames(datos) <- c("geo_level_1_id","area_percentage","damage_grade")



x_samplingSize = 10000
datos_svm <- datos[1:x_samplingSize,]
datos_svm <- datos_svm[datos_svm[,2] < 70,] #Eliminación de dato fuera de rango


ggplot2::ggplot(data = datos_svm, aes(x = geo_level_1_id, y = area_percentage, color = as.factor(damage_grade))) +
  geom_point(size = 2) +
  labs(title = "Clases no separables linealmente") +
  theme( legend.position = "none",
         plot.title = element_text(hjust = 0.5, size = 11)
  ) + scale_color_manual(values=c("springgreen3", "orange", "purple4"))



svm1 <- svm(damage_grade~., data=datos_svm, 
            method="C-classification", kernal="radial", 
            gamma=5, cost=30)

summary(svm1)

svm1$SV

plot(svm1, datos_svm, area_percentage ~ geo_level_1_id)

prediction <- predict(svm1, datos_svm)
xtab <- table(train_labels[1:nrow(datos_svm),]$damage_grade, prediction)
xtab

(xtab[1,1]+xtab[2,2]+xtab[3,3])/nrow(datos_svm)







modeloCV <- e1071::tune("svm", damage_grade~., data=datos_svm, 
                        kernel="radial", 
                        ranges = list(cost = c(0.9, 0.1, 1, 2, 10, 30),
                                      gamma = c(0.9, 1, 2, 3, 4, 5)))


summary(modeloCV)
