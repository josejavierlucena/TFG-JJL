#######
library(readr)
datos <- read_csv("breast-cancer.csv")
View(datos)
summary(datos)
any(is.na(datos))
which(is.na(datos))

boxplot(datos$radius_mean)
boxplot(datos$symmetry_mean)
boxplot(datos$concavity_se)

#Correlacion entre variables
install.packages("corrplot")
library(corrplot)
corrplot(cor(datos[,3:ncol(datos)]))
#objective is to train a classifier model 
#on cancer cells characteristics dataset to 
#predict whether 
#the cell is B = benign or M = malignant.

#radio lobulos 
#media textura superficial
#perimetro exterior de lobulos
#area media de lobulos
#media niveles de suavidad
#media compacidad
#media concavidad
#media de puntos concavos
#media simetria (cuanto difiere una mama de otra)
#media dimension fractal (mide nivel de anormalidad y agresividad del cancer de mama)
#error estandar del radio
#error estandar de la textura
#error estandar del perimetro
#error estandar del area
#error estandar de suavidad
#error estandar compacidad
#error estandar concavidad
#error estandar puntos cóncavos
#error estandar simetria
#error estandar dimension fractal