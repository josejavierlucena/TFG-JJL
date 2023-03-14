#######
library(readr)
datos <- read_csv("breast-cancer.csv")
View(datos)
summary(datos)
any(is.na(datos))
which(is.na(datos))


#Correlacion entre variables
install.packages("corrplot")
library(corrplot)
corrplot(cor(datos[,3:ncol(datos)]))
#objective is to train a classifier model 
#on cancer cells characteristics dataset to 
#predict whether 
#the cell is B = benign or M = malignant.

str(datos)
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


boxplot(x=datos$radius_mean)
boxplot(x=datos$texture_mean)
boxplot(x=datos$perimeter_mean)
boxplot(x=datos$area_mean)
boxplot(x=datos$smoothness_mean)
boxplot(x=datos$compactness_mean)
boxplot(x=datos$concavity_mean)
boxplot(x=datos$`concave points_mean`)
boxplot(x=datos$symmetry_mean)
boxplot(x=datos$fractal_dimension_mean)

boxplot(x=datos$radius_se)
boxplot(x=datos$texture_se)
boxplot(x=datos$perimeter_se)
boxplot(x=datos$area_se)
boxplot(x=datos$smoothness_se)
boxplot(x=datos$compactness_se)
boxplot(x=datos$concavity_se)
boxplot(x=datos$`concave points_se`)
boxplot(x=datos$symmetry_se)
boxplot(x=datos$fractal_dimension_se)

boxplot(x=datos$radius_worst)
boxplot(x=datos$texture_worst)
boxplot(x=datos$perimeter_worst)
boxplot(x=datos$area_worst)
boxplot(x=datos$smoothness_worst)
boxplot(x=datos$compactness_worst)
boxplot(x=datos$concavity_worst)
boxplot(x=datos$`concave points_worst`)
boxplot(x=datos$symmetry_worst)
boxplot(x=datos$fractal_dimension_worst)



##########
#Analisis en componentes pples


#de todas las variables
G <- cov.wt(datos[,3:ncol(datos)], method = "ML")$center
G

V<- cov.wt(datos[,3:ncol(datos)], method="ML")$cov
V

escala <- diag(V)
escala

Z <- scale(datos[,3:ncol(datos)], center=G, scale=sqrt(escala))
head(Z)

CP <- prcomp(Z, scale=TRUE)
CP

summary(CP)
plot(CP)
summary(CP)$importance[2,]
varExpli <- summary(CP)$importance[2,]
barplot(varExpli)

names(CP)
biplot(CP, scale=0, col=c("red","blue"))
#no se ve NADAAA claro
#################
#C. pples. en dos grupos
#Grupo 1
#(1 mean vs worst) radio, perímetro, área

meanvworst1=datos[,c(3,5,6,23,25,26)]

G1 <- cov.wt(meanvworst1, method = "ML")$center
G1

V1 <- cov.wt(meanvworst1, method="ML")$cov
V1

escala1 <- diag(V1)
escala1

Z1 <- scale(meanvworst1, center=G1, scale=sqrt(escala1))
head(Z1)

CP1 <- prcomp(Z1, scale=TRUE)
CP1

summary(CP1)
plot(CP1)
summary(CP1)$importance[2,]
varExpli1 <- summary(CP1)$importance[2,]
barplot(varExpli1)

names(CP1)
biplot(CP1, scale=0, col=c("red","blue"))




######
#Grupo 2
#(2 mean vs worst) texture, smoothness,
#compactness, concavity,
#concave_points, symmetry, fractal dimension.

meanvworst2=datos[,-c(1,2,3,5,6,13,14,15,16,17,18,19,20,21,22,23,25,26)]

G2 <- cov.wt(meanvworst2, method = "ML")$center
G2

V2 <- cov.wt(meanvworst2, method="ML")$cov
V2

escala2 <- diag(V2)
escala2

Z2 <- scale(meanvworst2, center=G2, scale=sqrt(escala2))
head(Z2)

CP2 <- prcomp(Z2, scale=TRUE)
CP2

summary(CP2)
plot(CP2)
summary(CP2)$importance[2,]
varExpli2 <- summary(CP2)$importance[2,]
barplot(varExpli2)

names(CP2)
biplot(CP2, scale=0, col=c("red","blue"))



