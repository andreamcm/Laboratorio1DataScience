# Universidad del Valle de Guatemala
# Ivette Cardona - 16020
# Andrea María Cordón - 16076

# Set del directorio donde estan los datos
# Andrea
setwd("~/2019/UVG/Segundo Semestre/DataScience/Laboratorios/Laboratorio1/Laboratorio1DataScience")
# Ivette
setwd("C:/Users/DELL/Documents/UVG/VIII_Semestre/Data Science/Laboratorio1DataScience")
# Librerias a utilizar
#install.packages("cluster")
library(cluster) #Para calcular la silueta
#install.packages("e1071")
library(e1071)#para cmeans
#install.packages("mclust")
library(mclust) #mixtures of gaussians
#install.packages("fpc")
library(fpc) #para hacer el plotcluster
#install.packages("NbClust")
library(NbClust) #Para determinar el número de clusters óptimo
#install.packages("factoextra")
library(factoextra) #Para hacer gráficos bonitos de clustering
#install.packages("ape")
library(ape)
#Rules
library(arules)
library(arulesViz)

# Se lee el archivo que contiene los datos
data <- read.csv("train.csv")

# Resumen de los datos
summary(data)
data.frame(data)
str(data)

# Separar variables cualitativas y cuantitativas
# Cuantitativas
cuantitativas <- data[, c("MSSubClass", "LotFrontage", "LotArea", "OverallQual", "OverallCond", 
                          "YearBuilt", "YearRemodAdd", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", 
                          "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", 
                          "GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", 
                          "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", "GarageYrBlt", 
                          "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", 
                          "X3SsnPorch", "ScreenPorch", "PoolArea", "MiscVal", "MoSold", "YrSold", "SalePrice")]
# Cualitativas
cualitativas <- data[, c("MSZoning", "Street", "Alley", "LotShape", "LandContour", "Utilities", 
                         "LotConfig", "LandSlope", "Neighborhood", "Condition1", "Condition2", 
                         "BldgType", "HouseStyle", "RoofStyle", "RoofMatl", "Exterior1st", "Exterior2nd", 
                         "MasVnrType", "ExterQual", "ExterCond", "Foundation", "BsmtQual", "BsmtCond", "BsmtExposure", 
                         "BsmtFinType1", "BsmtFinType2", "Heating", "HeatingQC", "CentralAir", "Electrical", 
                         "KitchenQual", "Functional", "FireplaceQu", "GarageType", "GarageFinish", "GarageQual", 
                         "GarageCond", "PavedDrive", "PoolQC", "Fence", "MiscFeature", "SaleType", "SaleCondition")]


# Analisis de correlacion
corrcuant <- cor(cuantitativas, use="complete.obs", method="pearson")
corrcuant


# Analisis de variables cualitativas
# ----------------------------------

# Tablas de frecuencia de las variables mas importantes
# LotConfig
dir_freq = as.data.frame(table(cualitativas$LotConfig))
colnames(dir_freq)<- c("LotConfig","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# Condition1
dir_freq = as.data.frame(table(cualitativas$Condition1))
colnames(dir_freq)<- c("Condition1","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# BldgType
dir_freq = as.data.frame(table(cualitativas$BldgType))
colnames(dir_freq)<- c("BldgType","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# HouseStyle
dir_freq = as.data.frame(table(cualitativas$HouseStyle))
colnames(dir_freq)<- c("HouseStyle","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# BsmtCond
dir_freq = as.data.frame(table(cualitativas$BsmtCond))
colnames(dir_freq)<- c("BsmtCond","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# Heating
dir_freq = as.data.frame(table(cualitativas$Heating))
colnames(dir_freq)<- c("Heating","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# Electrical
dir_freq = as.data.frame(table(cualitativas$Electrical))
colnames(dir_freq)<- c("Electrical","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# KitchenQual
dir_freq = as.data.frame(table(cualitativas$KitchenQual))
colnames(dir_freq)<- c("KitchenQual","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# FireplaceQu
dir_freq = as.data.frame(table(cualitativas$FireplaceQu))
colnames(dir_freq)<- c("FireplaceQu","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# GarageType
dir_freq = as.data.frame(table(cualitativas$GarageType))
colnames(dir_freq)<- c("GarageType","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# PoolQC
dir_freq = as.data.frame(table(cualitativas$PoolQC))
colnames(dir_freq)<- c("PoolQC","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# SaleType
dir_freq = as.data.frame(table(cualitativas$SaleType))
colnames(dir_freq)<- c("SaleType","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

# SaleCondition
dir_freq = as.data.frame(table(cualitativas$SaleCondition))
colnames(dir_freq)<- c("SaleCondition","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 


# Clusters
# ---------

# Cambio de NA a 0
cuantiPrueba <- cuantitativas
cuantiPrueba[is.na(cuantiPrueba)] <- 0

# Grafico de codo
wss <- (nrow(cuantiPrueba))

for (i in 2:37) 
  wss[i] <- sum(kmeans(cuantiPrueba, centers=i)$withinss)

# Dice que deberian ser 5-6 clusters
plot(1:37, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

# Kmeans

km <- kmeans(cuantiPrueba, 6)
cuantiPrueba$grupokm <- km$cluster

kmg1 <- cuantiPrueba[cuantiPrueba$grupokm==1,]
prop.table(table(kmg1$SalePrice))*100
nrow(kmg1)
summary(kmg1)

kmg2 <- cuantiPrueba[cuantiPrueba$grupokm==2,]
prop.table(table(kmg2$SalePrice))*100
nrow(kmg2)
summary(kmg2)

kmg3 <- cuantiPrueba[cuantiPrueba$grupokm==3,]
prop.table(table(kmg3$SalePrice))*100
nrow(kmg3)
summary(kmg3)

kmg4 <- cuantiPrueba[cuantiPrueba$grupokm==4,]
prop.table(table(kmg4$SalePrice))*100
nrow(kmg4)
summary(kmg4)

kmg5 <- cuantiPrueba[cuantiPrueba$grupokm==5,]
prop.table(table(kmg5$SalePrice))*100
nrow(kmg5)
summary(kmg5)

kmg6 <- cuantiPrueba[cuantiPrueba$grupokm==6,]
prop.table(table(kmg6$SalePrice))*100
nrow(kmg6)
summary(kmg6)

plotcluster(cuantiPrueba, km$cluster)

#RULES
lasreglas <- select(datos, OverallQual , OverallCond, SalePrice)
reglas<-apriori(lasreglas, parameter = list(support = 0.2,
                                            confidence = 0.70,
                                            target = "rules"))
reglas



