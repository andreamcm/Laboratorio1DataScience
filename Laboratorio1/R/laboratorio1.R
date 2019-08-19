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
library(corrplot)
library(Hmisc)
library(ggplot2)
library(ggpubr)
#PCA
install.packages("rela")
install.packages("psych")
install.packages("FactoMineR")
install.packages("corrplot")
library(rela)
library(psych)
library(FactoMineR)
#THEME
theme_set(theme_pubr())

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
corrplot(corrcuant, method = "square")

res2<-rcorr(as.matrix(cuantitativas))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res2$r, res2$P)


# Analisis de variables cualitativas
# ----------------------------------

# Tablas de frecuencia de las variables mas importantes
# LotConfig
dir_freq = as.data.frame(table(cualitativas$LotConfig))
colnames(dir_freq)<- c("LotConfig","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir

df <- cualitativas %>%
  group_by(LotConfig) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = LotConfig, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Condition1
dir_freq = as.data.frame(table(cualitativas$Condition1))
colnames(dir_freq)<- c("Condition1","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(Condition1) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = Condition1, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# BldgType
dir_freq = as.data.frame(table(cualitativas$BldgType))
colnames(dir_freq)<- c("BldgType","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(BldgType) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = BldgType, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# HouseStyle
dir_freq = as.data.frame(table(cualitativas$HouseStyle))
colnames(dir_freq)<- c("HouseStyle","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(HouseStyle) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = HouseStyle, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# BsmtCond
dir_freq = as.data.frame(table(cualitativas$BsmtCond))
colnames(dir_freq)<- c("BsmtCond","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(BsmtCond) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = BsmtCond, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Heating
dir_freq = as.data.frame(table(cualitativas$Heating))
colnames(dir_freq)<- c("Heating","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(Heating) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = Heating, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# Electrical
dir_freq = as.data.frame(table(cualitativas$Electrical))
colnames(dir_freq)<- c("Electrical","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(Electrical) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = Electrical, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

# KitchenQual
dir_freq = as.data.frame(table(cualitativas$KitchenQual))
colnames(dir_freq)<- c("KitchenQual","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(KitchenQual) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = KitchenQual, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# FireplaceQu
dir_freq = as.data.frame(table(cualitativas$FireplaceQu))
colnames(dir_freq)<- c("FireplaceQu","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(FireplaceQu) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = FireplaceQu, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# GarageType
dir_freq = as.data.frame(table(cualitativas$GarageType))
colnames(dir_freq)<- c("GarageType","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(GarageType) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = GarageType, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# PoolQC
dir_freq = as.data.frame(table(cualitativas$PoolQC))
colnames(dir_freq)<- c("PoolQC","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(PoolQC) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = PoolQC, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# SaleType
dir_freq = as.data.frame(table(cualitativas$SaleType))
colnames(dir_freq)<- c("SaleType","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(SaleType) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = SaleType, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# SaleCondition
dir_freq = as.data.frame(table(cualitativas$SaleCondition))
colnames(dir_freq)<- c("SaleCondition","Frecuencia")
dir = dir_freq[order(dir_freq[,2], decreasing = TRUE), ]
dir 

df <- cualitativas %>%
  group_by(SaleCondition) %>%
  summarise(counts = n())
df

ggplot(df, aes(x = SaleCondition, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()


# PCA
# ---
str(cuantiPrueba)
cuantiPaf <- cuantiPrueba[, c("MSSubClass", "LotArea", "OverallCond", 
                              "YearBuilt", "YearRemodAdd", "BsmtFinSF1", 
                              "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF",
                              "GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", 
                              "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", 
                              "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", 
                              "ScreenPorch", "PoolArea", "MiscVal", "YrSold", "SalePrice")]

pafData <- paf(as.matrix(cuantiPaf))

pafData$KMO # 0.7468, lo que dice que la adecuacion a la muestra es decente
pafData$Bartlett # 21505, es bastante alto, lo cual es bueno

# Verificar significancia
cortest.bartlett(cuantiPaf[, -1])

# Matriz de correlacion
cor(cuantiPaf[, -1], use = "pairwise.complete.obs")

# Se normalizan los datos
cuantinorm <- prcomp(cuantiPaf, scale. = T)
cuantinorm

summary(cuantinorm)

cuantinormPCA <- PCA(cuantiPaf[, -1], ncp = ncol(cuantiPaf[, -1]), scale.unit = T)

summary(cuantinormPCA)

fviz_eig(cuantinorm, addlabels = TRUE, ylim = c(0, 80))
fviz_pca_var(cuantinorm, col.var = "cos2",
             gradient_cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


#Se obtiene el scree plot de las componentes principales.
# Como se ve hacen falta 4 de las 7 componentes para explicar más del 80% de la variabilidad
fviz_eig(compPrinc, addlabels = TRUE, ylim = c(0, 80))

# En la siguiente grÃ¡fica se ilustra la calidad de la representaciÃ³n de los componentes en las dos primeras dimensiones.
fviz_pca_var(compPrinc, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


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

plotcluster(cuantiPrueba[, c("MSSubClass", "LotArea", "OverallCond", 
                             "YearBuilt", "YearRemodAdd", "BsmtFinSF1", 
                             "BsmtUnfSF", "TotalBsmtSF", "X1stFlrSF",
                             "GrLivArea", "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", 
                             "BedroomAbvGr", "KitchenAbvGr", "TotRmsAbvGrd", "Fireplaces", 
                             "GarageCars", "GarageArea", "WoodDeckSF", "OpenPorchSF", "EnclosedPorch", 
                             "ScreenPorch", "PoolArea", "MiscVal", "YrSold", "SalePrice")], km$cluster)

#PCA
pafDatos<-paf(as.matrix(cuantitativas[,2:8]))
pafDatos$KMO #0.62 Es la adecuación a la muestra
pafDatos$Bartlett #2212 Mientras mas alto sea mejor
summary(pafDatos)

#nivel de significancia 
cortest.bartlett(cuantitativas[,-1])
cor(cuantitativas[,-1],use = "pairwise.complete.obs")


#RULES
lasreglas <- select(datos, OverallQual , OverallCond, SalePrice)
reglas<-apriori(lasreglas, parameter = list(support = 0.2,
                                            confidence = 0.70,
                                            target = "rules"))
reglas



