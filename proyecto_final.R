
install.packages('[en caso de no tener alguno]')

library(tidyverse) ## manipulaci�n de objetos
library(dplyr) ## manipulaci�n de objetos
library(readr) ## Para importaci�n de archivos
library(ggplot2) ## Para gr�ficos
library(scales) ## colores en R
library(reshape2)
library(corrplot) ## Para sacar correlaciones
library(corrr) ### para hallar correlaciones
library(caret) ## Contiene la funci�n Findcorrelation que sirve para identificar las variables con las correlaciones m�s altas
library(plotly)
library(rpart)
library(rpart.plot)
library(viridis)
library(Metrics)
library(glmnet)
library(formattable)

mypath <- dirname(rstudioapi::getActiveDocumentContext()$path) 
setwd(mypath) 


# Fase 1: Entendimiento del negocio-problema asociado a los datos .--------------------------------------------------------------------------------

    # Ir a:  https://docs.google.com/document/d/1XLMYtpleCPQxuIe4c_8CxwYENd_s_JDdr4z95NNUmlE/edit?usp=sharing


# Fase 2: Entendimiento de los datos.---------------------------------------------------------------------------------------------------------------------

# Data
data <- read_csv("data.csv")

# estructura de los datos
str(data) #  contamos con un cojunto de 32 datos incluyendo el id
#>El conjunto de datos a pesar de que es n�merico en su totalidad 
#>la forma en la que estan almacenadas algunas variables son de tipo caracter

data %>% 
  head(10) %>% 
  formattable() %>% 
  as.datatable

# Informaci�n de atributos:

# 1) N�mero de identificaci�n
# 2) Diagn�stico (M = maligno, B = benigno)
# 3-32) Se calculan diez caracter�sticas de valor real para cada n�cleo celular:
  # a) radio (media de las distancias desde el centro hasta los puntos del per�metro)
  # b) textura (desviaci�n est�ndar de los valores de la escala de grises)
  # c) per�metro
  # d) �rea
  # e) uniformidad (variaci�n local en las longitudes del radio)
  # f) compacidad (per�metro^2 / �rea - 1,0)
  # g) concavidad (severidad de las partes c�ncavas del contorno)
  # h ) puntos c�ncavos (n�mero de porciones c�ncavas del contorno)
  # i) simetr�a
  # j) dimensi�n fractal ("aproximaci�n a la l�nea de costa" - 1)



# Fase 3: Preparaci�n de los datos.------------------------------------------------------------------------------------------------------------------------------------------

# _______________________ Data Cleaning ______________________________________________________________________________________________________

# Datos faltantes -----------------
missing_values = map_int(data, function(x) {
  sum(is.na(x) | x == '')
})
print(missing_values)

# Let's visulize the missing results:
missing_values = data.frame(columns = factor(names(missing_values)), 
                            missing_values = missing_values)
ggplot(missing_values, 
       aes(x = fct_reorder(columns,missing_values), 
           y = missing_values)) + 
  geom_bar(stat = 'identity', fill = 'steelblue') + 
  coord_flip() + xlab("Features") + 
  ylab("Missing Values")

# Eliminamos variables que no se usaran ----------------

# la ultima columna no tiene ningun un valor, asi que la eliminamos
ncol(data)
data <- subset(data[,-33])

missing_values = map_int(data, function(x) {
  sum(is.na(x) | x == '')
})
print(missing_values)

# Convertimos a factor  ------------------
# la variable objetivo - el diagn�stico
# el diagn�stico es la variable dependiente, por lo que debe clasificarse/factorizarse
table(data$diagnosis)

data$diagnosis<- as.factor(data$diagnosis)

# convertimos en n�mericas el resto del dataset -------------

data<- data %>% 
  mutate_if(is.character,as.numeric)

str(data) # verificamos que se haya efectuado el cambio
summary(data[,2:32])# Observamos algunas medidas de tendencia central de los datos


# _______________ An�lisis exploratorio de los datos _____________________________________

# primero observamos las frecuencias de nuestra variable dependiente que es el diagn�stico

table(data$diagnosis) # Observamos las frecuencias del tipo de diagn�stico

# Creamos un gr�fico de dona -------------
# observar en t�rminos porcentuales cu�ntos son los diagn�sticos begninos y los malignos
df<-data.frame(categorias=c("Benigno","Maligno"),
               porcentaje=c(62.7,37.3))
ggplot(df,mapping = aes(x="",y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",color="white")

png("Grafico de dona tipo de cancer.png")

ggplot(df,aes(x=2,y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=round(percent(porcentaje/100),2)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("steelblue","orange"))+
  theme_void()+
  labs(title="DIAGN�STICO DEL TIPO CANCER")+
  xlim(0.5,2.5)
dev.off()

# construimos histogramas  -------------------
# para ver la distribuci�n de cada una de las variables independientes 

# Para la media
data_mean <- data[ ,c("diagnosis", "radius_mean", "texture_mean",
                      "perimeter_mean", "area_mean", "smoothness_mean",
                      "compactness_mean", "concavity_mean", "concave points_mean", 
                      "symmetry_mean", "fractal_dimension_mean" )]

# Para el error estandar
data_se <- data[ ,c("diagnosis", "radius_se", "texture_se","perimeter_se",
                    "area_se", "smoothness_se", "compactness_se", "concavity_se",
                    "concave points_se", "symmetry_se", "fractal_dimension_se" )]

# dato atipico de cada variable
data_worst <- data[ ,c("diagnosis", "radius_worst", "texture_worst",
                       "perimeter_worst", "area_worst", "smoothness_worst",
                       "compactness_worst", "concavity_worst", "concave points_worst", 
                       "symmetry_worst", "fractal_dimension_worst" )]

#Histogramas para la media de cada variable seg�n el tipo de diagn�stico
png("Histogramas media_variables independientes.png")
ggplot(data = melt(data_mean, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales =      'free_x')
dev.off()

png("Histogramas es_variables independientes.png")
ggplot(data = melt(data_se, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales = 'free_x')
dev.off()

png("Histogramas worst_variables independientes.png")
ggplot(data = melt(data_worst, id.var = "diagnosis"), mapping = aes(x = value)) + 
  geom_histogram(bins = 10, aes(fill=diagnosis), alpha=0.5) + facet_wrap(~variable, scales = 'free_x')
dev.off()

# correlaciones de las variables independientes -----------------
# Gr�ifco de corelaciones
correlaciones<- cor(data[,3:31])
png("Correlaciones.png")
corrplot(correlaciones, order = "hclust", tl.cex = 0.7)
dev.off()

#>
library("PerformanceAnalytics") #chart.correlation
library("Hmisc")
hist.data.frame(data, n.unique=1, mtitl = "Breast Cancer Histogram")
WDBCdata_mean = cbind(diagnosis=data[,c(1)], data[,c(2:11)])
WDBCdata_se = cbind(diagnosis=data[,c(1)], data[,c(12:21)])
WDBCdata_worst = cbind(diagnosis=data[,c(1)], data[,c(22:31)])

#boxplot
par(cex.axis=0.8) # is for x-axis
boxplot(WDBCdata_mean, las=2, col=1:length(WDBCdata_mean), main="Cancer de mama por valor media", ylim = c(0,550))
boxplot(WDBCdata_se, las=2, col=1:length(WDBCdata_mean), main="Cancer de mama por valor SE", ylim = c(0,90))
boxplot(WDBCdata_worst, las=2, col=1:length(WDBCdata_mean), main="Cancer de mama por valor mayor ", ylim = c(0,150))

############################################
par(mfrow = c(3, 3))
invisible(lapply(3:ncol(WDBCdata_mean), function(i) 
  boxplot(WDBCdata_mean[, i],las=2,col = colors()[i+10],
          xlab =colnames(WDBCdata_mean[i]),
          ylab = "measures", main=paste(colnames(WDBCdata_mean[i]),"vs meas.")
  )))
mtext("Breast Cancer Mean Values", side = 3, line = -1.5,
      font = 2,      # Estilo
      cex = 1.2, outer = TRUE)

############################################
par(mfrow = c(3, 3))
invisible(lapply(3:ncol(WDBCdata_se), function(i) 
  boxplot(WDBCdata_se[, i],las=2,col = colors()[i+10],
          xlab =colnames(WDBCdata_se[i]),
          ylab = "measures", main=paste(colnames(WDBCdata_se[i]),"vs meas.")
  )))
mtext("Breast Cancer SE Values", side = 3, line = -1.5,
      font = 2,      # Estilo
      cex = 1.2, outer = TRUE)

############################################
par(mfrow = c(3, 3))
invisible(lapply(3:ncol(WDBCdata_worst), function(i) 
  boxplot(WDBCdata_worst[, i],las=2,col = colors()[i+10],
          xlab =colnames(WDBCdata_worst[i]),
          ylab = "measures", main=paste(colnames(WDBCdata_worst[i]),"vs meas.")
  )))
mtext("Breast Cancer Worst Values", side = 3, line = -1.5,
      font = 2,      # Estilo
      cex = 1.2, outer = TRUE)


#mean
chart.Correlation(WDBCdata_mean,histogram=FALSE,pch=19)
# SE
chart.Correlation(WDBCdata_se,histogram=FALSE,pch=19)
# Worst
chart.Correlation(WDBCdata_worst,histogram=FALSE,pch=19)

#>_____________________________________________________________

# En el gr�fico podemos observar que algunas de las variables 
# presentan altos niveles de correlaci�n entre si

# Identifiquemos cuales son
png("tabla_correlaciones_altas.png")
alta_correlaci�n <- findCorrelation(cor(correlaciones), cutoff = 0.9)
corrplot(cor(correlaciones[,alta_correlaci�n]), order = "hclust")
dev.off()

# las siguientes variables presentan un alto grado de correlaci�n entre ellas: 
    #> smootnes_mean, concavity_mean, concave_points_mean, Compactness_mean, concavity_worst
    #> Concave points_worst, perimeter_se, concave points_se y Compactness_se
    
#> Estas variables deben ser eliminadas del dataset -----------------
#> pues unas son combinaciones lineales de las otras 
#> y puede generar problemas en la etapa de modelado

borrar <- c("smoothness_mean","concavity_mean","concave points_mean",
            "compactness_mean","concavity_worst","concave points_worst", 
            "perimeter_se", "concave points_se", "compactness_se")
data <- data[ , !(names(data) %in% borrar)]
ncol(data)
head(data)


# ---------------- an�lisis de componentes principales ----------------------------------------------
#> se usa para reducir dimensionalidad
#> cada componente es una combinacion lineal de otras variables

# para ver cu�les son las componentes que presentan la mayor variabilidad 
# para clasificar el diagb�stico de cancer de mama

componentes_principales <- prcomp(data[, 3:22], center=TRUE, scale=TRUE)
png("Porporci�n acumulada de varianza explicada.png")
plot(componentes_principales, type="l", main='')
grid(nx = 10, ny = 14)
title(main = "Porporci�n acumulada de varianza explicada", sub = NULL, xlab = "Componentes")
box()
dev.off()

summary(componentes_principales)

# Ahora vamos a identificar cu�les son las variables que conforman la 
# componente principal 1, 2, 3 y 4 que explican el 60% de la variabilidad total
library(factoextra)
p1 <- fviz_contrib(componentes_principales, choice="var", axes=1, fill="lightgreen", color="grey", top=10)
p2 <- fviz_contrib(componentes_principales, choice="var", axes=2, fill="skyblue", color="grey", top=10)
p3 <- fviz_contrib(componentes_principales, choice="var", axes=3, fill="mediumpurple1", color="grey", top=10)
p4 <- fviz_contrib(componentes_principales, choice="var", axes=4, fill="moccasin", color="grey", top=10)
library(gridExtra)
grid.arrange(p1,p2,p3,p4,ncol=2)


# ------------- Fase 4: Modelados-----------------------------------------------------------------------------------------------------------------------------------------------------------

# ____________________________ KNN ____________________________________________________________________________________________________________________________________________________
# supervised machine learning algorithm 
# We have to normalize the quantitative variables to express them in the same range of values. 

# Normalization 
dataNorm <- data
dataNorm[,3:23] <- scale(data[,3:23])

# 70% train and 30% test
set.seed(5678)
ind <- sample(2, nrow(dataNorm), replace=TRUE, prob=c(0.7, 0.3))
trainData <- dataNorm[ind==1,]
testData <- dataNorm[ind==2,]

# usage------------------------
# knn(train, test, 
#     cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

library(class)      # Incluye funciones de clasificaci�n inclu�do el de knn

# Execution of k-NN with k=1
KnnTestPrediction_k1 <- knn(trainData[,3:23], testData[,3:23],
                            trainData$diagnosis, k=1, prob=TRUE)

# Execution of k-NN with k=2
KnnTestPrediction_k2 <- knn(trainData[,3:23], testData[,3:23],
                            trainData$diagnosis, k=2, prob=TRUE)

# Execution of k-NN with k=2 - cross validation
KnnTestPrediction_k2 <- knn.cv(trainData[,3:23],
                            trainData$diagnosis, k=2, prob=TRUE)

# Execution of k-NN with k=3
KnnTestPrediction_k3 <- knn(trainData[,3:23], testData[,3:23],
                            trainData$diagnosis, k=3, prob=TRUE)

# Execution of k-NN with k=4
KnnTestPrediction_k4 <- knn(trainData[,3:23], testData[,3:23],
                            trainData$diagnosis, k=4, prob=TRUE)

# ----------------------------- Evaluation ------------------------------------------------------------------------------------------
#  matriz de confusion y accuracy

# Confusion matrix of KnnTestPrediction_k1 ----------
table(testData$diagnosis, KnnTestPrediction_k1)
# accuracy 
sum(KnnTestPrediction_k1==testData$diagnosis)/length(testData$diagnosis)*100


# Confusion matrix of KnnTestPrediction_k2 --------------
table(testData$diagnosis, KnnTestPrediction_k2)
# accuracy
sum(KnnTestPrediction_k2==testData$diagnosis)/length(testData$diagnosis)*100


# Confusion matrix of KnnTestPrediction_k2 -------------- cross validation
table(trainData$diagnosis, KnnTestPrediction_k2)
# accuracy
sum(KnnTestPrediction_k2==trainData$diagnosis)/length(trainData$diagnosis)*100

  
# Confusion matrix of KnnTestPrediction_k3 ---------------
table(testData$diagnosis, KnnTestPrediction_k3)
# accuracy
sum(KnnTestPrediction_k3==testData$diagnosis)/length(testData$diagnosis)*100


# Confusion matrix of KnnTestPrediction_k4--------------
table(testData$diagnosis, KnnTestPrediction_k4)
# accuracy
sum(KnnTestPrediction_k4==testData$diagnosis)/length(testData$diagnosis)*100

#  Luego de realizar los 4 modelos se puede observar que para k = 3
#  tiene mejor accuracy


#  esta parte se puede quitar, es solo para comrpobar los k que dan mayor accuracy
# --------------------------- we can plot Accuracy vs Choice of `k`. -------------------------------------------------------------------

#### Se procede a analizar cu�les son los mejores valores que K deber�a tomar, para ello realiza un ciclo donde K va tomar valores entre 1 y 100
###y va realizar la predicci�n con el modelo de entrenamiento luego se evalua la exactitud de la predicci�n con el modelo de prueba. En este caso
### se puede observar que los valores optimos de K se encuentran en un intervalo entre 4 y 24

# Empty variables
KnnTestPrediction <- list()
accuracy <- numeric()

library(GGally)     # Este paquete est� dise�ado para poder graficar en forma de matriz un set de datos con m�ltiples variables, 
# el resultado es una correlaci�n de las variables elegidas en dicho dataset.

# From k=1 to k=100...
for(k in 1:100){
  
  # KnnTestPrediction for each k
  KnnTestPrediction[[k]] <- knn(trainData[,3:23], testData[,3:23], trainData$diagnosis, k, prob=TRUE)
  
  # Accuracy for each k   
  accuracy[k] <- sum(KnnTestPrediction[[k]]==testData$diagnosis)/length(testData$diagnosis)*100
  
}

# Accuracy vs Choice of k
plot(accuracy, type="b", col="dodgerblue", cex=1, pch=20,
     xlab="k, number of neighbors", ylab="Classification accuracy", 
     main="Accuracy vs Neighbors")

# Add lines indicating k with best accuracy
abline(v=which(accuracy==max(accuracy)), col="darkorange", lwd=1.5)

# Add line for max accuracy seen
abline(h=max(accuracy), col="grey", lty=2)

# Add line for min accuracy seen 
abline(h=min(accuracy), col="grey", lty=2)


# ____________________________ K-means ____________________________________________________________________________________________________________________________________________________

# unsupervised machine learning algorithm

# Execution of k-means with k=3--------------------------------

# usage
# kmeans(x, centers, iter.max = 10, nstart = 1,
#        algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
#                      "MacQueen"), trace=FALSE)

cancer_kmeans_k3 <- kmeans(dataNorm[,3:23], centers=3)


# Cluster to which each point is allocated
cancer_kmeans_k3$cluster # Podemos observar en qu� cluster clasific� a cada diagnostico

# Cluster centers
cancer_kmeans_k3$centers  # coordenadas de los 2 centroides para las 13 dimensiones

# Cluster size
cancer_kmeans_k3$size # me indica el n�mero de registros que quedaron en cada cluster


#Adem�s, la funci�n kmeans() devuelve algunas proporciones que nos permiten saber qu� tan compacto es un cl�ster
# y cu�n diferentes son varios grupos entre s�.

# (ENTRE)betweenss: La suma de cuadrados entre grupos. En una segmentaci�n �ptima, se espera que esta relaci�n sea tan
#  ALTO POSIBLE, ya que nos gustar�a tener grupos heterog�neos.

# (INTRA) withinss: Vector de suma de cuadrados dentro de un grupo, un componente por grupo. En una segmentaci�n �ptima,
# se espera que esta relaci�n sea LO MAS BAJA POSIBLE para cada grupo, ya que nos gustar�a tener homogeneidad
# dentro de los grupos.

# tot.withinss: suma total de cuadrados dentro del grupo.

# totss: La suma total de cuadrados.


# Between-cluster sum of squares
cancer_kmeans_k3$betweenss  # (ENTRE) lo m�s alejados posibles, para que sean muy heterogeneos

# Within-cluster sum of squares
cancer_kmeans_k3$withinss  # (INTRA) lo m�s peque�a posible, que sean muy homogeneos

# Total within-cluster sum of squares 
cancer_kmeans_k3$tot.withinss  ## es la suma de cuadrados dentro del conglomerado. Entonces da como resultado un vector con un n�mero para cada grupo. 
##Se espera que esta relaci�n sea lo m�s baja posible para cada conglomerado, 
##ya que nos gustar�a tener homogeneidad dentro de los conglomerados.

# Total sum of squares
cancer_kmeans_k3$totss


# ----------------------------------------- How many clusters?  ------------------------------------------------

library(gridExtra)  # Librer�a para realizar gr�ficos
library(GGally)     # graficar en forma de matriz un set de datos con m�ltiples variables, 
##el resultado es una correlaci�n de las variables elegidas en dicho dataset.
library(knitr)      # Esta librer�a se usa para documentos escritos en R markdown, combina textos y an�lsis hacia otros formatos
# To study graphically which value of `k` gives us the best partition, we can plot `betweenss` and `tot.withinss` 
# vs Choice of `k`.

bss <- numeric()
wss <- numeric()

# Run the algorithm for different values of k 
set.seed(1234)

for(i in 1:10){
  
  # For each k, calculate betweenss and tot.withinss
  bss[i] <- kmeans(dataNorm[,3:23], centers=i)$betweenss
  wss[i] <- kmeans(dataNorm[,3:23], centers=i)$tot.withinss
  
}
# (ENTRE)
# Between-cluster sum of squares vs Choice of k
p3 <- qplot(1:10, bss, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# (INTRA)
# Total within-cluster sum of squares vs Choice of k
p4 <- qplot(1:10, wss, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_bw()

# Subplot
grid.arrange(p3, p4, ncol=2)

# Gr�ficamente se puede observar que las mayores diatncias se dan en los 3 primeros puntos en ambos gr�ficos, 
# esto nos indica que el n�mero �ptimo de cluster deber�an ser 3

#  PERO debido a que solo tenemos 2 tipos de diagnostico, seleccionaremos k = 2


# Results

# Execution of k-means with k=2  ------------------------------------------------------------------
set.seed(1234)

cancer_kmeans_k2 <- kmeans(dataNorm[,3:23], centers=2)

# Mean values of each cluster
aggregate(data[,3:23], by=list(cancer_kmeans_k2$cluster), mean)

# ------------------- VISUALIZACION DE CLUSTERS --------------------
# Clustering 
# solo se grafican las primeras 6 columnas para facilidad en el entendimiento del grafico
ggpairs(cbind(data[,3:23], Cluster=as.factor(cancer_kmeans_k2$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_bw()


a<-dataNorm %>%
  as_tibble(dataNorm$diagnosis) %>%   
  mutate(diagnosis = case_when(
    diagnosis == 'M' ~ "1",
    diagnosis == 'B' ~ "2",
    TRUE ~ "other"))

a<-as.factor(a$diagnosis)

# MATRIS DE CONFUSION 
table(a,cancer_kmeans_k2$cluster)


# ____________________________ Naive Bayes ____________________________________________________________________________________________________________________________________________

library(e1071)    # Funciones miscel�neas del Departamento de Estad�stica para el an�lisis de clases latentes, transformada de Fourier de tiempo corto, 
# agrupamiento difuso, m�quinas de vectores de soporte, c�lculo de la ruta m�s corta, 
# agrupamiento en bolsas, clasificador de Bayes ingenuo, k-vecino m�s cercano generalizado ...

##Estima el modelo de bayes y obtiene las probabilidades relativas por cada una de las 
#categor�as de an�lisis frente a la variable de supervivencia
cancer_nb_model = naiveBayes(diagnosis ~., data = trainData[,2:23])
cancer_nb_model
summary(cancer_nb_model)

# Make prediction using the classifier:

# Predecir la probabilidad de supervivencia y las etiquetas:
cancer_nb_prob_pred = predict(cancer_nb_model, testData, type = 'raw')
cancer_nb_pred = predict(cancer_nb_model, testData)   # obtiene las clasificaciones que realiza el modelo con el conjunto de prueba

# Uso de la matriz de confusi�n para evaluar el rendimiento del modelo: ------------------------------------
confusionMatrix(cancer_nb_pred, testData$diagnosis)

# metricas:
# Accuracy-exactitud: El modelo clasific� correctamente el 94% de las observaciones tanto en verdaderos positivos, como verdaderos negativos
# Sensitividad: El 94.6% de los casos positivos fueron correctamente clasificados
# Especificidad: El modelo clasific� bien el 93% de los verdaderos negativos del total de negativos


# ligeramente inferior al modelo de regresi�n log�stica. A continuaci�n, 
# intentemos predecir usando el clasificador de �rboles de decisi�n.


# _______________________  Decision Tree Classifier____________________________________________________________________________________________________________________________________________

# Build the decision tree classifier:
set.seed(1234)
cancer_dt_model = rpart(diagnosis ~ ., data = trainData,cp= 0)
rpart.plot(cancer_dt_model, type = 2, fallen.leaves = F, cex = 1, extra = 2)


# Make predictions based on the classifier:

cancer_dt_pred = predict(cancer_dt_model, newdata = testData, type = "class")
confusionMatrix(cancer_dt_pred, testData$diagnosis)


# The decision tree classifier gives an accuracy rate of 94%

# _______________________  Random Forest ____________________________________________________________________________________________________________________________________________
library(randomForest)
#Baseline Random Forest Model
cancerRF<-randomForest(diagnosis~.,dataNorm,ntree=150)
cancerRF #  de aqui se saca el acuraccy ------------------------------------------------ faltaaaaaaaaaaaaaaaaaaaaaa

# La precisi�n general de nuestro modelo es bastante buena, alrededor del 96.3 % en general ya que la tasa de error es del 3.7% 

# ---------------------- calculo de la importancia de cada una de las variables ---------------------------------

### Se observa cu�l o cuales son las variables m�s importantes para la clasificaci�n de los diagnosticos como benignos o malignos

# Get importance

##Obtiene la variable importancia de la clasificaci�n
importance    <- importance(cancerRF)

## La pasa de una lista a un data frame para poder ordenarla en un granking
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

##realiza el gr�fico para ver cu�les son las variables m�s importantes en la clasificaci�n
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_classic()

##Se puede observar que los resultados de la clasificaci�n son congruentes con los de los an�lisis exploratorios



#> _______________________  Regresion logistica _____________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________________
library("sjstats")
library("pROC")
library("psych") #describe
library("kableExtra")
library("PerformanceAnalytics")


data <- read.csv("data.csv", header = TRUE, sep = ",")
data$X <- NULL
data$id <- NULL
data$diagnosis<-ifelse(data$diagnosis == "B", 0, 1)
bc1<-BoxCoxTrans(data$radius_se)
bc2<-BoxCoxTrans(data$perimeter_se)
bc3<-BoxCoxTrans(data$area_se )
bc5<-BoxCoxTrans(data$fractal_dimension_se)
WDBCdatafull<-data
#eliminar columnas, eliminar predictores altamente correlacionados
data<-subset(data, 
             select=-c(area_mean,radius_mean,area_worst,compactness_mean,perimeter_worst,
                       compactness_se,concavity_worst,fractal_dimension_worst))

set.seed(123)

indx<-createDataPartition(data$diagnosis, p=0.7, list=FALSE)
train_x<-data[indx,-1]
train_y<-data$diagnosis[indx] 

test_x<-data[-indx,-1]
test_y<-data$diagnosis[-indx]

"el subconjunto a continuación es con predictores completos, no se puede usar con el
modelo logístico, solo se puede usar con el modelo que acepta predictores altamente correlacionados."


trainfull_x<-WDBCdatafull[indx,-1]
trainfull_y<-WDBCdatafull$diagnosis[indx]

testfull_x<-WDBCdatafull[-indx,-1]
testfull_y<-WDBCdatafull$diagnosis[-indx]

#aplicar la transformación de potencia para el conjunto de datos de prueba y tren

train_x$radius_se<-train_x$radius_se^bc1$lambda
train_x$perimeter_se<-train_x$perimeter_se^bc2$lambda    
train_x$area_se<-train_x$area_se^bc3$lambda
train_x$fractal_dimension_se<-train_x$fractal_dimension_se^bc5$lambda

test_x$radius_se<-test_x$radius_se^bc1$lambda
test_x$perimeter_se<-test_x$perimeter_se^bc2$lambda    
test_x$area_se<-test_x$area_se^bc3$lambda
test_x$fractal_dimension_se<-test_x$fractal_dimension_se^bc5$lambda

#analisis de componentes principales en la matreiz de datos dada y devuelve los resultados como un objeto
pca_wdbc <- prcomp(train_x[,2:ncol(train_x)],center = TRUE, scale=TRUE)
pca_wbdc_test<-prcomp(test_x[,2:ncol(test_x)],center = TRUE, scale=TRUE)

plot(pca_wdbc, type='l', main="PCA - Principal Components Analysis Chart", col="red")

#extrae los resultados solo para variables
pca_wdbc_var <- get_pca_var(pca_wdbc)
#conjuntos aleatorios a elegir, son las caracteristicas
res <- kmeans(pca_wdbc_var$coord,centers = 5, nstart=25)
grp <- as.factor(res$cluster)


fviz_pca_var(pca_wdbc, col.var=grp, palette='jco', legend.title='Cluster') 

#Modelo de regresion logistica que tenga todos los predictores con variables correlacionadas
model_11_logit_full <-glm(trainfull_y ~ . ,family=binomial, trainfull_x)
summary(model_11_logit_full)
#Modelo de regresion logistica sin variables correlacionadas
model_12_logit_corr <-glm(train_y~.,family=binomial,data=train_x)
summary(model_12_logit_corr)
#se seleccionaron unos cuantos predictores del modelo 12
model_13_logit_corr_final <- update(model_12_logit_corr, .~.-symmetry_se-concave.points_worst-texture_se-perimeter_se-radius_se-symmetry_mean-fractal_dimension_mean-concave.points_mean-texture_mean-compactness_worst-symmetry_worst-smoothness_mean-area_se-smoothness_se )
summary(model_13_logit_corr_final)

#Regresion logistica sobre variables transformadas por PCA, se pierde la ventaja de la interpretabilidad
model_14_pca <-glm( train_y~.-PC11 -PC6 -PC8 -PC10 -PC5 -PC9 -PC13 -PC14 -PC12 -PC7,family=binomial,data=pca_wdbc$x[,c(1:14)] %>% data.frame())
summary(model_14_pca)


#Convert to 0/1
conv_13_logit_corr <- ifelse(predict(model_13_logit_corr_final) > 0.5,1,0)
#conf_13_logit_corr <- confusionMatrix(conv_13_logit_corr, train_y, positive="1")
conf_13_logit_corr <- confusionMatrix(table(conv_13_logit_corr, train_y), positive = "1")

#Convert to 0/1
conv_12_logit_corr <- ifelse(predict(model_12_logit_corr) > 0.5,1,0)
#conf_13_logit_corr <- confusionMatrix(conv_13_logit_corr, train_y, positive="1")
conf_12_logit_corr <- confusionMatrix(table(conv_12_logit_corr, train_y), positive = "1")

conv_14_pca <- ifelse(predict(model_14_pca) > 0.5,1,0)
#conf_14_pca <- confusionMatrix(conv_14_pca, train_y, positive="1")
conf_14_pca <- confusionMatrix(table(conv_14_pca, train_y), positive="1")


acc13<-conf_13_logit_corr$overall["Accuracy"]
acc14<-conf_14_pca$overall["Accuracy"]

auc13<-roc(train_y ~ conv_13_logit_corr, train_x)$auc
auc14<-roc(train_y ~ conv_14_pca, train_x)$auc

df<-data.frame(accuracy=c(acc13,acc14),auc=c(auc13,auc14))
row.names(df)<-c("Logistic","Logistic PCA")

kable(round(df,2), caption = "Performance metrics train")    

#Convert to 0/1
conv_13_logit_t <- ifelse(predict(object=model_13_logit_corr_final, newdata=test_x, type="response") > 0.5,1,0)
#conf_13_logit_t <- confusionMatrix(conv_13_logit_t, test_y, positive="1")
conf_13_logit_t <- confusionMatrix(table(conv_13_logit_t, test_y), positive="1")

conv_14_pca_t <- ifelse(predict(model_14_pca, newdata=as.data.frame(pca_wbdc_test$x[,c(1:14)]), type="response") > 0.5,1,0)
#conf_14_pca_t <- confusionMatrix(conv_14_pca_t, test_y, positive="1")
conf_14_pca_t <- confusionMatrix(table(conv_14_pca_t, test_y), positive="1")


# compute accuracy

acc13_t<-conf_13_logit_t$overall["Accuracy"]
acc14_t<-conf_14_pca_t$overall["Accuracy"]

# compute AUC

auc13_t<-roc(test_y ~ conv_13_logit_t, test_x)$auc
auc14_t<-roc(test_y ~ conv_14_pca_t, test_x)$auc

df<-data.frame(accuracy=c(acc13_t,acc14_t),auc=c(auc13_t,auc14_t))
row.names(df)<-c("Logistic","Logistic PCA")

kable(round(df,3), caption = "Performance metrics test") 

#> Luego de realizar la regresion logistica mediante el uso de todas sus variables
#> y luego realizarlo usando la reduccion de dimensionalidad por pca, se puede
#> observar que hay una mayor presicion cuando se realiza el modelo con todas sus variables 


#> La Fase 5: Evaluaci�n de mejores modelos (m�tricas) se realiza 
#> a medida que se entrena cada uno de los modelos----------------------------------------------------------------------------------------------------------------------------


# Fase 6: Despliegue (Valor obtenido para el negocio)--------------------------------------------------------------------------------------------------------------------------

#> El an�lisis exploratorio generalmente resulta fundamental para poder hacerse
#>  una idea de cu�les son aquellas variables que podr�an tener un mayor poder 
#>  predictivo para clasificar el diagn�stico como bueno o malo, sin embargo, 
#>  en este trabajo se pudo observar que de las 30 variables son pocas las que 
#>  muestran una distribuci�n diferente cuando se analiza por tipo de diagn�stico,
#>  lo que dificulta entender de forma m�s precisa cu�les son esos atributos
#>  que son determinantes para clasificar un diagn�stico como bueno o malo. 
#>    
#>  A pesar de ello, se puede observar a lo largo de todo el an�lisis que aquellas
#>  relacionadas con el radio, el �rea y el per�metro son las que m�s influyen para hacer la clasificaci�n. 



#> La m�trica que se emple� como factor de decisi�n para definir el mejor modelo
#>  fue la exactitud ya que en t�rminos generales nos interesa que los algoritmos
#>  hagan bien las clasificaciones tanto de los verdaderos positivos como
#>  de los verdaderos negativos. Por un lado, porque resultar�a muy grave
#>  decirle a un paciente que tiene c�ncer cuando en realidad no lo tiene,
#>  esto podr�a tener repercusiones graves en la salud mental y emocional
#>  del paciente y su familia. Por otra parte, tambi�n ser�a muy grave
#>  decirle a un paciente que no tiene c�ncer cuando en realidad si lo
#>  tiene y no brindarle un tratamiento oportuno y adecuado que podr�a
#>  concluir en un avance de la patolog�a y ser mortal. 



#> En l�nea con lo anterior, aplicados y evaluados todos los algoritmos de
#>  aprendizaje autom�tico, el mejor algoritmo es el de regresi�n log�stica
#>  ya que presenta el mayor porcentaje de exactitud, siendo este de 98,8%
#>  esto quiere decir que clasifica bien tanto los diagn�sticos buenos como los malos. 




