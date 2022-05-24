install.packages("psych")
install.packages("kableExtra")
install.packages("Hmisc")
install.packages("PerformanceAnalytics")
install.packages("GGally") 
install.packages("factoextra") 
install.packages("sjstats")
install.packages("pROC") #roc
library("tidyverse")
library("caret")
library("psych") #describe
library("kableExtra")
library("ggplot2")
library("PerformanceAnalytics") #chart.correlation
library("GGally") #ggcorr
library("factoextra") #get_pca_var
library("sjstats")
library("pROC")
library("caret") #confusion matrix

midirectorio <- dirname(rstudioapi::getActiveDocumentContext()$path) #obtener mi directorio actual
setwd(midirectorio) #setear directorio de trabajo en R
file <- "data.csv"
cancer.df <- read.csv(file, header = TRUE, sep = ",")
cancer.df$X <- NULL
cancer.df$id <- NULL
print(cancer.df)
head(cancer.df) #vista previa de los datos
#569 pacientes y 31 caracteristicas
cancer.df %>% 
  dim
#Visualización de contenido de cada caracteristica del dataframe
cancer.df %>%
  str()
#Distribución de los diagnosticos B 62.7% y m 37.3%
#Siendo B benign y M Malignant
cancer.df %>% 
  count(diagnosis) %>%
  group_by(diagnosis) %>%
  plyr::summarize(perc_dx = round((n / 569)* 100, 2))
#Estadísticas descriptivas estándar, media, desviación estándar, valor mínimo máximo
summary(cancer.df)

cancer.df$diagnosis<-ifelse(cancer.df$diagnosis == "B", 0, 1)

WDBC_tbl<- describe(cancer.df,IQR=T)[,c(1:5,8:10,11,12)]

kable(WDBC_tbl, caption = "Selected Stats", format = "html") %>% 
  kable_styling(latex_options = "striped", font_size=10)

rm(WDBC_tbl)
library("Hmisc")
hist.data.frame(cancer.df, n.unique=1, mtitl = "Breast Cancer Histogram")
WDBCdata_mean = cbind(diagnosis=cancer.df[,c(1)], cancer.df[,c(2:11)])
WDBCdata_se = cbind(diagnosis=cancer.df[,c(1)], cancer.df[,c(12:21)])
WDBCdata_worst = cbind(diagnosis=cancer.df[,c(1)], cancer.df[,c(22:31)])

#boxplot
par(cex.axis=0.8) # is for x-axis
boxplot(WDBCdata_mean, las=2, col=1:length(WDBCdata_mean), main="Cáncer de mama por valor media", ylim = c(0,550))
boxplot(WDBCdata_se, las=2, col=1:length(WDBCdata_mean), main="Cáncer de mama por valor SE", ylim = c(0,90))
boxplot(WDBCdata_worst, las=2, col=1:length(WDBCdata_mean), main="Cáncer de mama por valor mayor ", ylim = c(0,150))

#mean
chart.Correlation(WDBCdata_mean,histogram=FALSE,pch=19)
# SE
chart.Correlation(WDBCdata_se,histogram=FALSE,pch=19)
# Worst
chart.Correlation(WDBCdata_worst,histogram=FALSE,pch=19)

ggcorr(cancer.df, nbreaks=8, palette='PRGn', label=TRUE, label_size=2, size = 1.8, label_color='black', geom = "circle") + 
  ggtitle("Matriz de correlación Cáncer de mama") + 
  theme(plot.title = element_text(hjust = 0.5, color = "grey15"))

z = cor(cancer.df)

z = round(z,4)

z[abs(z)<0.9]=NA # remove low relationship
z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
z=as.data.frame(as.table(z))  #Turn into a 3-column table
z=na.omit(z)  #Get rid of the junk we flagged above
z=z[order(-abs(z$Freq)),]    #Sort by highest correlation (whether +ve or -ve)
z
rm(z)

barplot(table(cancer.df$diagnosis), col="blue", ylab="count", xlab="Diagnosis", main="Distribución diagnóstico 0=B 1=M")
#estimacion de transformaciones o normalizacion
bc1<-BoxCoxTrans(cancer.df$radius_se)
bc2<-BoxCoxTrans(cancer.df$perimeter_se)
bc3<-BoxCoxTrans(cancer.df$area_se )
bc5<-BoxCoxTrans(cancer.df$fractal_dimension_se)

par(mfrow=c(1,2))
hist(cancer.df$radius_se, main="Histograma radius_se", xlab="", col="red")
hist(cancer.df$radius_se^bc1$lambda, main="Histograma radius_se transf.", xlab="", col="green")
par(mfrow=c(1,2))
hist(cancer.df$perimeter_se, main="Histograma perimeter_se", xlab="", col="red")
hist(cancer.df$perimeter_se^bc2$lambda, main="Histogram perimeter_se transf.", xlab="",col="green")
par(mfrow=c(1,2))
hist(cancer.df$area_se, main="Histograma area_se",xlab="", col="red")
hist(cancer.df$area_se^bc3$lambda, main="Histograma area_se transf.",xlab="",col="green")
par(mfrow=c(1,2))
hist(cancer.df$fractal_dimension_se, main="Histograma dimension_se",xlab="", col="red")
hist(cancer.df$fractal_dimension_se^bc5$lambda, main="Histograma dimension_se transf.",xlab="",col="green")
par(mfrow=c(1,1))

WDBCdatafull<-cancer.df
#eliminar columnas, eliminar predictores altamente correlacionados
cancer.df<-subset(cancer.df, 
                 select=-c(area_mean,radius_mean,area_worst,compactness_mean,perimeter_worst,
                           compactness_se,concavity_worst,fractal_dimension_worst))

set.seed(123)

indx<-createDataPartition(cancer.df$diagnosis, p=0.7, list=FALSE)
train_x<-cancer.df[indx,-1]
train_y<-cancer.df$diagnosis[indx] 

test_x<-cancer.df[-indx,-1]
test_y<-cancer.df$diagnosis[-indx]

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

