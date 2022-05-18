#### Universidad Autonóma de occidente######
#### Especialización en analítica de big data
#### Curso: Aprendizaje Automático
#### Proyecto final#########
#### Estudiantes:
#### Juan Camilo Urbano, Joan Camilo Tamayo, Andrés Salgado, Juan Sebastían Sanchez

#### Objetivo:
#### Aplicar todos los modelos vistos en el curso de aprendizaje automático a una base de datos 
#### En este caso el análisis se realizará sobre la base de cancer de mama de de la Universidad de Wisconsin en Madison, Wisconsin, EE. UU.
#### Con los datos disponibles se busca identificar cuáles son los atributos que permiten clasificar mejor el cancer de mama como benigno o maligno
#### También se busca identificar cuáles son los mejores modelos que permiten clasificar/predecir el cancer de mama

####Librerias##

library(tidyverse) ## manipulación de objetos
library(dplyr) ## manipulación de objetos
library(readr) ## Para importación de archivos
library(ggplot2) ## Para gráficos
library(scales) ## colores en R
library(reshape2)
library(corrplot) ## Para sacar correlaciones
library(corrr) ### para hallar correlaciones
library(caret) ## Contiene la función Findcorrelation que sirve para identificar las variables con las correlaciones más altas
###Carga de los datos

##identificamos el directorio de trabajo

##Escritorio de trabajo
newpath <- "C:/Users/jurbano/Desktop/Especializacion Analitica/Semestre 1/Aprendizaje automatico/Proyecto final"
setwd("C:/Users/jurbano/Desktop/Especializacion Analitica/Semestre 1/Aprendizaje automatico/Proyecto final") ##definimos el escritorio de trabajo

### cargamos los datos
data <- read_csv2("data.csv")

########___________PREPARACIÓN DE LOS DATOS______________________###############

##Echamos un vistaso de la estructura de los datos
(str(data)) ### Se puede ver que inicialmente contamos con un cojunto de 32 datos incluyendo el id
          ### El conjunto de datos a pesar de que es númerico en su totalidad la forma en la que estan almacenadas algunas variables son de tipo caracter

#### Ahora revisemos algunos datos, los primeros 6 registros

head(data)

### Miramos si existen registros perdidos (NA) dentro del data set
sapply(data, function(x) sum(is.na(x)))


## Comenzaremos eliminando las variables que no necesitamos en el análisis

data<- data[,-1] ## Se eliminó la primera columna correspondiente al id

##Convertimos a factor la variable objetivo - el diagnóstico
table(data$diagnosis)

data$diagnosis<- as.factor(data$diagnosis)

## convertimos en númericas el resto del dataset

data<- data %>% mutate_if(is.character,as.numeric)
str(data) ##verificamos que se haya efectuado el cambio
summary(data) ## Observamos algunas medidas de tendencia central de los datos una vez se han convertido a numéricos


##Análisis exploratorio de los datos

### primero observamos las frecuencias de nuestra variable dependiente que es el diagnóstico


table(data$diagnosis) ## Observamos las frecuencias del tipo de diagnóstico

## Creamos un gráfico de dona para observar en términos porcentuales cuántos son los diagnósticos begninos y los malignos
df<-data.frame(categorias=c("Benigno","Maligno"),
               porcentaje=c(62.7,37.3))
ggplot(df,mapping = aes(x="",y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",color="white")

png("Grafico de dona tipo de cancer.png")
ggplot(df,aes(x=2,y=porcentaje, fill=categorias))+
  geom_bar(stat = "identity",
           color="white")+
  geom_text(aes(label=percent(porcentaje/100)),
            position=position_stack(vjust=0.5),color="white",size=6)+
  coord_polar(theta = "y")+
  scale_fill_manual(values=c("steelblue","orange"))+
  theme_void()+
  labs(title="DIAGNÓSTICO DEL TIPO CANCER")+
  xlim(0.5,2.5)
dev.off()

##### Ahora construimos histogramas para ver la distribución de cada una de las variables independientes 

data_mean <- data[ ,c("diagnosis", "radius_mean", "texture_mean","perimeter_mean", "area_mean", "smoothness_mean", "compactness_mean", "concavity_mean", "concave points_mean", "symmetry_mean", "fractal_dimension_mean" )]

data_se <- data[ ,c("diagnosis", "radius_se", "texture_se","perimeter_se", "area_se", "smoothness_se", "compactness_se", "concavity_se", "concave points_se", "symmetry_se", "fractal_dimension_se" )]

data_worst <- data[ ,c("diagnosis", "radius_worst", "texture_worst","perimeter_worst", "area_worst", "smoothness_worst", "compactness_worst", "concavity_worst", "concave points_worst", "symmetry_worst", "fractal_dimension_worst" )]

#Histogramas para la media de cada variable según el tipo de diagnóstico
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

## Podemos Observar que la mayoría de las variables presentan una distribución semejante a la normal


### Ahora miramos las correlaciones de las variables independientes
correlaciones<- cor(data[,2:31])
## Gráifco de corelaciones
png("Correlaciones.png")
corrplot(correlaciones, order = "hclust", tl.cex = 0.7)
dev.off()

### En el gráfico podemos observar que algunas de las variables presentan altos niveles de correlación entre si, 
### Identifiquemos cuales son
png("tabla_correlaciones_altas.png")
alta_correlación <- findCorrelation(cor(correlaciones), cutoff = 0.9)
corrplot(cor(correlaciones[,alta_correlación]),method="number", order = "hclust")
dev.off()

#### identificamos que las siguientes variables presentan un alto grado de correlación entre ellas: 
## smootnes_mean, concavity_mean, concave_points_mean, Compactness_mean, concavity_worst
## Concave points_worst, perimeter_se, concave points_se y Compactness_se
### Estas variables deben ser eliminadas del dataset pues unas son combinaciones lineales de las otras y puede generar problemas en la etapa de modelado


borrar <- c("smoothness_mean","concavity_mean","concave points_mean","compactness_mean","concavity_worst","concave points_worst", "perimeter_se", "concave points_se", "compactness_se")
data_1 <- data[ , !(names(data) %in% borrar)]
ncol(data_1)
head(data_1)

## para finalizar el esta parte de análisis exploratorio vamos a correr un análisis de componentes principales
## para ver cuáles son las componentes que presentan la mayor variabilidad para clasificar el diagbóstico de cancer de mama

#####________ Análisis de componentes principales_________###



