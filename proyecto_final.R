
install.packages('[en caso de no tener alguno]')

library(tidyverse) ## manipulación de objetos
library(dplyr) ## manipulación de objetos
library(readr) ## Para importación de archivos
library(ggplot2) ## Para gráficos
library(scales) ## colores en R
library(reshape2)
library(corrplot) ## Para sacar correlaciones
library(corrr) ### para hallar correlaciones
library(caret) ## Contiene la función Findcorrelation que sirve para identificar las variables con las correlaciones más altas
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



# Fase 2: Entendimiento de los datos.---------------------------------------------------------------------------------------------------------------------

# Data
data <- read_csv("data.csv")

# estructura de los datos
str(data) #  contamos con un cojunto de 32 datos incluyendo el id
#>El conjunto de datos a pesar de que es númerico en su totalidad 
#>la forma en la que estan almacenadas algunas variables son de tipo caracter

data %>% 
  head(10) %>% 
  formattable() %>% 
  as.datatable

# Información de atributos:

# 1) Número de identificación
# 2) Diagnóstico (M = maligno, B = benigno)
# 3-32) Se calculan diez características de valor real para cada núcleo celular:
  # a) radio (media de las distancias desde el centro hasta los puntos del perímetro)
  # b) textura (desviación estándar de los valores de la escala de grises)
  # c) perímetro
  # d) área
  # e) uniformidad (variación local en las longitudes del radio)
  # f) compacidad (perímetro^2 / área - 1,0)
  # g) concavidad (severidad de las partes cóncavas del contorno)
  # h ) puntos cóncavos (número de porciones cóncavas del contorno)
  # i) simetría
  # j) dimensión fractal ("aproximación a la línea de costa" - 1)



# Fase 3: Preparación de los datos.------------------------------------------------------------------------------------------------------------------------------------------

# el diagnóstico es la variable dependiente, por lo que debe clasificarse/factorizarse
data$diagnosis <- as.factor(data$diagnosis)
str(data)


# _______________________ Data Cleaning ________________________________

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
# la variable objetivo - el diagnóstico
table(data$diagnosis)

data$diagnosis<- as.factor(data$diagnosis)

# convertimos en númericas el resto del dataset -------------

data<- data %>% 
  mutate_if(is.character,as.numeric)

str(data) # verificamos que se haya efectuado el cambio
summary(data) # Observamos algunas medidas de tendencia central de los datos


# _______________ Análisis exploratorio de los datos _______________________________

# primero observamos las frecuencias de nuestra variable dependiente que es el diagnóstico

table(data$diagnosis) # Observamos las frecuencias del tipo de diagnóstico

# Creamos un gráfico de dona -------------
# observar en términos porcentuales cuántos son los diagnósticos begninos y los malignos
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

# construimos histogramas  -------------------
# para ver la distribución de cada una de las variables independientes 

data_mean <- data[ ,c("diagnosis", "radius_mean", "texture_mean",
                      "perimeter_mean", "area_mean", "smoothness_mean",
                      "compactness_mean", "concavity_mean", "concave points_mean", 
                      "symmetry_mean", "fractal_dimension_mean" )]

data_se <- data[ ,c("diagnosis", "radius_se", "texture_se","perimeter_se",
                    "area_se", "smoothness_se", "compactness_se", "concavity_se",
                    "concave points_se", "symmetry_se", "fractal_dimension_se" )]

data_worst <- data[ ,c("diagnosis", "radius_worst", "texture_worst",
                       "perimeter_worst", "area_worst", "smoothness_worst",
                       "compactness_worst", "concavity_worst", "concave points_worst", 
                       "symmetry_worst", "fractal_dimension_worst" )]

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

# correlaciones de las variables independientes -----------
# Gráifco de corelaciones
correlaciones<- cor(data[,3:31])
png("Correlaciones.png")
corrplot(correlaciones, order = "hclust", tl.cex = 0.7)
dev.off()

# En el gráfico podemos observar que algunas de las variables 
# presentan altos niveles de correlación entre si

# Identifiquemos cuales son
png("tabla_correlaciones_altas.png")
alta_correlación <- findCorrelation(cor(correlaciones), cutoff = 0.9)
corrplot(cor(correlaciones[,alta_correlación]),method="number", order = "hclust")
dev.off()

# las siguientes variables presentan un alto grado de correlación entre ellas: 
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


# análisis de componentes principales --------------------------

# para ver cuáles son las componentes que presentan la mayor variabilidad 
# para clasificar el diagbóstico de cancer de mama




# Fase 4: Modelados-----------------------------------------------------------------------------------------------------------------------------------------------------------


# Fase 5: Evaluación de mejores modelos (métricas)----------------------------------------------------------------------------------------------------------------------------

# Fase 6: Despliegue (Valor obtenido para el negocio)--------------------------------------------------------------------------------------------------------------------------











