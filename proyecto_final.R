install.packages('tidyverse')
install.packages('plotly')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('viridis')
install.packages('Metrics')
install.packages('glmnet')
install.packages('formattable')
library(tidyverse)
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

str(data)

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

# Data Cleaning----------------------------------------

# Datos faltantes
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

# la ultima columna no tiene ningun un valor, asi que la eliminamos
ncol(data)
data <- subset(data[,-33])

missing_values = map_int(data, function(x) {
  sum(is.na(x) | x == '')
})
print(missing_values)

# Fase 4: Modelados-----------------------------------------------------------------------------------------------------------------------------------------------------------



# Fase 5: Evaluación de mejores modelos (métricas)----------------------------------------------------------------------------------------------------------------------------

# Fase 6: Despliegue (Valor obtenido para el negocio)--------------------------------------------------------------------------------------------------------------------------











