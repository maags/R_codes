#!//usr/local/bin/Rscript 
#31 de mayo, 2021
#Introducción a la estadística
#Marco G

#Cargamos las librerías necesarias
library(ggplot2)
library(tidyverse)
#Variables y observaciones
#La estadística es la ciencia de los datos. Cualquier conjunto de datos
#que contenga información sobre un grupo de individuos, puede ser organizada
#en variables

#     * Los individuos / Las Observaciones son los objetos descritos por el
#       conjunto de datos.
#     * Las variables son las características de los individos / las obs.
#       y puede tomar distintos valores para distintos individuos / obs.

#Observemos un set de datos pregrabado en R
data(iris)
flores <- iris
View(iris)
rm(iris)
#Para obtener los datos de este set podemos utilizar la función str()
#La cual nos da un breve resumen de nuestro set de datos
str(flores)

#Nuestra tabla está acomodado en un formato llamado 'long format'
#Este formato es escencial para en análisis de datos en R. 
#Las variables son las COLUMNAS, y cada fila las OBSERVACINES.
#Observamos que contiene 5 variables diferentes
#Y 150 observaciones / individuos medidos

#Tipos de variables
#A grandes rasgos existen dos tipos de variables diferentes
#Las variables numéricas y las categóricas o factores
#Las variables CATEGÓRICAS indican a qué categoría pertenecen los ind.
#las viarables numéricas o CUANTITATIVAS  toman valores numéricos 
#con los que se pueden hacer distintas operaciones matemáticas

#Análisis exploratorios de datos
#Las herramientas y las ideas estadísticas nos ayudan a examinar datos para 
#describir sus características principales. Este examen se llama análisis 
#exploratorio de datos
#Existen dos estrategias básicas que nos ayudan a organizar nuestra 
#exploración de un conjunto de datos
#        * Empieza examinando cada variable de forma separada. Luego, 
#          pasa al estudio de las relaciones entre variables
#        * Empieza con los gráficos. Luego, añade resúmenes numéricos 
#          de aspectos concretos de los datos.


#Variables categóricas
#diagrama de barras y diagrama de sectores
#Los valores de una variable categórica son etiquetas asignadas a las 
#categorías de un mismo conjunto. La distribución de una variable 
#categórica lista las categorías y da el recuento o el porcentaje de 
#individuos de cada categoría
summary(flores)

#Gráfico de barras
#El diagrama de barras de la figura 1.1(a) compara de forma rápida la 
#frecuencia de las distintas categorías para la variable Species
ggplot(data = flores,mapping = aes(x = Species))+
  geom_bar(mapping = aes(fill=Species),stat = "count")+
  coord_flip()+
  theme_minimal()+
  labs(title = "Gráfico de barras")

#Diagrama de sectores
#El diagrama de sectores nos ayuda a visualizar la importancia relativa de 
#cada categoría respecto al total
ggplot(data = flores,mapping = aes(x = Species,fill=Species))+
  geom_bar(stat = "count",width=1)+
  coord_polar()+
  theme_void()+
  labs(title = "Diagrama de sectores")

#Histogramas
#Cuando las variables cuantitativas toman muchos valores, 
#el gráfico de la distribución es más claro si se agrupan los valores 
#próximos.
#El gráfico más común para describir la distribución de una variable 
#cuantitativa es un histograma
#Las barras de un histograma deben cubrir todo el recorrido de una variab

flores2 <- flores%>%  
  group_by(Species) %>%
  summarise(Mean.pl = mean(Petal.Length))

ggplot(data = flores,mapping = aes(x = Petal.Length,
                                   fill=Species))+
  geom_histogram(color="black",alpha = 0.9)+
  facet_wrap(~Species,ncol = 1)+
  theme_minimal()+
  geom_vline(data = flores2, 
               aes(xintercept=Mean.pl), lty = 2)+
  ylab(label = "Frecuencia")+
  xlab(label = "Longitud del pétalo")+
  labs(fill="Especies")+
  theme(strip.text = element_text(size=12),
        axis.title = element_text(size = 14),
        legend.text = element_text(size=13))
  
#Dibujar un gráfico estadístico no es un fin en sí mismo. 
#Su objetivo es ayudarnos a comprender los datos. Después de hacer un 
#gráfico, pregunta siempre: “¿qué veo?”. 

#Cuando describas una distribución, concéntrate en sus características 
#principales. Fíjate en los picos mayores; no te preocupes por las pequeñas 
#subidas y bajadas de las barras del histograma. Busca las observaciones 
#atípicas claras; no busques sólo los valores máximo y mínimo. 
#Identifica simetrías o asimetrías claras


#gráficos de tiempo
#Muchas variables se miden a lo largo del tiempo. Por ejemplo, 
#podríamos medir la altura de un niño en crecimiento o el precio de 
#una acción al final de cada mes. En estos ejemplos, nuestro interés 
#principal son los cambios a lo largo del tiempo. Para mostrarlos 
#dibujaremos un gráfico temporal

#creemos una serie de fechas y una variable
date<-as.Date(x = as.character(seq(from=1900,to=2020,by=10)),format = "%Y")
var<-NULL;for(i in 1:13){var[i]<-rnorm(n = 13,mean = i/2,sd = 1)}
data <- data.frame(var,date)

#graficamos
ggplot(data = data,mapping = aes(x = date,y = var))+
  geom_line(lty=2)+
  geom_label(mapping = aes(label=round(var,digits = 2)))+
  theme_classic()+
  xlab(label = "Años transcurridos")+
  ylab(label = "Variable")+
  labs(title = "Precio de la gasolina\n(dls)")

#Agregando una linea de tendencia
ggplot(data = data,mapping = aes(x = date,y = var))+
  geom_line(lty=2)+
  geom_smooth(method = "lm",se = TRUE,alpha=0.2)+
  geom_label(mapping = aes(label=round(var,digits = 2)))+
  theme_classic()+
  xlab(label = "Años transcurridos")+
  ylab(label = "Variable")+
  labs(title = "Precio de la gasolina\n(dls)")

#Cuando examines un gráfico temporal, fíjate una vez más en su aspecto 
#general y en las desviaciones importantes de dicho aspecto. 
#Un aspecto general que aparece con frecuencia es una tendencia; se 
#trata de una variación, a largo plazo, creciente o decreciente.

