#### Datos y librerias ####
rm(list=ls())
setwd("D:/CTIC-R4DS/Evaluacion3")
require(ggplot2)
require(tidyr) 
require(dplyr)
library(gridExtra)
library(rpart)
library(rpart.plot)

# Evaluacion de la 6ta Clase
#### Pregunta 1 ####
##  Realice un analisis descriptivo del data frame iris (Libreria datasets) 
##  por especie : histograma, boxplot , diagrama de dispersion [12 graficos]
##  y en conjunto para las 3 especies : histograma, boxplot , diagrama de dispersion [4 graficos]
data(iris)
class(iris)
#View(iris)
levels(iris$Species)
colnames(iris)
rownames(iris)
summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
summary(iris$Petal.Length)
summary(iris$Petal.Width)

# graficando diagrama de dispersión
iris %>% 
  mutate(rowNum = 1:nrow(.)) %>% #agrega la columan de número de filas
  gather(Measure, Value, -(Species:rowNum)) %>% 
  ggplot(aes(x = rowNum, y = Value, group = Species, color = Species)) +
  geom_point() +
  facet_wrap(~Measure, nrow = 2) 

# graficando histograma
Plt1<-ggplot(iris,aes(x=Sepal.Length))+
  geom_histogram(binwidth = 0.2, color = "black", aes(fill = Species))
Plt2<-ggplot(iris,aes(x=Sepal.Width))+
  geom_histogram(binwidth = 0.2, color = "black", aes(fill = Species))
Plt3<-ggplot(iris,aes(x=Petal.Length))+
  geom_histogram(binwidth = 0.2, color = "black", aes(fill = Species))
Plt4<-ggplot(iris,aes(x=Petal.Width))+
  geom_histogram(binwidth = 0.2, color = "black", aes(fill = Species))
grid.arrange(Plt1,Plt2,Plt3,Plt4,nrow=2)

# graficando caja de bigotes
Plt1<-ggplot(iris, aes(factor(Species), Sepal.Length, fill = Species)) +
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar', width = 0.5) 
Plt2<-ggplot(iris, aes(factor(Species), Sepal.Width, fill = Species)) +
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar', width = 0.5) 
Plt3<-ggplot(iris, aes(factor(Species), Petal.Length, fill = Species)) +
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar', width = 0.5) 
Plt4<-ggplot(iris, aes(factor(Species), Petal.Width, fill = Species)) +
  geom_boxplot() + 
  stat_boxplot(geom ='errorbar', width = 0.5) 
grid.arrange(Plt1,Plt2,Plt3,Plt4,nrow=2)


#### Pregunta 2 ####
##  Contruya un arbol de regresion para el data frame iris (Libreria datasets)  
## y genere las predicciones para el siguiente data frame:

mod1 <- rpart(iris$Species ~ ., data = iris)
class(mod1)
typeof(mod1)
mod1
mod1$splits
plot(mod1)
text(mod1, use.n=TRUE)

NuevaEspecie <- data.frame(
  Sepal.Length = 6.5, Sepal.Width = 3.0,
  Petal.Length = 5.2, Petal.Width = 2.0
)
mod1.pred<-predict(mod1,NuevaEspecie)
mod1.pred


#### Pregunta 3 ####
## Cree un arbol de clasificacion para predecir la variable "diabetes" en los siguientes dos escenarios
## Escenario 1 : no considere la variable "pedigree" 
## Escenario 2 : no considere la variable "glucose" 
## Para cada uno de los escenario, genere nuevos data frames para predecir sus nuevos resultados 
## (predecir la variable "diabetes" cuyos posibles resultados son pos-neg)
library(mlbench)
data("PimaIndiansDiabetes2")
str(PimaIndiansDiabetes2)
colnames(PimaIndiansDiabetes2)

# Limpieza de datos faltantes
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

Escenario1<-PimaIndiansDiabetes2[,-7]
colnames(Escenario1)
mod3 <- rpart(Escenario1$diabetes ~ ., data = Escenario1)
plot(mod3)
text(mod3, use.n=TRUE)
NuevaData <- data.frame(
  insulin = 128, glucose=160, age = 30, mass=35, pregnant=5,
  pressure = 70, triceps = 20
)
mod3.pred<-predict(mod3,NuevaData)
mod3.pred

Escenario2<-PimaIndiansDiabetes2[,-2]
mod4 <- rpart(Escenario2$diabetes ~ ., data = Escenario2)
plot(mod4)
text(mod4, use.n=TRUE)
NuevaData <- data.frame(
  insulin = 128, pedigree=0.5, age = 30, mass=35, pregnant=5,
  pressure = 70, triceps = 25
)
mod4.pred<-predict(mod4,NuevaData)
mod4.pred


#### Pregunta 4 ####
## Construta un arbol de regresion para el data frame Boston (libreria MASS) y genere las predicciones
library(MASS)
data("Boston")
View(Boston)
colnames(Boston)
## para los siguientes dos escenarios 
Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,
            193, 194, 209, 210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,
            366 ,367 ,371 ,378, 393 ,401, 406 ,422, 423 ,453 ,455 ,485, 496, 505)
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario1 <- Boston[Prueba1,-14] # dataframe de prueba 1
colnames(Escenario1)
Escenario2 <- Boston[Prueba2,-14] # dataframe de prueba 2
colnames(Escenario2)

#   Deacuerdo al arbol de regresion y a los resultados obtenidos usando los data frames de prueba :  Escenario1 y 
#   Escenario2, diga usted con cual de estos dos dataframes de prueba le fue "mejor" a las predicciones objtenidas
#   Sustente su respuesta a esta pregunta con un indicador cuantitativo y con un grafico.

mod5 <- rpart(Escenario1$ptratio ~ ., data = Escenario1)
plot(mod5)
text(mod5, use.n=TRUE)
mod5.pred<-predict(mod5,Escenario1)
hist(mod5.pred)

mod6 <- rpart(Escenario2$ptratio ~ ., data = Escenario2)
plot(mod6)
text(mod6, use.n=TRUE)
mod6.pred<-predict(mod6,Escenario2)
hist(mod6.pred)

# Se observa que el Escenario 1 da una mejor representación del arbol de regresion









