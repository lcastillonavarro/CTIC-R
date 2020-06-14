rm(list=ls())
setwd("D:/CTIC-R4DS/Evaluacion1")
getwd()
dir()

library(lubridate)
library(ggplot2)
library(gridExtra)
library(stringi)
library(dplyr)
library(fpp2)

#### Lectura Julio 2018 ####
OsinergJulio2k18<-read.table("201807_TABLA04_SICLI.txt", header=TRUE, sep="\t",
                             col.names = c("CodEmpresa","Suministro","PuntoSuministro","Fecha",
                                           "RegistroActiva","RegistroPasiva","Periodo"),
                             colClasses = c("factor","factor","factor","character","numeric",
                                            "numeric","character"))

str(OsinergJulio2k18)
dim(OsinergJulio2k18)
OsinergJulio2k18<-na.omit(OsinergJulio2k18)

# Conversion de la variable fecha
OsinergJulio2k18$FechaDate<-ymd_hm(OsinergJulio2k18$Fecha)
class(OsinergJulio2k18$FechaDate)

#usando funciones de lubridate
OsinergJulio2k18$año<-year(OsinergJulio2k18$FechaDate)
OsinergJulio2k18$dia<-day(OsinergJulio2k18$FechaDate)
OsinergJulio2k18$mes<-month(OsinergJulio2k18$FechaDate)
OsinergJulio2k18$Nhora<-hour(OsinergJulio2k18$FechaDate)
OsinergJulio2k18$Nminuto<-minute(OsinergJulio2k18$FechaDate)

#### Lectura Julio 2019 ####
OsinergJulio2k19<-read.table("201907_TABLA4.txt", header=TRUE, dec=",", sep="\t",
                             col.names = c("CodEmpresa","Suministro","PuntoSuministro","Fecha",
                                           "RegistroActiva","RegistroPasiva","Periodo"),
                             colClasses = c("factor","factor","factor","character","character",
                                            "character","character"))

OsinergJulio2k19$RegistroActiva<-gsub("[,]", ".",OsinergJulio2k19$RegistroActiva)
OsinergJulio2k19$RegistroPasiva<-gsub("[,]", ".",OsinergJulio2k19$RegistroPasiva)
OsinergJulio2k19$Fecha<-gsub("[,]", ".",OsinergJulio2k19$Fecha)
OsinergJulio2k19$Fecha<-gsub("[/]", "",OsinergJulio2k19$Fecha)
OsinergJulio2k19$RegistroActiva<-as.numeric(OsinergJulio2k19$RegistroActiva)
OsinergJulio2k19$RegistroPasiva<-as.numeric(OsinergJulio2k19$RegistroPasiva)

str(OsinergJulio2k19)
dim(OsinergJulio2k19)
OsinergJulio2k19<-na.omit(OsinergJulio2k19)

# Conversion de la variable fecha #
OsinergJulio2k19$FechaDate<-dmy_hms(OsinergJulio2k19$Fecha)
class(OsinergJulio2k19$FechaDate)

#usando funciones de lubridate
OsinergJulio2k19$año<-year(OsinergJulio2k19$FechaDate)
OsinergJulio2k19$dia<-day(OsinergJulio2k19$FechaDate)
OsinergJulio2k19$mes<-month(OsinergJulio2k19$FechaDate)
OsinergJulio2k19$Nhora<-hour(OsinergJulio2k19$FechaDate)
OsinergJulio2k19$Nminuto<-minute(OsinergJulio2k19$FechaDate)

#### Visualizar data Julio 2018-2019 ####
View(OsinergJulio2k18)
View(OsinergJulio2k19)
str(OsinergJulio2k18$CodEmpresa)
str(OsinergJulio2k19$CodEmpresa)
str(OsinergJulio2k18$Suministro)
str(OsinergJulio2k19$Suministro)

#### Resumen estadistico Julio 2018-2019 ####
summary(OsinergJulio2k18$RegistroActiva)
summary(OsinergJulio2k18$RegistroPasiva)
summary(OsinergJulio2k19$RegistroActiva)
summary(OsinergJulio2k19$RegistroPasiva)

# Determinar la cantidad de # CodEmpresa para Julio 2018 y 2019
a<-OsinergJulio2k18$CodEmpresa[!duplicated(OsinergJulio2k18$CodEmpresa)]
b<-OsinergJulio2k19$CodEmpresa[!duplicated(OsinergJulio2k19$CodEmpresa)]
str(a)
str(b)

# Determinar la empresa que marque el máximo de RegistroActiva y RegistroPasiva para Julio 2018-2019
# Tambien se visualiza la fecha donde ocurre ese máximo
# De los resultados el valor máximo se da con CodEmpresa=ELE y Suministro=CL0671
maximoRA<-max(OsinergJulio2k18$RegistroActiva)
View(OsinergJulio2k18[OsinergJulio2k18$RegistroActiva==maximoRA,])
maximoRP<-max(OsinergJulio2k18$RegistroPasiva)
View(OsinergJulio2k18[OsinergJulio2k18$RegistroPasiva==maximoRP,])

maximoRA<-max(OsinergJulio2k19$RegistroActiva)
View(OsinergJulio2k19[OsinergJulio2k19$RegistroActiva==maximoRA,])
maximoRP<-max(OsinergJulio2k19$RegistroPasiva)
View(OsinergJulio2k19[OsinergJulio2k19$RegistroPasiva==maximoRP,])

#Gráficas de Energía Activa y Pasiva por CodEmpresa="ELP" en función del Suministro para el 2018
CodEmpresa.max2018<-OsinergJulio2k18[OsinergJulio2k18$CodEmpresa=="ELP",]
Plt1<-ggplot(CodEmpresa.max2018, aes(x=FechaDate, y=RegistroActiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  geom_point( size=0.5, shape=21, fill="white") + 
  theme_minimal()
Plt2<-ggplot(CodEmpresa.max2018, aes(x=FechaDate, y=RegistroPasiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  geom_point( size=0.5, shape=21, fill="white") + 
  theme_minimal()
grid.arrange(Plt1,Plt2,nrow=2)

#Gráficas de Energía Activa y Pasiva por CodEmpresa="ELP" en función del Suministro para el 2019
CodEmpresa.max2019<-OsinergJulio2k19[OsinergJulio2k19$CodEmpresa=="ELP ",]

Plt3<-ggplot(CodEmpresa.max2019, aes(x=FechaDate, y=RegistroActiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  geom_point( size=0.5, shape=21, fill="white") + 
  theme_minimal()
Plt4<-ggplot(CodEmpresa.max2019, aes(x=FechaDate, y=RegistroPasiva, group = Suministro, colour = Suministro )) + 
  geom_line()  + 
  geom_point( size=0.5, shape=21, fill="white") + 
  theme_minimal()
grid.arrange(Plt3,Plt4,nrow=2)

#### Resumen estadistico CodEmpresa=ELE y Suministro=CL0671 en Julio 2018-2019 ####
Suministro.max2018<-CodEmpresa.max2018[CodEmpresa.max2018$Suministro=="CL0671",]
summary(Suministro.max2018$RegistroActiva)
summary(Suministro.max2018$RegistroPasiva)
Suministro.max2019<-CodEmpresa.max2019[CodEmpresa.max2019$Suministro=="CL0671",]
summary(Suministro.max2019$RegistroActiva)
summary(Suministro.max2019$RegistroPasiva)


#### Correlación entre valores maximos de Julio 2018 y Julio 2019 correspondientes al
#Suministro = "CL0671"
plot(Suministro.max2018$RegistroActiva,Suministro.max2019$RegistroActiva)
cor(Suministro.max2018$RegistroActiva,Suministro.max2019$RegistroActiva)
plot(Suministro.max2018$RegistroPasiva,Suministro.max2019$RegistroPasiva)
cor(Suministro.max2018$RegistroPasiva,Suministro.max2019$RegistroPasiva)

#### Descomposición de serie de tiempo con una frecuencia diaria para Julio 2018
#Suministro = "CL0671"
ts1<-ts(Suministro.max2018$RegistroActiva, frequency=96)
fit<- stl(ts1, s.window="periodic",robust=TRUE)
#Graficar descomposicion
autoplot(fit)+
  labs(title = "Descomposición de la serie de tiempo",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+
  theme_bw()

#### Descomposición de serie de tiempo con una frecuencia diaria para Julio 2019
#Suministro = "CL0671"
ts2<-ts(Suministro.max2019$RegistroActiva, frequency=96)
fit<- stl(ts2, s.window="periodic",robust=TRUE)
#Graficar descomposicion
autoplot(fit)+
  labs(title = "Descomposición de la serie de tiempo",                   
       x = "Tiempo",
       y = "Valor",
       colour = "Gears")+
  theme_bw()



