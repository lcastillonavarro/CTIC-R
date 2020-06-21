# Describir modelos AR(2) , graficarlos para valores diferentes 
# de los argumentos (ar = c(p1,p2))
# AR(2)
# Probar varias combinaciones de p1 y p2 , graficar las series de tiempo
# simuladas, y sus correspondientes funciones de autocorrelacion simple
# y funciones de autocorrelacion parcial 
# Repetir lo mismo para los procesos MA(2) 

rm(list =ls())
setwd("D:/CTIC-R4DS/Evaluacion2")
getwd()
dir()

#### Fijemos una semilla ####
set.seed(777)

#### Generamos modelos AR(2) p1=0.1 y p2=[-0.8,0.8] #### 
AR2_1<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,-0.8)),n = 100, sd = 0.1)
AR2_2<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,-0.4)),n = 100, sd = 0.1)
AR2_3<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,-0.2)),n = 100, sd = 0.1)
AR2_4<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.2)),n = 100, sd = 0.1)
AR2_5<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.4)),n = 100, sd = 0.1)
AR2_6<- arima.sim(model = list(order=c(2,0,0), ar=c(0.1,0.8)),n = 100, sd = 0.1)

graphics.off()
jpeg("AR[2]_Series de tiempo_ p1=0.1 (fijo) y p2=[-0.8,0.8].jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
ylm = c(min(AR2_1,AR2_2,AR2_3,AR2_3,AR2_4,AR2_5,AR2_6), 
        max(AR2_1,AR2_2,AR2_3,AR2_3,AR2_4,AR2_5,AR2_6))
plot.ts(AR2_1, ylim = ylm, main = "AR[2] p1=0.1 ^ p2=-0.8")
plot.ts(AR2_2, ylim = ylm, main = "AR[2] p1=0.1 ^ p2=-0.4")
plot.ts(AR2_3, ylim = ylm, main = "AR[2] p1=0.1 ^ p2=-0.2")
plot.ts(AR2_4, ylim = ylm, main = "AR[2] p1=0.1 ^ p2=0.2")
plot.ts(AR2_5, ylim = ylm, main = "AR[2] p1=0.1 ^ p2=0.4")
plot.ts(AR2_6, ylim = ylm, main = "AR[2] p1=0.1 ^ p2=-0.8")
dev.off()

graphics.off()
jpeg("AR[2]_Autocorrelacion_ p1=0.1 (fijo) y p2=[-0.8,0.8].jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
acf(AR2_1, main = "AR[2] p1=0.1 ^ p2=-0.8")
acf(AR2_2, main = "AR[2] p1=0.1 ^ p2=-0.4")
acf(AR2_3, main = "AR[2] p1=0.1 ^ p2=-0.2")
acf(AR2_4, main = "AR[2] p1=0.1 ^ p2=0.2")
acf(AR2_5, main = "AR[2] p1=0.1 ^ p2=0.4")
acf(AR2_6, main = "AR[2] p1=0.1 ^ p2=-0.8")
dev.off()

graphics.off()
jpeg("AR[2]_Autocorrelacion parcial_ p1=0.1 (fijo) y p2=[-0.8,0.8].jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
pacf(AR2_1, main = "AR[2] p1=0.1 ^ p2=-0.8")
pacf(AR2_2, main = "AR[2] p1=0.1 ^ p2=-0.4")
pacf(AR2_3, main = "AR[2] p1=0.1 ^ p2=-0.2")
pacf(AR2_4, main = "AR[2] p1=0.1 ^ p2=0.2")
pacf(AR2_5, main = "AR[2] p1=0.1 ^ p2=0.4")
pacf(AR2_6, main = "AR[2] p1=0.1 ^ p2=-0.8")
dev.off()

#### Generamos modelos AR(2) p1=[-0.8,0.8] y p2=0.1 #### 
AR2_7<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.8,0.1)),n = 100, sd = 0.1)
AR2_8<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.4,0.1)),n = 100, sd = 0.1)
AR2_9<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.2,0.1)),n = 100, sd = 0.1)
AR2_10<- arima.sim(model = list(order=c(2,0,0), ar=c(0.2,0.1)),n = 100, sd = 0.1)
AR2_11<- arima.sim(model = list(order=c(2,0,0), ar=c(0.4,0.1)),n = 100, sd = 0.1)
AR2_12<- arima.sim(model = list(order=c(2,0,0), ar=c(0.8,0.1)),n = 100, sd = 0.1)

graphics.off()
jpeg("AR[2]_Series de tiempo_ p1=[-0.8,0.8] y p2=0.1 (fijo).jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
ylm = c(min(AR2_7,AR2_8,AR2_9,AR2_10,AR2_11,AR2_12), 
        max(AR2_7,AR2_8,AR2_9,AR2_10,AR2_11,AR2_12))
plot.ts(AR2_7, ylim = ylm, main = "AR[2] p1=-0.8 & p2=0.1")
plot.ts(AR2_8, ylim = ylm, main = "AR[2] p1=-0.4 & p2=0.1")
plot.ts(AR2_9, ylim = ylm, main = "AR[2] p1=-0.2 & p2=0.1")
plot.ts(AR2_10, ylim = ylm, main = "AR[2] p1=0.2 & p2=0.1")
plot.ts(AR2_11, ylim = ylm, main = "AR[2] p1=0.4 & p2=0.1")
plot.ts(AR2_12, ylim = ylm, main = "AR[2] p1=0.8 & p2=0.1")
dev.off()

graphics.off()
jpeg("AR[2]_Autocorrelacion_ p1=[-0.8,0.8] y p2=0.1 (fijo).jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
acf(AR2_7, main = "AR[2] p1=-0.8 ^ p2=0.1")
acf(AR2_8, main = "AR[2] p1=-0.4 ^ p2=0.1")
acf(AR2_9, main = "AR[2] p1=-0.2 ^ p2=0.1")
acf(AR2_10, main = "AR[2] p1=0.2 ^ p2=0.1")
acf(AR2_11, main = "AR[2] p1=0.4 ^ p2=0.1")
acf(AR2_12, main = "AR[2] p1=0.8 ^ p2=0.1")
dev.off()

graphics.off()
jpeg("AR[2]_Autocorrelacion parcial_ p1=[-0.8,0.8] y p2=0.1 (fijo).jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
pacf(AR2_7, main = "AR[2] p1=-0.8 ^ p2=0.1")
pacf(AR2_8, main = "AR[2] p1=-0.4 ^ p2=0.1")
pacf(AR2_9, main = "AR[2] p1=-0.2 ^ p2=0.1")
pacf(AR2_10, main = "AR[2] p1=0.2 ^ p2=0.1")
pacf(AR2_11, main = "AR[2] p1=0.4 ^ p2=0.1")
pacf(AR2_12, main = "AR[2] p1=0.8 ^ p2=0.1")
dev.off()

#### Generamos modelos MA(2) p1=0.1 y p2=[-0.8,0.8] #### 
MA2_1<- arima.sim(model = list(order=c(0,0,2), ma=c(0.1,-0.8)),n = 100, sd = 0.1)
MA2_2<- arima.sim(model = list(order=c(0,0,2), ma=c(0.1,-0.4)),n = 100, sd = 0.1)
MA2_3<- arima.sim(model = list(order=c(0,0,2), ma=c(0.1,-0.2)),n = 100, sd = 0.1)
MA2_4<- arima.sim(model = list(order=c(0,0,2), ma=c(0.1,0.2)),n = 100, sd = 0.1)
MA2_5<- arima.sim(model = list(order=c(0,0,2), ma=c(0.1,0.4)),n = 100, sd = 0.1)
MA2_6<- arima.sim(model = list(order=c(0,0,2), ma=c(0.1,0.8)),n = 100, sd = 0.1)

graphics.off()
jpeg("MA[2]_Series de tiempo_ p1=0.1 (fijo) y p2=[-0.8,0.8].jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
ylm = c(min(MA2_1,MA2_2,MA2_3,MA2_3,MA2_4,MA2_5,MA2_6), 
        max(MA2_1,MA2_2,MA2_3,MA2_3,MA2_4,MA2_5,MA2_6))
plot.ts(MA2_1, ylim = ylm, main = "MA[2] p1=0.1 ^ p2=-0.8")
plot.ts(MA2_2, ylim = ylm, main = "MA[2] p1=0.1 ^ p2=-0.4")
plot.ts(MA2_3, ylim = ylm, main = "MA[2] p1=0.1 ^ p2=-0.2")
plot.ts(MA2_4, ylim = ylm, main = "MA[2] p1=0.1 ^ p2=0.2")
plot.ts(MA2_5, ylim = ylm, main = "MA[2] p1=0.1 ^ p2=0.4")
plot.ts(MA2_6, ylim = ylm, main = "MA[2] p1=0.1 ^ p2=-0.8")
dev.off()

graphics.off()
jpeg("MA[2]_Autocorrelacion_ p1=0.1 (fijo) y p2=[-0.8,0.8].jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
acf(MA2_1, main = "MA[2] p1=0.1 ^ p2=-0.8")
acf(MA2_2, main = "MA[2] p1=0.1 ^ p2=-0.4")
acf(MA2_3, main = "MA[2] p1=0.1 ^ p2=-0.2")
acf(MA2_4, main = "MA[2] p1=0.1 ^ p2=0.2")
acf(MA2_5, main = "MA[2] p1=0.1 ^ p2=0.4")
acf(MA2_6, main = "MA[2] p1=0.1 ^ p2=-0.8")
dev.off()

graphics.off()
jpeg("MA[2]_Autocorrelacion parcial_ p1=0.1 (fijo) y p2=[-0.8,0.8].jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
pacf(MA2_1, main = "MA[2] p1=0.1 ^ p2=-0.8")
pacf(MA2_2, main = "MA[2] p1=0.1 ^ p2=-0.4")
pacf(MA2_3, main = "MA[2] p1=0.1 ^ p2=-0.2")
pacf(MA2_4, main = "MA[2] p1=0.1 ^ p2=0.2")
pacf(MA2_5, main = "MA[2] p1=0.1 ^ p2=0.4")
pacf(MA2_6, main = "MA[2] p1=0.1 ^ p2=-0.8")
dev.off()

#### Generamos modelos MA(2) p1=[-0.8,0.8] y p2=0.1 #### 
MA2_7<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.8,0.1)),n = 100, sd = 0.1)
MA2_8<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.4,0.1)),n = 100, sd = 0.1)
MA2_9<- arima.sim(model = list(order=c(2,0,0), ar=c(-0.2,0.1)),n = 100, sd = 0.1)
MA2_10<- arima.sim(model = list(order=c(2,0,0), ar=c(0.2,0.1)),n = 100, sd = 0.1)
MA2_11<- arima.sim(model = list(order=c(2,0,0), ar=c(0.4,0.1)),n = 100, sd = 0.1)
MA2_12<- arima.sim(model = list(order=c(2,0,0), ar=c(0.8,0.1)),n = 100, sd = 0.1)

graphics.off()
jpeg("MA[2]_Series de tiempo_ p1=[-0.8,0.8] y p2=0.1 (fijo).jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
ylm = c(min(MA2_1,MA2_2,MA2_3,MA2_3,MA2_4,MA2_5,MA2_6), 
        max(MA2_1,MA2_2,MA2_3,MA2_3,MA2_4,MA2_5,MA2_6))
plot.ts(MA2_7, ylim = ylm, main = "MA[2] p1=-0.8 & p2=0.1")
plot.ts(MA2_8, ylim = ylm, main = "MA[2] p1=-0.4 & p2=0.1")
plot.ts(MA2_9, ylim = ylm, main = "MA[2] p1=-0.2 & p2=0.1")
plot.ts(MA2_10, ylim = ylm, main = "MA[2] p1=0.2 & p2=0.1")
plot.ts(MA2_11, ylim = ylm, main = "MA[2] p1=0.4 & p2=0.1")
plot.ts(MA2_12, ylim = ylm, main = "MA[2] p1=0.8 & p2=0.1")
dev.off()

graphics.off()
jpeg("MA[2]_Autocorrelacion_ p1=[-0.8,0.8] y p2=0.1 (fijo).jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
acf(MA2_7, main = "MA[2] p1=-0.8 ^ p2=0.1")
acf(MA2_8, main = "MA[2] p1=-0.4 ^ p2=0.1")
acf(MA2_9, main = "MA[2] p1=-0.2 ^ p2=0.1")
acf(MA2_10, main = "MA[2] p1=0.2 ^ p2=0.1")
acf(MA2_11, main = "MA[2] p1=0.4 ^ p2=0.1")
acf(MA2_12, main = "MA[2] p1=0.8 ^ p2=0.1")
dev.off()

graphics.off()
jpeg("MA[2]_Autocorrelacion parcial_ p1=[-0.8,0.8] y p2=0.1 (fijo).jpeg",width= 3200, height=1820,
     units="px",res = 300)
par(mfrow = c(2,3))
pacf(MA2_7, main = "MA[2] p1=-0.8 ^ p2=0.1")
pacf(MA2_8, main = "MA[2] p1=-0.4 ^ p2=0.1")
pacf(MA2_9, main = "MA[2] p1=-0.2 ^ p2=0.1")
pacf(MA2_10, main = "MA[2] p1=0.2 ^ p2=0.1")
pacf(MA2_11, main = "MA[2] p1=0.4 ^ p2=0.1")
pacf(MA2_12, main = "MA[2] p1=0.8 ^ p2=0.1")
dev.off()














