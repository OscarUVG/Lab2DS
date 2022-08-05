library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(dplyr)
library(ggplot2)


dataOriginal1 <- read.csv("IMPORTACION-VOLUMEN-2022-06.csv")
dataOriginal2 <- read.csv("CONSUMO-2022-06.csv")

# Set de datos de importaciones de gasolina mensual
# 258 filas, desde jan/2000 hasta jun/2022
data1 <- dataOriginal1[1:258,] %>% 
  select(Gasolina.superior, Gasolina.regular, Diesel) %>%
  mutate(k = row_number(), 
         Year = 2001 + k %/% 12, 
         Month = case_when(k%%12!=0 ~ k%%12, 
                           k%%12==0 ~ 12)) %>%
  select(-k)

data1[] <- lapply(data1, function(x) as.numeric(gsub(",","",x)))

# Set de datos de consumo de gasolina mensual
# 258 filas, desde jan/2000 hasta jun/2022
data2 <- dataOriginal2[1:258,] %>%
  select(Gasolina.superior, Gasolina.regular, Diesel) %>%
  mutate(k = row_number(), 
         Year = 2001 + k %/% 12, 
         Month = case_when(k%%12!=0 ~ k%%12, 
                           k%%12==0 ~ 12)) %>%
  select(-k)

data2[] <- lapply(data2, function(x) as.numeric(gsub(",","",x)))

# ========== Exploracion ==========

# Histogramas de la distribucion de cada variable
hist(data1$Gasolina.superior)
hist(data1$Gasolina.regular)
hist(data1$Diesel)
hist(data2$Gasolina.superior)
hist(data2$Gasolina.regular)
hist(data2$Diesel)

# Cantidad de gasolina por tipo por mes
ggplot(data1, aes(x = as.factor(Month), y = Gasolina.superior)) + geom_col()
ggplot(data1, aes(x = as.factor(Month), y = Gasolina.regular)) + geom_col()
ggplot(data1, aes(x = as.factor(Month), y = Diesel)) + geom_col()
ggplot(data2, aes(x = as.factor(Month), y = Gasolina.superior)) + geom_col()
ggplot(data2, aes(x = as.factor(Month), y = Gasolina.regular)) + geom_col()
ggplot(data2, aes(x = as.factor(Month), y = Diesel)) + geom_col()

# Cantidad de gasolina total por mes
tabla1 <- data1 %>%
  group_by(Month) %>%
  summarize(totGas = sum(Gasolina.superior) + sum(Gasolina.superior) + sum(Diesel))
tabla2 <- data2 %>%
  group_by(Month) %>%
  summarize(totGas = sum(Gasolina.superior) + sum(Gasolina.superior) + sum(Diesel))
ggplot(tabla1, aes(x = as.factor(Month), y = totGas)) + geom_col()
ggplot(tabla2, aes(x = as.factor(Month), y = totGas)) + geom_col()

# Importacion vs consumo
gasSup <- data.frame(data1$Gasolina.superior, data2$Gasolina.superior)
gasReg <- data.frame(data1$Gasolina.regular, data2$Gasolina.regular)
gasDiesel <- data.frame(data1$Diesel, data2$Diesel)

plot(data1$Gasolina.superior, data2$Gasolina.superior)
plot(data1$Gasolina.regular, data2$Gasolina.regular)
plot(data1$Diesel, data2$Diesel)

# ========== Series de tiempo ==========


# Creamos 6 time series; una por cada tipo de gasolina para cada data set. 
sup1 <- ts(data1$Gasolina.superior, start=c(2001,1), end=c(2022,6), frequency=12)
reg1 <- ts(data1$Gasolina.regular, start=c(2001,1), end=c(2022,6), frequency=12)
die1 <- ts(data1$Diesel, start=c(2001,1), end=c(2022,6), frequency=12)

sup2 <- ts(data2$Gasolina.superior, start=c(2001,1), end=c(2022,6), frequency=12)
reg2 <- ts(data2$Gasolina.regular, start=c(2001,1), end=c(2022,6), frequency=12)
die2 <- ts(data2$Diesel, start=c(2001,1), end=c(2022,6), frequency=12)
  
# Trabajamos con los datos del 2019 para atras para entrenar al modelo. 
trainSup1 <- ts(data1$Gasolina.superior, start=c(2001,1), end=c(2019,12), frequency=12)
trainReg1 <- ts(data1$Gasolina.regular, start=c(2001,1), end=c(2019,12), frequency=12)
trainDie1 <- ts(data1$Diesel, start=c(2001,1), end=c(2019,12), frequency=12)

trainSup2 <- ts(data2$Gasolina.superior, start=c(2001,1), end=c(2019,12), frequency=12)
trainReg2 <- ts(data2$Gasolina.regular, start=c(2001,1), end=c(2019,12), frequency=12)
trainDie2 <- ts(data2$Diesel, start=c(2001,1), end=c(2019,12), frequency=12)

# Todas las series de tiempo inician en enero 2001 y terminan en diciembre 2019. 
# Todas las series tienen frecuencia 12. 
start(trainSup1)
end(trainSup1)
frequency(trainSup1)

# Graficos de series de tiempo
plot(trainSup1)
abline(reg=lm(trainSup1~time(trainSup1)), col=c("red"))
plot(trainReg1)
abline(reg=lm(trainReg1~time(trainReg1)), col=c("red"))
plot(trainDie1)
abline(reg=lm(trainDie1~time(trainDie1)), col=c("red"))

plot(trainSup2)
abline(reg=lm(trainSup2~time(trainSup2)), col=c("red"))
plot(trainReg2)
abline(reg=lm(trainReg2~time(trainReg2)), col=c("red"))
plot(trainDie2)
abline(reg=lm(trainDie2~time(trainDie2)), col=c("red"))

# Descomposicion de las series. 
dec.trainSup1<-decompose(trainSup1)
plot(dec.trainSup1)
dec.trainReg1<-decompose(trainReg1)
plot(dec.trainReg1)
dec.trainDie1<-decompose(trainDie1)
plot(dec.trainDie1)

dec.trainSup2<-decompose(trainSup2)
plot(dec.trainSup2)
dec.trainReg2<-decompose(trainReg2)
plot(dec.trainReg2)
dec.trainDie2<-decompose(trainDie2)
plot(dec.trainDie2)

# ========== Modelos ARIMA ==========

# VARIANZA: las series de tiempo de importacion poseen mucha variabilidad. 
# La transformacion logaritmica mejora un poco esta condicion. Por otro lado, 
# las series de consumo poseen variabilidad relativamente constantes en varianza. 

# sup1: usamos version log
plot(trainSup1)
plot(log(trainSup1))

# reg1: usamos version log
plot(trainReg1)
plot(log(trainReg1))

# die1: usamos version log
plot(trainDie1)
plot(log(trainDie1))

# sup2: usamos version normal
plot(trainSup2)
plot(log(trainSup2))

# reg2: usamos version normal
plot(trainReg2)
plot(log(trainReg2))

# die2: usamos version normal
plot(trainDie2)
plot(log(trainDie2))

# MEDIA: Todos los modelos requieren exactamente una diferenciación, 
# de acuerdo de los test de estacionariedad en media con 0.01. 
# sup1: d=1 
adfTest(trainSup1)
unitrootTest(trainSup1)

adfTest(diff(trainSup1))
unitrootTest(diff(trainSup1))

# reg1: d=1
adfTest(trainReg1)
unitrootTest(trainReg1)

adfTest(diff(trainReg1))
unitrootTest(diff(trainReg1))

# die1: d=1
adfTest(trainDie1)
unitrootTest(trainDie1)

adfTest(diff(trainReg1))
unitrootTest(diff(trainReg1))

# sup2: d=1 
adfTest(trainSup2)
unitrootTest(trainSup2)

adfTest(diff(trainSup1))
unitrootTest(diff(trainSup1))

# reg2: d=1
adfTest(trainReg1)
unitrootTest(trainReg1)

adfTest(diff(trainReg1))
unitrootTest(diff(trainReg1))

# die2: d=1
adfTest(trainDie1)
unitrootTest(trainDie1)

adfTest(diff(trainReg1))
unitrootTest(diff(trainReg1))

# BUSQUEDA DE PARAMETROS P,Q : 

# q=4, p=1. 
acf(log(trainSup1), 100)
pacf(log(trainSup1), 100)

# q=5, p=1
acf(log(trainReg1), 100)
pacf(log(trainReg1), 100)

# q=4, p=2
acf(log(trainDie1), 100)
pacf(log(trainDie1), 100)

# q=4, p=1
acf(trainSup2, 100)
pacf(trainSup2, 100)

plot(dec.trainReg2$seasonal)
# q=5, p=1
acf(diff(trainReg2), 100)
pacf(trainReg2, 100)

# q=6, p=1
acf(trainDie2, 100)
pacf(trainDie2, 100)

# ========== Construccion de modelos ==========

fitSup1 <- arima(log(trainSup1), c(1, 1, 4), 
                 seasonal = list(order = c(0, 1, 0), period = 12))
predSup1 <- predict(fitSup1, n.ahead = 3*12)
ts.plot(trainSup1,2.718^predSup1$pred, log = "y", lty = c(1,3))

fitReg1 <- arima(log(trainReg1), c(1, 1, 5), 
                 seasonal = list(order = c(0, 1, 0), period = 12))
predReg1 <- predict(fitReg1, n.ahead = 3*12)
ts.plot(trainReg1,2.718^predReg1$pred, log = "y", lty = c(1,3))

fitDie1 <- arima(log(trainDie1), c(2, 1, 4), 
                 seasonal = list(order = c(0, 1, 0), period = 12))
predDie1 <- predict(fitDie1, n.ahead = 3*12)
ts.plot(trainDie1,2.718^predDie1$pred, log = "y", lty = c(1,3))

fitSup2 <- arima(log(trainSup2), c(1, 1, 4), 
                 seasonal = list(order = c(0, 1, 0), period = 12))
predSup2 <- predict(fitSup2, n.ahead = 3*12)
ts.plot(trainSup2,2.718^predSup2$pred, log = "y", lty = c(1,3))

fitReg2 <- arima(log(trainReg2), c(1, 1, 5), 
                 seasonal = list(order = c(0, 1, 0), period = 12))
predReg2 <- predict(fitReg2, n.ahead = 3*12)
ts.plot(trainReg2,2.718^predReg2$pred, log = "y", lty = c(1,3))

fitDie2 <- arima(log(trainDie2), c(1, 1, 6), 
                 seasonal = list(order = c(0, 1, 0), period = 12))
predDie2 <- predict(fitDie2, n.ahead = 3*12)
ts.plot(trainDie2,2.718^predDie2$pred, log = "y", lty = c(1,3))


