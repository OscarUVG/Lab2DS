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
  select(Gasolina.superior, Gasolina.regular, Diesel)
data1[] <- lapply(data1, function(x) as.numeric(gsub(",","",x)))
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
  select(Gasolina.superior, Gasolina.regular, Diesel)
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
  summarize(totGas = sum(totSup) + sum(totReg) + sum(totDie))
tabla2 <- data2 %>%
  group_by(Month) %>%
  summarize(totGas = sum(totSup) + sum(totReg) + sum(totDie))

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
sup1 <- ts(data1$Gasolina.superior, start=c(2001,1), end=c(2021,12), frequency=12)
sup1 <- ts(data1$Gasolina.superior, start=c(2001,1), end=c(2022,6), frequency=12)
reg1 <- ts(data1$Gasolina.regular, start=c(2001,1), end=c(2022,6), frequency=12)
die1 <- ts(data1$Diesel, start=c(2001,1), end=c(2022,6), frequency=12)

sup2 <- ts(data2$Gasolina.superior, start=c(2001,1), end=c(2022,6), frequency=12)
reg2 <- ts(data2$Gasolina.regular, start=c(2001,1), end=c(2022,6), frequency=12)
die2 <- ts(data2$Diesel, start=c(2001,1), end=c(2022,6), frequency=12)

# ========== Exploracion ==========
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
plot(trainReg1)
plot(trainDie1)
plot(trainSup2)
plot(trainReg2)
plot(trainDie2)

# Descomposicion de las series. No hay e
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
