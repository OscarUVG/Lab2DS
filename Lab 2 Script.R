
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(dplyr)

dataOriginal1 <- read.csv("IMPORTACION-VOLUMEN-2022-06.csv")
dataOriginal2 <- read.csv("CONSUMO-2022-06.csv")

# Set de datos de importaciones de gasolina mensual
# 258 filas, desde jan/2000 hasta jun/2022
data1 <- dataOriginal1[1:258,] %>%
  select(Gasolina.superior, Gasolina.regular, Diesel)
data1[] <- lapply(data1, function(x) as.numeric(gsub(",","",x)))


# Set de datos de consumo de gasolina mensual
# 258 filas, desde jan/2000 hasta jun/2022
data2 <- dataOriginal2[1:258,] %>%
  select(Gasolina.superior, Gasolina.regular, Diesel)
data2[] <- lapply(data2, function(x) as.numeric(gsub(",","",x)))


# Creamos 6 time series; una por cada tipo de gasolina para cada data set. 
sup1 <- ts(data1$Gasolina.superior, start=c(2001,1), end=c(2021,12), frequency=12)
reg1 <- ts(data1$Gasolina.regular, start=c(2001,1), end=c(2022,6), frequency=12)
die1 <- ts(data1$Diesel, start=c(2001,1), end=c(2022,6), frequency=12)

sup2 <- ts(data2$Gasolina.superior, start=c(2001,1), end=c(2022,6), frequency=12)
reg2 <- ts(data2$Gasolina.regular, start=c(2001,1), end=c(2022,6), frequency=12)
die2 <- ts(data2$Diesel, start=c(2001,1), end=c(2022,6), frequency=12)
  
# ========== Exploracion ==========



  
  