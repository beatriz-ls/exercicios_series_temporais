# bibliotecas

library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(readODS)

##### baixando dados -----------------------------------------------------------

data <- read_ods("atividade_pratica/temperatura.ods")

# transformando em formato time series

data1 <- ts(data$Ubatuba,  start = c(1976, 1), frequency = 12)

# checagem dos dados

any(is.na(data))

sum(is.na(data))

colSums(is.na(data))

##### decomposição -------------------------------------------------------------

decompose <- decompose(data1)

plot(decompose)

stl <- stl(data1, s.window = "periodic")

plot(stl)

mean(data1)

data2 <- data1 - mean(data1)

decompose <- decompose(data2)

plot(decompose)

stl <- stl(data2, s.window = "periodic")

plot(stl)
