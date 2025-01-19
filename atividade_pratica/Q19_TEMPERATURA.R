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


