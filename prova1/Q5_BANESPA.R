# bibliotecas

library(dplyr)
library(forecast)

##### baixando dados -----------------------------------------------------------

data <- read.table("prova1/banespa.txt", header = FALSE, sep = "\t")

data <- data %>% mutate(retorno_diario = (V1 / lag(V1)) - 1)

data <- data %>% filter(!is.na(retorno_diario))

##### gráficos -----------------------------------------------------------------

acf(data$retorno_diario, main = "Função de Autocorrelação - ACF")

pacf(data$retorno_diario, main = "Função de Autocorrelação parcial - PACF")

##### selecionando o melhor modelo arima ---------------------------------------

auto.arima(data$retorno_diario)
