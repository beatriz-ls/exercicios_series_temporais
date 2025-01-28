# bibliotecas

library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(readODS)
library(tidyverse)

##### baixando dados -----------------------------------------------------------

data <- read_ods("atividade_pratica/temperatura.ods")

# transformando em formato time series

data_ts <- ts(data$Ubatuba,  start = c(1976, 1), frequency = 12)

# checagem dos dados

any(is.na(data))

sum(is.na(data))

colSums(is.na(data))

##### decomposição -------------------------------------------------------------

decompose <- decompose(data_ts)

plot(decompose)

stl <- stl(data_ts, s.window = "periodic")

plot(stl)

# retirando a constante para melhor visualização

mean(data_ts)

data_ts1 <- data_ts - mean(data_ts)

decompose1 <- decompose(data_ts1)

plot(decompose1)

stl1 <- stl(data_ts1, s.window = "periodic")

plot(stl1)


##### modelo de decomposição ---------------------------------------------------

data_fit <- data %>% select(-c(Cananeia))

## teste de estacionariedade

adf.test(data_fit$Ubatuba) # série estacionária

## assumindo modelo com tendencia não consideravel e sazonalidade deterministica

# Converter ano para numérico e criar variável de tempo
data_fit$t <- 1:nrow(data_df)
data_fit$t2 <- data_df$t^2

# ajustar o modelo de regressão linear
modelo <- lm(Ubatuba ~ factor(Mes), data = data_fit)

# Exibir os resultados do modelo
summary(modelo)

## analisando os residuos do modelo proposto

# Diagnóstico gráfico dos resíduos
par(mfrow = c(2,2))
plot(modelo)

# Teste de Ljung-Box para verificar autocorrelação nos resíduos
Box.test(residuals(modelo), lag = 12, type = "Ljung-Box")


modelo_hw <- HoltWinters(serie_ts, beta = FALSE)





