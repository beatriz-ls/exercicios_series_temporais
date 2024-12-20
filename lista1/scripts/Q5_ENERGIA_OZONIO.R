# bibliotecas

library(dplyr) # manipulação de dados
library(readr) # baixar dados de acordo com formatação
library(tidyverse)

##### baixando dados -----------------------------------------------------------

data1 <- read_csv("data/ENERGIA.csv")

data2 <- read_csv("data/OZONIO.csv")

# tratando dados

data1 <- data1 %>%
  mutate(
    Ano = as.numeric(as.character(Ano)),
    Mes = as.numeric(as.character(Mes)),
    date = as.Date(paste(Ano, Mes, "01", sep = "-"), format = "%Y-%m-%d"),
    energia = Energia
  ) %>%
  select(date, energia)

data2 <- data2 %>%
  mutate(
    Ano = as.numeric(as.character(Ano)),
    Mes = as.numeric(as.character(Mes)),
    date = as.Date(paste(Ano, Mes, "01", sep = "-"), format = "%Y-%m-%d"),
    ozonio = Ozonio
  ) %>%
  select(date, ozonio)

##### calculando média amostral ------------------------------------------------

media_amostral1 <- mean(data1$energia)

media_amostral2 <- mean(data2$ozonio)

##### decompondo a série temporal ----------------------------------------------

# tansformando em formato time-series

data1_ts <- ts(data1$energia,  start = c(1968, 1), frequency = 12)

data2_ts <- ts(data2$ozonio, start = c(1956, 1), frequency = 12)

# gráficos de sazonalidade e tendencia

decompose_graph1 <- decompose(data1_ts)

decompose_graph2 <- decompose(data2_ts)

##### calculando autocorrelação e autocovariancia ------------------------------

# calculando a autocorrelação (ACF) para K = 1, ..., 36

acf1 <- acf(data1_ts, lag.max = 36, plot = FALSE)$acf[-1] # remove o lag 0

acf2 <- acf(data2_ts, lag.max = 36, plot = FALSE)$acf[-1]

# calculando a variância da série

var1 <- var(data1_ts)

var2 <- var(data2_ts)

# calculando a autocovariância

autocov1 <- acf1 * var1

autocov2 <- acf2 * var2




