# bibliotecas

library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)
library(readODS)
library(tidyverse)
library(trend)
library(forecast)

##### baixando dados -----------------------------------------------------------

data <- read_ods("atividade_pratica/data/temperatura.ods")

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


##### suavização médias móveis -------------------------------------------------

data_fit <- data %>% select(-c(Cananeia))

## teste de estacionariedade

adf.test(data_fit$Ubatuba) # série estacionária

## testes de tendencia

cox.stuart.test(data_fit$Ubatuba) # há tendencia

mk.test(data_fit$Ubatuba) # há tendencia

## assumindo modelo com tendencia e sazonalidade

# ajustar o modelo de holt-winters
modelo_hw <- hw(data_ts, seasonal = "additive")


# criando um dataframe para visualização
df_plot <- data.frame(
  Data = time(data_ts), 
  Original = as.numeric(data_ts),
  Aditivo = as.numeric(modelo_hw$fitted)
)

# plotando com ggplot2
ggplot(df_plot, aes(x = Data)) +
  geom_line(aes(y = Original, color = "Original")) +
  geom_line(aes(y = Suavizado, color = "Suavizado"), linetype = "dashed") +
  labs(title = "Série Original vs Série Suavizada (Holt-Winters)",
       x = "Tempo", y = "Valor") +
  scale_color_manual(values = c("Original" = "black", "Suavizado" = "red")) +
  theme_minimal()



# Suavização por média móvel de 12 meses
media_movel_12 <- filter(data_ts, rep(1/12, 12), sides = 2)

# Plotando a série original e a suavizada
plot(data_ts, type = "l", col = "blue", main = "Temperatura em Ubatuba e Média Móvel de 12 meses", ylab = "Temperatura (°C)")
lines(media_movel_12, col = "red", lwd = 2)
legend("topright", legend = c("Original", "Média Móvel de 12 meses"), col = c("blue", "red"), lwd = 2)


# Suavização por média móvel de 6 meses
media_movel_6 <- filter(data_ts, rep(1/6, 6), sides = 2)

# Plotando a série original e a suavizada
plot(data_ts, type = "l", col = "blue", main = "Temperatura em Ubatuba e Média Móvel de 6 meses", ylab = "Temperatura (°C)")
lines(media_movel_6, col = "green", lwd = 2)
legend("topright", legend = c("Original", "Média Móvel de 6 meses"), col = c("blue", "green"), lwd = 2)





