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

data_ts <- ts(data$Cananeia,  start = c(1976, 1), frequency = 12)

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

data_fit <- data %>% select(-c(Ubatuba))

## teste de estacionariedade

adf.test(data_fit$Cananeia) # série estacionária

## testes de tendencia

cox.stuart.test(data_fit$Cananeia) # há tendencia

mk.test(data_fit$Cananeia) # há tendencia

## assumindo modelo com tendencia e sazonalidade

# ajustar o modelo de holt-winters
modelo_hw <- hw(data_ts, seasonal = "additive")

plot(data_ts, type = "l", col = "blue", main = "Série Original e Holt-Winters", ylab = "Valor")
lines(modelo_hw$fitted, col = "red", lwd = 2)
legend("topright", legend = c("Série Original", "Ajuste HW"), col = c("blue", "red"), lwd = 2)

# criando um dataframe para visualização
df_plot <- data.frame(
  Data = time(data_ts), 
  Original = as.numeric(data_ts),
  Suavizado = as.numeric(modelo_hw$fitted)
)

# plotando com ggplot2
ggplot(df_plot, aes(x = Data)) +
  geom_line(aes(y = Original, color = "Original")) +
  geom_line(aes(y = Suavizado, color = "Suavizado"), linetype = "dashed") +
  labs(title = "Série Original vs Série Suavizada (Holt-Winters)",
       x = "Tempo", y = "Valor") +
  scale_color_manual(values = c("Original" = "black", "Suavizado" = "red")) +
  theme_minimal()



# suavização por médias móveis

ma_6 <- ma(data_fit$Cananeia, order = 6)
ma_12 <- ma(data_fit$Cananeia, order = 12)

df_ma <- cbind(data_fit$Cananeia,
               ma_6,
               ma_12)


ts.plot(df_ma, col = c("black", "blue", "red"), lty = 1:3,
        main = "Temperatura em Cananeia e Médias Móveis")
legend("topright", legend = c("Original", "Média Móvel 6", "Média Móvel 12"),
       col = c("black", "blue", "red"), lty = 1:3, bty = "n")



# Criando um dataframe para comparação
df_comparacao <- data.frame(
  Data = time(data_ts),
  Original = as.numeric(data_ts),
  HoltWinters = as.numeric(modelo_hw$fitted),
  MediaMovel6 = as.numeric(ma_6),
  MediaMovel12 = as.numeric(ma_12)
)

# Cálculo das métricas de erro
mae <- function(real, previsto) mean(abs(real - previsto), na.rm = TRUE)
mse <- function(real, previsto) mean((real - previsto)^2, na.rm = TRUE)
mape <- function(real, previsto) mean(abs((real - previsto) / real), na.rm = TRUE) * 100

resultados_erro <- data.frame(
  Metodo = c("Holt-Winters", "Média Móvel 6", "Média Móvel 12"),
  MAE = c(mae(df_comparacao$Original, df_comparacao$HoltWinters),
          mae(df_comparacao$Original, df_comparacao$MediaMovel6),
          mae(df_comparacao$Original, df_comparacao$MediaMovel12)),
  MSE = c(mse(df_comparacao$Original, df_comparacao$HoltWinters),
          mse(df_comparacao$Original, df_comparacao$MediaMovel6),
          mse(df_comparacao$Original, df_comparacao$MediaMovel12)),
  MAPE = c(mape(df_comparacao$Original, df_comparacao$HoltWinters),
           mape(df_comparacao$Original, df_comparacao$MediaMovel6),
           mape(df_comparacao$Original, df_comparacao$MediaMovel12))
)

print(resultados_erro)
