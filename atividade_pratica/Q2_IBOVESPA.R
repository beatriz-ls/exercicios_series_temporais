# bibliotecas

library(tidyquant)
library(ggplot2)
library(tseries)
library(tidyverse)
library(trend)
library(forecast)

##### baixando dados pelo pacote tidyquant -------------------------------------

data <- tq_get(
  "^BVSP",               
  from = "1995-01-03",    
  to = "2000-12-27",      
  get = "stock.prices")

data <- data %>% 
  select(-symbol) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# data no formato TS

data_ts <- ts(data$adjusted, start = c(1995,1,3), frequency = 365)


# checagem dos dados

any(is.na(data))

sum(is.na(data))

colSums(is.na(data))

datas_completas <- seq.Date(min(data$date), max(data$date), by = "day")
datas_faltantes <- as.Date(setdiff(datas_completas, data$date),
                           format = "%Y-%m-%d")

##### gráfico da série temporal ------------------------------------------------

ggplot(data, aes(x = date, y = adjusted)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Série Temporal do IBOVESPA",
    x = "Data",
    y = "adjusted"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

##### decomposição -------------------------------------------------------------

decompose <- decompose(data_ts1)

plot(decompose)

stl <- stl(data_ts1, s.window = "periodic")

plot(stl)


## assumindo modelo com tendencia e sazonalidade deterministica

# ajustar o modelo de holt-winters
modelo_hw <- hw(data_ts1, seasonal = "multiplicative")


# Criando um dataframe para visualização
df_plot <- data.frame(
  Data = time(data_ts), 
  Original = as.numeric(data_ts),
  Suavizado = as.numeric(modelo_hw$fitted)
)

# Plotando com ggplot2
ggplot(df_plot, aes(x = Data)) +
  geom_line(aes(y = Original, color = "Original")) +
  geom_line(aes(y = Suavizado, color = "Suavizado"), linetype = "dashed") +
  labs(title = "Série Original vs Série Suavizada (Holt-Winters)",
       x = "Tempo", y = "Valor") +
  scale_color_manual(values = c("Original" = "black", "Suavizado" = "red")) +
  theme_minimal()

# suavização por médias móveis

df_ma <- cbind(data$adjusted,
               ma(data$adjusted, order = 6),
               ma(data$adjusted, order = 12))


ts.plot(df_ma, col = c("black", "blue", "red"), lty = 1:3,
        main = "Temperatura em Ubatuba e Médias Móveis")
legend("topright", legend = c("Original", "Média Móvel 6", "Média Móvel 12"),
       col = c("black", "blue", "red"), lty = 1:3, bty = "n")














