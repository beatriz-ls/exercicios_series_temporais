# bibliotecas

library(tidyquant)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)

##### baixando dados pelo pacote tidyquant -------------------------------------

data <- tq_get(
  "^BVSP",               
  from = "1994-07-04",    
  to = "2010-08-19",      
  get = "stock.prices")

data <- data %>% 
  select(-symbol) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))


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

# calculando log retorno diário

data <- data %>%
  arrange(date) %>%
  mutate(log_return = log(adjusted / lag(adjusted)))

# gráfico do log retorno

ggplot(data, aes(x = date, y = log_return)) +
  geom_line(color = "lightblue", size = 1) +
  labs(
    title = "Série Temporal do IBOVESPA (Log Retorno)",
    x = "Data",
    y = "Log Retorno do adjusted"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")

##### medidas descritivas ------------------------------------------------------

stats <- data.frame(
  Mean = mean(data$adjusted, na.rm = TRUE),
  Median = median(data$adjusted, na.rm = TRUE),
  Variance = var(data$adjusted, na.rm = TRUE),
  Skewness = skewness(data$adjusted, na.rm = TRUE),
  Kurtosis = kurtosis(data$adjusted, na.rm = TRUE)
)

##### histograma e qqplot ------------------------------------------------------

ggplot(data, aes(x = adjusted)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data$adjusted, na.rm = TRUE), 
                                         sd = sd(data$adjusted, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histograma IBOVESPA com Comparação com a Distribuição Normal",
       x = "Preço de Ajustado",
       y = "Densidade") +
  theme_minimal()

# qqplots

qqnorm(data$adjusted, main = "QQ Plot IBOVESPA - Comparação com a Normal")
qqline(data$adjusted, col = "red")

##### testando se a serie é ruido branco ---------------------------------------

# gráfico da função de autocorrelação

adjusted <- na.omit(data$adjusted)

acf(adjusted, main = "Função de Autocorrelação - ACF")

# teste de Ljung-box

Box.test(adjusted, lag = 10, type = "Ljung-Box")

##### fazendo analises para log retorno ----------------------------------------

# medidas descritivas para log retorno

stats_logreturn <- data.frame(
  Mean = mean(data$log_return, na.rm = TRUE),
  Median = median(data$log_return, na.rm = TRUE),
  Variance = var(data$log_return, na.rm = TRUE),
  Skewness = skewness(data$log_return, na.rm = TRUE),
  Kurtosis = kurtosis(data$log_return, na.rm = TRUE)
)

# histograma para log retorno

ggplot(data, aes(x = log_return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "skyblue", color = "black", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(data$log_return, na.rm = TRUE), 
                                         sd = sd(data$log_return, na.rm = TRUE)), 
                color = "red", size = 1) +
  labs(title = "Histograma IBOVESPA com Comparação com a Distribuição Normal",
       x = "Log Retorno do preço ajustado",
       y = "Densidade") +
  theme_minimal()

# qqplots log retorno

qqnorm(data$log_return, main = "QQ Plot IBOVESPA - Comparação com a Normal")
qqline(data$log_return, col = "red")

# testando ruid branco

log_return <- na.omit(data$log_return)

acf(log_return, main = "Função de Autocorrelação - ACF")

Box.test(log_return, lag = 10, type = "Ljung-Box")

