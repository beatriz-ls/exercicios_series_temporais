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
  mutate(date = as.Date(date, format = "%Y-%m-%d"),
         log_return = log(adjusted / lag(adjusted)))


# checagem dos dados

any(is.na(data))

sum(is.na(data))

colSums(is.na(data))

datas_completas <- seq.Date(min(data$date), max(data$date), by = "day")
datas_faltantes <- as.Date(setdiff(datas_completas, data$date),
                           format = "%Y-%m-%d")

##### log retorno mensal -------------------------------------------------------

data <- data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(
    log_return_mes = sum(log_return, na.rm = TRUE)
  )

##### medidas descritivas ------------------------------------------------------

stats_data <- data %>% 
  summarise(
    media = mean(log_return_mes, na.rm = TRUE),
    variancia = var(log_return_mes, na.rm = TRUE),
    assimetria = skewness(log_return_mes, na.rm = TRUE),
    curtose = kurtosis(log_return_mes, na.rm = TRUE),
    max = max(log_return_mes, na.rm = TRUE),
    min = min(log_return_mes, na.rm = TRUE)
  )

##### gráfico de histograma ----------------------------------------------------

ggplot(data, aes(x = log_return_mes)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma da série temporal de IBOVESPA", x = "Preço Ajustado", y = "Frequência")

##### calculando autocorrelação ------------------------------------------------

acf <- acf(data$log_return_mes, plot = FALSE)

