# bibliotecas

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

##### medidas descritivas ------------------------------------------------------

stats_data <- data %>% 
  summarise(
    media = mean(adjusted, na.rm = TRUE),
    variancia = var(adjusted, na.rm = TRUE),
    assimetria = skewness(adjusted, na.rm = TRUE),
    curtose = kurtosis(adjusted, na.rm = TRUE),
    max = max(adjusted, na.rm = TRUE),
    min = min(adjusted, na.rm = TRUE)
  )

##### gráfico de histograma ----------------------------------------------------

ggplot(data, aes(x = adjusted)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma da série temporal de IBOVESPA", x = "Preço Ajustado", y = "Frequência")

