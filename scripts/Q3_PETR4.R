# bibliotecas

library(tidyquant)
library(dplyr)

##### baixando dados pelo pacote tidyquant

data <- tq_get("^GSPC",
               from = "2000-03-01",
               to = "2001-12-27",
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

##### analises de log retorno --------------------------------------------------

data <- data %>%
  arrange(date) %>%
  mutate(
    retorno_simples = (adjusted / lag(adjusted)) - 1,
    log_retorno = log(adjusted / lag(adjusted))
  )

# (a) retorno simples
retorno_dia1_para_dia2 <- data$retorno_simples[2]
retorno_dia1_para_dia7 <- (data$adjusted[7] / data$adjusted[1]) - 1

# (b) logretornos
logretorno_dia5_para_dia6 <- log(data$adjusted[6] / data$adjusted[5])
logretorno_dia5_para_dia10 <- log(data$adjusted[10] / data$adjusted[5])

# (c) autocorrelação
logretornos <- na.omit(data$log_retorno)
acf_logretornos <- acf(logretornos, main = "Função de Autocorrelação dos Logretornos do PETR4")
