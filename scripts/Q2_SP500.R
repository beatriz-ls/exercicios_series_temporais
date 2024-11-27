# bibliotecas

library(tidyquant)
library(dplyr)

##### baixando dados pelo pacote tidyquant

data <- tq_get("^GSPC",
               from = "1994-06-01",
               to = "2001-08-01",
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

##### calculo do retorno médio anual -------------------------------------------


n_anos <- as.numeric(difftime(max(data$date), min(data$date), units = "days")) / 365

valor_inicial <- first(data$adjusted)
valor_final <- last(data$adjusted)

retorno_medio_anual <- (valor_final / valor_inicial)^(1 / n_anos) - 1

##### calculo do retorno simples médio anualizado ------------------------------

data <- data %>%
  arrange(date) %>%
  mutate(retorno_diario = (adjusted / lag(adjusted)) - 1)

data <- data %>% filter(!is.na(retorno_diario))

media_retorno_diario <- mean(data$retorno_diario)

retorno_simples_anualizado <- media_retorno_diario * 252

##### simulando investimento ---------------------------------------------------

# Se vc investir R/. 1.00 no ativo no final de Dezembro de 1994, qual é o valor do
# investimento ao final do Dezembro de 2001?

preco_inicial <- data %>% 
  filter(date == as.Date("1994-12-30")) %>% 
  pull(adjusted)

preco_final <- data %>% 
  filter(date == as.Date("2001-12-31")) %>% 
  pull(adjusted)

investimento_inicial <- 1
valor_final_investimento <- investimento_inicial * (preco_final / preco_inicial)







