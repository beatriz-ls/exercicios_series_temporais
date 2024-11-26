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