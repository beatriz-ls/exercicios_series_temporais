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
