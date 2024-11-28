# bibliotecas

library(dplyr)

##### baixando dados -----------------------------------------------------------

data1 <- read_csv("data/ENERGIA.csv")

data2 <- read_csv("data/OZONIO.csv")

# tratando dados

data1 <- data1 %>%
  mutate(date = as.Date(paste(Ano, Mes, sep = "-"), format = "%Y-%m"),
         energia = Energia) %>%
  select(date,energia)

data2 <- data2 %>%
  mutate(date = as.Date(paste(Ano, Mes, sep = "-"), format = "%Y-%m"),
         ozonio = Ozonio) %>%
  select(date,ozonio)

