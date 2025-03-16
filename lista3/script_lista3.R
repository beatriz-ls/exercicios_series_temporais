# bibliotecas

library(tseries)
library(urca)

# dados ------------------------------------------------------------------------
d_petro <- read.csv("data/d_petro95.00.dat", header=FALSE)
d_banespa <- read.csv("data/d-ban95.00.dat", header=FALSE)
m_ibv <- read.csv("data/m-ibv94.01.dat", header=FALSE)
m_cbond <- read.csv("data/m-cbond94.01.dat", header=FALSE)


# testes -----------------------------------------------------------------------

# Teste ADF para a série de preços da Petrobrás
adf_petro <- ur.df(d_petro$V1, type="none", selectlags="AIC")
summary(adf_petro)

# Teste ADF para a série de preços do Banespa
adf_banespa <- ur.df(d_banespa$V1, type="none", selectlags="AIC")
summary(adf_banespa)

# Teste ADF para o índice Bovespa
adf_ibv <- ur.df(m_ibv$V1, type="none", selectlags="AIC")
summary(adf_ibv)

# Teste ADF para o C-bond
adf_cbond <- ur.df(m_cbond$V1, type="none", selectlags="AIC")
summary(adf_cbond)


# Teste PP para a série de preços da Petrobrás
pp_petro <- ur.pp(d_petro$V1, type="Z-alpha", model="constant", lags="short")
summary(pp_petro)

# Teste PP para a série de preços do Banespa
pp_banespa <- ur.pp(d_banespa$V1, type="Z-alpha", model="constant", lags="short")
summary(pp_banespa)

# Teste PP para o índice Bovespa
pp_ibv <- ur.pp(m_ibv$V1, type="Z-alpha", model="constant", lags="short")
summary(pp_ibv)

# Teste PP para o C-bond
pp_cbond <- ur.pp(m_cbond$V1, type="Z-alpha", model="constant", lags="short")
summary(pp_cbond)

