# bibliotecas

library(tseries)
library(urca)

# dados ------------------------------------------------------------------------

petro <- read.csv("lista3/data/d-petro95.00.dat", header=FALSE)

ibv_cbond <- read.csv("lista3/data/m-ibv94.01.dat", header=TRUE, sep = "")

ibov <- ibv_cbond$IBOV
cbond <- ibv_cbond$CBOND

# gráficos ---------------------------------------------------------------------

plot_petro <- ts.plot(petro)

plot_ibov <- ts.plot(ibov)

plot_cbond <- ts.plot(cbond)

# teste ADF --------------------------------------------------------------------

### Teste ADF para a série de preços da Petrobrás

adf_petro_trend <- ur.df(petro$V1, type="trend", selectlags="AIC")
summary(adf_petro_trend)

# obs: tendencia significativa, mas intercepto não significativo

#adf_petro_drift <- ur.df(petro$V1, type="drift", selectlags="AIC")
#summary(adf_petro_drift)

### Teste ADF para o índice Bovespa
adf_ibv_trend <- ur.df(ibov, type="trend", selectlags="AIC")
summary(adf_ibv_trend)

adf_ibv_drift <- ur.df(ibov, type= "drift", selectlags="AIC")
summary(adf_ibv_drift)

# tendencia não significativa, intercepto significativo, drift é mais adequado

# Teste ADF para o juros do C-bond brasileiro
adf_cbond_trend <- ur.df(cbond, type="trend", selectlags="AIC")
summary(adf_cbond_trend)

adf_cbond_drift <- ur.df(cbond, type = "drift", selectlags = "AIC")
summary(adf_cbond_drift)

# tendencia não significativa, intercepto significativo, drift é mais adequado

# teste PP ---------------------------------------------------------------------

# Teste PP para a série de preços da Petrobrás
pp_petro <- ur.pp(petro$V1, type="Z-alpha", model="constant", lags="short")
summary(pp_petro)

# Teste PP para o índice Bovespa
pp_ibv <- ur.pp(ibov, type="Z-alpha", model="constant", lags="short")
summary(pp_ibv)

# Teste PP para o C-bond
pp_cbond <- ur.pp(cbond, type="Z-alpha", model="constant", lags="short")
summary(pp_cbond)

