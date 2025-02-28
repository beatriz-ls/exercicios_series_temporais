
ibov <- read.csv("ibovespa.csv")

log_retornos <- diff(log(ibov$ibv))

## Testando diferentes modelos ARIMA

lista_modelos <- NULL

for (d in 0:1){
  for (p in 0:5){
    for (q in 0:5){
      modelo <- forecast::Arima(log_retornos, order = c(p, d, q))
      lista_modelos <- rbind(lista_modelos, c(p, d, q, modelo$aic))
      print(paste0("Modelo ARIMA(",p, ",", d, ",", q,") finalizado."))
    }
  }
}

lista_modelos <- as.data.frame(lista_modelos)
colnames(lista_modelos) <- c("p", "d", "q", "aic")

modelos <- lista_modelos |>
  dplyr::arrange(aic)

## Escolhendo o modelo com menor AIC

melhor_ARIMA <- forecast::Arima(log_retornos,
                                order = c(modelos[1,]$p,
                                          modelos[1,]$d,
                                          modelos[1,]$q)
                                )
melhor_ARIMA

## Utilizando a função auto.arima para comparação

forecast::auto.arima(log_retornos)

## Testando diferentes modelos GARCH

erros_quadrados <- (melhor_ARIMA$residuals)^2

## ACF e PACF

acf(erros_quadrados)
pacf(erros_quadrados)

## Teste de Ljung-Box

# H0: não apresenta correlação
# H1: apresenta correlação

Box.test(erros_quadrados, type = "Ljung-Box", lag = 5)

## Testando diferentes modelos GARCH

lista_modelos <- NULL

for (p in 0:5){
  for (q in 0:5){
    if (p != 0 | q != 0){
      modelo <- tseries::garch(erros_quadrados, order = c(p, q))
      lista_modelos <- rbind(lista_modelos, c(p, q, AIC(modelo)))
      print(paste0("Modelo GARCH(",p, ",", q,") finalizado."))
    }
  }
}

lista_modelos <- as.data.frame(lista_modelos)
colnames(lista_modelos) <- c("p", "q", "aic")

modelos <- lista_modelos |>
  dplyr::arrange(aic)

## Escolhendo o modelo com menor AIC

melhor_GARCH <- tseries::garch(erros_quadrados,
                               order = c(modelos[1,]$p,
                                         modelos[1,]$q)
)

melhor_GARCH






