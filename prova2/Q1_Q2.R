# bibliotecas
library(readxl)
library(forecast)

# Carregando os dados
Petrobras <- read_excel("PROVA BEATRIZ/Petrobras.xls")

### Questão 1 ------------------------------------------------------------------

# Calculando o log retorno
log_retornos <- diff(log(Petrobras$PETROBRAS))

# Inicializando lista para armazenar os modelos
lista_modelos <- NULL

# Testando diferentes modelos ARMA (sem diferenciação)
for (p in 0:5){
  for (q in 0:5){
    modelo <- forecast::Arima(log_retornos, order = c(p, 0, q))  # d=0 para ARMA
    lista_modelos <- rbind(lista_modelos, c(p, 0, q, modelo$aic))
    #print(paste0("Modelo ARMA(", p, ", 0, ", q, ") finalizado."))
  }
}

# Convertendo a lista para um data.frame
lista_modelos <- as.data.frame(lista_modelos)
colnames(lista_modelos) <- c("p", "d", "q", "aic")

# Ordenando os modelos pelo AIC
modelos <- lista_modelos %>%
  dplyr::arrange(aic)

# Escolhendo o modelo ARMA com o menor AIC
melhor_ARMA <- forecast::Arima(log_retornos,
                               order = c(modelos[1,]$p,
                                         0,  # Diferenciação = 0 (ARMA)
                                         modelos[1,]$q)
)

# Exibindo o modelo escolhido
melhor_ARMA


### Questão 2 ------------------------------------------------------------------

## Testando diferentes modelos GARCH

residuos <- (melhor_ARIMA$residuals)

## ACF e PACF

acf(erros_quadrados)
pacf(erros_quadrados)

## Teste de Ljung-Box

# H0: não apresenta correlação
# H1: apresenta correlação

Box.test(erros_quadrados, type = "Ljung-Box", lag = 5)

## Testando diferentes modelos GARCH
r_values <- 0:6
s_values <- 0:0
best_aic_garch <- Inf
best_garch_model <- NULL
# Loop para ajustar diferentes modelos GARCH(r, 0)
for (r in r_values) {
  for (s in s_values) {
    spec <- ugarchspec(
      variance.model = list(garchOrder = c(r, s)),
      mean.model = list(armaOrder = c(0, 0)),
      distribution.model = "norm"
    )
    possible_model <- tryCatch(
      {
        fit <- ugarchfit(spec, residuos)
        list(model = fit, aic = infocriteria(fit)[1])
      },
      error = function(e) NULL
    )
    # Se o modelo foi ajustado corretamente, verifica o AIC
    if (!is.null(possible_model) && possible_model$aic < best_aic_garch) {
      best_aic_garch <- possible_model$aic
      best_garch_model <- possible_model$model
    }
  }
}
best_garch_model

