
### TIME SERIES


# Link: https://www.youtube.com/watch?v=koxayzGOITc
# Link: https://www.youtube.com/watch?v=NCZ-l99OAdQ
# Link: https://www.youtube.com/watch?v=CtdiMoor7CE
# Link: http://rstudio-pubs-static.s3.amazonaws.com/15068_38c9cb4792864a878f439bb3acb0800e.html
# Link: http://sillasgonzaga.com/material/curso_series_temporais/arima.html
# Link: https://www.youtube.com/watch?v=EXgIhhXvIkU
# Link: https://www.youtube.com/watch?v=PPumHDWBCcA
# Link: https://www.youtube.com/watch?v=7CEUfeRCfHU
# Link: https://www.youtube.com/watch?v=Txuo9JQjnKE

# Link: https://medium.com/techbloghotmart/dicas-para-criar-um-modelo-de-previs%C3%A3o-de-s%C3%A9ries-temporais-d4bb2e32e148



# link: https://support.minitab.com/pt-br/minitab/18/help-and-how-to/modeling-statistics/time-series/how-to/partial-autocorrelation/interpret-the-results/partial-autocorrelation-function-pacf/



bibliotecas = c("forecast", "lmtest", "nortest","dplyr","readr","tidyverse")
install.packages(bibliotecas)

library(forecast)
library(lmtest)
library(nortest)
library(dplyr)
library(readr)
library(tidyverse)


### LEITURA DOS DADOS
dados <- read_delim("C:/ts_rede/TS_BASE.csv", ";", escape_double = FALSE, trim_ws = TRUE)


### SÉRIES TEMPORAIS
dados1 <- ts(dados[,2], start = c(2007,1), end = c(2019,12), frequency = 12)






### GRÁFICO DA SÉRIE TEMPORAL
autoplot(dados1, xlab = "Data", ylab = "Acidentes")



# DECOMPOSIÇÃO DA SÉRIE TEMPORAL
componentes <- stats::decompose(dados1, type=c("additive")) # additive or multiplicative
plot(componentes)


# COMPONENTE ALEATÓRIA DA SÉRIE
shapiro.test(componentes$random)



# GRAFICOS DE ACF E PACF
ggtsdisplay(dados1)

#TRANSFORMAÇÃO DE BOX COX PARA ESTABILIZAÇÃO DA VARIÂNCIA DA SÉRIE DE DADOS
lambda <- BoxCox.lambda(dados1)  
lambda

dados1_bc <- BoxCox(dados1, lambda = lambda)

# GRAFICOS DE ACF E PACF COM TRANSFORMAÇÃO BOX COX
ggtsdisplay(dados1_bc)



# NÚMERO DE DIFERENCIAÇÕES NÃO SAZONAIS PARA TRANSFORMA A SÉRIE ESTACIONÁRIA
ndiffs(dados1_bc)  # não apresenta diferenciações






# teste com os modelos ARIMA e SARIMA para obter o melhor modelo
auto.arima(dados1, trace = TRUE, approximation = FALSE)




# implementar o arima com os parâmetros encontrados em 1
#mod <- Arima(dados1, order=c(0,0,1), seasonal = c(0,0,2))
mod <- Arima(dados1, order=c(0,0,1), seasonal = list(order=c(1,0,0),period=12))




# comparando os valores ajustados com a série original
plot(dados1)
lines(mod$fitted,col='blue')




# checar a qualidade do modelo
checkresiduals(mod$fitted)



# fazendo as previsões
prev <- forecast(mod, h=12)
autoplot(prev)



# valores médios
prev$mean

# valores do limíte inferior
prev$lower

# valores do limíte superior
prev$upper

