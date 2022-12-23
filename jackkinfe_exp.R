#---------------------------------------------------------------------------------------
# Example 1: bias do estimador de Máxima verossimilhança
#---------------------------------------------------------------------------------------
# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
install.packages('hrbrthemes')
#------------------------------------------------------------
# Estimador de Jackknife para exponencial
#------------------------------------------------------------
n <- 20
beta <- 3
x <- rexp(n=n, rate=beta)

b.hat <- 1 / mean(x)
b.hat


b.jack = numeric(n)
for(i in 1:n) {
  b.jack[i] <- 1 / mean(x[-i])
}

beta_jack <- mean(b.jack)
beta_jack

# Função para o bias e variância do estimador de jack da exponencial

bias.J <- (n-1)*(mean(b.jack) - b.hat)
bias.J


dp_jack <- function(n) {
  dp.J <- sqrt(((n-1)/n)*(sum((b.jack - mean(b.jack))^2)) )
  return(dp.J)
}

dp_jack(20)


# Variância do estimador da Exponencial
dp_exp <- function(n, b) {
  variance <- ((n*b)^2) / (((n-1)^2)*(n-2))
  dp <- sqrt(variance)
  return(dp)
}
dp_exp(20, beta)


# bias do estimador da Exponencial
bias.EMV <- beta / (n-1)
bias.EMV











#------------------------------------------------------------
# Simulação de Monte Carlo para a exponencial
#------------------------------------------------------------
n = 10
beta = 0.5
estimador_jack = numeric(1000)
estimador_emv = numeric(1000)
dp_estimador_jack = numeric(1000)
bias_estimador_jack = numeric(1000)
dp_emv = numeric(1000)
bias_emv = numeric(1000)


for (i in 1:1000){
  x <- rexp(n=n, rate=beta)
  b.jack = numeric(n)
  for(j in 1:n) {
    b.jack[j] <- 1 / mean(x[-j])
  }
  b.hat <- 1/mean(x)
  estimador_emv[i] <- b.hat
  beta_jack <- mean(b.jack)
  estimador_jack[i] = beta_jack
  dp_estimador_jack[i] = sqrt(((n-1)/n)*(sum((b.jack - mean(b.jack))^2)) )
  bias_estimador_jack[i] = (n-1)*(mean(b.jack) - b.hat)
  dp_emv[i] = dp_exp(n, beta)
  bias_emv[i] = beta/(n-1)
}



#Resultados para tabela
mean(estimador_jack)
mean(estimador_emv)
mean(dp_estimador_jack)
mean(bias_estimador_jack)
mean(dp_emv)
mean(bias_emv)



#histograma



hist(estimador_jack, 
     col="blue",
     border="black",
     prob = TRUE,
     xlab = "valor estimador",
     main = "")

lines(density(estimador_jack),
      lwd = 2,
      col = "red")
