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
  dp.J <- 
  return(dp.J)
}

dp_jack(30)


# Variância do estimador da Exponencial
dp_exp <- function(n, b) {
  variance <- ((n*b)^2) / (((n-1)^2)*(n-2))
  dp <- sqrt(variance)
  return(dp)
}
dp_exp(20, beta)


# bias do estimador da Exponencial
bias.EMV <- b / (n-1)
bias.EMV









#----------------------------------------------------------------------------------------------
# Estimador de Jackknife para uniforme
#----------------------------------------------------------------------------------------------
n <- 20
theta <- 3
runif(n,0,theta)
theta.jack = numeric(n)
for(i in 1:n) {
  theta.jack[i] <- max(x[-i])
}
 
  
# Variância do estimador da Uniforme
var_uniform <- function(n, theta) {
  
  variance <- ((n*theta^2) / (n^3 + 4*n^2 + 5*n +2))
  return(variance)
}
sqrt(var_uniform(20,3))

# bias do estimador da Uniforme



# Função para o bias e variância do estimador de jackknife uniforme
bias.J <- (n-1)*(mean(theta.jack) - theta.hat)
bias.J

theta.hat <- max(x)

bias.J <- (n-1)*(mean(theta.jack) - theta.hat)

bias.J

dp <- function(n, theta) {
  dp.J <- sqrt( ((n-1)/n) * (sum((theta.jack - mean(theta.jack))^2)) )
  return(dp.J)
}


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
  dp_emv[i] = dp_exp(n, b.hat)
  bias_emv[i] = b.hat / (n-1)
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
#------------------------------------------------------------
# Simulação de Monte Carlo para a uniforme
#------------------------------------------------------------
n <- 100
theta <- 1
estimador_jack = numeric(1000)
estimador_emv = numeric(1000)
dp_estimador_jack = numeric(1000)
bias_estimador_jack = numeric(1000)
dp_emv = numeric(1000)
bias_emv = numeric(1000)


for (i in 1:1000){
  x <- runif(n,0,theta)
  theta.jack = numeric(n)
  for(j in 1:n) {
    theta.jack[j] <- max(x[-j])
  }
  theta.hat <- max(x)
  estimador_emv[i] <- theta.hat
  t_jack <- mean(theta.jack)
  estimador_jack[i] = t_jack
  dp_estimador_jack[i] = sqrt( ((n-1)/n) * (sum((theta.jack - t_jack)^2)) )
  bias_estimador_jack[i] = (n-1)*(t_jack - theta.hat)
  dp_emv[i] = dp_uniform(n, theta.hat)
  bias_emv[i] = ((n*theta)/(n-1)) - theta.hat
}


#Resultados para tabela
mean(estimador_jack)
mean(estimador_emv)
mean(dp_estimador_jack)
mean(bias_estimador_jack)
mean(dp_emv)
mean(bias_emv)

#histograma


hist(bias_estimador_jack, 
     col="blue",
     border="black",
     prob = TRUE,
     xlab = "valor estimador",
     main = "")

lines(density(bias_estimador_jack),
      lwd = 2,
      col = "red")
