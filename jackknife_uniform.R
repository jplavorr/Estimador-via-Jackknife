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

theta_jack <-mean(theta.jack)
theta_jack

# desvio padrão do estimador da Uniforme
dp_uniform <- function(n, theta) {
  
  variance <- ((n*theta^2) / (n^3 + 4*n^2 + 5*n +2))
  dp <- sqrt(variance)
  return(dp)
}
sqrt(var_uniform(20,3))

# bias do estimador da Uniforme




#EMV para Uniforme
theta.hat <- max(x)
theta.hat

# Função para o bias e variância do estimador de jackknife uniforme
bias.J <- (n-1)*(mean(theta.jack) - theta.hat)
bias.J

theta.hat <- max(x)
theta.hat
theta_jack

bias.J <- (n-1)*(theta_jack - theta.hat)

bias.J

dp_jack <- function(n) {
  dp.J <- sqrt( ((n-1)/n) * (sum((theta.jack - mean(theta.jack))^2)) )
  return(dp.J)
}


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
  bias_emv[i] = ((n*theta)/(n-1)) - theta
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
