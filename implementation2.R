# alpha reparameterization

# PROBABILITY DENSITY FUNCTION

dUGo <- function(x, mu=.5, sigma=1.2, tau=.5)
{
  
  fx1<- log(tau)/(1 - mu^(-sigma)) * sigma * x^(-(1 + sigma)) * exp((log(tau) / (1 - mu^(-sigma))) *
                                                   (1 - x^(-sigma)))
  
  return(fx1)
  
}

integrate(dUGo,0,1) 

# CUMULATIVE DISTRIBUTION FUNCTION 

pUGo <-  function(q, mu=.5, sigma=1.2, tau=.5) 
{
  cdf <- exp(((log(tau)) / (1 - mu^(-sigma))) * (1 - q^(-sigma)))
  return(cdf)
}


# QUANTILE FUNCTION
qUGo<-function(u, mu=.5, sigma=1.2, tau=.5)
{
  q<-(1 - (log(u) * (1 - mu^(-sigma)) / log(tau)))^(-1 / sigma)
  return(q)
}


# CHEKING LOG-LIKELIHOOD 

x= .5 
mu=.5 
sigma=1.2 
tau=.5


llik<-function(x, mu=.5, sigma=1, tau=.5)
{
  ll<-log(log(tau)/(1-mu^-sigma))+log(sigma)+(-1-sigma)*log(x)+(log(tau)/(1-mu^-sigma))*(1-x^-sigma)
  return(ll)
}

log(dUGo(x, mu=.5, sigma=1, tau=.5))
llik(x, mu=.5, sigma=1, tau=.5)

z=c(.3,.4,.5)

log(dUGo(z[1], mu=.5, sigma=1, tau=.5)*dUGo(z[2], mu=.5, sigma=1, tau=.5)*dUGo(z[3], mu=.5, sigma=1, tau=.5))

sum(llik(z, mu=.5, sigma=1, tau=.5))



### GRAFICOS

# Valores dos parâmetros
sigma_values <- c(0.5, 1.0, 1.5)
tau <- 0.5
mu_fixed <- 0.5

# Gerar uma sequência de x para o gráfico
x <- seq(0.1, 10, length.out = 100)

# Configurar a janela gráfica para um único gráfico
par(mar = c(4, 4, 2, 1))  # Margens: baixo, esquerda, topo, direita

# Definir tipos de linha diferentes
line_types <- c(1, 2, 3, 4, 5)  # Tipos de linha: sólido, pontilhado, tracejado, pontilhado e traço longo

# Plotar o gráfico com todas as curvas
plot(x, dUGo(x, mu = mu_fixed, sigma = sigma_values[1], tau = tau), type = "l", lwd = 2,
     lty = line_types[1], xlab = "x", ylab = "Density",
     main = paste("Densidade para diferentes valores de sigma com mu =", mu_fixed))

# Adicionar linhas para os outros valores de sigma
for (i in seq_along(sigma_values)) {
  lines(x, dUGo(x, mu = mu_fixed, sigma = sigma_values[i], tau = tau), lty = line_types[i], lwd = 2)
}

# Adicionar legenda
legend("topright", legend = paste("sigma =", sigma_values), lty = line_types, lwd = 2)








# Criar gráficos pra mu
mu_values <- c(0.3, 0.5, 0.7, 0.9)



for (mu in mu_values) {
  for (sigma in sigma_values) {
    plot(x, dUGo(x, mu = mu, sigma = sigma, tau = tau), type = "l", lwd = 2,
         main = paste("mu =", mu, ", sigma =", sigma), xlab = "x", ylab = "Density")
  }
}








# Restaurar configuração gráfica
par(mfrow = c(1, 1))
