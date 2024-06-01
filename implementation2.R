# alpha reparameterization

# PROBABILITY DENSITY FUNCTION

dUGo <- function(x, mu=.5, sigma=1, tau=.5)
{
  
  fx<-log(tau) * sigma * x^(-(1 + sigma)) * exp((log(tau) / (1 - mu^(-sigma))) *
                                                   (1 - x^(-sigma)))
  
  return(fx)
  
}

integrate(dUGo,0,1) #VERIFICAR A INTEGRAL AQUI



# CUMULATIVE DISTRIBUTION FUNCTION 

pUGo <-  function(q, mu=.5, sigma=1, tau=.5) 
{
  cdf <- exp(((log(tau)) / (1 - mu^(-sigma))) * (1 - q^(-sigma)))
  return(cdf)
}



# QUANTILE FUNCTION
qUGo<-function(u, mu=.5, sigma=1, tau=.5)
{
  q<-(1 - (log(u) * (1 - mu^(-sigma)) / log(tau)))^(-1 / sigma)
  return(q)
}



