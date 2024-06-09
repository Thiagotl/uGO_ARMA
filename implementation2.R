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



