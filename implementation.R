# FUNCTION CHECK

# PROBABILITY DENSITY FUNCTION

dUGo <- function(x, alpha = 0.5, theta = 2, log = FALSE)
{
  if (any(alpha <=0) | any(alpha >=1)) stop(paste("alpha must be between 0 and 1"))
  if (any(theta<=0)) stop(paste("theta must be posite"))
  if (any(x <= 0) | any(x >= 1)) stop(paste("x must be between 0 and 1"))
  fx1 <- alpha*theta*x^(-1-theta)*exp(alpha*(1-x^-theta)) 
  if(log==FALSE) fx<-fx1 else fy<-log(fx1)
  fx

}

#Check integration in the range 0 to 1
#integrate(dUGo,0, 1)

"# Density Graph
x_values <- seq(0.001, 0.999, length.out = 1000)

y_values <- dUGo(x_values)

plot(x_values, y_values, type = "l", col = "blue", lwd = 2, 
     main = "Gráfico da função dUGo",
     xlab = "x", ylab = "dUGo(x)")"


