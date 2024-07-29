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
# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Definição da função dUGo
dUGo <- function(x, mu = 0.5, sigma = 1.2, tau = 0.5) {
  log(tau) / (1 - mu^(-sigma)) * sigma * x^(-(1 + sigma)) * exp((log(tau) / (1 - mu^(-sigma))) * (1 - x^(-sigma)))
}

# Valores dos parâmetros
sigma_values <- c(0.5, 1.0, 1.5)
tau <- 0.5
mu_fixed <- 0.5

# Sequência de valores de x
x <- seq(0, 5, length.out = 100)

# Criar data frame para armazenar os valores de x, sigma e densidade
data <- expand.grid(x = x, sigma = sigma_values)
data$density <- mapply(dUGo, data$x, mu_fixed, data$sigma, tau)

# Plotar as densidades usando ggplot2
ggplot(data, aes(x = x, y = density, color = factor(sigma), linetype = factor(sigma))) +
  geom_line(size = 1) +
  labs(title = paste("Densidade para diferentes valores de sigma com mu =", mu_fixed),
       x = "x", y = "Density", color = "sigma", linetype = "sigma") +
  theme_classic() +
  theme(legend.position = c(0.8, 0.8), # Posição da legenda dentro do gráfico
        legend.background = element_rect(fill = alpha('white', 0.6)), # Fundo transparente para a legenda
        aspect.ratio = 1) # Tornar a imagem mais quadrada





# Carregar pacotes necessários
library(ggplot2)
library(dplyr)

# Definição da função dUGo
dUGo <- function(x, mu = 0.5, sigma = 1.2, tau = 0.5) {
  log(tau) / (1 - mu^(-sigma)) * sigma * x^(-(1 + sigma)) * exp((log(tau) / (1 - mu^(-sigma))) * (1 - x^(-sigma)))
}

# Valores dos parâmetros
sigma_values <- c(0.5, 1.0, 2, 3, 4)
tau <- 0.5
mu_fixed <- 0.5

# Sequência de valores de x
x <- seq(0.01, 3, length.out = 100)

# Criar data frame para armazenar os valores de x, sigma e densidade
data <- expand.grid(x = x, sigma = sigma_values)
data$density <- mapply(dUGo, data$x, mu_fixed, data$sigma, tau)

# Garantir que 'sigma' seja um fator ordenado
data$sigma <- factor(data$sigma, levels = sigma_values)

# Plotar as densidades usando ggplot2
ggplot(data, aes(x = x, y = density, color = sigma, linetype = sigma)) +
  geom_line(size = 1) +
  labs(
    x = "y", y = "f(y)", color = "sigma", linetype = "sigma"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.8), # Posição da legenda dentro do gráfico
    legend.background = element_rect(fill = alpha('white', 0.6)), # Fundo transparente para a legenda
    axis.line = element_line(color = "black"), # Linha do eixo
    axis.ticks = element_line(color = "black"), # Adicionar todos os ticks
    axis.ticks.length = unit(0.3, "cm"), # Ajustar comprimento dos ticks
    axis.ticks.x.top = element_blank(), # Remover ticks no topo
    axis.ticks.y.right = element_blank(), # Remover ticks à direita
    axis.text.x = element_text(size = 12, color = "black"), # Texto dos ticks do eixo X em preto e tamanho 12
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x.top = element_blank(), # Remover texto dos ticks no topo
    axis.text.y.right = element_blank(), # Remover texto dos ticks à direita
    aspect.ratio = 1 # Tornar a imagem mais quadrada
  ) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) + # Eixo secundário Y sem rótulo
  scale_x_continuous(sec.axis = dup_axis(name = NULL)) + # Eixo secundário X sem rótulo
  scale_color_manual(values = c("#CB4335", "#212F3D", "#D4AC0D", "#1E8449", "#6C3483")) + # Definir cores específicas para as densidades
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "dotdash", "dotted")) # Definir tipos de linha específicos para cada sigma


# ggplot(data, aes(x = x, y = density, color = factor(sigma))) +
#   geom_line(size = 1) +
#   labs(title = "", # Se desejar adicionar um título, descomente e personalize
#        x = "y", y = "f(y)", color = "sigma") +
#   theme_classic() +
#   theme(
#     legend.position = c(0.8, 0.8), # Posição da legenda dentro do gráfico
#     legend.background = element_rect(fill = alpha('white', 0.6)), # Fundo transparente para a legenda
#     axis.line = element_line(color = "black"), # Linha dos eixos em preto
#     axis.line.x = element_line(color = "black"), # Linha do eixo inferior em preto
#     axis.line.y = element_line(color = "black"), # Linha do eixo esquerdo em preto
#     axis.line.x.top = element_line(color = "black"), # Linha do eixo superior em preto
#     axis.line.y.right = element_line(color = "black"), # Linha do eixo direito em preto
#     axis.ticks = element_line(color = "black"), # Ticks dos eixos em preto
#     axis.ticks.length = unit(0.3, "cm"), # Ajustar comprimento dos ticks
#     axis.ticks.x.top = element_blank(), # Remover ticks no topo
#     axis.ticks.y.right = element_blank(), # Remover ticks à direita
#     axis.text.x = element_text(size = 12, color = "black"), # Texto dos ticks do eixo X em preto e tamanho 14
#     axis.text.y = element_text(size = 12, color = "black"), # Texto dos ticks do eixo Y em preto e tamanho 14
#     axis.text.x.top = element_blank(), # Remover texto dos ticks no topo
#     axis.text.y.right = element_blank(), # Remover texto dos ticks à direita
#     aspect.ratio = 1 # Tornar a imagem mais quadrada
#   ) +
#   scale_y_continuous(sec.axis = dup_axis(name = NULL)) + # Eixo secundário Y sem rótulo
#   scale_x_continuous(sec.axis = dup_axis(name = NULL)) + # Eixo secundário X sem rótulo
#   scale_color_manual(values = c("#CB4335", "#212F3D", "#D4AC0D", "#2E86C1")) # Definir cores específicas para as densidades


ggsave("grafico2.png", plot = last_plot(), width = 8, height = 6, dpi = 600)

ggsave("gf1.pdf", plot = last_plot(), width = 10, height = 8, units = "in", dpi = 300)

# MU VALUES

# Definir os valores dos parâmetros
mu_values <- c(0.2, 0.4, 0.5, 0.7, 0.8)
sigma_fixed <- 1.2
tau <- 0.5

# Sequência de valores de x
x <- seq(0.01, 3, length.out = 100)

# Criar data frame para armazenar os valores de x, mu e densidade
data <- expand.grid(x = x, mu = mu_values)
data$density <- mapply(dUGo, data$x, data$mu, sigma_fixed, tau)

# Garantir que 'mu' seja um fator ordenado
data$mu <- factor(data$mu, levels = mu_values)

# Plotar as densidades usando ggplot2
ggplot(data, aes(x = x, y = density, color = mu, linetype = mu)) +
  geom_line(size = 1) +
  labs(
    x = "y", y = "f(y)", color = "mu", linetype = "mu"
  ) +
  theme_classic() +
  theme(
    legend.position = c(0.8, 0.8), # Posição da legenda dentro do gráfico
    legend.background = element_rect(fill = alpha('white', 0.6)), # Fundo transparente para a legenda
    axis.line = element_line(color = "black"), # Linha do eixo
    axis.ticks = element_line(color = "black"), # Adicionar todos os ticks
    axis.ticks.length = unit(0.3, "cm"), # Ajustar comprimento dos ticks
    axis.ticks.x.top = element_blank(), # Remover ticks no topo
    axis.ticks.y.right = element_blank(), # Remover ticks à direita
    axis.text.x = element_text(size = 12, color = "black"), # Texto dos ticks do eixo X em preto e tamanho 12
    axis.text.y = element_text(size = 12, color = "black"),
    axis.text.x.top = element_blank(), # Remover texto dos ticks no topo
    axis.text.y.right = element_blank(), # Remover texto dos ticks à direita
    aspect.ratio = 1 # Tornar a imagem mais quadrada
  ) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL), limits = c(0, 4), breaks = seq(0, 4, by = 1)) + # Eixo secundário Y sem rótulo
  scale_x_continuous(sec.axis = dup_axis(name = NULL)) + # Eixo secundário X sem rótulo
  scale_color_manual(values = c("#CB4335", "#212F3D", "#D4AC0D", "#1E8449", "#6C3483")) + # Definir cores específicas para os valores de mu
  scale_linetype_manual(values = c("solid", "longdash", "twodash", "dotdash", "dotted")) # Definir tipos de linha específicos para cada valor de mu


ggsave("gf2.png", plot = last_plot(), width = 8, height = 6, dpi = 600)

ggsave("gf2.pdf", plot = last_plot(), width = 10, height = 8, units = "in", dpi = 300)

