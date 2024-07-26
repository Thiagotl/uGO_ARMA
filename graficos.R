# Ajusta o layout para 2 linhas e 2 colunas
par(mfrow = c(2, 2))

# Cria 4 gráficos
plot(1:10, main = "Gráfico 1")
plot(10:1, main = "Gráfico 2")
plot(rnorm(10), main = "Gráfico 3")
plot(rnorm(10, mean = 5), main = "Gráfico 4")

# Ajusta as margens do gráfico
par(mar = c(5, 4, 4, 2) + 0.1)

# Cria um gráfico simples
plot(1:10, main = "Gráfico com margens ajustadas")

# Ajusta as margens externas
par(oma = c(2, 2, 2, 2))

# Cria um gráfico simples
plot(1:10, main = "Gráfico com margens externas")


# Ajusta o tamanho do texto
par(cex = 1.5)

# Cria um gráfico com texto aumentado
plot(1:10, main = "Texto aumentado com cex = 1.5")


# Ajusta o tipo e a largura da linha
par(lty = 2, lwd = 2)

# Cria um gráfico com linha tracejada e largura aumentada
plot(1:10, type = "l", main = "Linha tracejada e largura 2")


# Ajusta os rótulos dos eixos
plot(1:10, xlab = "Eixo X", ylab = "Eixo Y", main = "Gráfico com rótulos personalizados")



# Define o layout do gráfico
par(mfrow = c(2, 2))

# Cria 4 gráficos
plot(1:10, main = "Gráfico 1")
plot(10:1, main = "Gráfico 2")
plot(rnorm(10), main = "Gráfico 3")
plot(rnorm(10, mean = 5), main = "Gráfico 4")

# Restaura o layout padrão
par(mfrow = c(1, 1))

# Cria um gráfico simples com layout padrão
plot(1:10, main = "Gráfico com layout padrão")


