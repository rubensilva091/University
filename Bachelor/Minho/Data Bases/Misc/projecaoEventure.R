# Definir o custo inicial em bytes
custo_inicial <- 7203917

# Converter bytes para megabytes (1 byte = 1e-6 megabytes)
custo_inicial_MB <- custo_inicial * 1e-6

# Criar um vetor para representar os anos de 1 a 10
anos <- 1:10

# Definir as funções de crescimento em megabytes
funcao_n1 <- function(ano) {
  return(custo_inicial_MB * 1.1^ano)
}

funcao_n2 <- function(ano) {
  return(custo_inicial_MB * 1.15^ano)
}

funcao_n3 <- function(ano) {
  return(custo_inicial_MB * 1.2^ano)
}

funcao_n4 <- function(ano) {
  return(custo_inicial_MB * 1.25^ano)
}

funcao_n5 <- function(ano) {
  return(custo_inicial_MB * 1.3^ano)
}

# Criar o gráfico com grid
plot(anos, sapply(anos, funcao_n1), type = "l", col = "red", lwd = 2, ylim = c(0, max(funcao_n5(anos))), xlab = "Anos", ylab = "Custo em MegaBytes")
grid()

# Adicionar linhas ao gráfico
lines(anos, sapply(anos, funcao_n2), col = "blue", lwd = 2)
lines(anos, sapply(anos, funcao_n3), col = "green", lwd = 2)
lines(anos, sapply(anos, funcao_n4), col = "orange", lwd = 2)
lines(anos, sapply(anos, funcao_n5), col = "purple", lwd = 2)

# Adicionar rótulos e título ao gráfico
title(main = "Crescimento ao longo de 10 anos")

# Adicionar uma legenda no canto esquerdo superior
legend("topleft", legend = c("10%", "15%", "20%", "25%", "30%"),
       col = c("red", "blue", "green", "orange", "purple"), lty = 1, lwd = 2, title = "Crescimento anual em %")

