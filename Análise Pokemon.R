library(MASS)       
library(ggplot2)    
library(dplyr)      

ataque <- ataque
defesa <- defesa
mu1 <- mean(ataque)
mu2 <- mean(defesa)
sigma1 <- sd(ataque)
sigma2 <- sd(defesa)
rho <- cor(ataque, defesa)
rho
plot(ataque, defesa)

hist(ataque)
hist(defesa)

Sigma <- matrix(c(sigma1^2, rho * sigma1 * sigma2,
                  rho * sigma1 * sigma2, sigma2^2),
                nrow = 2)

#distribuição condicional de Y2 (defesa) dado Y1 (ataque):
#fórmula: E[Y2 | Y1 = y1] = mu2 + rho * (sigma2 / sigma1) * (y1 - mu1)
#variância condicional: sigma2^2 * (1 - rho^2)
y1 <- 90  #nota de ataque para prever a nota de Defesa
y2_cond_mean <- mu2 + rho * (sigma2 / sigma1) * (y1 - mu1)
y2_cond_var <- sigma2^2 * (1 - rho^2)

cat("Nota esperada de Defesa dado Ataque =", y1, ":", y2_cond_mean, "\n")
cat("Variância condicional:", y2_cond_var, "\n")

#sem curvas de nível:
ggplot(Pokemon, aes(x = Pokemon$Attack, y = Pokemon$Defense)) +
  geom_point(alpha = 0.3, color = "blue") +  # Pontos dos dados
  geom_vline(xintercept = y1, linetype = "dashed", alpha = 0.7, color = "black") +  #linha para Y1 = 90
  geom_hline(yintercept = y2_cond_mean, linetype = "dashed", alpha = 0.7, color = "black") +  #linha para E[Y2 | Y1 = 90], valor esperado de y2 dado y1
  labs(title = "Distribuição Normal Bivariada",
       x = "Nota de Ataque",
       y = "Nota de Defesa",
       subtitle = paste("Previsão de Defesa para Ataque =", y1, ":", round(y2_cond_mean, 2))) +
  theme_minimal()

#com curvas de nível
ggplot(Pokemon, aes(x = Pokemon$Attack, y = Pokemon$Defense)) +
  geom_point(alpha = 0.45, color = "blue") +  
  geom_density_2d(alpha = 0.55, color = "red") +          
  geom_vline(xintercept = y1, linetype = "dashed", alpha = 0.7, color = "black") +  #linha para Y1 = 90
  geom_hline(yintercept = y2_cond_mean, linetype = "dashed", alpha = 0.7, color = "black") +  #linha para E[Y2 | Y1 = 90], valor esperado de y2 dado y1
  labs(title = "Distribuição Normal Bivariada",
       x = "Nota de Ataque",
       y = "Nota de Defesa",
       subtitle = paste("Previsão de Defesa para Ataque =", y1, ":", round(y2_cond_mean, 2))) +
  theme_minimal()

#podemos ver a montanha alongada na direção da correlação positiva

library(mvtnorm) 

x <- seq(min(ataque), max(ataque), length.out = 40)
y <- seq(min(defesa), max(defesa), length.out = 40)
#sequência de 40 valores igualmente espaçados entre os valores mínimo e máximo de "Ataque" e "Defesa".
#objetivo: garantir que a grade de pontos seja suficientemente detalhada, 
#mas sem sobrecarregar o gráfico com um número excessivo de pontos.
#facilita a visualização 

#matriz de valores z com base na função de densidade bivariada normal
z <- matrix(NA, nrow = length(x), ncol = length(y))

#matriz z com valores da função de densidade bivariada normal
for(i in 1:length(x)) {
  for(j in 1:length(y)) {
    z[i, j] <- dnorm(x[i], mean = mu1, sd = sigma1) * dnorm(y[j], mean = mu2, sd = sigma2) *
      exp(-rho * (x[i] - mu1) * (y[j] - mu2) / (sigma1 * sigma2))
  }
}

#gráfico 3D usando a função persp (não interativo)
persp(x, y, z, theta = 30, phi = 30, col = "lightblue",
      xlab = "Ataque", ylab = "Defesa", zlab = "Densidade",
      main = "Gráfico 3D da Densidade Bivariada Normal")
#ângulos de rotação do gráfico: 
#theta controla a rotação ao redor do eixo z
#phi controla a rotação ao redor do eixo x.


#função plot_ly produz um gráfico visualmente mais atrativo que a função persp, 
#além disso, o gráfico é interativo.
library(tidyverse)
library(plotly)
plot_ly(x= ~x, y= ~y, z=~z)%>% 
  add_surface()

