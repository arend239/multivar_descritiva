if (!require("fmsb")) install.packages("fmsb")
if (!require("dplyr")) install.packages("dplyr")
if (!require("scales")) install.packages("scales")
library(fmsb)
library(dplyr)
library(scales)

tipos_selecionados <- c("Dragon", "Flying", "Steel")
dados_tipos <- Pokemon %>%
  filter(`Type 1` %in% tipos_selecionados) %>%
  group_by(`Type 1`) %>%
  summarise(across(c(HP, Attack, Defense, `Sp. Atk`, `Sp. Def`, Speed), mean)) %>% #agrupando por média
  rename(Type = `Type 1`)

#normalizando
dados_normalizados <- dados_tipos %>%
  mutate(across(-Type, ~ rescale(.x, to = c(10, 110)))) %>%
  select(-Type) %>%
  as.data.frame()

dados_plot <- rbind(
  rep(120, 6) %>% setNames(names(dados_normalizados)),
  rep(10, 6) %>% setNames(names(dados_normalizados)),
  dados_normalizados
)

#cores
type_colors <- c(
  "Dragon" = rgb(106, 13, 173, max = 255, alpha = 180),
  "Flying" = rgb(135, 206, 235, max = 255, alpha = 180),
  "Steel" = rgb(192, 192, 192, max = 255, alpha = 180)
)

#área de plotagem com espaço pra legenda
par(mar = c(3, 6, 3, 5))  # Margem direita aumentada para a legenda

#criando gráfico com legenda
radarchart(
  dados_plot,
  axistype = 1,
  pcol = substr(type_colors, 1, 7),
  pfcol = type_colors,
  plwd = 3,
  plty = 1,
  cglcol = "gray50",
  cglty = 1,
  cglwd = 1,
  axislabcol = "gray20",
  vlcex = 1,  # Rótulos aumentados
  title = "Comparação de Atributos Pokemon por Tipo",
  seg = 5,
  centerzero = FALSE,
  cex.main = 1.25
)

#ajustando legenda
legend(
  x = "bottomright",              
  inset = c(-0.10, 0),         
  legend = c("Dragon", "Flying", "Steel"),
  bty = "n",
  pch = 19,
  col = substr(type_colors, 1, 7),
  text.col = "black",
  cex = 0.8,
  pt.cex = 2,
  y.intersp = 1.3,
  xpd = TRUE                  
)

#nota de rodapé
mtext("Valores normalizados (10-110) | Fonte: Pokemon with stats - Alberto Barradas", 
      side = 1, line = 1, cex = 0.8, col = "gray40")
