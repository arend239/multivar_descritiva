library(tidyverse)
library(corrplot)
library(reshape2)
library(viridisLite)
library(viridis)
library(RColorBrewer)

dt <- read.csv("data/Pokemon.csv")
head(dt)

summary(dt)
sapply(dt, class)
str(dt)

## - GRÁFICO 1
var_num <- c("HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")


paleta <- colorRampPalette(brewer.pal(9, "Blues"))

cor_matrix <- cor(dt[var_num], use = "complete.obs")
cor_matrix
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "#01080f",
  tl.srt = 0,         
  addCoef.col = "#dadaec",  
  number.cex = 0.9,
  col = paleta(200),  
  title = "Correlação entre Estatísticas de Pokémon",
  mar = c(1, 1, 2, 1),  
)


tipo.status <- dt %>%
  group_by(Type.1) %>%  
  summarise(across(all_of(var_num), mean, na.rm = TRUE))

tp.stat.normalizado <- tipo.status %>%
  mutate(across(all_of(var_num), ~ scales::rescale(.x)))

tp.sts.long <- tp.stat.normalizado %>%
  pivot_longer(cols = -Type.1, names_to = "Stat", values_to = "Value")  

# Gráfico atualizado
ggplot(tp.sts.long, aes(x = Stat, y = Type.1, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "viridis") +  # Usando a paleta viridis
  labs(
    title = "Estatísticas Médias por Tipo de Pokémon",
    y = "Tipo", 
  ) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, color = "#04070a", size = 12),
    axis.text.y = element_text(color = "#04070a", size = 12),
    axis.title = element_text(color = "#04070a", size = 14),
    plot.title = element_text(hjust = 0, color = "#04070a", face = "bold", size = 16, margin = margin(b = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(color = "#370f54", size = 12),
    legend.text = element_text(color = "#370f54", size = 10)
  )


## -- Gráfico 3
corrplot(
  cor_matrix,
  type="upper",
  method = "color",
  col = rev(inferno(1000)),     
  mar = c(1, 1, 2, 1),  
  addCoef.col = "white",  
  tl.col = "black",
  title = "Correlação entre Estatísticas de Pokémon",
)




# Gráfico atualizado - alternativa corrigida
ggplot(tp.sts.long, aes(x = Stat, y = Type.1, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  brewer.pal(9, "Reds") +  
  labs(
    title = "Estatísticas Médias por Tipo de Pokémon",
    y = "Tipo", 
  ) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, color = "#04070a", size = 12),
    axis.text.y = element_text(color = "#04070a", size = 12),
    axis.title = element_text(color = "#04070a", size = 14),
    plot.title = element_text(hjust = 0, color = "#04070a", face = "bold", size = 16, margin = margin(b = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(color = "#370f54", size = 12),
    legend.text = element_text(color = "#370f54", size = 10)
  )



### -- TESTE GRÁFICO VERMELHOS
# Criando uma paleta de vermelho com muitas gradações
paleta_vermelhos <- colorRampPalette(brewer.pal(9, "RdYlBu"))(100)

# Gráfico atualizado com paleta de vermelho mais suave
ggplot(tp.sts.long, aes(x = Stat, y = Type.1, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(colors = paleta_vermelhos) +  # Usando muitas gradações de vermelho
  labs(
    title = "Estatísticas Médias por Tipo de Pokémon",
    y = "Tipo"
  ) +
  scale_x_discrete(position = "top") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0, color = "#04070a", size = 12),
    axis.text.y = element_text(color = "#04070a", size = 12),
    axis.title = element_text(color = "#04070a", size = 14),
    plot.title = element_text(hjust = 0, color = "#04070a", face = "bold", size = 16, margin = margin(b = 20)),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.title = element_text(color = "#8B0000", size = 12),
    legend.text = element_text(color = "#8B0000", size = 10)
  )