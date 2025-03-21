library(tidyverse)
library(corrplot)
library(reshape2)
library(viridisLite)
library(viridis)

dt <- read.csv("data/Pokemon.csv")
head(dt)

summary(dt)
sapply(dt, class)
str(dt)
## - GRÁFICO 1


var_num <- c("HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")

library(RColorBrewer)

paleta_blues <- colorRampPalette(brewer.pal(8, "Blues"))(200)

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
  col = paleta_blues,  
  title = "Correlação entre Estatísticas de Pokémon",
  mar = c(1, 1, 2, 1),  
)


## - GRÁFICO 2
tipo.status <- dt %>%
  group_by(Type.1) %>%  
  summarise(across(all_of(var_num), mean, na.rm = TRUE)) %>% 

tp.stat.normalizado <- tipo.status %>%
  mutate(across(all_of(var_num), ~ scales::rescale(.x)))

tp.sts.long <- tp.stat.normalizado %>%
  pivot_longer(cols = -Type.1, names_to = "Stat", values_to = "Value")  


ggplot(tp.sts.long, aes(x = Stat, y = Type.1, fill = Value)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradient(low = "#5a95cf", high = "#00498F") +  
  labs(
    title = "Estatísticas Médias por Tipo de Pokémon",
    y = "Tipo"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, color = "#04070a"),
    axis.text.y = element_text(color = "#04070a"),
    axis.title = element_text(color = "#04070a"),
    plot.title = element_text(hjust = 0, color = "#04070a", face = "bold", size = 14, margin = margin(b = 10)),  # Título alinhado à esquerda
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    legend.title = element_text(color = "#00498F"),
    legend.text = element_text(color = "#00498F")
  )