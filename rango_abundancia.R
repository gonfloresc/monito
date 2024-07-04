# Cargar librerías necesarias
library(dplyr)
library(ggplot2)

# import data

data_spp <- read.csv("Resultados/Graficos/Abundancia relativa/Abundancia relativa.csv", sep = ";")

# Ordenar las especies por abundancia relativa dentro de cada sitio y asignarles un ranking
df <- data_spp %>%
  group_by(sitio) %>%
  arrange(desc(abun_rel)) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Convertir los niveles numéricos a etiquetas categóricas
df$sitio <- factor(df$sitio, levels = c(1, 2, 3), labels = c("KAW", "KOD", "LLA"))

# Graficar usando ggplot2 sin etiquetas de texto adicionales
ggplot(df, aes(x = rank, y = abun_rel, color = sitio)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = "Rango de abundancia", y = "Abundancia Relativa",
       title = "Curvas de Rango de Abundancias de los 3 Sitios", color = "Sitio") +
  scale_x_continuous(breaks = unique(df$rank), minor_breaks = NULL) +
  theme(axis.text.x = element_text(hjust = 1))
