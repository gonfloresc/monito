# Instalar y cargar kableExtra
library(ggplot2)
library(dplyr)
library(tidyr)
# Cargar paquete lme4
library(lme4)
install.packages("glmmTMB")
library(glmmTMB)

#install.packages("kableExtra")
#library(kableExtra)
library(MASS)

# Import Datos
data_casas <- read.csv(file = "Resultados/Casas_totales_2023.csv")
data_monito <- read.csv(file = "Resultados/Eventos_unicos_spp/2023_24h/Monito del monte_24h.csv", sep = ",")
data_rata <- read.csv(file = "Resultados/Eventos_unicos_spp/2023_1h/Rata negra_1h.csv", sep = ";")
data_raya <- read.csv(file = "Resultados/Eventos_unicos_spp/2023_12h/Rayadito_12h.csv", sep = ",")
data_nochestrampa <- read.delim("Resultados/Resultado_noches_trampa.csv", sep = ";")
data_cr <- read.csv(file = "Resultados/Taza Captura/cr12_poisson.csv", sep = ";")

# Contar el número de eventos por casa
eventos_por_casa <- as.data.frame(table(data_monito$Casa))
eventos_por_casa_rata <- as.data.frame(table(data_rata$Casa))
eventos_por_casa_raya <- as.data.frame(table(data_raya$Casa))

# Renombrar las columnas
names(eventos_por_casa) <- c("casa", "n_eventos")
names(eventos_por_casa_rata) <- c("casa", "n_eventos")
names(eventos_por_casa_raya) <- c("casa", "n_eventos")

eventos_por_casa$casa <- as.numeric(sub("casa", "", eventos_por_casa$casa))
eventos_por_casa_rata$casa <- as.numeric(sub("casa", "", eventos_por_casa_rata$casa))
eventos_por_casa_raya$casa <- as.numeric(sub("casa", "", eventos_por_casa_raya$casa))

### Taza de captura por casa monito
taza_captura <- data.frame(matrix(nrow = nrow(data_casas), ncol = 0))
taza_captura$casa <- data_casas$x
taza_captura$casa <- gsub("casa", "", taza_captura$casa)
# merge
taza_captura <- merge(taza_captura, eventos_por_casa[, c("casa", "n_eventos")], by = "casa", all.x = TRUE)
taza_captura$n_eventos[is.na(taza_captura$n_eventos)] <- 0

# Renombrar la columna "Casa" de "noches_trampa" a "casa" para que coincida con "taza_captura"
colnames(data_nochestrampa)[colnames(data_nochestrampa) == "Casa.Anidera"] <- "casa"
data_nochestrampa$casa <- gsub(" extra", "", data_nochestrampa$casa)
taza_captura <- merge(taza_captura, data_nochestrampa[, c("casa", "Duracion_total")], by = "casa", all.x = TRUE)
colnames(taza_captura)[colnames(taza_captura) == "Duracion_total"] <- "n_trampa"

orden_indices <- order(as.numeric(gsub("casa", "", taza_captura$casa)))
taza_captura <- taza_captura[orden_indices, , drop = FALSE]
rownames(taza_captura) <- NULL

## Taza de captura relativa a noches trampas * 100 (ajuste standard)
taza_captura$cr_100 <- (taza_captura$n_eventos/taza_captura$n_trampa)*100
write.csv(taza_captura, "Resultados/Taza Captura/cr_rata_24h.csv", row.names = FALSE)


### Taza de captura por casa rata
taza_captura_rata <- data.frame(matrix(nrow = nrow(data_casas), ncol = 0))
taza_captura_rata$casa <- data_casas$x
taza_captura_rata$casa <- gsub("casa", "", taza_captura_rata$casa)
# merge
taza_captura_rata <- merge(taza_captura_rata, eventos_por_casa_rata[, c("casa", "n_eventos")], by = "casa", all.x = TRUE)
taza_captura_rata$n_eventos[is.na(taza_captura_rata$n_eventos)] <- 0

# Renombrar la columna "Casa" de "noches_trampa" a "casa" para que coincida con "taza_captura"
colnames(data_nochestrampa)[colnames(data_nochestrampa) == "Casa.Anidera"] <- "casa"
data_nochestrampa$casa <- gsub(" extra", "", data_nochestrampa$casa)
taza_captura_rata <- merge(taza_captura_rata, data_nochestrampa[, c("casa", "Duracion_total")], by = "casa", all.x = TRUE)
colnames(taza_captura_rata)[colnames(taza_captura_rata) == "Duracion_total"] <- "n_trampa"

orden_indices <- order(as.numeric(gsub("casa", "", taza_captura_rata$casa)))
taza_captura_rata <- taza_captura_rata[orden_indices, , drop = FALSE]
rownames(taza_captura_rata) <- NULL

## Taza de captura relativa a noches trampas * 100 (ajuste standard)
taza_captura_rata$cr_100 <- (taza_captura_rata$n_eventos/taza_captura_rata$n_trampa)*100
write.csv(taza_captura, "Resultados/Taza Captura/cr_rata_24h.csv", row.names = FALSE)

### Taza de captura por casa rayadito
taza_captura_raya <- data.frame(matrix(nrow = nrow(data_casas), ncol = 0))
taza_captura_raya$casa <- data_casas$x
taza_captura_raya$casa <- gsub("casa", "", taza_captura_raya$casa)
# merge
taza_captura_raya <- merge(taza_captura_raya, eventos_por_casa_raya[, c("casa", "n_eventos")], by = "casa", all.x = TRUE)
taza_captura_raya$n_eventos[is.na(taza_captura_raya$n_eventos)] <- 0

# Renombrar la columna "Casa" de "noches_trampa" a "casa" para que coincida con "taza_captura"
colnames(data_nochestrampa)[colnames(data_nochestrampa) == "Casa.Anidera"] <- "casa"
data_nochestrampa$casa <- gsub(" extra", "", data_nochestrampa$casa)
taza_captura_raya <- merge(taza_captura_raya, data_nochestrampa[, c("casa", "Duracion_total")], by = "casa", all.x = TRUE)
colnames(taza_captura_raya)[colnames(taza_captura_raya) == "Duracion_total"] <- "n_trampa"

orden_indices <- order(as.numeric(gsub("casa", "", taza_captura_raya$casa)))
taza_captura_raya <- taza_captura_raya[orden_indices, , drop = FALSE]
rownames(taza_captura_raya) <- NULL

## Taza de captura relativa a noches trampas * 100 (ajuste standard)
taza_captura_raya$cr_100 <- (taza_captura_raya$n_eventos/taza_captura_raya$n_trampa)*100
write.csv(taza_captura_raya, "Resultados/Taza Captura/cr_rayadito_12h.csv", row.names = FALSE)

## Regresion logista sobre la tazas de captura
taza_captura_dos <- merge(taza_captura, taza_captura_rata[, c("casa", "n_eventos", "cr_100")], by = "casa", all.x = TRUE)
orden_indices <- order(as.numeric(gsub("casa", "", taza_captura_dos$casa)))
taza_captura_dos <- taza_captura_dos[orden_indices, , drop = FALSE]
rownames(taza_captura_dos) <- NULL
taza_captura_dos$casa <- as.character(taza_captura_dos$casa)





### modelos

# ajuste a Z-score
# Estandarizar variables X1 y X2 a puntuaciones z
# Estandarización a puntuaciones z, asegurando que no haya valores negativos
data_cr$cr_100_mon <- scale(data_cr$cr_100_mon, center = TRUE, scale = TRUE)
data_cr$noches_trampa <- scale(data_cr$noches_trampa)


# Definición manual de modelo GLMM con zero-inflation (Poisson)
# Ajustar el modelo GLMM con zero-inflation usando la distribución Poisson
modelo <- glmmTMB(cr_100_mon ~  noches_trampa,
                  data = data_cr, 
                  family = poisson(link = "log"),
                  ziformula = ~ cr_100_rat)

# Ajustar el modelo GLMM con zero-inflation usando la distribución binomial negativa
modelo <- glmmTMB(cr_100_mon ~ cr_100_rat,
                  data = data_cr,
                  family = nbinom2(link = "log"),
                  ziformula = ~ cr_100_rat)  # Zero-inflation formula

# Ver los resultados del modelo
summary(modelo)




















  ### Taza de captura por sitio

data_cr$sitio <- ifelse(data_cr$casa > 0 & data_cr$casa <= 40, "KAW", ifelse(data_cr$casa > 40 & data_cr$casa <= 120, "KOD", "LLA"))
PIC <- subset(data_cr, casa >= 81 & casa <= 120)
LLA <- subset(data_cr, casa >= 121 & casa <= 159)

data_cr_long$mean_mon->mean(data_cr_long$cr_100.x)
mean(LLA$cr_100.y)
sd(LLA$cr_100.x)
sd(LLA$cr_100.y)

# Crear el gráfico
# Convertir el dataframe de formato ancho a largo

data_cr$casa <- factor(data_cr$casa, levels = unique(data_cr$casa))
data_cr_long <- data_cr %>%
  pivot_longer(cols = c(cr_100_mon, cr_100_rat), names_to = "variable", values_to = "value")

y_max <- max(data_cr_long$value)

# Definir colores para las barras y los puntos
my_colors <- c("cr_100_mon" = "#74c476", "cr_100_rat" = "#fc9272")  # Colores similares a los usados en ggplot por defecto

# Definir colores para las barras y los puntos
my_colors2 <- c("cr_100_mon" = "#31a354", "cr_100_rat" = "#fb6a4a")  # Colores similares a los usados en ggplot por defecto

# 1. Crear el gráfico de barras con puntos coloreados por variable
ggplot(data_cr_long, aes(x = sitio, y = value, fill = variable)) + 
  #geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +  # Barras
  geom_boxplot(aes(color = value), position = position_dodge(width = 0.7), outlier.shape = NA, width = 0.5)  +  # Puntos con colores similares a las barras
  labs(title = "Tasas de captura LLancalil (LLA)",
       x = "Casas Nido",
       y = "Capture rate (12h)",
       fill = "Especie",
       color = "Especie") +  # Etiqueta la leyenda correctamente
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotar etiquetas del eje x
        plot.title = element_text(hjust = 0.5)) + # Centrar el título) 
  scale_y_continuous(breaks = seq(0, 15, by = 1), limits = c(0, 15)) +  # Ajustar el eje y de 5 en 5 y establecer los límites
  scale_fill_manual(values = my_colors, labels = c("Monito del monte", "Rata negra")) +  # Colores para las barras
  scale_color_manual(values = my_colors2, labels = c("Monito del monte", "Rata negra")) # Colores para los puntos
  
# 2. Crear el gráfico de barras con puntos coloreados por variable
# Reestructurar los datos para ggplot2
data_cr_long <- tidyr::pivot_longer(data_cr, cols = c(cr_100_mon, cr_100_rat), 
                               names_to = "especie", values_to = "tasa_captura (logn)")

# Crear el boxplot(
ggplot(data_cr_long, aes(x = sitio, y = log(tasa_captura), fill = especie)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red", position = position_dodge(width = 0.75)) +
  labs(title = "Tasa de Captura por Sitio y Especie",
       x = "Sitio",
       y = "log - Tasa de Captura (12h)",
       fill = "Especie") +
  theme_minimal()














# Determinar el rango máximo del eje y
y_max <- ceiling(max(PIC_long$value, na.rm = TRUE))

# Crear el gráfico de puntos con líneas
ggplot(PIC_long, aes(x = casa, y = log(value), color = variable, group = variable)) + 
# Ajustar la posición de los puntos
  geom_boxplot(aes(color = variable), position = position_dodge(width = 0.7), outlier.shape = NA, width = 0.5) +
  geom_line(position = position_dodge(width = 0.5)) +  # Añadir líneas que unen los puntos
  labs(title = "Tasas captura KAW",
       x = "Casa",
       y = "Tasa",
       color = "Variable") +  # Etiqueta la leyenda correctamente
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  scale_y_continuous(breaks = seq(0, y_max, by = 5), limits = c(0, y_max))  # Ajustar el eje y de 5 en 5 y establecer los límites





# Crear el gráfico de barras
ggplot(KAW_long, aes(x = casa, y = value, fill = variable)) + 
  geom_bar(stat = "identity", position = position_dodge()) +  # Coloca las barras una al lado de la otra
  labs(title = "Tasas captura KAW",
       x = "Casa",
       y = "Tasa",
       fill = "Variable") +  # Etiqueta la leyenda correctamente
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas del eje x
  scale_y_continuous(breaks = seq(0, y_max + 10, by = 5), limits = c(0, y_max))  # Ajustar el eje y de 5 en 5 y establecer los límites










# Calculo abundancia relativa ajustada a 100 noches trampas
# (Suma eventos / 92(noches trampas) * numero casas Sitio) * 100

# Crear dataframes para cada variable
Abun_rel_KAW <- data.frame(Sitio = "KAW", Abundancia_Relativa = (sum(KAW$Numero_de_eventos) / (92*25))*100)
Abun_rel_PIC <- data.frame(Sitio = "PIC", Abundancia_Relativa = (sum(PIC$Numero_de_eventos) / (92*31))*100)
Abun_rel_LLA <- data.frame(Sitio = "LLA", Abundancia_Relativa = (sum(LLA$Numero_de_eventos) / (92*29))*100)

# Combinar los dataframes en uno solo
abun_rel_monito <- rbind(Abun_rel_KAW, Abun_rel_PIC, Abun_rel_LLA)
abun_rel_rata <- rbind(Abun_rel_KAW, Abun_rel_PIC, Abun_rel_LLA)


