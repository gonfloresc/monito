# Instalar y cargar kableExtra
#install.packages("kableExtra")
#library(kableExtra)

# Import Datos
data_casas <- read.csv(file = "Resultados/Casas_totales_2023.csv")
data_monito <- read.csv(file = "Resultados/Eventos_unicos_spp/2023_24h/Monito del monte_24h.csv", sep = ",")
data_rata <- read.csv(file = "Resultados/Eventos_unicos_spp/2023_24h/Rata negra_24h.csv", sep = ",")
data_nochestrampa <- read.delim("Resultados/Resultado_noches_trampa.csv", sep = ";")

# Contar el número de eventos por casa
eventos_por_casa <- as.data.frame(table(data_monito$Casa))
eventos_por_casa_rata <- as.data.frame(table(data_rata$Casa))

# Renombrar las columnas
names(eventos_por_casa) <- c("casa", "n_eventos")
names(eventos_por_casa_rata) <- c("casa", "n_eventos")

eventos_por_casa$casa <- as.numeric(sub("casa", "", eventos_por_casa$casa))
eventos_por_casa_rata$casa <- as.numeric(sub("casa", "", eventos_por_casa_rata$casa))

### Taza de captura por casa
taza_captura <- data.frame(matrix(nrow = nrow(data_casas), ncol = 0))
taza_captura$casa <- data_casas$x
taza_captura$casa <- gsub("casa", "", taza_captura$casa)
# merge
taza_captura <- merge(taza_captura, eventos_por_casa_rata[, c("casa", "n_eventos")], by = "casa", all.x = TRUE)
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
  
  



### Taza de captura por sitio

KAW <- subset(eventos_por_casa, Numero_Casa >= 1 & Numero_Casa <= 40)
PIC <- subset(eventos_por_casa, Numero_Casa >= 81 & Numero_Casa <= 120)
LLA <- subset(eventos_por_casa, Numero_Casa >= 121 & Numero_Casa <= 159)

KAW <- subset(eventos_por_casa_rata, Numero_Casa >= 1 & Numero_Casa <= 40)
PIC <- subset(eventos_por_casa_rata, Numero_Casa >= 81 & Numero_Casa <= 120)
LLA <- subset(eventos_por_casa_rata, Numero_Casa >= 121 & Numero_Casa <= 159)

# Calculo abundancia relativa ajustada a 100 noches trampas
# (Suma eventos / 92(noches trampas) * numero casas Sitio) * 100

# Crear dataframes para cada variable
Abun_rel_KAW <- data.frame(Sitio = "KAW", Abundancia_Relativa = (sum(KAW$Numero_de_eventos) / (92*25))*100)
Abun_rel_PIC <- data.frame(Sitio = "PIC", Abundancia_Relativa = (sum(PIC$Numero_de_eventos) / (92*31))*100)
Abun_rel_LLA <- data.frame(Sitio = "LLA", Abundancia_Relativa = (sum(LLA$Numero_de_eventos) / (92*29))*100)

# Combinar los dataframes en uno solo
abun_rel_monito <- rbind(Abun_rel_KAW, Abun_rel_PIC, Abun_rel_LLA)
abun_rel_rata <- rbind(Abun_rel_KAW, Abun_rel_PIC, Abun_rel_LLA)

