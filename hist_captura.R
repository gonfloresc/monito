####   Historial de captura 24h
#install.packages("camtrapR")
library(camtrapR)

## Import data:
data_casas <- read.csv("Resultados/Casas_totales_2023.csv")
data_mon <- read.csv("Resultados/Eventos_unicos_spp/2023_12h/Monito del monte_12h.csv")
data_rat <- read.csv("Resultados/Eventos_unicos_spp/2023_12h/Rata negra_12h.csv")
data_nochestrampa <- read.csv("Resultados/Resultado_noches_trampa.csv", sep = ";")

##  Creacion data.frame principal

# Sitio muestreo
# limpieza $casa
data_nochestrampa$casa<- as.numeric(data_nochestrampa$Casa.Anidera)
data_nochestrampa$Fecha_instalacion <- as.POSIXct(data_nochestrampa$Fecha_instalacion, format = "%d-%m-%Y") 
data_nochestrampa$Fecha_falla <- as.POSIXct(data_nochestrampa$Fecha_falla, format = "%d-%m-%Y")
data_nochestrampa$Fecha_arreglo <- as.POSIXct(data_nochestrampa$Fecha_arreglo, format = "%d-%m-%Y")
data_nochestrampa$Fecha_falla2 <- as.POSIXct(data_nochestrampa$Fecha_falla2, format = "%d-%m-%Y")
data_nochestrampa$Fecha_retiro <- as.POSIXct(data_nochestrampa$Fecha_retiro, format = "%d-%m-%Y")

hist_cap <- data.frame(casa = data_nochestrampa$casa)
hist_cap$casa <- gsub("casa", "", data_nochestrampa$casa)
hist_cap$casa <- as.numeric(hist_cap$casa)

# Separar la columna DateTime en Date y Time
data_mon$DateTime <- as.POSIXct(data_mon$DateTime, format = "%Y-%m-%d %H:%M")
data_mon$Date <- as.Date(data_mon$DateTime, format = "%Y-%m%-%d")  # Extrae solo la fecha
data_mon$Time <- format(data_mon$DateTime, "%H:%M")

data_mon$Casa <- gsub("casa", "", data_mon$Casa)
data_mon$Casa <- as.numeric(data_mon$Casa)

data_rat$DateTime <- as.POSIXct(data_rat$DateTime, format = "%Y-%m-%d %H:%M")
data_rat$Date <- as.Date(data_rat$DateTime, format = "%Y-%m%-%d")  # Extrae solo la fecha
data_rat$Time <- format(data_rat$DateTime, "%H:%M")

data_rat$Casa <- gsub("casa", "", data_rat$Casa)
data_rat$Casa <- as.numeric(data_rat$Casa)

# Se agrega "sitio" muestreo
data_mon$sitio <- ifelse(data_mon$Casa > 0 & data_mon$Casa <= 40, "KAW", 
                         ifelse(data_mon$Casa >= 41 & data_mon$Casa <= 120, 
                                "KOD", "LLA"))

data_rat$sitio <- ifelse(data_rat$Casa > 0 & data_rat$Casa <= 40, "KAW", 
                         ifelse(data_rat$Casa >= 41 & data_rat$Casa <= 120, 
                                "KOD", "LLA"))

# Crear una lista con los nombres de las columnas
columnas <- paste0("o", 1:175)

# Crear un DataFrame vacío con las columnas especificadas
df <- data.frame(matrix(ncol = 175, nrow = 0))
colnames(df) <- columnas

# Definir las fechas de inicio y fin
fecha_inicio <- as.Date("2023-05-02 -04")
fecha_fin <- as.Date("2023-10-24 -04")

# Crear el vector de fechas
fechas <- seq.Date(from = fecha_inicio, to = fecha_fin, by = "day")
fechas <- as.POSIXct(fechas)

# Valor 1 por presencia de evento en ocasion de observacion "o"
for (i in 1:nrow(hist_cap)) {
  data_casa <- data_mon[data_mon$Casa == hist_cap$casa[i],]
  for (j in 0:176) {
    no_col <- paste0("o", j)              
    df[i, no_col] <- ifelse(any(data_casa$Date == fechas[j]), 1, NA)
  }
}

for (i in 1:nrow(hist_cap)) {
  data_casa <- data_rat[data_rat$Casa == hist_cap$casa[i],]
  for (j in 0:176) {
    no_col <- paste0("o", j)              
    df[i, no_col] <- ifelse(any(data_casa$Date == fechas[j]), 1, NA)
  }
}

# Considerar funcionamiento camara
for (i in 1:nrow(hist_cap)) {
  for (j in 1:176) {
    no_col <- paste0("o", j)
    if (is.na(df[i, no_col])) {
      if (fechas[j] >= data_nochestrampa$Fecha_instalacion[i] & 
        fechas[j] <= data_nochestrampa$Fecha_retiro[i]) {
        if (is.na(data_nochestrampa$Fecha_falla[i]) & 
            is.na(data_nochestrampa$Fecha_falla2[i])) {
          df[i, no_col] <- 0
        }
        if (!is.na(data_nochestrampa$Fecha_falla[i]) & 
            fechas[j] <= data_nochestrampa$Fecha_falla[i]) {
          df[i, no_col] <- 0
        }
        if (!is.na(data_nochestrampa$Fecha_arreglo[i]) &
            fechas[j] >= data_nochestrampa$Fecha_arreglo[i]) {
          df[i, no_col] <- 0
        }
        if (!is.na(data_nochestrampa$Fecha_falla2[i]) & 
            fechas[j] <= data_nochestrampa$Fecha_falla2[i]) {
          df[i, no_col] <- 0
        }
      }
    }  
  }
}

hist_cap <- cbind(hist_cap, df)
orden_indices <- order(hist_cap$casa)
hist_cap <- hist_cap[orden_indices, , drop = FALSE]
rownames(hist_cap) <- NULL
write.csv(hist_cap, "Resultados/Historial de captura/hist_rata.csv", row.names = FALSE)

