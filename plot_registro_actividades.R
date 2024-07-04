#install.packages("occupancy")
library(occupancy)

data_monito <- read.csv("Resultados/Eventos_unicos_spp/2023_24h/Monito del monte_24h.csv")
atributos_veg <- read.csv("Transecto Vegetacional/Datos terreno 2023 - Covariables_CSV.csv")
data_rata <- read.csv("Resultados/Eventos_unicos_spp/2023_24h/Rata negra_24h.csv")

# Supongamos que tienes tus datos en un data frame llamado "datos"
# Cargar el paquete
library(unmarked)

data_monito$Numero_Casa <- as.numeric(sub("casa", "", data_monito$Casa))
data_rata$Numero_Casa <- as.numeric(sub("casa", "", data_rata$Casa))
data_rata <- subset(data_rata, !grepl("\\.MP4", File))
data_rata <- subset(data_rata, !grepl("\\.AVI", File))

##  Separacion por sitio
KAW <- subset(data_monito, Numero_Casa >= 1 & Numero_Casa <= 40)
PIC <- subset(data_monito, Numero_Casa >= 81 & Numero_Casa <= 120)
LLA <- subset(data_monito, Numero_Casa >= 121 & Numero_Casa <= 159)

KAW <- subset(data_rata, Numero_Casa >= 1 & Numero_Casa <= 40)
PIC <- subset(data_rata, Numero_Casa >= 81 & Numero_Casa <= 120)
LLA <- subset(data_rata, Numero_Casa >= 121 & Numero_Casa <= 159)

####  Graficos eventos
# Convertir la columna DateTime a tipo Date (solo fecha)
KAW$Fecha <- as.Date(KAW$DateTime)
LLA$Fecha <- as.Date(LLA$DateTime)
PIC$Fecha <- as.Date(PIC$DateTime)

# Agrupar por día y contar eventos en cada día
eventos_por_dia <- table(PIC$Fecha)
eventos_por_dia <- table(LLA$Fecha)
eventos_por_dia <- table(KAW$Fecha)
eventos_por_dia_rata <- table(PIC$Fecha)
eventos_por_dia_rata <- table(KAW$Fecha)
eventos_por_dia_rata <- table(LLA$Fecha)

# Crear un dataframe para el gráfico de puntos
df_eventos <- data.frame(Fecha = as.Date(names(eventos_por_dia)),
                         Num_Eventos = as.numeric(eventos_por_dia))
df_eventos_rata <- data.frame(Fecha = as.Date(names(eventos_por_dia_rata)),
                              Num_Eventos = as.numeric(eventos_por_dia_rata))

# Normalizar eventos al rango [0, 1]
df_eventos$Num_Eventos_rad <- df_eventos$Num_Eventos / max(df_eventos_rata$Num_Eventos)
df_eventos_rata$Num_Eventos_rad <- df_eventos_rata$Num_Eventos / max(df_eventos_rata$Num_Eventos)
# Convertir a radianes
df_eventos$Num_Eventos_rad <- df_eventos$Num_Eventos_rad * 2 * pi
df_eventos_rata$Num_Eventos_rad <- df_eventos_rata$Num_Eventos_rad * 2 * pi

overlapPlot(df_eventos$Num_Eventos_rad, df_eventos_rata$Num_Eventos_rad, adjust = 0.8,
            main = "Overlap patrón actividad Monito-Rata", type = "histogram",  colors = c(1,4), ylim = c(0, 0.5))

ggplot(data, aes(x = fecha, y = eventos_radianes)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name = "Eventos (radianes)", limits = c(0, 2*pi), breaks = seq(0, 2*pi, by = pi/2), labels = c("0", expression(pi/2), expression(pi), expression(3*pi/2), expression(2*pi))) +
  scale_x_date(name = "Fecha") +
  theme_minimal() +
  ggtitle("Número de eventos en radianes a lo largo del tiempo")

# Graficar el gráfico de puntos
plot(df_eventos$Fecha, df_eventos$Num_Eventos, 
     type = "p", pch = 16, col = "blue",
     xlab = "Fecha", ylab = "Número de eventos",
     main = "Distribución de eventos de monito del monte por día KAW",
     ylim = c(1, 12), xlim = as.Date(c("2023-05-02", "2023-10-24")))
plot(df_eventos_rata$Fecha, df_eventos_rata$Num_Eventos, 
     type = "p", pch = 16, col = "blue",
     xlab = "Fecha", ylab = "Número de eventos",
     main = "Distribución de eventos de Rata Negra por día KAW",
     ylim = c(1, 12), xlim = as.Date(c("2023-05-02", "2023-10-24")))

# Calcular la densidad de kernel
cantidad_eventos <- df_eventos$Num_Eventos
densidad <- density(cantidad_eventos)

# Graficar la densidad estimada
plot(densidad, main = "Estimación de Densidad de Kernel de Cantidad de Eventos por Casa", 
     xlab = "Cantidad de Eventos", ylab = "Densidad")
