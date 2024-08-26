# Cargar las librerías necesarias
library(dplyr)
library(lubridate)
library(ggplot2)

# import data
data_mon <- read.csv("Resultados/Eventos_unicos_spp/2023_12h/Monito del monte_12h.csv")
data_rat <- read.csv("Resultados/Eventos_unicos_spp/2023_12h/Rata negra_12h.csv")
data_noches <- read.csv("Resultados/Resultado_noches_trampa.csv", sep = ";")
tasa_monito <- read.csv("Resultados/Tasa Captura/cr_monito_24h.csv")
tasa_rata <- read.csv("Resultados/Tasa Captura/cr_rata_24h.csv")

# Convertir la columna de fecha_hora a formato POSIXct si no lo está ya
data_mon$DateTime <- as.POSIXct(data_mon$DateTime, format="%Y-%m-%d %H:%M:%S")
data_rat$DateTime <- as.POSIXct(data_rat$DateTime, format="%Y-%m-%d %H:%M:%S")
data_mon$Casa <- as.numeric(sub("casa", "", data_mon$Casa))
data_rat$Casa <- as.numeric(sub("casa", "", data_rat$Casa))

# Agregar una columna para el mes y año
data_mon <- data_mon %>%
  mutate(mes = as.Date(floor_date(DateTime, "month")))
data_rat <- data_rat %>%
  mutate(mes = as.Date(floor_date(DateTime, "month")))

# Crear una nueva columna para el sitio basado en las casas
data_mon <- data_mon %>%
  mutate(sitio = case_when(
    Casa >= 0 & Casa <= 40 ~ "KAW",
    Casa >= 41 & Casa <= 120 ~ "KOD",
    Casa >= 121 & Casa <= 159 ~ "LLA"
  ))
data_rat <- data_rat %>%
  mutate(sitio = case_when(
    Casa >= 0 & Casa <= 40 ~ "KAW",
    Casa >= 41 & Casa <= 120 ~ "KOD",
    Casa >= 121 & Casa <= 159 ~ "LLA"
  ))

tasa_monito <- tasa_monito %>% 
  mutate(sitio = case_when(
    casa >= 0 & casa <= 40 ~ "KAW",
    casa >= 41 & casa <= 120 ~ "KOD",
    casa >= 121 & casa <= 159 ~ "LLA"
  ))

tasa_rata <- tasa_rata %>% 
  mutate(sitio = case_when(
    casa >= 0 & casa <= 40 ~ "KAW",
    casa >= 41 & casa <= 120 ~ "KOD",
    casa >= 121 & casa <= 159 ~ "LLA"
  ))

# Asegurarse de que Casa sea un factor
data_mon$Casa <- as.factor(data_mon$Casa)
data_rat$Casa <- as.factor(data_rat$Casa)

# Agrupar los tos por Casa y Month y contar el número de eventos
df_summary <- data_mon %>%
  group_by(Casa, mes, sitio) %>%
  summarise(Eventos = n(), .groups = 'drop')
# Crear un dataframe con todas las combinaciones únicas de Casa y Month
all_combinations <- expand.grid(Casa = unique(data_noches$Casa.Anidera), mes = unique(data_mon$mes))
df_summary_rat <- data_rat %>%
  group_by(Casa, mes, sitio) %>%
  summarise(Eventos = n(), .groups = 'drop')
# Crear un dataframe con todas las combinaciones únicas de Casa y Month
all_combinations_rat <- expand.grid(Casa = unique(data_noches$Casa.Anidera), mes = unique(data_rat$mes))

all_combinations <- all_combinations %>%
  mutate(sitio = case_when(
    Casa >= 0 & Casa <= 40 ~ "KAW",
    Casa >= 41 & Casa <= 120 ~ "KOD",
    Casa >= 121 & Casa <= 159 ~ "LLA"
  ))
all_combinations_rat <- all_combinations_rat %>%
  mutate(sitio = case_when(
    Casa >= 0 & Casa <= 40 ~ "KAW",
    Casa >= 41 & Casa <= 120 ~ "KOD",
    Casa >= 121 & Casa <= 159 ~ "LLA"
  ))

# Asegurarse de que Casa sea un factor
all_combinations$Casa <- as.factor(all_combinations$Casa)
all_combinations_rat$Casa <- as.factor(all_combinations_rat$Casa)


# Contar los eventos únicos por mes y sitio, manteniendo la fecha_hora
eventos_por_mes <- data_mon %>%
  group_by(sitio, mes) %>%
  summarise(eventos_unicos = n_distinct(paste(DateTime)))
eventos_por_mes$cr <- eventos_por_mes$eventos_unicos / (30*30)*100

eventos_por_mes_rat <- data_rat %>%
  group_by(sitio, mes) %>%
  summarise(eventos_unicos = n_distinct(paste(DateTime)))
eventos_por_mes_rat$cr <- eventos_por_mes_rat$eventos_unicos / (30*30)*100

# Graficar los resultados incluyendo la fecha en el eje x
ggplot(eventos_por_mes, aes(x = mes, y = cr, color = sitio)) +
  geom_point() +
  geom_line() +
  labs(title = "Eventos únicos por mes",
       x = "Mes",
       y = "Capture Rate") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Unir usando left_join para asegurarse de que todas las combinaciones estén presentes
df_full <- left_join(all_combinations, df_summary, by = c("Casa", "mes", "sitio"))
df_full_rat <- left_join(all_combinations_rat, df_summary_rat, by = c("Casa", "mes", "sitio"))

# Reemplazar NA en Eventos con 0 usando ifelse
df_full$Eventos <- ifelse(is.na(df_full$Eventos), 0, df_full$Eventos)
df_full_rat$Eventos <- ifelse(is.na(df_full_rat$Eventos), 0, df_full_rat$Eventos)

df_full$cr <- (df_full$Eventos / 30) * 100
df_full_rat$cr <- (df_full_rat$Eventos / 30) * 100



# Crear el gráfico eventos por mes/casa
ggplot(df_full_rat, aes(x = mes, y = cr, color = sitio)) +
  geom_point(position = position_jitter(width = 8, height = 0), size = 2.5) +
  scale_color_manual(values = c("KOD" = "#F4D03F", "LLA" = "#76D7C4", "KAW" = "#F1948A")) +
  labs(x = "Mes", y = "Tasa de Captura (12 hr)", title = "Tasa de Captura por Casa Rata", color = "Sitio") +
  scale_x_date(date_labels = "%B") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
# Crear el gráfico promedio tasa de capturas por sitio
# Combinamos los dos data frames en uno solo
tasa_monito$especie <- "Monito"
tasa_rata$especie <- "Rata"

tasa_total <- rbind(tasa_monito, tasa_rata)

tasa_promedio <- aggregate(cr_100 ~ sitio + especie, data = tasa_total, FUN = mean)

# Creamos el gráfico de barras
ggplot(tasa_promedio, aes(x = sitio, y = cr_100, fill = especie)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.5) +
  labs(x = "Sitio de estudio", y = "Tasa de captura", fill = "Especie") +
  theme_minimal()
