# install.packages("readr") ## ya instalado
library(readr) 
library(dplyr)
library(stringi)
library(lubridate)

#  crea una lista de los .csv a importar
## !! modificar aqui la ruta donde se encuentran los archivos
files <- list.files(path = "csv_revisiones/v2024_05_14/2023/", pattern = "*.csv", full.names = TRUE)

### Eliminar columna $DeleteFlag y equipara el numero de columnas en archivos .csv

eliminar_deleteflag <- function(file_path) {
  # Leer el archivo CSV con delimitador ","
  datos <- read_delim(file_path, delim = ",", col_names = TRUE)
  if (ncol(datos) == 1) {
  # Leer el archivo CSV con delimitador ";"
    datos <- read_delim(file_path, delim = ";", col_names = TRUE)
  }
  
  # Verificar si la columna DeleteFlag existe
  if ("DeleteFlag" %in% colnames(datos)) {
    # Eliminar la columna DeleteFlag
    datos <- subset(datos, select = -DeleteFlag)
    
    # Escribir los datos modificados de vuelta al archivo CSV
    cat("Se ha eliminado la columna DeleteFlag en el archivo:", file_path, "\n")
  } else {
    cat("El archivo", file_path, "no tiene la columna DeleteFlag\n")
  }
  
  # Verificar si existen columnas date y time separadas
  if ("Date" %in% colnames(datos) && "Time" %in% colnames(datos)) {
    
    # Obtener la posición de la columna Date
    pos_date <- which(colnames(datos) == "Date")
    
    # Combina las columnas date y time en una nueva columna DateTime
    datos$DateTime <- paste(datos$Date, datos$Time, sep = " ")
    
    # Insertar la columna DateTime en la misma posición que Date
    datos <- cbind(datos[, 1:(pos_date - 1)], DateTime = datos$DateTime, datos[, (pos_date + 1):ncol(datos)])
    datos <- datos[, -ncol(datos)]
    
    # Elimina las columnas date y time
    datos <- subset(datos, select = -c(Time))
  }
  # Verificar si la última columna no es $Revisada
  if (length(colnames(datos)) > 0 && tail(colnames(datos), 1) != "Revisada") {
    # Eliminar la última columna
    datos <- datos[, -ncol(datos)]
  }
  write_csv(datos, file_path)
}


# Aplicar la función a cada archivo CSV en la lista
lapply(files, eliminar_deleteflag)

###### importar y combinar los .csv en uno solo "csv_database"

csv_database <- data.frame()

extract_house_number <- function(casa) {
  # Buscar el número de casa en el texto usando expresiones regulares
  house <- gsub(".*[Cc]asa(\\d+).*", "\\1", casa)
  # Convertir a número entero
  if (is.na(as.integer(house))) {
    house <- gsub(".*[Cc]asa_(\\d+).*", "\\1", casa)
  }
  if (is.na(as.integer(house))) {
    house <- gsub(".*[Cc]asa (\\d+).*", "\\1", casa)
  }
  as.integer(house)
}

casa <- seq_len(nrow(csv_database))

for (file in files) {
  house_number <- extract_house_number(file)
  house_name <- paste("casa", house_number, sep = "")
  
  datos_temporales <- read_delim(file, delim = ",", locale = locale(decimal_mark = ",", grouping_mark = "."))
  datos_temporales$Casa <- house_name
  if (ncol(datos_temporales) == 1) {
    # Leer el archivo CSV con delimitador ";"
    datos_temporales <- read_delim(file, delim = ";", col_names = TRUE)
  }
  csv_database <- rbind(csv_database, datos_temporales)
}

## creacion db_event database

# Estructura Datos. Agregar columnas "evento total" "evento casa"
db_event <- data.frame()
db_event <- csv_database
ev_total <- seq_len(nrow(db_event))
ev_casa <- seq_len(nrow(db_event))
db_event <- cbind(Evento_total = ev_total, Evento_casa = ev_casa, Casa = csv_database$Casa, db_event)
# Eliminar la última columna
db_event <- subset(db_event, select = -ncol(db_event))

## Extraer Total Casas
total_casas <- unique(db_event$Casa)
numeros <- as.numeric(gsub("casa", "", total_casas))
indice_orden <- order(numeros)
total_casas <- total_casas[indice_orden]
write.csv(total_casas, "Resultados/Casas_totales_2023.csv", row.names = FALSE)

#extraer simbolos y letras de temperatura
for (i in seq_len(nrow(db_event))) {
  temp_str <- db_event$Temperatura[i]
  db_event$Temperatura[i] <- stri_extract_first_regex(temp_str, "^([0-9]+)")
}


###
###   Filtrar db_event por periodo de tiempo. 
db_event$DateTime <- ifelse(!is.na(as.POSIXct(as.integer(db_event$DateTime), origin = "1970-01-01")), 
                            format(as.POSIXct(as.integer(db_event$DateTime), origin = "1970-01-01"), 
                                   "%Y-%m-%d %H:%M:%S"), db_event$DateTime)
db_event_flt_date <- db_event %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M")) %>% 
  filter(DateTime >= as.POSIXct("2023-08-01") & DateTime <= as.POSIXct("2023-10-24")) ## Aqui establecer periodo

# Export registros totales para tiempo de muestreo
write.csv(db_event_flt_date, "Resultados/Periodos/Agosto-Septiembre-Octubre/registros_p2_2023.csv", row.names = FALSE)


###   Filtrar evento unico x spp
# Obtener todos los valores únicos de especie (y limpieza spp gato guiña a gato guina)
especies_unicas <- unique(db_event_flt_date$Especie1)
especies_unicas <- stri_replace_all_fixed(especies_unicas, "gato gui\xf1a", "gato guina")
especies_unicas <- stri_replace_all_fixed(especies_unicas, "Jabal\xed", "jabali")

# Crear una lista para almacenar los dataframes separados por especie
lista_spp <- list()

# Iterar sobre cada especie única
for (especie in especies_unicas) {
  # Filtrar el dataframe para la especie actual
  df_spp <- filter(db_event_flt_date, Especie1 == especie)
  # Guardar el dataframe filtrado en la lista con el nombre de la especie
  lista_spp[[especie]] <- df_spp
}

#Elimina df NA
#lista_spp[[1]] <- NULL

lista_spp_uni <- list()
evento <- numeric()
df <- data.frame()
df_nuevo <- data.frame()
ult_casa <- NA
conteo_eventos <- 1

conteo_eventos_casa <- 1

### Revisión evento independiente por spp. Importante verificar .csv de salida y rango de tiempo evento unico!!
for (i in seq_along(lista_spp)) {
  df <- lista_spp[[i]]
  casas_unicas <- unique(df$Casa)
  df_name <- names(lista_spp)[i]
  # Crear un dataframe vacío con las mismas columnas que df
  df_nuevo <- df[0, ]
  
  for (casa in casas_unicas) {
    df <- lista_spp[[i]]
    df <- filter(df, Casa == casa)
    df <- df %>% arrange(DateTime)
    
    for (j in 1:nrow(df)) {
      if (j == 1 && is.na(ult_casa)) {
        df_nuevo[j,] <- df[j,]
        evento <- df$DateTime[j]
        ult_casa <- df$Casa[j]
        
        df_nuevo$Evento_casa[conteo_eventos] <- conteo_eventos_casa
        conteo_eventos_casa <- conteo_eventos_casa + 1
        conteo_eventos <- conteo_eventos + 1
      } 
      if (j == 1 && casa != ult_casa) {
        df_nuevo <- rbind(df_nuevo, df[j,])
        evento <- df$DateTime[j]
        ult_casa <- df$Casa[j]
        
        df_nuevo$Evento_casa[conteo_eventos] <- conteo_eventos_casa
        conteo_eventos_casa <- conteo_eventos_casa + 1
        conteo_eventos <- conteo_eventos + 1
      }
      df$DateTime <- as.POSIXct(df$DateTime)
      # Calcular la diferencia de tiempo entre la fila 1 y evento
      diferencia_tiempo <- difftime(df$DateTime[j], evento, units = "mins")
      diferencia_tiempo <- as.numeric(diferencia_tiempo)
      if (diferencia_tiempo >= 1440) {        ###### <----Aqui establecer rango evento único (mins)
        df_nuevo <- rbind(df_nuevo, df[j,])
        evento <- df$DateTime[j]
        ult_casa <- df$Casa[j]
        
        df_nuevo$Evento_casa[conteo_eventos] <- conteo_eventos_casa
        conteo_eventos_casa <- conteo_eventos_casa + 1
        conteo_eventos <- conteo_eventos + 1
      }
    }
    conteo_eventos_casa <- 1
    
  }
  if (nrow(df_nuevo != 0)) {
    df_nuevo$Evento_total <- 1:nrow(df_nuevo)
  }
  conteo_eventos <- 1
  conteo_eventos_casa <- 1
  
  lista_spp_uni[[df_name]] <- df_nuevo
  ult_casa <- NA
  
  ### .csv de salida
  write.csv(lista_spp_uni[[df_name]], file = paste0("Resultados/Periodos/Agosto-Septiembre-Octubre/Eventos_unicos_spp/2023_p2_24h/", df_name, ".csv"), row.names = FALSE)
}



