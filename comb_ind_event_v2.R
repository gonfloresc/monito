# install.packages("readr") ## ya instalado
library(readr)
library(dplyr)
library(stringi)

#  crea una lista de los .csv a importar
## !! modificar aqui la ruta donde se encuentran los archivos
files <- list.files(path = "test_csv/csv originales/v2024_04_18/2022/", pattern = "*.csv", full.names = TRUE)

### Eliminar columna $DeleteFlag en archivos .csv y equipara el numero de columnas

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
  house_name <- paste("Casa", house_number, sep = "")
  
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

#extraer simbolos y letras de temperatura
for (i in seq_len(nrow(db_event))) {
  temp_str <- db_event$Temperatura[i]
  db_event$Temperatura[i] <- stri_extract_first_regex(temp_str, "^([0-9]+)")
}


###
###   Filtrar db_evento por periodo de tiempo. Aqui establecer tiempo

db_event_flt_date <- db_event %>% 
  mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S")) %>% 
  filter(DateTime >= as.POSIXct("2022-06-21") & DateTime <= as.POSIXct("2022-09-21"))


###   Filtrar evento unico x spp
# Obtener todos los valores únicos de especie
especies_unicas <- unique(db_event_flt_date$Especie1)

# Crear una lista para almacenar los dataframes separados por especie
lista_spp <- list()

# Iterar sobre cada especie única
for (especie in especies_unicas) {
  # Filtrar el dataframe para la especie actual
  df_spp <- filter(db_event_flt_date, Especie1 == especie)
  # Guardar el dataframe filtrado en la lista con el nombre de la especie
  lista_spp[[especie]] <- df_spp
}


lista_spp_uni <- list()
evento <- numeric()
df <- data.frame()
df_nuevo <- data.frame()

# Revisión evento independiente por spp
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
      } 
      if (j == 1 && casa != ult_casa) {
        df_nuevo <- rbind(df_nuevo, df[j,])
        evento <- df$DateTime[j]
        ult_casa <- df$Casa[j]
      }
      df$DateTime <- as.POSIXct(df$DateTime)
      #diferencia_tiempo <- numeric()
      # Calcular la diferencia de tiempo entre la fila 1 y evento
      diferencia_tiempo <- difftime(df$DateTime[j], evento, units = "mins")
      diferencia_tiempo <- as.numeric(diferencia_tiempo)
      if (diferencia_tiempo >= 60) {
        df_nuevo <- rbind(df_nuevo, df[j,])
        evento <- df$DateTime[j]
        ult_casa <- df$Casa[j]
      }
    }
  }
  lista_spp_uni[[df_name]] <- df_nuevo
  ult_casa <- NA
  write.csv(lista_spp_uni[[df_name]], file = paste0("Resultados/Eventos_unicos_spp/2022/", df_name, ".csv"), row.names = FALSE)
}

### Creacion output.csv con resumen 









































#################
# Funcion evento unico
filtrar_por_tiempo <- function(df) {
  # Ordenar el dataframe por la columna DateTime para asegurar que esté ordenado cronológicamente
  df <- df[order(df$DateTime), ]
  
  # Calcular la diferencia de tiempo en horas entre cada fila y la fila anterior
  diferencia_tiempo <- difftime(df$DateTime, lag(df$DateTime), units = "hours")
  
  # Filtrar las filas donde la diferencia de tiempo es mayor a 1 hora
  df_filtrado <- df[diferencia_tiempo > 1, ]
  
  return(df_filtrado)
}
#################


#######################
db_event_flt_ind <- db_event_flt_date %>% arrange(Casa, DateTime, Especie1)

db_event_flt_ind_nuevo <- data.frame()

# Calcular la diferencia de tiempo dentro de cada grupo de "Casa"
for (i in 1:nrow(db_event_flt_ind)) {
  if (i == 1 || db_event_flt_ind$Casa[i] != db_event_flt_ind$Casa[i-1]) {
    db_event_flt_ind_nuevo[i] <- db_event_flt_ind[i]
  } else if (is.na(db_event_flt_ind$Especie1[i - 1]) || db_event_flt_ind$Especie1[i] != db_event_flt_ind$Especie1[i-1]) {
    db_event_flt_ind_nuevo[i] <- db_event_flt_ind[i]
  } else if (difftime(DateTime[i], lag(DateTime[i-1]), units = "hours") >= 1){
    db_event_flt_ind_nuevo[i] <- db_event_flt_ind[i]
  }
}

for (casa in unique(db_event_flt_ind)) {
  for (i in seq_along(nrow(db_event_flt_ind))) {

  }
}

# Crear un nuevo dataframe con el nombre de la columna "Especie1"
nuevo_dataframe <- data.frame(Especie1 = filas_true$Casa, DateTime = filas_true$DateTime)












################################################3
####  Creacion de dataframes por casa

house_dataframes <- list()

##  Creacion listas con dataframes por Casas
for (i in seq_len(nrow(db_event))) {
  if (is.na(db_event$RelativePath[i])) {
    # Extraer el dato de numero de casa
    house_number <- extract_house_number(db_event$RootFolder[i])
  } else {
    # Si no es NA, extraer el dato de numero de casa de RelativePath
    house_number <- extract_house_number(db_event$RelativePath[i])
  }
  
  house_name <- paste("Casa", house_number, sep = "")
  
  #Agregar dato a columna casa
  db_event$Casa[i] <- house_name
  
  ### crea el dataframe correspondiente a la casa solo con las columnas necesarias
  selected_columns <- db_event[i, c("Evento_total", "Evento_casa", "Casa", "File", "DateTime", "Temperatura", "Especie1", "Numero_Especies1", "Cavidad_occupancy", "Especie2", "Numero_Especies2", "Especie3", "Interaccion" )] #Modificar aqui las columnas necesarias
  #agrega columnas nuevas: evento total, evento casa, casa
    
  if (is.data.frame(house_dataframes[[house_name]])) {
    # Si ya existe, agregar las columnas seleccionadas usando rbind()
    house_dataframes[[house_name]] <- rbind(house_dataframes[[house_name]], selected_columns)
  } else {
    # Si no existe, simplemente asignar las columnas seleccionadas
    house_dataframes[[house_name]] <- selected_columns
  }
}

##  Extraccion de dataframe Casas ## opcional!!
for (i in seq_along(house_dataframes)) {
  # Obtener el nombre del dataframe
  df_name <- names(house_dataframes)[i]
  df_file <- house_dataframes[[i]]
  assign(df_name, df_file)
}

### Calcular eventos independientes
# Crea la lista y dataframes que contendran los eventos independientes
df_eventos_ind <- data.frame()
df_eventos_ind <- data.frame(matrix(ncol = ncol(house_dataframes[[1]]), nrow = 0))
names(df_eventos_ind) <- names(house_dataframes[[1]])
tiempo_anterior <- NA
conteo_eventos <- 1
conteo_eventos_casa <- 1

# Calculo eventos independientes
for (i in seq_along(house_dataframes)) {
  house <- house_dataframes[[i]]
  
  tiempo_anterior <- NA
  conteo_eventos_casa <- 1
  
  for (j in seq_len(nrow(house))) {
    file <- house[j, ]
    
    #transformar formato tiempo y calcular diferencia de tiempo con valor anterior
    fecha_hora <- file$DateTime
    fecha_hora <- as.POSIXct(fecha_hora, format = "%d-%m-%Y %H:%M")
    
    if (!is.na(tiempo_anterior)) {
      diferencia_tiempo <- difftime(fecha_hora, tiempo_anterior, units = "mins")
      
      if (diferencia_tiempo >= 30 | file$Especie1 != house$Especie1[j - 1]) {
        df_eventos_ind <- rbind(df_eventos_ind, file)
        df_eventos_ind$Evento_casa[conteo_eventos] <- conteo_eventos_casa
        
        #Conteo eventos casa
        conteo_eventos_casa <- conteo_eventos_casa + 1
        conteo_eventos <- conteo_eventos + 1
      }
    } else {
      df_eventos_ind <- rbind(df_eventos_ind, file)
      df_eventos_ind$Evento_casa[conteo_eventos] <- conteo_eventos_casa
      
      #conteo eventos casa
      conteo_eventos_casa <- conteo_eventos_casa + 1
      conteo_eventos <- conteo_eventos + 1
    }

    # Actualizar la fecha y hora anterior para la próxima iteración. Conteo evento casa
    tiempo_anterior <- fecha_hora
  }
}

#conteo eventos totales
rownames(df_eventos_ind) <- NULL
df_eventos_ind$Evento_total <- 1:nrow(df_eventos_ind)

#extraer simbolos y letras de temperatura
for (i in seq_len(nrow(df_eventos_ind))) {
  temp_str <- df_eventos_ind$Temperatura[i]
  df_eventos_ind$Temperatura[i] <- stri_extract_first_regex(temp_str, "^([0-9]+)")
}

