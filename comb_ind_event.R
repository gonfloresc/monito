# install.packages("readr") ## ya instalado
library(readr)
library(dplyr)
library(stringi)

#  crea una lista de los .csv a importar
## !! modificar aqui la ruta donde se encuentran los archivos
files <- list.files(path = "test_csv/csv originales/", pattern = "*.csv", full.names = TRUE)

# importar y combinar los .csv en uno solo "csv_database"

csv_database <- data.frame()
data1 <- data.frame()

for (file in files) {
  datos_temporales <- read_csv2(file)
  csv_database <- rbind(csv_database, datos_temporales)
}

##
#  filtrar la database para obtener solo los eventos de animales

db_event <- csv_database %>%
  filter(!is.na(Especie1))

# Estructura Datos. Agregar columnas "evento total" "evento casa" "casa"
ev_total <- seq_len(nrow(db_event))
ev_casa <- seq_len(nrow(db_event))
casa <- seq_len(nrow(db_event))

db_event <- cbind(Evento_total = ev_total, Evento_casa = ev_casa, Casa = casa, db_event)

####  Creacion de dataframes por casa
extract_house_number <- function(casa) {
  # Buscar el número de casa en el texto usando expresiones regulares
  house <- gsub(".*Casa (\\d+).*", "\\1", casa)
  # Convertir a número entero
  as.integer(house)
}

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

