# Cargar las librerías necesarias
library(dplyr)
library(tidyr)

## Edicion hist captura

hist_mon <- read.csv("Resultados/Historial de captura/hist_monito_csv.csv", sep = ",")
hist_rat <- read.csv("Resultados/Historial de captura/hist_rata_csv.csv", sep =";")

# Eliminar la primera columna
hist_mon <- hist_mon[, -1]

# Obtener el número de columnas originales
n_occasions <- ncol(hist_mon)

# Crear un nuevo dataframe para almacenar los datos agrupados
grouped_hist_mon <- data.frame(matrix(ncol = ceiling(n_occasions / 5), nrow = nrow(hist_mon)))
colnames(grouped_hist_mon) <- paste0("o", 1:ncol(grouped_hist_mon))

# Agrupar las columnas cada 7 días y determinar la presencia
for (i in 1:ncol(grouped_hist_mon)) {
  start_col <- (i - 1) * 5 + 1
  end_col <- min(i * 5, n_occasions)
  grouped_hist_mon[, i] <- apply(hist_mon[, start_col:end_col], 1, function(x) ifelse(any(x == 1, na.rm = TRUE), 1, 0))
}

write.csv(grouped_hist_mon, "Resultados/Historial de captura/hist_monito_g5.csv", row.names = FALSE)
write.csv(hist_rat, "Resultados/Historial de captura/hist_rata_csv.csv")
