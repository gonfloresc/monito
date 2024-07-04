####   Historial de captura 24h
library(dplyr)
library(unmarked)

## Import data:
data_casas <- read.csv("Resultados/Casas_totales_2023.csv")
data_mon_KAW <- read.csv("Resultados/Historial de captura/hist_monito_KAW.csv", sep = ";")
data_mon_KOD <- read.csv("Resultados/Historial de captura/hist_monito_KOD.csv", sep = ";")
data_mon_LLA <- read.csv("Resultados/Historial de captura/hist_monito_LLA.csv", sep = ";")
data_rat_KAW <- read.csv("Resultados/Historial de captura/hist_rata_KAW.csv", sep = ";")
data_rat_KOD <- read.csv("Resultados/Historial de captura/hist_rata_KOD.csv", sep = ";")

# Función para agrupar cada 5 días
agrupar_ocasion <- function(df, periodos) {
  n <- ncol(df)
  semanas <- ceiling(seq_along(1:n) / periodos)
  resultado <- t(sapply(unique(semanas), function(i) {
    periodo <- df[, semanas == i]
    # Si todas las filas del periodo son NA, retorna NA
    if (all(is.na(periodo))) {
      return(rep(NA, nrow(df)))
    } else {
      return(as.integer(rowSums(periodo, na.rm = TRUE) > 0))
    }
  }))
  return(as.data.frame(t(resultado)))
}

# Agrupar por periodos de 5 días
agrupados_monito <- agrupar_ocasion(data_mon_LLA, 5)
agrupados_rata <- agrupar_ocasion(data_rat_KOD, 5)

write.csv(agrupados_monito, "Resultados/Modelos de ocupacion/monito_ocasion5.csv")

umf <- unmarkedFrameOccu(y = agrupados_rata)

fm <- occuRN(~ 1 ~ 1, data = umf)
summary(fm)
