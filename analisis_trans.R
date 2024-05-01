## Análisis .csv Transecto Vegetativo

#importacion .csv
transecto_df <- read.csv("Transecto Vegetacional/Datos terreno 2023 - Formato CSV.csv")

##  Sitio 1
# Promedio DAP
mean_dap_sitio1 <- mean(subset(transecto_df, Sitio == 1)$DAP_prom)
mean_dap_sitio1_rounded <- round(mean_dap_sitio1, 2)

#Standard Deviation (sd)
desviacion_estandar <- sd(subset(transecto_df, Sitio == 1)$DAP_prom)
sd_sitio1_rounded <- round(desviacion_estandar, 2)

#Standard Error (se)
# Calcular el tamaño de la muestra
n <- length(subset(transecto_df, Sitio == 1)$N.)
# Calcular el error estándar
error_estandar <- desviacion_estandar / sqrt(n)
se_sitio1_rounded <- round(error_estandar, 2)

##  Sitio 2
# Promedio DAP
mean_dap_sitio2 <- mean(subset(transecto_df, Sitio == 2)$DAP_prom)
mean_dap_sitio2_rounded <- round(mean_dap_sitio2, 2)

#Standard Deviation (sd)
desviacion_estandar <- sd(subset(transecto_df, Sitio == 2)$DAP_prom)
sd_sitio2_rounded <- round(desviacion_estandar, 2)

#Standard Error (se)
# Calcular el tamaño de la muestra
n <- length(subset(transecto_df, Sitio == 2)$N.)
# Calcular el error estándar
error_estandar <- desviacion_estandar / sqrt(n)
se_sitio2_rounded <- round(error_estandar, 2)

##  Sitio 3
# Promedio DAP
mean_dap_sitio3 <- mean(subset(transecto_df, Sitio == 3)$DAP_prom)
mean_dap_sitio3_rounded <- round(mean_dap_sitio1, 2)

#Standard Deviation (sd)
desviacion_estandar <- sd(subset(transecto_df, Sitio == 3)$DAP_prom)
sd_sitio3_rounded <- round(desviacion_estandar, 2)

#Standard Error (se)
# Calcular el tamaño de la muestra
n <- length(subset(transecto_df, Sitio == 2)$N.)
# Calcular el error estándar
error_estandar <- desviacion_estandar / sqrt(n)
se_sitio3_rounded <- round(error_estandar, 2)

#graficos
promedios <- c(mean_dap_sitio1_rounded, mean_dap_sitio2_rounded, mean_dap_sitio3_rounded)
desviaciones <- c(sd_sitio1_rounded, sd_sitio2_rounded, sd_sitio3_rounded)
errores <- c(se_sitio1_rounded, se_sitio2_rounded, se_sitio3_rounded)
sitios <- c("KAW", "KOD", "LLA")

########  Grafico no log
# Define el rango que deseas considerar
rango_min <- 5  # Define el valor mínimo del rango
rango_max <- 80  # Define el valor máximo del rango

transecto_df_filtrado <- transecto_df[transecto_df$DAP_prom >= rango_min & transecto_df$DAP_prom <= rango_max,]
#Valor filtrados
promedios <- tapply(transecto_df_filtrado$DAP_prom, transecto_df_filtrado$Sitio, mean)
desviaciones <- tapply(transecto_df_filtrado$DAP_prom, transecto_df_filtrado$Sitio, sd)
errores <- desviaciones / sqrt(tapply(transecto_df_filtrado$DAP_prom, transecto_df_filtrado$Sitio, length))

boxplot(DAP_prom ~ Sitio, data = transecto_df_filtrado, 
        xlab = "Sitio", ylab = "Valor",
        main = "Boxplot con Promedios, Desviaciones y Errores")

# Agregar promedios, desviaciones y errores
points(1:length(sitios), promedios, 
       pch = 16, col = "red", cex = 1.5)
segments(1:length(sitios), promedios - desviaciones,
         1:length(sitios), promedios + desviaciones,
         lwd = 2)
segments(1:length(sitios), promedios - errores,
         1:length(sitios), promedios + errores,
         lwd = 2, col = "blue")

# Leyenda
legend("topleft", legend = c("Promedio", "Desviación", "Error"),
       pch = c(16, NA, NA), lwd = c(NA, 2, 2), col = c("red", "black", "blue"), 
       xjust = -1, yjust = 1)
########

######## Grafico LOG
transecto_df$Valor_ln <- log(transecto_df$DAP_prom)
desviaciones_ln <- log(desviaciones)
errores_ln <- log(errores)
promedios_ln <- log(promedios)

#Obtener el rango de valores logarítmicos
y_min <- 1
y_max <- 7

boxplot(Valor_ln ~ Sitio, data = transecto_df, 
        xlab = "Sitio", ylab = "Valor log",
        main = "Boxplot con Promedios, Desviaciones y Errores",
        ylim = c(y_min, y_max))

# Agregar promedios, desviaciones y errores
points(1:length(sitios), promedios_ln, 
       pch = 16, col = "red", cex = 1.5)
segments(1:length(sitios), promedios_ln - desviaciones_ln,
         1:length(sitios), promedios_ln + desviaciones_ln,
         lwd = 2)
segments(1:length(sitios), promedios_ln - errores_ln,
         1:length(sitios), promedios_ln + errores_ln,
         lwd = 2, col = "blue")

# Leyenda
legend("topleft", legend = c("Promedio", "Desviación", "Error"),
       pch = c(16, NA, NA), lwd = c(NA, 2, 2), col = c("red", "black", "blue"), horiz = TRUE)
