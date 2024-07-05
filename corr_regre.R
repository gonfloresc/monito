### Script para determinar presencia spp en casas


# import datos
casas <- read.csv("Resultados/Casas_totales_2023.csv")
eventos_monito <- read.csv("Resultados/Periodos/Agosto-Septiembre-Octubre/Eventos_unicos_spp/2023_p2_12h/Monito del monte.csv", sep = ",")
eventos_ratus <- read.csv("Resultados/Periodos/Agosto-Septiembre-Octubre/Eventos_unicos_spp/2023_p2_12h/Rata negra.csv", sep = ",")
atributos_veg <- read.csv("Resultados/co-variables vegetacion/co-variables veg.csv", sep = ";")

colnames(casas)[colnames(casas) == "x"] <- "Casa"

# Comprobar presencia en casas
presencia_monito <- casas$Casa %in% eventos_monito$Casa
presencia_ratus <- casas$Casa %in% eventos_ratus$Casa

# Creación dataframe
casas$pre_mon <- ifelse(presencia_monito, 1,  0)
casas$pre_rat <- ifelse(presencia_ratus, 1, 0)
casas$Casa <- as.numeric(gsub("casa", "", casas$Casa))
casas$Sitio <- ifelse(casas$Casa <= 40, 1, ifelse(casas$Casa <= 120, 2, 3))
casas <- casas[, c("Sitio", "Casa", "pre_mon", "pre_rat")]

## .csv Datos presencia/Ausencia monito y rata por casa
write.csv(casas, "Resultados/Periodos/Agosto-Septiembre-Octubre/pres_mon_rat_2023.csv", row.names = FALSE)

## Agregar co-variables vegetacionales a utilizar. Variables no usadas han sido comentadas "#"
casas <- merge(casas, atributos_veg[c("Casa", "Altitud", "Pendiente", "Prom_sotob", 
                                      "PROM_BAMBU", "Cob_dosel_Superior", "Riqueza_ARB", 
                                      "DAP", "Cavidades", "Descomposicion")], by = "Casa", all.x = TRUE)

# Re-Orden columnas
casas <- casas[, c("Sitio", "Casa", "pre_mon", "pre_rat", "Altitud", "Pendiente", "Prom_sotob", 
                   "PROM_BAMBU", "Cob_dosel_Superior", "Riqueza_ARB", 
                   "DAP", "Cavidades", "Descomposicion")]
write.csv(casas, "Resultados/Regresión Logística/Periodos/p2/Tabla_reglog_p2.csv", row.names = FALSE)



#Modelo Regresion logística
modelo <- glm(pre_mon ~ pre_rat, data = casas, family = binomial)
modelo <- glm(pre_mon ~ DAP, data = casas, family = binomial)
modelo <- glm(pre_mon ~ PROM_BAMBU, data = casas, family = binomial)
modelo <- glm(pre_mon ~ Prom_sotob, data = casas, family = binomial)
modelo <- glm(pre_mon ~ pre_rat + PROM_BAMBU + Prom_sotob, data = casas, family = binomial)
modelo <- glm(pre_mon ~ pre_rat + Pendiente + PROM_BAMBU + Prom_sotob + DAP + Descomposición, data = casas, family = binomial)
summary(modelo)
