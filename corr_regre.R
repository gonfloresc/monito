### Script para determinar presencia spp en casas
#library(occupancy)
#install.packages("unmarked")
#library(unmarked)

# import datos
casas <- read.csv("Resultados/Casas_totales_2023.csv")
eventos_monito <- read.csv("Resultados/Eventos_unicos_spp/2023_1h/Monito del monte.csv")
eventos_ratus <- read.csv("Resultados/Eventos_unicos_spp/2023_1h//Rata negra.csv", sep = ",")
atributos_veg <- read.csv("Resultados/Regresión Logística/Datos terreno 2023 - Covariables_CSV.csv")

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

write.csv(casas, "Resultados/Regresión Logística/pres_mon_rat_2023.csv", row.names = FALSE)

## Agregar co-variables vegetacionales a utilizar. Variables no usadas han sido comentadas "#"
casas <- merge(casas, atributos_veg[c("Casa", "Pendiente", "PROM_BAMBU", "Prom_sotob", "DAP", "Descomposición")], by = "Casa", all.x = TRUE)
#casas$Altitud <- ifelse(is.na(casas$Altitud), atributos_veg$Altitud, casas$Altitud)
casas$Pendiente <- ifelse(is.na(casas$Pendiente), atributos_veg$Pendiente, casas$Pendiente)
casas$Prom_sotob <- ifelse(is.na(casas$Prom_sotob), atributos_veg$Prom_sotob, casas$Prom_sotob)
casas$PROM_BAMBU <- ifelse(is.na(casas$PROM_BAMBU), atributos_veg$PROM_BAMBU, casas$PROM_BAMBU)
#casas$Cob_dosel_Superior <- ifelse(is.na(casas$Cob_dosel_Superior), atributos_veg$Cob_dosel_Superior, casas$Cob_dosel_Superior)
#casas$Riqueza_ARB <- ifelse(is.na(casas$Riqueza_ARB), atributos_veg$Riqueza_ARB, casas$Riqueza_ARB)
casas$DAP <- ifelse(is.na(casas$DAP), atributos_veg$DAP, casas$DAP)
#casas$Cavidades <- ifelse(is.na(casas$Cavidades), atributos_veg$Cavidades, casas$Cavidades)
casas$Descomposición <- ifelse(is.na(casas$Descomposición), atributos_veg$Descomposición, casas$Descomposición)

# Re-Orden columnas
casas <- casas[, c("Sitio", "Casa", "pre_mon", "pre_rat", "Pendiente", "PROM_BAMBU", "Prom_sotob", "DAP", "Descomposición")]
write.csv(casas, "Resultados/Regresión Logística/datos_reg_2023.csv", row.names = FALSE)

#Modelo Regresion logística
modelo <- glm(pre_mon ~ pre_rat, data = casas, family = binomial)
modelo <- glm(pre_mon ~ DAP, data = casas, family = binomial)
modelo <- glm(pre_mon ~ PROM_BAMBU, data = casas, family = binomial)
modelo <- glm(pre_mon ~ Prom_sotob, data = casas, family = binomial)
modelo <- glm(pre_mon ~ pre_rat + PROM_BAMBU + Prom_sotob, data = casas, family = binomial)
modelo <- glm(pre_mon ~ pre_rat + Pendiente + PROM_BAMBU + Prom_sotob + DAP + Descomposición, data = casas, family = binomial)
summary(modelo)
