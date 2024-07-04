####   Historial de captura 24h
#install.packages("rmulti")
#install.packages("glmulti")
library(tidyr)
library(dplyr)
library(lme4)
library(glmmTMB)
library(glmulti)
library(DHARMa)

## Import data:
data_casas <- read.csv("Resultados/Casas_totales_2023.csv", sep = ";")
data_veg <- read.csv("Resultados/co-variables vegetacion/co-variables veg.csv", sep = ";")
data_mon <- read.csv("Resultados/Historial de captura/hist_monito_csv.csv", sep = ";")
data_rat <- read.csv("Resultados/Historial de captura/hist_rata_csv.csv", sep = ";")
data_mon_KAW <- read.csv("Resultados/Historial de captura/hist_monito_KAW.csv", sep = ";")
data_mon_KOD <- read.csv("Resultados/Historial de captura/hist_monito_KOD.csv", sep = ";")
data_mon_LLA <- read.csv("Resultados/Historial de captura/hist_monito_LLA.csv", sep = ";")
data_rat_KAW <- read.csv("Resultados/Historial de captura/hist_rata_KAW.csv", sep = ";")
data_rat_KOD <- read.csv("Resultados/Historial de captura/hist_rata_KOD.csv", sep = ";")

#Paso 1: Lista de covariables

covar <- c("Lugar", "Sitio", "Casa", "Altitud", "Pendiente", "Prom_sotob", "PROM_BAMBU", 
           "Cob_dosel_Superior", "Riqueza_ARB", "DAP", "Cavidades", "Descomposicion")

#Paso 2: Funcion para generar las combinaciones de variables
combinaciones <- unlist(lapply(1:length(covar), function(x) { combn(covar, x, simplify = FALSE)
}), recursive = FALSE)

#Paso 3: Lista para almacenar los resultados
comparar_modelos <- list()

# Formato lista casas
data_casas$x <- gsub("casa", "", data_casas$x)
data_casas$x <- as.numeric(data_casas$x)

data_mon_KAWKOD <- data_mon[1:56, ]
data_rat_KAWKOD <- data_rat[1:56, ]
data_casas_KAWKOD <- data_casas[1:56, ]
data_casas_KAW <- data_casas[1:26, ]
data_casas_KOD <- data_casas[27:56, ]
data_casas_LLA <- data_casas[57:86, ]

# Añadir columnas de co-variables y convertir a formato largo
data_mon_KAWKOD$Casa <- data_casas_KAWKOD

df_mon_long <- data_mon_KAWKOD %>%
  pivot_longer(cols = -Casa, names_to = "Occasion", values_to = "Capture_mon") %>%
  mutate(Occasion = as.numeric(gsub("o", "", Occasion)))

data_rat_KAWKOD$Casa <- data_casas_KAWKOD

df_rat_long <- data_rat_KAWKOD %>%
  pivot_longer(cols = -Casa, names_to = "Occasion", values_to = "Capture_rat") %>%
  mutate(Occasion = as.numeric(gsub("o", "", Occasion)))

# Unir los dos dataframes por sitio y ocasión
df_combined <- inner_join(df_mon_long, df_rat_long, by = c("Casa", "Occasion"))

# Unir el dataframe de covariables con df_combined por "Site"
df_combined <- inner_join(df_combined, data_veg, by = "Casa")
# Ver los datos combinados
head(df_combined)


# Definir una fórmula para el modelo completo
formula_full <- Capture_mon ~ Capture_rat + Pendiente + Prom_sotob + PROM_BAMBU + Cob_dosel_Superior + 
                              Riqueza_ARB + DAP + Cavidades + Descomposicion

#Combinaciones de modelos y formulas
for (combinacion in combinaciones) {
  #formula del modelo
  formula <- as.formula(paste("Capture_mon ~", paste(combinacion, collapse = "+"), 
                              "+ (1 | Casa) + (1 | Occasion)"))
  
  #ajustar el modelo
  modelo <- glmer(formula, data = df_combined, family = binomial) ## Seleccion de data !!
  comparar_modelos[[paste(combinacion, collapse = "+")]] <- modelo
  
}

 # Ajustar el modelo utilizando glmulti
glmulti_result <- glmulti(formula_full, 
                          data = df_combined, 
                          level = 1, # Solo modelos con efectos fijos
                          method = "h", # Hierarchical approach
                          crit = "aic", # Criterio de información de Akaike
                          confsetsize = 100) # Tamaño del conjunto de confianza

comparar_modelos <- list()

  # Obtener el mejor modelo
best_formula <- formula(glmulti_result@objects[[5]])
# Ajustar el mejor modelo utilizando glmer con efectos aleatorios
best_model <- glmer(formula(paste(deparse(best_formula), "+ (1 | Casa) + (1 | Occasion)")), 
                    data = df_combined, 
                    family = binomial)
summary(best_model)
best_model <- glmer( Capture_mon ~ Capture_rat + Prom_sotob + PROM_BAMBU + Riqueza_ARB + Descomposicion + (1 | Casa) + (1 | Occasion), data = df_combined, family = binomial)
summary(best_model)
comparar_modelos[[paste(best_formula, collapse = "")]] <- best_model

tabla <- NULL
model_names <- names(comparar_modelos)
tabla <- aictab(cand.set = comparar_modelos, modnames = model_names, second.ord = FALSE)
tabla <- tabla[order(tabla$AIC), ]
summary(best_model)

# Ajustar un GLMM considerando la dependencia temporal
# Incluimos (1 | Site) para los efectos aleatorios de los sitios
# y (1 | Site:Occasion) para los efectos aleatorios de las ocasiones de muestreo dentro de los sitios
modelo_nulo <- glmer(Capture_mon ~ 1 + (1 | Casa) + (1 | Occasion), 
                      data = df_combined, 
                      family = binomial)
                       
comparar_modelos[["modelo_nulo"]] <- modelo_nulo

summary(modelo_nulo)
glmm_aic <- AICc(temporal_glmm)
