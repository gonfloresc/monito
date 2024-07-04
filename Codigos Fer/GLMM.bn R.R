library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(AICcmodavg)
library(MASS)
library(car)


datos<- read.csv("Codigos Fer/cr12_BINOMIAL.csv",sep = ";")
str(datos)
summary(datos)

## Filtrar Datos
datos_KAWKOD <- subset(datos, Sitio == "Kawelluco" | Sitio == "Kodkod")
datos_LLA <- subset(datos, Sitio == "Llancalil")

#probar modelos alternativos
modelo_poisson <- glm(num_eventos_mon ~ Cob_dosel_Superior + Abundancia_tree + cr_100_rat + DAP + Decay + Prom_sotob + PROM_BAMBU + Pendiente + offset(log(noches_trampa)), data = datos, family = poisson)
summary(modelo_poisson)
dispersion_ratio1 <- modelo_poisson$deviance / modelo_poisson$df.residual
cat("Razón de dispersión:", dispersion_ratio1, "\n")


modelo_quasi <- glm(num_eventos_mon ~ Cob_dosel_Superior + Abundancia_tree + cr_100_rat + DAP + Decay + Prom_sotob + PROM_BAMBU + Pendiente + offset(log(noches_trampa)), data = datos, family = quasipoisson)
summary(modelo_quasi)
dispersion_ratio2 <- modelo_quasi$deviance / modelo_quasi$df.residual
cat("Razón de dispersión:", dispersion_ratio2, "\n")

modelo_nbinom <- glm.nb(num_eventos_mon ~ Cob_dosel_Superior + Abundancia_tree + cr_100_rat + DAP + Decay + Prom_sotob + PROM_BAMBU + Pendiente + offset(log(noches_trampa)), data = datos, control = glm.control(maxit = 50))
summary(modelo_nbinom)

dispersion_ratio3 <- modelo_nbinom$deviance / modelo_nbinom$df.residual
cat("Razón de dispersión:", dispersion_ratio3, "\n")

summary(modelo_quasi)$deviance
summary(modelo_nbinom)$deviance

dev_quasi <- summary(modelo_quasi)$deviance
dev_nbinom <- summary(modelo_nbinom)$deviance
df_residual_quasi <- modelo_quasi$df.residual
df_residual_nbinom <- modelo_nbinom$df.residual

cat("Devianza residual para modelo de cuasi-Poisson:", dev_quasi, "con", df_residual_quasi, "grados de libertad\n")
cat("Devianza residual para modelo binomial negativo:", dev_nbinom, "con", df_residual_nbinom, "grados de libertad\n")

# Test estadísticos de comparación
anova(modelo_poisson, modelo_nbinom, test="Chisq")

######################################################################3


#Generar combinaciones de variables
#Paso 1: Lista de covariables

covar <- c("Cob_dosel_Superior", "Abundancia_tree", "cr_100_rat", "DAP", "Decay", "Prom_sotob", "PROM_BAMBU", "Pendiente")

#Paso 2: Funcion para generar las combinaciones de variables
combinaciones <- unlist(lapply(1:length(covar), function(x) { combn(covar, x, simplify = FALSE)
}), recursive = FALSE)

#Paso 3: Lista para almacenar los resultados
comparar_modelos <- list()

# Definir un modelo nulo (solo el intercepto y el offset)
formula_nula <- as.formula("num_eventos_mon ~ 1 + offset(log(noches_trampa)) + (1 | Sitio)")
modelo_nulo <- glmmTMB(formula_nula, data = datos_LLA, family = nbinom2)  ## Seleccion de data !!
# Guardar el modelo nulo en la lista
comparar_modelos[["modelo_nulo"]] <- modelo_nulo
View(modelo_nulo)

#Paso 4:ajustar los modelos para cada combinacion de variables
for (combinacion in combinaciones) {
  #formula del modelo
  formula <- as.formula(paste("num_eventos_mon ~", paste(combinacion, collapse = "+"), "+ offset(log(noches_trampa)) + (1 | Sitio)"))
  
  #ajustar el modelo
  modelo <- glmmTMB(formula, data = datos_LLA, family = nbinom2) ## Seleccion de data !!
  comparar_modelos[[paste(combinacion, collapse = "+")]] <- modelo
}



# Asegurarse de que todos los modelos sean de la clase glmmTMB
if (!all(sapply(comparar_modelos, inherits, "glmmTMB"))) {
  invalid_models <- names(comparar_modelos)[!sapply(comparar_modelos, inherits, "glmmTMB")]
  stop(paste("Los siguientes modelos no son de la clase 'glmmTMB':", paste(invalid_models, collapse = ", ")))
}



# Seleccionar el mejor modelo con AICc más bajo
model_names <- names(comparar_modelos)
tabla <- aictab(cand.set = comparar_modelos, modnames = model_names, second.ord = TRUE)
tabla <- tabla[order(tabla$AICc), ]
print(tabla)
write.csv(tabla, file = "C:/Users/Lenovo/Desktop/Lunes/Tabla_AICcTabOTROMAS.csv", row.names = FALSE)

View(comparar_modelos)
str(comparar_modelos)


#Paso 1: extraer AICs de todos los modelos
aics <- sapply(comparar_modelos, AICc)
str(aics)
print(aics)



#Paso 2: Identificar el mejor modelo
best_model<-which.min(aics)
best_model_formula<-names(aics)[best_model]
best_model_aic<- aics[best_model]



#paso 3: mostrar resultado de mejor modelo
cat("El mejor modelo es:", best_model_formula, "con AIC =", best_model_aic, "\n")



#Detalles del mejor modelo
summary(comparar_modelos[[best_model_formula]])







###################
#Revisar el mejor modelo de parametro no informativo
best1 <- glmmTMB(num_eventos_mon ~ 1 + offset(log(noches_trampa)) + (1 | Sitio), family = nbinom2, data = datos_LLA)  ## Seleccion de data!!
summary(best1)

best2 <- glmmTMB(num_eventos_mon ~ cr_100_rat + Pendiente + offset(log(noches_trampa)) + (1 | Sitio), family = nbinom2, data = datos)
summary(best2)

best3 <- glmmTMB(num_eventos_mon ~ Pendiente + PROM_BAMBU + offset(log(noches_trampa)) + (1 | Sitio), family = nbinom2, data = datos)
summary(best3)

best4 <- glmmTMB(num_eventos_mon ~ Pendiente + Abundancia_tree + offset(log(noches_trampa)) + (1 | Sitio), family = nbinom2, data = datos)
summary(best4)

best5 <- glmmTMB(num_eventos_mon ~ Pendiente + DAP + offset(log(noches_trampa)) + (1 | Sitio), family = nbinom2, data = datos)
summary(best5)

plot(fitted(modelo_G), residuals(modelo_G, type = "pearson"))
abline(h = 0, col = "red")
library(lmtest)
bptest(modelo_G)

library(lmtest)
dwtest(modelo_G)

#diagnosticar el modelo (bondad de ajuste y dispersion)
residuos_sim<- simulateResiduals(fittedModel=modelo_G)
dispersion_test <- testDispersion(residuos_sim)
print(dispersion_test)
# Calcular la prueba de Durbin-Watson
# Calcular los residuos del modelo
residuos <- residuals(modelo_G, type = "pearson")

# Calcular la suma de los cuadrados de los residuos
SSR <- sum(diff(residuos)^2)

# Calcular la suma de los cuadrados de las diferencias entre residuos adyacentes
SSD <- sum((diff(residuos))^2)

# Calcular la prueba de Durbin-Watson
D <- SSD / SSR

# Imprimir el valor de la prueba de Durbin-Watson
print(D)

plot(fitted(modelo_G), residuals(modelo_G))




