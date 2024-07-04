### Modelos
# Cargar paquete lme4
library(lme4)
#install.packages("glmmTMB")
library(glmmTMB)

data_cr <- read.csv(file = "Resultados/Taza Captura/cr12_poisson.csv", sep = ";")

# Estandarización a puntuaciones z, asegurando que no haya valores negativos
data_cr$noches_trampa <- scale(data_cr$cr_100_mon, center = TRUE, scale = TRUE)
data_cr$cr_100_ray <- scale(data_cr$cr_100_ray)

# Definición manual de modelo GLMM con zero-inflation (Poisson)
# Ajustar el modelo GLMM con zero-inflation usando la distribución Poisson
modelo <- glmmTMB(cr_100_mon ~ cr_100_rat + cr_100_ray + pendiente + prom_densidad_sotobosque
                  + dap + descomposicion,
                  data = KAW,
                  ziformula = ~ cr_100_rat,
                  family = poisson(link = "log"))

# Ver los resultados del modelo
summary(modelo)

# Ajustar el modelo GLMM con zero-inflation usando la distribución binomial negativa
modelo <- glmmTMB(cr_100_mon ~ cr_100_rat + cr_100_ray + pendiente + prom_densidad_bambu + prom_densidad_sotobosque 
                  + dap + descomposicion,
                  data = KAW,
                  family = nbinom2(link = "log"),
                  ziformula = ~ cr_100_rat)  # Zero-inflation formula

# Ver los resultados del modelo
summary(modelo)

##  Subset por sitio

KAW <- subset(data_cr, casa >= 1 & casa <= 40)
PIC <- subset(data_cr, casa >= 81 & casa <= 120)
LLA <- subset(data_cr, casa >= 121 & casa <= 159)
