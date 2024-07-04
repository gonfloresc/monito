library(glmmTMB)
library(MASS)

datos<- read.csv("Resultados/Regresión Logística/cr12_BINOMIAL.csv",sep = ";")

# Filtrar datos para el Sitio 1, 2 y 3
KAW <- subset(datos, Sitio == 1)
KOD <- subset(datos, Sitio == 2)
LLA <- subset(datos, Sitio == 3)
datos

#modelo global
modelo_area <- glm.nb(num_eventos_mon ~ cr_100_rat + cr_100_ray + Altitud + Pendiente +  
                        Prom_sotob + PROM_BAMBU + Cob_dosel_Superior + Riqueza_ARB + DAP + 
                        Cavidades + Decay + Abundancia_tree + offset(log(noches_trampa)),
                      data = datos)
modelo_area0 <- glm.nb(num_eventos_mon ~ Prom_sotob + Cob_dosel_Superior + DAP + Decay + 
                         Abundancia_tree + offset(log(noches_trampa)),
                      data = LLA)
modelo_area1 <- glm.nb(num_eventos_mon ~ offset(log(noches_trampa)),
                       data = LLA)
modelo_area2 <- glm.nb(num_eventos_mon ~ Prom_sotob + offset(log(noches_trampa)),
                       data = LLA)
modelo_area3 <- glm.nb(num_eventos_mon ~ Cob_dosel_Superior + offset(log(noches_trampa)),
                       data = LLA)
modelo_area4 <- glm.nb(num_eventos_mon ~ DAP + offset(log(noches_trampa)),
                       data = LLA)
modelo_area5 <- glm.nb(num_eventos_mon ~ Decay + offset(log(noches_trampa)),
                       data = LLA)
modelo_area6 <- glm.nb(num_eventos_mon ~ Abundancia_tree + offset(log(noches_trampa)),
                       data = LLA)
summary(modelo_area6)
#Kawelluco
modelo_kaw <- glm.nb(num_eventos_mon ~ cr_100_rat + cr_100_ray + Altitud + Pendiente +   
                       Prom_sotob + PROM_BAMBU + Cob_dosel_Superior + Riqueza_ARB + DAP + 
                       Cavidades + Decay + Abundancia_tree + offset(log(noches_trampa)),
                     data = KAW)
summary(modelo_kaw)

#kodkod
modelo_kod <- glm.nb(num_eventos_mon ~ + cr_100_rat + cr_100_ray + Altitud +  Pendiente + 
                       Prom_sotob + PROM_BAMBU + Cob_dosel_Superior + Riqueza_ARB + DAP + 
                       Cavidades + Decay + Abundancia_tree + offset(log(noches_trampa)),
                     data = KOD)
summary(modelo_kod)

#Llancalil
modelo_lla <- glm.nb(num_eventos_mon ~ cr_100_rat + cr_100_ray + Altitud +  Pendiente + 
                       Prom_sotob + PROM_BAMBU + Cob_dosel_Superior + Riqueza_ARB + DAP + 
                       Cavidades + Decay + Abundancia_tree +
                       offset(log(noches_trampa)),
                     data = LLA)
summary(modelo_lla)
