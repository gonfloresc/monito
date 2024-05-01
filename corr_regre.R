### Script para determinar presencia spp en casas

# import datos
casas <- read.csv("Resultados/Casas_totales_2022.csv")
eventos_monito <- read.csv("Resultados/Eventos_unicos_spp/2022/Monito del monte.csv")

colnames(casas)[colnames(casas) == "x"] <- "Casa"

# Comprobar presencia en casas
presente_en_casa <- casas$Casa %in% eventos_monito$Casa

# Creación dataframe
presencia_monito <- casas
presencia_monito$pre_mon <- ifelse(presente_en_casa, 1,  0)

