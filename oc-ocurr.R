install.packages("cooccur")
library(cooccur)
library(tidyverse)

# import datos
data <- read.csv("Resultados/Regresión Logística/pre_mon_rat_long.csv", sep = ";", header = FALSE, row.names = 1)
data_new <- data.frame(casa = data$Casa, monito = data$pre_mon, rata = data$pre_rat)

# Eliminar filas con NA
data_new <- na.omit(data_new)

dataframe2 <- as.data.frame(t(data_new[, -1])) 
# Asignar nombres de fila a partir de 'Especie' en dataframe1
rownames(dataframe2) <- data_new$ 

data_long <- data_new %>%
  pivot_longer(cols = -casa, names_to = "especie", values_to = "presencia")

# Usar pivot_wider para transformar al formato deseado
data_wide <- data_long %>%
  pivot_wider(names_from = casa, values_from = presencia, names_prefix = "casa")
# Ejemplo de cómo debería estar configurado spp_names
rownames(data_wide) <- data_wide$especie

# Convertir a matriz binaria
data_wide_bin <- ifelse(data_wide[, -1] > 0, 1, 0) 


# Convertir a matriz para eliminar la columna de especies
data_matrix <- as.matrix(data_wide[,-1])
rownames(data_matrix) <- data_wide$especie


# Crear el modelo de co-ocurrencia
modelo_coocurrencia <- cooccur(mat = data,  spp_names = TRUE, thresh = FALSE)
# Ver los resultados
summary(modelo_coocurrencia)
prob.table(modelo_coocurrencia)

plot(modelo_coocurrencia, plotrand = TRUE) # add "plotrand = TRUE" to include completely random species
pair(mod = modelo_coocurrencia, spp = "Rata")
