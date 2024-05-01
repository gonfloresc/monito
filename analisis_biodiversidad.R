## Analisis Biodiversidad

# Ruta archivo .csv a analizar:
df_biodiv <- read.csv("Transecto Vegetacional/biodiversidad_CSV.csv")

# Conteo spp e individuos totales por sitio
conteo_especies <- aggregate(spp ~ Sitio, data = df_biodiv, FUN = function(x) length(unique(x)))
conteo_individuos <- aggregate(Casas ~ Sitio, data = df_biodiv, FUN = length)
df_combined <- cbind(conteo_individuos, conteo_especies$spp)

# Orden dataframe
df_combined$individuos <- df_combined$Casas
df_combined$nspp <- df_combined$`conteo_especies$spp`
df_combined <- df_combined[, -which(names(df_combined) == "Casas")]
df_combined <- df_combined[, -which(names(df_combined) == "conteo_especies$spp")]

# Creacion DataFrame "Indices" y Calculo indice Riqueza relativa
Indices <- cbind(conteo_especies$spp / conteo_individuos$Casas)
Indices <- cbind(df_combined, Indices)
Indices$riq_rel <- Indices$Indices
Indices <- Indices[, -which(names(Indices) == "Indices")]

# Calculo Abundancia Relativa spp x Sitio
# funcion abundancia relativa x spp
func_abund <- function (spp) {
  df_spp <- data.frame(Sitio = character(), spp = character(), nspp = integer(), total_ind = integer(), abun_relativ = numeric(),  stringsAsFactors = FALSE)
  
  i <- 1
  for (sitio in unique(df_biodiv$Sitio)) {
    df_sitio <- subset(df_biodiv, Sitio == sitio)
    
    cantidad_spp <- sum(df_sitio$spp == spp)
    total_ind <- length(df_sitio$spp)
    abun_relat <- cantidad_spp / total_ind
    
    df_spp[i,] <- list(sitio, spp, cantidad_spp, total_ind, abun_relat)
    i <- i + 1
  }

  return(df_spp)
}

# se corre funcion abundancia relativa en todas las spp
abundancias <- data.frame()

for (spp in unique(df_biodiv$spp)) {
  df_spp <- func_abund(spp)
  
  nombre_df <- paste("abun_", spp, sep = "")
  assign(nombre_df, df_spp)
  abundancias <- rbind(abundancias, df_spp)
}
  
##  Indice de Shannon (log n) x Sitio

shannon_i <- numeric()
i <- 1
constant_thre <- 0.0001     ## Constante threshold para evitar log(0)

for (sitio in unique(abundancias$Sitio)) {
  df_sitio <- subset(abundancias, Sitio == sitio)

  abun_correct <- df_sitio$abun_relativ + constant_thre
  
  shannon_i <- -sum(abun_correct * log(abun_correct)) ##  log n o log2
  
  #Se agrega indice Shannon a dataframe Indices
  Indices$Shannon_ind[i] <- shannon_i
  i <- i + 1
}

## Indice de Uniformidad

Uni_ind <- numeric()
i <- 1

for (i in length(nrow(Indices))) {
  Uni_ind <- Indices$Shannon_ind[i] / log(Indices$nspp[i])    
  Indices$Uni_ind[i] <- Uni_ind
  i <- i + 1
}

## Indice de Simpson

Sim_ind <- numeric()
i <- 1 

for (sitio in unique(abundancias$Sitio)) {
  df_sitio <- subset(abundancias, Sitio == sitio)
  
  abun_correct <- df_sitio$abun_relativ + constant_thre
  
  Sim_ind <- sum(df_sitio$abun_relativ ^ 2)  
  
  #Se agrega indice Shannon a dataframe Indices
  Indices$Sim_ind[i] <- Sim_ind
  i <- i + 1
}


