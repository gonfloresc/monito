#install.packages("overlap")
library(overlap)
library(activity)
library(car)

# impor data
data_mon <- read.csv("Resultados/Eventos_unicos_spp/2023_1h/Monito del monte.csv", sep = ";")
data_rat <- read.csv("Resultados/Eventos_unicos_spp/2023_1h/Rata negra_1h.csv", sep = ";")
data_ray <- read.csv("Resultados/Eventos_unicos_spp/2023_1h/Rayadito.csv")
data_noches <- read.csv("Resultados/Resultado_noches_trampa.csv", sep = ";")
data_co <- read.csv("Resultados/Regresión Logística/pres_mon_rat_2023.csv", sep = ";")

# Separar la columna DateTime en Date y Time
data_mon$DateTime <- as.POSIXct(data_mon$DateTime, format = "%d-%m-%Y %H:%M")
data_mon$Date <- as.Date(data_mon$DateTime, format = "%d-%m-%Y")  # Extrae solo la fecha
data_mon$Time <- format(data_mon$DateTime, "%H:%M")

data_mon$Casa <- gsub("casa", "", data_mon$Casa)
data_mon$Casa <- as.numeric(data_mon$Casa)

# Separar la columna DateTime en Date y Time
data_rat$DateTime <- as.POSIXct(data_rat$DateTime, format = "%d-%m-%Y %H:%M")
data_rat$Date <- as.Date(data_rat$DateTime, format = "%d-%m-%Y")  # Extrae solo la fecha
data_rat$Time <- format(data_rat$DateTime, "%H:%M")

data_rat$Casa <- gsub("casa", "", data_rat$Casa)
data_rat$Casa <- as.numeric(data_rat$Casa)


## Hist eventos
data_hist<- data.frame(casa = data_noches$Casa.Anidera)

### dataframe Time
data_time <- data.frame(matrix(ncol = 24, nrow = nrow(data_hist)))

# Generar nombres de columna
nombres_columnas <- paste0("h", 1:24)
# Asignar los nombres de columna al dataframe
colnames(data_time) <- nombres_columnas


#formateo Time
horas <- seq.POSIXt(from = as.POSIXct("00:00", format = "%H:%M"), 
                    to = as.POSIXct("24:00", format = "%H:%M"), 
                    by = "hour")

for (i in 1:nrow(data_hist)) { 
  data_casa <- data_mon[data_mon$Casa == data_hist$casa[i],]
  for (j in 1:24) {
    conteo <- data_casa[data_casa$Time >= horas[j] & data_casa$Time < horas[j+1], ]
    conteo <- nrow(conteo)
    no_col <- paste0("h", j)              
    data_time[i, no_col] <- conteo
  }
}  


## Density Plot

# Convertir la hora a fracción del día (en este caso, 0 horas de 24)
data_mon$time_frac <- as.numeric(strptime(data_mon$Time, "%H:%M")$hour) / 24
data_rat$time_frac <- as.numeric(strptime(data_rat$Time, "%H:%M")$hour) / 24

#### Casas con co-ocurrencia
# filtrar casa solo co-ocurrencia
data_co <- data_co[data_co$pre_mon == 1 & data_co$pre_rat == 1,]
data_mon <- data_mon[data_mon$Casa %in% data_co$Casa, ]
data_rat <- data_rat[data_rat$Casa %in% data_co$Casa, ]

# Calcular radianes (considerando un círculo completo = 2*pi radianes)
T_rad <- data.frame(Zone = data_mon$Casa, Sps = data_mon$Especie1, Time = data_mon$time_frac * 2 * pi )
T_rad_rat <- data.frame(Zone = data_rat$Casa, Sps = data_rat$Especie1, Time = data_rat$time_frac * 2 * pi )




KAW <- subset(T_rad, as.numeric(Zone) >= 1 & as.numeric(Zone) <= 40)
PIC <- subset(T_rad, as.numeric(Zone) >= 81 & as.numeric(Zone) <= 120)
LLA <- subset(T_rad, as.numeric(Zone) >= 121 & as.numeric(Zone) <= 159)
KAW_rat <- subset(T_rad_rat, as.numeric(Zone) >= 1 & as.numeric(Zone) <= 40)
KAWKOD_rat <- subset(T_rad_rat, as.numeric(Zone) >= 1 & as.numeric(Zone) <= 120)
PIC_rat <- subset(T_rad_rat, as.numeric(Zone) >= 81 & as.numeric(Zone) <= 120)
LLA_rat <- subset(T_rad_rat, as.numeric(Zone) >= 121 & as.numeric(Zone) <= 159)

densityPlot(KAWKOD_rat$Time, rug = TRUE)

# Ajustar los límites del eje x para que coincidan con los datos
xlim_values <- c(0, 24)

densityPlot(KAW_rat$Time, rug=TRUE, adjust = 0.8, main = NULL,
            ylim = c(0, 0.12), xlim = xlim_values, 
            xlab = "Time of day", ylab = "Density of activity")

# Overlap
tigmac2est <- overlapEst(PIC$Time, PIC_rat$Time)
tigmac2est

#bootstrapping 10.000 iteraciones
T_rad_boot <- resample(PIC$Time, 10000)
T_rad_boot2 <- resample(LLA$Time, 10000)
T_rad_rat_boot <- resample(PIC_rat$Time, 10000)
T_rad_rat_boot2 <- resample(PIC_rat$Time, 10000)
dim(T_rad_boot)
dim(T_rad_rat_boot)

mon_rat_2 <- bootEst(T_rad_boot, T_rad_rat_boot, adjust = c(0.8, 1, NA))
dim(mon_rat_2)

#Confidence interval (CI)
BSmean <- colMeans(mon_rat_2)
tmp <- mon_rat_2[, 2]
bootCI(tigmac2est[2], tmp, conf = 0.95)

#Fit without confidence limits 
mod1 <- fitact(PIC$Time)
mod2 <- fitact(PIC$Time)
mod1_rat <- fitact(PIC_rat$Time)
mod2_rat <- fitact(PIC_rat$Time)
plot(mod2)

compareCkern(mod1,mod1_rat,reps=10000)

# Ajustar los márgenes del gráfico para dejar espacio a la leyenda
par(mar = c(5, 4, 4, 10) + 0.1)

# Definir los límites del eje y
ylim <- c(0, 0.12)  # Por ejemplo, cambia los límites según tu necesidad
overlapPlot(PIC$Time, PIC_rat$Time, adjust = 0.8, 
            main = NULL, colors = c(1,4), ylim = c(0, 0.12), xlab = "Time of day", 
            ylab = "Density of activity")

# Agregar leyenda fuera del área del gráfico
legend("topright", c("Monito", "Rata"), lty = c(1, 2), col = c(1, 4), bty = 'n', inset = c(-0.4, 0.1), xpd = TRUE)



