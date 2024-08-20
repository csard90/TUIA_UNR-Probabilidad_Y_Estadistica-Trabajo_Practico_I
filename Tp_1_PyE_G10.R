# ------------------Actividad 1---------------------------------------
#--------------------------------------------------------------------

## Carga de datos del primer sheet
install.packages("readxl")
library(readxl)
datos_TP01 <- read_excel("Datos_TP01.xlsx")
## Recategorización en una nueva columna
### Librería para manipular el dataframe
install.packages("dplyr")
library(dplyr)
### Definimos como se van a agrupar
categorias_ejercicio_frecuente <- c("Sí, más de dos veces por semana", "Sí, más de 3 veces por semana", "Sí, más de 4 veces por semana")
categorias_ejercicio_no_frecuente <- c("No", "No, pero uso la calle recreativa los domingos", "Sí, los sábados")
### Reagrupamos
datos_TP01 <- datos_TP01 %>%
  mutate("¿Realiza ejercicio físico más de dos veces por semana?" = case_when( 
     `¿Realiza ejercicio físico de manera frecuente?` %in% categorias_ejercicio_frecuente ~ "Sí", 
     `¿Realiza ejercicio físico de manera frecuente?` %in% categorias_ejercicio_no_frecuente ~ "No", 
     TRUE ~ `¿Realiza ejercicio físico de manera frecuente?` #Si no mantener valor original
  ))

## Análisis descriptivo 
### Tabla de frecuencias
tabla_ejercicio_frecuente <- table(datos_TP01$`¿Realiza ejercicio físico más de dos veces por semana?`)
df_ejercicio_frecuente <- as.data.frame(tabla_ejercicio_frecuente)
colnames(df_ejercicio_frecuente)[colnames(df_ejercicio_frecuente) == 'Var1'] <- 'Ejercicitación Frecuente'
df_ejercicio_frecuente$Freq_relativa <- df_ejercicio_frecuente$Freq / sum(tabla_ejercicio_frecuente) * 100
### Gráfico
pie(df_ejercicio_frecuente$Freq_relativa, labels = df_ejercicio_frecuente$`Ejercicitación Frecuente`, col = c("pink", "lightblue"))
title(main = "Ejercicitación frecuente")
porcentajes <- round((df_ejercicio_frecuente$Freq_relativa/sum(df_ejercicio_frecuente$Freq_relativa)) * 100, 2)
text(x = 1, y = 1, labels = paste(porcentajes[1], "%"))
text(x = -1, y = -1, labels = paste(porcentajes[2], "%"))

# ------------------Actividad 2---------------------------------------
#--------------------------------------------------------------------

# ACLARACION: Antes de comenzar a correr el codigo se debe copiar 
# la columna "Nº de defectos" de la base de datos

datos_defectos <- read.delim("clipboard", col.names = "nro_defectos")

# Generamos la tabla con frecuencias absolutas
tabla_defectos <- table(datos_defectos)

# Convierte la tabla anterior a data frame
df_defectos <- as.data.frame(tabla_defectos)

# Cambiamos el nombre a la columna en indice 2
colnames(df_defectos)[2] <- "frec_abs"

# Creamos la columna con las frecuencia relativa
df_defectos$frec_rel <- df_defectos$frec_abs / sum(df_defectos$frec_abs) * 100

# Creamos la columna con la frecuencia absoluta acumulada
df_defectos$frec_abs_acum <- cumsum(df_defectos$frec_abs)

# Creamos la columna con la frecuencia relativa acumulada
df_defectos$frec_rel_acum <- cumsum(df_defectos$frec_rel)

# Calculo de la medida de resumen de interes para el estudio

media <- mean(datos_defectos$nro_defectos)

# Medidas de localizacion:

# Moda:

# df_defectos$nro_defectos[indice] devuelve el valor alojado en la fila indice
# which.max() devuelve el indice correspondiente a la fila del valor maximo de la columna
moda <- df_defectos$nro_defectos[which.max(df_defectos$frec_abs)]

# Mediana:

mediana <- median(datos_defectos$nro_defectos)

# Medidas de dispersion:

# Rango:

rango <- max(datos_defectos$nro_defectos) - min(datos_defectos$nro_defectos)

desvio_estandar <- sd(datos_defectos$nro_defectos)

# Graficos:
# Instalacion de paquetes
install.packages("readxl")
install.packages("ggplot2")
library(readxl)
library(ggplot2)

ggplot(data=df_defectos) +
  geom_segment(aes(x=nro_defectos,y=0,xend=nro_defectos, yend=frec_abs)) +
  geom_point(aes(nro_defectos,frec_abs),size=1.5) +
  labs(x = "Nro. de defectos", y = "Cantidad de placas") +
  theme_classic()+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12))

# La columna de clases nos quedo como tipo factor y la necesitamos como integer por que hay que
# sumarle 1 para graficar el escalonado, por lo tanto la convertimos a integer
df_defectos$nro_defectos <- as.character(df_defectos$nro_defectos)
df_defectos$nro_defectos <- as.integer(df_defectos$nro_defectos)

# Grafico escalonado:

ggplot(data=df_defectos) +
  geom_segment(aes(x=nro_defectos,y=frec_rel_acum, 
                   xend=nro_defectos+1, yend=frec_rel_acum)) +
  geom_point(aes(nro_defectos,frec_rel_acum), 
             size=1.5, shape=1) +
  labs(x = "Cantidad de defectos", y = "Porcentado acumulado") +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 12),
        axis.title.y = element_text(face="bold", colour="black", size = 12))+
  scale_x_continuous(expand=c(0,0), limits = c(-0.1,6), breaks=c(0:6)) +
  scale_y_continuous(expand=c(0,0), limits = c(0,110))

# ------------------Actividad 3---------------------------------------
#--------------------------------------------------------------------

# ACLARACION: Antes de comenzar a correr el codigo se debe copiar 
# la columna "Profundidad del corte" de la base de datos

datos <- read.delim("clipboard", col.names = "Profundidad del corte", header = FALSE)

datos_trabajados <- gsub(",", ".", datos$Profundidad.del.corte) # Se cambian las , por .
datos_trabajados <- as.numeric(datos_trabajados) # Se convierte a datos numéricos para así poder trabajar con ellos más fácilmente.

# Se almacena en contadores la cantidad de cortes que cumplen con las medidas de interés.
Mayor_7 <- 0
Menor_7 <- 0
for(i in datos_trabajados) {
  if (i > 7) {
    Mayor_7 <- Mayor_7 + 1
  } else {
    Menor_7 <- Menor_7 + 1
  }
}

datos_finales_hist <- as.data.frame(datos_trabajados) # Se crea un dataframe con los datos trabajados anteriormente para el histograma.
proporciones_pie <- c(Mayor_7, Menor_7) # Se almacenan en variables las proporciones, porcentajes y etiquetas para el diagrama de tarta.
porcentaje_pie <- round(proporciones_pie/sum(proporciones_pie)*100)
etiquetas_pie <- c("Mayor a 7cm -", "Menor a 7cm -")

etiquetas_pie <- paste(etiquetas_pie, porcentaje_pie) # Se le añaden los porcentajes a las etiquetas.
etiquetas_pie <- paste(etiquetas_pie, "%", sep="")

par(mfrow = c(1, 2)) # Dividimos la pantalla para que entren dos gráficos y se crean los gráficos a continuación.
pie(proporciones_pie, labels = etiquetas_pie, col = rainbow(length(etiquetas_pie)), main = "Diagrama de tarta")
hist(datos_finales_hist$datos_trabajados, ylab = "Frecuencia", xlab = "Profundidad del corte", main = "Histograma")
