# Librería lubridate
library(lubridate)

# Punto 1: Crear un vector de fechas desde el 01-10-2022 hasta el 30-09-2023
fechas <- seq.Date(from = as.Date("2022-10-01"), to = as.Date("2023-09-30"), by = "day")

set.seed(2405)  # Semilla para obtener siempre los mismos resultados!!!

# Vector de temperaturas aleatorias en grados Fahrenheit
temperaturas <- runif(length(fechas), min = 32, max = 104)

# Vector de presiones atmosféricas aleatorias 
presiones <- runif(length(fechas), min = 95000, max = 105000)

# Función para convertir de Fahrenheit a Celsius
fahrenheit_to_celsius <- function(fahrenheit) {
  return((fahrenheit - 32) * 5/9)
}

temperaturas_celsius <- fahrenheit_to_celsius(temperaturas)

# Redondear las temperaturas sin decimales
temperaturas <- round(temperaturas, 0)
temperaturas_celsius <- round(temperaturas_celsius, 0)

# Información adicional de las fechas
meses <- month(fechas, label = TRUE)        # Mes en formato abreviado 
dias_semana <- wday(fechas, label = TRUE)   # Día de la semana 

# Crear un data frame para visualizar los datos
data <- data.frame(
  Fecha = fechas,
  Mes = meses,
  Dia_Semana = dias_semana,
  Temperatura_F = temperaturas,
  Temperatura_C = temperaturas_celsius,
  Presion_Pa = presiones
)

# Mostrar los primeros registros del data frame
head(data)



