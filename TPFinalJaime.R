install.packages("foreign")
install.packages("base")
install.packages("sf")
install.packages("dplyr")
install.packages("readxl")
install.packages("scales")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("tinytex")
install.packages("GGally")

library(tinytex)
library(readxl)
library(dplyr)
library(sf)
library(foreign)
library(scales)
library(ggplot2)
library(tidyr)
library(GGally)
tinytex::install_tinytex()

# Carga y limpieza de datos
# Leemos el archivo Excel con el dataset principal
ruta_archivo <- "C:\\Users\\Analía\\Downloads\\TPFINALUTN\\BASE_MOC.xlsx"
opcam <- read_excel(ruta_archivo)

# Verificamos si el dataset cumple con las dimensiones mínimas (50,000 filas y 10 columnas)
verificar_dimensiones <- function(dataset) {
  dims <- dim(dataset)
  if (dims[1] >= 50000 && dims[2] >= 10) {
    return("El dataset cumple con los requisitos.")
  } else {
    return("El dataset NO cumple con los requisitos.")
  }
}

# Llamamos a la función para verificar dimensiones
verificar_dimensiones(opcam)

# Renombramos las columnas para que los nombres sean más entendibles
opcam <- opcam %>%
  rename(
    Registro = N_COD_ENT,
    FechaInfo = C_F_INF,
    FechaOper = D_F_OPE,
    Tipo = C_TIPO_OPE,
    Boleto = C_BOLETO,
    Contraparte = N_ENT_OPE,
    TipoID = C_TIPO_ID,
    NroID = C_NRO_ID,
    Corresponsal = C_CORRESP,
    InstVtaARS = C_INST_VTA,
    InstCompMonex = C_INST_CPR,
    PaisBenef = C_PAIS_BEN,
    DenoBenef = C_DENO_BEN,
    PaisOrig = C_PAIS_ORI,
    Concepto = C_CONCEPTO,
    FechaEmbarque = D_F_EMBARQ,
    Moneda = C_MONEDA,
    ImporteOriginal = N_IMP_ORIG,
    Pesos = N_IMP_PESO,
    USD = EQUIV_USD,
    Tolerancia = N_TOLERA,
    NroOfic = C_NRO_OFIC,
    Cliente = C_DENO_CLI,
    Sexo = SEXO,
    Edad = EDAD,
    Residencia = C_RESIDENC,
    Condicion = N_COND_CLI,
    Jurisdiccion = CD_JURISDI
  )

# Cargamos otro archivo Excel para complementar la información (entidades)
ruta_listado <- "C:\\Users\\Analía\\Downloads\\TPFINALUTN\\Listado.xlsx"
entidades <- read_excel(ruta_listado)

# Revisamos las primeras filas para asegurarnos de que se ha cargado correctamente
head(entidades)

# Comprobamos si hay valores nulos y duplicados
sum(is.na(opcam))  # Verificamos valores nulos
sum(duplicated(opcam))  # Verificamos duplicados

# Verificamos los nombres de las columnas en ambos datasets
colnames(opcam)
colnames(entidades)

# Convertimos las columnas "Registro" a tipo carácter para hacer coincidir los formatos
opcam$Registro <- as.character(opcam$Registro)
entidades$Registro <- as.character(entidades$Registro)

# Unimos las dos tablas usando la columna "Registro"
tabla_unida <- left_join(opcam, entidades, by = "Registro")

# Planteamos nuestras preguntas de interés
# 1. ¿Cuáles son los registros con el mayor total en pesos?
# 2. ¿Cómo varía el total en pesos por denominación?
# 3. ¿Cómo evoluciona mensualmente el total en USD?
# 4. ¿Existen diferencias significativas entre las categorías "Casa" y "Agencia"?
# 5. ¿Cuál es el monto operado en USD por sexo?

# Empezamos con el análisis de los datos y la creación de tablas y gráficos

# 1. ¿Cuáles son los registros con el mayor total en pesos?
# Total de "Pesos" por "Registro" y "Denominación"
total_pesos_por_registro <- tabla_unida %>%
  group_by(Registro, Denominación) %>%
  summarise(Total_Pesos = sum(Pesos, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(Total_Pesos))

# Formateamos los números
total_pesos_por_registro$Total_Pesos <- scales::comma(total_pesos_por_registro$Total_Pesos)

# Mostramos la tabla con el total en pesos por registro
print(total_pesos_por_registro)

# Preparamos los datos para los gráficos
total_pesos_por_registro$Total_Pesos <- as.numeric(gsub(",", "", total_pesos_por_registro$Total_Pesos))

# Filtramos los 10 registros con más pesos
top_10_pesos_por_registro <- total_pesos_por_registro %>%
  top_n(10, wt = Total_Pesos)

# Paleta de colores para el gráfico
color_palette <- colorRampPalette(c("blue", "green", "red", "orange", "purple", "brown", "pink", "gray", "cyan", "yellow"))(length(unique(top_10_pesos_por_registro$Denominación)))

# Gráfico de barras para los 10 registros con más pesos
gg <- ggplot(data = top_10_pesos_por_registro, aes(x = reorder(Registro, -Total_Pesos), y = Total_Pesos, fill = Denominación)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Pesos por Registro y Denominación (Top 10)", x = "Registro", y = "Total de Pesos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = color_palette)

# Mostramos el gráfico
print(gg)

# Ahora seguimos con los registros con menor cantidad de pesos
bottom_10_pesos_por_registro <- total_pesos_por_registro %>%
  top_n(-10, wt = Total_Pesos)

# Paleta de colores personalizada para este gráfico
color_palette <- colorRampPalette(c("blue", "green", "red", "orange", "purple", "brown", "pink", "gray", "cyan", "yellow"))(length(unique(bottom_10_pesos_por_registro$Denominación)))

# Gráfico de barras para los 10 registros con menos pesos
gg <- ggplot(data = bottom_10_pesos_por_registro, aes(x = reorder(Registro, Total_Pesos), y = Total_Pesos, fill = Denominación)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Pesos por Registro y Denominación (Bottom 10)", x = "Registro", y = "Total de Pesos") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    legend.position = "bottom"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = color_palette)

# Mostramos el gráfico
print(gg)

# 2. ¿Cómo varía el total en pesos por moneda?
# Agrupamos por denominación y sumamos el total en pesos
total_pesos_por_moneda <- tabla_unida %>%
  group_by(Moneda) %>%
  summarise(Total_Pesos = sum(Pesos, na.rm = TRUE)) %>%
  arrange(desc(Total_Pesos))


# Crear un boxplot para ver la variación del total en pesos por denominación
ggplot(tabla_unida, aes(x = Moneda, y = Pesos)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Variación del Total en Pesos por Denominación",
       x = "Denominación", y = "Pesos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Gira las etiquetas de denominación

# 3. ¿Cómo evoluciona mensualmente el total en USD?

# Seguimos con el análisis de USD por mes
tabla_unida$FechaOper <- as.character(tabla_unida$FechaOper)
tabla_unida <- tabla_unida %>%
  mutate(mes = substr(FechaOper, 5, 6))

total_usd_por_mes <- tabla_unida %>%
  group_by(mes) %>%
  summarise(Total_USD = sum(USD, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(mes)

# Gráfico de la evolución mensual en USD
gg <- ggplot(data = total_usd_por_mes, aes(x = mes, y = Total_USD, fill = mes)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Evolución Mensual del Total de USD", x = "Mes", y = "Total de USD") +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set3")

# gráfico
print(gg)


# Agrupamos por categoría y sumamos el total en USDNetos
total_usd_por_categoria <- tabla_unida %>%
  group_by(Categoria) %>%
  summarise(Total_USD = sum(USDNetos, na.rm = TRUE))

# Crear el gráfico de barras para el total de USD por categoría
ggplot(total_usd_por_categoria, aes(x = Categoria, y = Total_USD, fill = Categoria)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Total de USD Operado por Categoría (Casa y Agencia)", 
       x = "Categoría", 
       y = "Total USD Operado") +
  scale_y_continuous(labels = scales::comma) + # Formato de números en miles
  theme_minimal() +
  scale_fill_manual(values = c("Casa" = "blue", "Agencia" = "red")) # Personalizar colores para cada categoría

# Mostrar el gráfico
print(gg)

#5. ¿Cuál es el monto operado en USD por sexo?

# Veo si la columna 'Anulada' esté presente y sea numérica
tabla_unida <- tabla_unida %>%
  mutate(Anulada = as.numeric(Anulada))

# Crear las columnas 'PesosNetos' y 'USDNetos'
tabla_unida <- tabla_unida %>%
  mutate(
    PesosNetos = if_else(Anulada == 1, Pesos, if_else(Anulada == 2, -Pesos, Pesos)),
    USDNetos = if_else(Anulada == 1, USD, if_else(Anulada == 2, -USD, USD))
  )

# Verifico si las columnas se han creado correctamente
head(tabla_unida)


# Calculamos el total de USDNetos por cada sexo
total_usd_por_sexo <- tabla_unida %>%
  group_by(Sexo) %>%
  summarise(Total_USD = sum(USDNetos, na.rm = TRUE))

# Verificamos el resultado
print(total_usd_por_sexo)

# Crear el gráfico de barras para visualizar el monto operado en USD por sexo
ggplot(total_usd_por_sexo, aes(x = Sexo, y = Total_USD, fill = Sexo)) +
  geom_bar(stat = "identity") +
  labs(title = "Monto Operado en USD por Sexo", x = "Sexo", y = "Total USD Operado") +
  theme_minimal()

# Mostramos el gráfico
print(gg)

# Asegúrate de que 'Anulada' esté presente
if (!"Anulada" %in% colnames(tabla_unida)) {
  # Crear la columna 'Anulada' extrayendo un carácter de 'Tipo' o con la lógica que corresponde
  tabla_unida <- tabla_unida %>%
    mutate(Anulada = substr(Tipo, ceiling(nchar(Tipo) / 2), ceiling(nchar(Tipo) / 2)))  # Asegúrate de que esta lógica es la correcta
}

# Verificar que 'Anulada' es numérica
tabla_unida <- tabla_unida %>%
  mutate(Anulada = as.numeric(Anulada))

# Crear las columnas 'PesosNetos' y 'USDNetos'
tabla_unida <- tabla_unida %>%
  mutate(
    PesosNetos = if_else(Anulada == 1, Pesos, if_else(Anulada == 2, -Pesos, Pesos)),
    USDNetos = if_else(Anulada == 1, USD, if_else(Anulada == 2, -USD, USD))
  )


# Ahora calculamos el total de USDNetos por cada sexo
total_usd_por_sexo <- tabla_unida %>%
  group_by(Sexo) %>%
  summarise(Total_USD = sum(USDNetos, na.rm = TRUE))

# Verificar el resultado
print(total_usd_por_sexo)

# Crear el gráfico de torta para visualizar el monto operado en USD por sexo
ggplot(total_usd_por_sexo, aes(x = "", y = Total_USD, fill = Sexo)) +
  geom_bar(stat = "identity", width = 1) + # Usamos geom_bar para generar las porciones
  coord_polar("y") + # Esto convierte el gráfico en una torta
  labs(title = "Distribución del Monto Operado en USD por Sexo") +
  theme_void() + # Eliminamos los elementos de fondo para que parezca una torta
  theme(legend.title = element_blank()) + # Eliminamos el título de la leyenda
  scale_fill_manual(values = c("F" = "pink", "M" = "lightblue")) # Colores personalizados para cada sexo

# gráfico
print(gg)

# Asegurarnos de que la columna 'Edad' sea numérica
tabla_unida <- tabla_unida %>%
  mutate(Edad = as.numeric(Edad))

# Crear el gráfico de dispersión que relacione la edad y los montos operados (USDNetos)
ggplot(tabla_unida, aes(x = Edad, y = USDNetos)) +
  geom_point(alpha = 0.5, color = "blue") + # Los puntos de dispersión
  labs(title = "Relación entre Edad y Montos Operados en USD", 
       x = "Edad", 
       y = "Montos Operados (USDNetos)") +
  theme_minimal() # Tema simple y limpio

# gráfico
print(gg)




