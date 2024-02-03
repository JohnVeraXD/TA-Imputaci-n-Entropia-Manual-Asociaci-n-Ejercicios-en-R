# Cargar librerías para utilizar funciones y el algoritmo apriori
library(ggplot2)
library(dplyr)
library(arules)

# Cargar el conjunto de datos
fname <- "C:/Users/ANNOUS SUONNA/Downloads/Inteligencia Negocios archivos/iris/iris.data"
iris <- read.csv(fname, header = FALSE)
colnames(iris) <- c("sepalLength", "sepalWidth", "petalLength", "petalWidth", "class")

# Variables necesarios para el ejercicio (sepalLength, petalWidth, petalLength)
iris_req <- select(iris, sepalLength, petalWidth, petalLength)

# Discretizar el conjunto de datos, sin la columna categórica
iris_disc <- discretizeDF(iris_req)

# Convertir el conjunto de datos a formato transaccional
trans_data <- as(iris_disc, "transactions")

# Aplicar algoritmo Apriori
rules <- apriori(trans_data, parameter = list(support = 0.1, confidence = 0.7))

# Convertir reglas a un formato de datos más manejable
rules_df <- as.data.frame(inspect(rules))

# Filtrar reglas relacionadas con la longitud del sépalo
sepalo_rules <- subset(rules_df, grepl("sepalLength", lhs))

# Mostrar las reglas y sus métricas
print(sepalo_rules)

