#1.	Realice imputación por regresión en conjunto de datos iris, considere valores faltantes:
#columna 1 (23,24,38,2,35,16,17)
#Columna 3 (5,78,120,89,145,50,78,103,128)

#Cargar el conjunto de datos iris
fname = file.choose()
dset_iris= read.table(fname,sep = ",",dec = ",")

#Creamos una copia
dset_iris_na = dset_iris

# Convertir V1,V2,V3,V4 a tipo numérico
dset_iris_na$V1 <- as.numeric(as.character(dset_iris_na$V1))
dset_iris_na$V2 <- as.numeric(as.character(dset_iris_na$V2))
dset_iris_na$V3 <- as.numeric(as.character(dset_iris_na$V3))
dset_iris_na$V4 <- as.numeric(as.character(dset_iris_na$V4))A


#Colocar los valores faltantes NA en las columnas especificadas
dset_iris_na$V1[c(23,24,38,2,35,16,17)] = NA
dset_iris_na$V3[c(5,78,120,89,145,50,78,103,128)] = NA

#Obtener un modelo de regresión con la variable V1 como dependiente
#y todas las demás como independientes.
# Imputación por regresión para la columna V1
vf1 = which(is.na(dset_iris_na$V1), arr.ind = TRUE)
l1 = lm(V1 ~ V2 + V3 + V4, data = dset_iris_na[complete.cases(dset_iris_na), ])
lmimp1 = predict(l1, dset_iris_na[vf1, ])

#Obtener un modelo de regresión con la variable V3 como dependiente
#y todas las demás como independientes.
# Imputación por regresión para la columna V3
vf3 = which(is.na(dset_iris_na$V3), arr.ind = TRUE)
l3 = lm(V3 ~ V2 +V1 + V4, data = dset_iris_na[complete.cases(dset_iris_na), ])
lmimp3 = predict(l3, dset_iris_na[vf3, ])

# Llenar los valores imputados en el conjunto de datos original
dset_iris_na$V1[vf1] = round(lmimp1, 1)
dset_iris_na$V3[vf3] = round(lmimp3, 1)

# Verificar el resultado
head(dset_iris_na)
head(dset_iris)
dset_iris_na$V1[c(23,24,38,2,35,16,17)]
dset_iris_na$V3[c(5,78,120,89,145,50,78,103,128)]
dset_iris$V1[c(23,24,38,2,35,16,17)]
dset_iris$V3[c(5,78,120,89,145,50,78,103,128)]