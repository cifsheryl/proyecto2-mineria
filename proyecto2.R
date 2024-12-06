install.packages("arules")
library(arules)
install.packages("readxl")
library(readxl)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)



file_path <- "C:\\Users\\Mario CIfuentes\\Downloads\\personas.xlsx"
data <- read_excel(file_path)
View(data)



data2 <- data [, c("P03A02", "areag", "P04A05A", "P06C01")]
View(data2)


data2[] <- lapply(data2, factor)
str(data2)


transactions <- as(data2, "transactions")
summary(transactions)
rules <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.8))
inspect(head(rules))
        

----------------#Arbol de decisiones---------------
# 2. Convertir las variables categóricas en factores
data$P03A02 <- as.factor(data$P03A02)  # Género
data$areag <- as.factor(data$areag)    # Área geográfica
data$P06C01 <- as.factor(data$P06C01)  # Recibió remesas
data$P04A05A <- as.factor(data$P04A05A)    # Nivel educativo alcanzado



# 3. Crear el árbol de decisión
modelo_arbol <- rpart(
  P04A05A ~ P03A02 + areag + P06C01,  # Nivel educativo como objetivo
  data = data,                     # Conjunto de datos
  method = "class",                 # Clasificación
  control = rpart.control(minsplit = 20, cp = 0.01)  # Configuración del árbol
)

rpart.plot(
  modelo_arbol, 
  type = 3, 
  extra = 102, 
  under = TRUE, 
  fallen.leaves = TRUE, 
  box.palette = "Blues"  
)

------------------------------------------------------------------------------------------------
  # Modelo de árbol de decisión para predecir el nivel educativo según género y área geográfica
modelo1 <- rpart(
  P04A05A ~ P03A02 + areag, 
  data = data, 
  method = "class")

# Visualizar el árbol de decisión
rpart.plot(modelo1, type = 3, extra = 102, under = TRUE, fallen.leaves = TRUE, box.palette = "green" )

---------------------------------------------------------------------------------------------
# Modelo de árbol de decisión para predecir si se reciben remesas según nivel educativo y área geográfica
modelo2 <- rpart(P06C01 ~ P04A05A + areag, data = data, method = "class")

# Visualizar el árbol de decisión
rpart.plot(modelo2, type = 3, extra = 102, under = TRUE, fallen.leaves = TRUE, box.palette = "blue")


------------------------------------------------------------------------------------------------------------------
  
   # Modelo de árbol de decisión para predecir si se reciben remesas según género y área geográfica
modelo3 <- rpart(P06C01 ~ P03A02 + areag, data = data, method = "class")

# Visualizar el árbol de decisión
rpart.plot(modelo3, type = 3, extra = 102, under = TRUE, fallen.leaves = TRUE, box.palette = "gray")
---------------------------------------------------------------------------------------------------------------
  # Modelo de árbol de decisión para predecir el nivel educativo según área geográfica
  modelo4 <- rpart(P04A05A ~ areag, data = data, method = "class")

# Visualizar el árbol de decisión
rpart.plot(modelo4, type = 3, extra = 102, under = TRUE, fallen.leaves = TRUE, box.palette = "red")
-------------------------------------------------------------------------------------------------------------
  # Evaluar el modelo con una matriz de confusión
  tabla_confusion <- table(Predicción = predict(modelo1, data, type = "class"), Real = data$P04A05A)
print(tabla_confusion)

# Calcular la precisión
precision <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
print(paste("Precisión del modelo:", round(precision, 2)))
-----------------------------------------------------------------------------------------------------------------

nuevos_datos <- expand.grid(P03A02 = factor(c(1, 2), levels = c(1, 2)), areag = factor(c(1, 2), levels = c(1, 2)))


 
   # Realizar predicciones con el modelo
nuevos_datos <- data.frame(P03A02 = 1, areag = 1)  # Ejemplo de datos de entrada
predicciones <- predict(modelo1, nuevos_datos, type = "class")
print(predicciones)


summary(modelo1)

summary(nuevos_datos)  # Verifica si hay valores NA
