install.packages("arules")
library(arules)
install.packages("readxl")
library(readxl)

install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

install.packages("arulesViz")
library(arulesViz)


install.packages("randomForest")
install.packages("caret")
library(randomForest)
library(caret)

install.packages("parallelly")




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

summary(nuevos_datos)


  
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")

plot(rules, method = "graph", engine = "igraph", control = list(type = "items"))

plot(rules, method = "matrix", measure = c("lift", "confidence"))


strong_rules <- subset(rules, lift > 1.5 & confidence > 0.8)
inspect(strong_rules)


#RANDOM FOREST
# Preparar los datos
data2[] <- lapply(data2, factor)  # Convertir variables en factores
set.seed(123)  # Para reproducibilidad

# Dividir en entrenamiento y prueba (70%-30%)
trainIndex <- createDataPartition(data2$P06C01, p = 0.7, list = FALSE)
train_data <- data2[trainIndex, ]
test_data <- data2[-trainIndex, ]


str(train_data$P04A05A)

# Agregar un nuevo nivel "desconocido" al factor
levels(train_data$P04A05A) <- c(levels(train_data$P04A05A), "desconocido")

# Reemplazar los valores NA con "desconocido"
train_data$P04A05A[is.na(train_data$P04A05A)] <- "desconocido"


colSums(is.na(train_data))

train_data <- na.omit(train_data)

test_data$P03A02 <- as.factor(test_data$P03A02)
test_data$areag <- as.factor(test_data$areag)
test_data$P04A05A <- as.factor(test_data$P04A05A)


levels(test_data$P03A02) <- levels(train_data$P03A02)
levels(test_data$areag) <- levels(train_data$areag)
levels(test_data$P04A05A) <- levels(train_data$P04A05A)

## Predicción 1: ¿Recibe remesas?

# Entrenar el modelo Random Forest
rf_model1 <- randomForest(P06C01 ~ P03A02 + areag + P04A05A, data = train_data, ntree = 100, importance = TRUE)
print(rf_model1)

pred1 <- predict(rf_model1, test_data)

summary(train_data$P04A05A)

# Realizar predicciones
pred1 <- predict(rf_model1, test_data)

# Evaluar el modelo
confusionMatrix(pred1, test_data$P06C01)

# Importancia de las variables
varImpPlot(rf_model1)

##Prediccion 2:  Nivel Educativo Más Alto Aprobado

# Entrenar el modelo Random Forest

table(train_data$P04A05A)

train_data$P04A05A <- droplevels(train_data$P04A05A)

table(train_data$P04A05A)

rf_model2 <- randomForest(P04A05A ~ P03A02 + areag + P06C01, data = train_data, ntree = 100, importance = TRUE)
print(rf_model2)

# Realizar predicciones
pred2 <- predict(rf_model2, test_data)

levels(test_data$P04A05A)
levels(pred2)


common_levels <- union(levels(test_data$P04A05A), levels(pred2))

test_data$P04A05A <- factor(test_data$P04A05A, levels = common_levels)
pred2 <- factor(pred2, levels = common_levels)



levels(test_data$P04A05A) <- levels(pred2)


# Evaluar el modelo
confusionMatrix(pred2, test_data$P04A05A)

# Importancia de las variables
varImpPlot(rf_model2)


----
  str(train_data)
str(new_data1)

str(new_data1)


new_data1$variable_name <- as.factor(new_data1$variable_name)
nrow(new_data1)


if (length(new_data1$variable_name) > 0) {
  new_data1$variable_name <- as.factor(new_data1$variable_name)
} else {
  cat("La columna variable_name está vacía.")
}

new_data1$variable_name[is.na(new_data1$variable_name)] <- "Desconocido"  # O cualquier otro valor predeterminado
new_data1$variable_name <- as.factor(new_data1$variable_name)

if (!"variable_name" %in% colnames(new_data1)) {
  new_data1$variable_name <- rep("Desconocido", nrow(new_data1))
}

sum(is.na(new_data1$variable_name))


if(length(new_data1$variable_name) > 0) {
  new_data1$variable_name[is.na(new_data1$variable_name)] <- "Desconocido"
}

new_data1$P03A02 <- factor(new_data1$P03A02, levels = levels(train_data$P03A02))
new_data1$areag <- factor(new_data1$areag, levels = levels(train_data$areag))
new_data1$P04A05A <- factor(new_data1$P04A05A, levels = levels(train_data$P04A05A))

  
new_data1 <- data.frame(P03A02 = factor("2", levels = c("1", "2")), 
                          areag = factor("2", levels = c("1", "2")), 
                          P04A05A = factor("2", levels = levels(data2$P04A05A)))
predict(rf_model1, new_data1)


# Visualizar la importancia de las variables
importance(rf_model1)  
varImpPlot(rf_model1) 


# Visualizar uno de los árboles del modelo
plot(rf_model1, main = "Árboles del Modelo de Bosque Aleatorio")

