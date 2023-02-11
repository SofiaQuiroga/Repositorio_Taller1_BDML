## Punto 5
# Establecer la semilla
set.seed(3312)

# Dividir los datos en la muestra de entrenamiento (70%) y prueba (30%)
nuevos_datos <- na.omit(nuevos_datos)
sample <- sample(c(TRUE, FALSE), nrow(nuevos_datos), replace = TRUE, prob = c(0.7, 0.3))
sum(sample)/nrow(nuevos_datos)

# Generar la partición de los datos 
train <- nuevos_datos[sample, ]
test <- nuevos_datos[!sample, ]

# Modelo normal Edad 
model1 <- lm(log_salario ~ edad + edad_2, data = train)
stargazer(model1, type="text")

test$model1 <- predict(model1, newdata = test)
mse11 <- with(test, round(mean((log_salario - model1)^2),3))

# Modelo complejo para primer modelo Edad
modelcom1 <- lm(log_salario ~ edad + edad_2 + poly(tiempoTrabajo,2,raw=TRUE) + 
                  I(edad*tiempoTrabajo) + I(edad*(tiempoTrabajo^2)) +
                  poly(maxEducativo,2,raw=TRUE) + 
                  I(edad*maxEducativo) + I(edad*(maxEducativo^2)) + 
                  sex + I(edad*sex) + factor(estrato), data=train)

stargazer(modelcom1, type="text")

test$modelcom1 <- predict(modelcom1, newdata = test)
mse1 <- with(test, round(mean((log_salario - modelcom1)^2),3))

# Modelo normal Género 
model2 <- lm(log_salario ~ sex, data = train)
stargazer(model2, type="text")

test$model2 <- predict(model2, newdata = test)
mse22 <- with(test, round(mean((log_salario - model2)^2),3))

# Modelo complejo para segundo modelo Género
modelcom2 <- lm(log_salario ~ sex + poly(tiempoTrabajo,2,raw=TRUE) + 
                  I(sex*tiempoTrabajo) + I(sex*(tiempoTrabajo^2)) +
                  poly(maxEducativo,2,raw=TRUE) + 
                  I(sex*maxEducativo) + I(sex*(maxEducativo^2)) + 
                  edad + + edad_2 + I(edad_2*sex) + factor(estrato), data=train)

stargazer(modelcom2, type="text")

test$modelcom2 <- predict(modelcom2, newdata = test)
mse2 <- with(test, round(mean((log_salario - modelcom2)^2),3))

# Tabla comparación Errores de Predicción 
mse <- c(mse11, mse1, mse22, mse2)
errores <- data.frame(Modelos = factor(c("Modelo 1", "Modelo Complejo 1", "Modelo 2", "Modelo Complejo 2"), ordered=TRUE), MSE=(mse))
errores

# Distribución de los errores 
plot(hist((test$log_salario - test$model1)^2))
plot(hist((test$log_salario - test$modelcom1)^2))
plot(hist((test$log_salario - test$model2)^2))
plot(hist((test$log_salario - test$modelcom2)^2))

# LOOCV Primer modelo
library(caret)

control <- trainControl(method = "LOOCV", p = 0.7)
set.seed(3312)
modelcom11 <- train(log_salario ~ edad + edad_2 + poly(tiempoTrabajo,2,raw=TRUE) + 
                      I(edad*tiempoTrabajo) + I(edad*(tiempoTrabajo^2)) +
                      poly(maxEducativo,2,raw=TRUE) + 
                      I(edad*maxEducativo) + I(edad*(maxEducativo^2)) + 
                      sex + I(edad*sex) + factor(estrato), data = nuevos_datos, method = "lm", trControl = control)
