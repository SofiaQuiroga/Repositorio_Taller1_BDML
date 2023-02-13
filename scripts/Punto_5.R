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
mse1 <- with(test, round(mean((log_salario - model1)^2),3))

# Modelo complejo para primer modelo Edad
model2 <- lm(log_salario ~ edad + edad_2 + poly(tiempoTrabajo,2,raw=TRUE) + 
                  I(edad*tiempoTrabajo) + I(edad*(tiempoTrabajo^2)) +
                  poly(maxEducativo,2,raw=TRUE) + 
                  I(edad*maxEducativo) + I(edad*(maxEducativo^2)) + 
                  sex + I(edad*sex) + factor(estrato), data=train)

stargazer(model2, type="text")

test$model2 <- predict(model2, newdata = test)
mse2 <- with(test, round(mean((log_salario - model2)^2),3))

# Modelo normal Género 
model3 <- lm(log_salario ~ sex, data = train)
stargazer(model3, type="text")

test$model3 <- predict(model3, newdata = test)
mse3 <- with(test, round(mean((log_salario - model3)^2),3))

# Modelo Género con Variables de control
model4 <- lm(log_salario ~ sex + maxEducativo + tiempoTrabajo + factor(estrato) + edad + edad_2, data = train)
stargazer(model4, type="text")

test$model4 <- predict(model4, newdata = test)
mse4 <- with(test, round(mean((log_salario - model4)^2),3))

# Modelo complejo para segundo modelo Género
model5 <- lm(log_salario ~ sex + poly(tiempoTrabajo,2,raw=TRUE) + 
                  I(sex*tiempoTrabajo) + I(sex*(tiempoTrabajo^2)) +
                  poly(maxEducativo,2,raw=TRUE) + 
                  I(sex*maxEducativo) + I(sex*(maxEducativo^2)) + 
                  edad + + edad_2 + I(edad*sex) + factor(estrato), data=train)

stargazer(model5, type="text")

test$model5 <- predict(model5, newdata = test)
mse5 <- with(test, round(mean((log_salario - model5)^2),3))

# Tabla comparación Resultados
stargazer(model1, model2, model3, model4, model5, type="text")

# Tabla comparación Errores de Predicción 
mse <- c(mse1, mse2, mse3, mse4, mse5)
errores <- data.frame(Modelos = factor(c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", "Modelo 5"), ordered=TRUE), MSE=(mse))
errores

# Distribución de los errores 
plot(hist((test$log_salario - test$model2)^2), 
     main ="Gráfico 5.1. Distribución errores de predicción", 
     xlab = "Error de predicción MSE", 
     ylab = "Frecuencia", 
     col="#69b3a2")

# LOOCV Primer Modelo
library(caret, boot)
set.seed(3312)

ctrl <- trainControl(method = "LOOCV", p = 0.7)

model_loocv1 <- train(log_salario ~ edad + edad_2 + poly(tiempoTrabajo,2,raw=TRUE) + 
                        I(edad*tiempoTrabajo) + I(edad*(tiempoTrabajo^2)) +
                        poly(maxEducativo,2,raw=TRUE) + 
                        I(edad*maxEducativo) + I(edad*(maxEducativo^2)) + 
                        sex + I(edad*sex) + factor(estrato), data = nuevos_datos, method = "lm", trControl = ctrl)

model_loocv1

# LOOCV Segundo Modelo
set.seed(3312)

ctrl <- trainControl(method = "LOOCV", p = 0.7)

model_loocv2 <- train(log_salario ~ sex + poly(tiempoTrabajo,2,raw=TRUE) + 
                        I(sex*tiempoTrabajo) + I(sex*(tiempoTrabajo^2)) +
                        poly(maxEducativo,2,raw=TRUE) + 
                        I(sex*maxEducativo) + I(sex*(maxEducativo^2)) + 
                        edad + + edad_2 + I(edad_2*sex) + factor(estrato), data = nuevos_datos, method = "lm", trControl = ctrl)

model_loocv2

