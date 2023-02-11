#PUNTO 4

#Requerimos pacman para cargar paquetes
require("pacman")
p_load("tidyverse","rio","stargazer")

#Creamos la variable log salario
#sumamos 1 al salario de cada observación para evitar valores indefinidos del logaritmo del salario
datos<- datos %>% mutate(salario= salario+1)
datos$logw <- log10(datos$salario)

#Creamos la variable "female" que toma el valor de 1 si la persona es de sexo femenino y 0 de lo contrario
datos$female <- ifelse(datos$sex==0, 1,0)

#Primera regresión: Log(wage)= B0 + B1Female + u
reg1<- lm(logw ~ female , data=datos)

#Equal Pay for Equal Work
##objetivo: encontrar si hay variación del salario que se explica por ser una persona de sexo femenino

#Introducimos los controles que pueden ser indicadores de características similares de trabajo:
### maxEducativo, TiempoTrabajo, estrato
reg2<- lm(logw ~ female + maxEducativo + tiempoTrabajo +estrato , data=datos)
#Step 1: regresamos las variables de X1= {maxEducativo, tiempoTrabajo, estrato} en X2= {female} para obtener los residuals
datos<-datos %>% mutate(maxEducResidF=lm(maxEducativo~female,datos)$residuals)
datos<-datos %>% mutate(tiempoTrabajoResidF=lm(tiempoTrabajo~female,datos)$residuals)
datos<-datos %>% mutate(estratoResidF=lm(estrato~female,datos)$residuals)

#Step 2: regresamos el logaritmo del salario en X2 para obtener los residuals
datos<-datos %>% mutate(logwResidF=lm(logw~female,datos)$residuals)

#Step 3: regresamos los residuos del primer paso en los residuos del segundo paso
reg3<-lm(logwResidF~maxEducResidF+tiempoTrabajoResidF+estratoResidF,datos)
stargazer(reg1,reg2,reg3,type="text",digits=7)

#SSR
sum(resid(reg2)^2)
sum(resid(reg3)^2)

#Prueba FWL
r1<-lm(logw~female, datos)
r2<-lm(logw~maxEducativo + tiempoTrabajo +estrato, datos)
r3<-lm(logw~female+maxEducativo + tiempoTrabajo +estrato, datos)
stargazer(r2,r3,type="text",digits=7)

corr()
