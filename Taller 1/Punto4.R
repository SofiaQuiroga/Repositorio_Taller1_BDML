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
### maxEducativo, TiempoTrabajo, estrato, edad, edad^2

datos$agesq<- (datos$edad)^2
reg2<- lm(logw ~ female + maxEducativo + tiempoTrabajo +estrato + edad+ agesq , data=datos)
#Step 1: regresamos las variables de X1= {maxEducativo, tiempoTrabajo, estrato, edad, edad^2} en X2= {female} para obtener los residuals
datos<-datos %>% mutate(maxEducResidF=lm(maxEducativo~female,datos)$residuals)
datos<-datos %>% mutate(tiempoTrabajoResidF=lm(tiempoTrabajo~female,datos)$residuals)
datos<-datos %>% mutate(estratoResidF=lm(estrato~female,datos)$residuals)
datos<-datos %>% mutate(edadResidF=lm(edad~female,datos)$residuals)
datos<-datos %>% mutate(agesqResidF=lm(agesq~female,datos)$residuals)
#Step 2: regresamos el logaritmo del salario en X2 para obtener los residuals
datos<-datos %>% mutate(logwResidF=lm(logw~female,datos)$residuals)

#Step 3: regresamos los residuos del primer paso en los residuos del segundo paso
reg3<-lm(logwResidF~maxEducResidF+tiempoTrabajoResidF+estratoResidF+edadResidF+agesqResidF,datos)
stargazer(reg1,reg2,reg3,type="text",digits=7)

#SSR
sum(resid(reg2)^2)
sum(resid(reg3)^2)

#Prueba FWL
r1<-lm(logw~female, datos)
r2<-lm(logw~maxEducativo + tiempoTrabajo +estrato +edad + agesq, datos)
r3<-lm(logw~female+maxEducativo + tiempoTrabajo +estrato + edad+ agesq, datos)
stargazer(r2,r3,type="text",digits=7)
#al omitir la variable "female" los coeficientes son diferentes


#Bootstrap con FWL
p_load(boot)
#wage gap sin controles
modelo1<-lm(logw~female,datos)
#wage gap condicionado - el mismo modelo que usamos con FWL 
modelo2 <-lm(logw~female+maxEducativo + tiempoTrabajo +estrato + edad+ agesq, datos)
stargazer(modelo1,modelo2,type="text")
#coeficiente de la variable female cambia cuando se incluyen los controles

coefs<- modelo2$coef
coefs
#Coeficientes a escalares
b0<-coefs[1] #intercepto
b1<-coefs[2] #female
b2<-coefs[3] #maxEducativo
b3<-coefs[4] #tiempoTrabajo
b4<-coefs[5] #estrato
b5<-coefs[6] #edad
b6<-coefs[7] #edad al cuadrado

#estimamos el wage gap condicional
wage_bar<- mean(datos$logw)
wage_gap<- b1*wage_bar
wage_gap
wage_bar

#errores estándar
wp_modelo2_fn<-function(data,index,
                      wage_bar=mean(datos$logw)){
  
 
  coefs<-lm(logw~female+maxEducativo + tiempoTrabajo +estrato + edad+ agesq, datos, subset=index)$coefficients
  
 
  b1<-coefs[2] #coeficiente de la variable female

  wage_gap<- b1*wage_bar
  
  
  #return el gender wage gap condicionado
  return(wage_gap)
}
wp_modelo2_fn(datos,1:nrow(datos))

results<- boot(datos, wp_modelo2_fn,R=1000)
results
##error estándar  bootstrap:0.081073   FWL:0.8084172

#Predicted age-wage profiles 

