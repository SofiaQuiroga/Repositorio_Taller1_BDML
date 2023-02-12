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

#Step 3: regresamos los residuos del segundo paso en los residuos del primer paso
reg3<-lm(logwResidF~maxEducResidF+tiempoTrabajoResidF+estratoResidF+edadResidF+agesqResidF,datos)

##comparamos los coeficientes
stargazer(reg1,reg2,reg3,type="text",digits=7)
#SSR
sum(resid(reg2)^2) ##10806.26
sum(resid(reg3)^2) ##10806.26
##verificamos que los SSR son iguales

#Prueba FWL
r1<-lm(logw~female, datos)
r2<-lm(logw~maxEducativo + tiempoTrabajo +estrato +edad + agesq, datos)
r3<-lm(logw~female+maxEducativo + tiempoTrabajo +estrato + edad+ agesq, datos)
stargazer(r2,r3,type="text",digits=7)
#al omitir la variable "female" los coeficientes son diferentes

#EXPORTAR RESULTADOS
p_load(jtools)
p_load(officer)
p_load(flextable)
install.packages("huxtable")
export_summs(reg1, reg2, scale = TRUE, to.file = "docx", file.name = "regresionesBDML1.docx")
export_summs(reg2, reg3, scale = TRUE, to.file = "docx", file.name = "regresionesBDML1.docx")


#Bootstrap con FWL

#cargamos el paquete boot para hacer el bootstrap
p_load(boot)

#wage gap sin controles
modelo1<-lm(logw~female,datos)
#wage gap condicionado - el mismo modelo que usamos con FWL 
modelo2 <-lm(logw~female+maxEducativo + tiempoTrabajo +estrato + edad+ agesq, datos)
stargazer(modelo1,modelo2,type="text", digits=7)
#coeficiente de la variable female cambia cuando se incluyen los controles

#guardamos los coeficientes en un vector
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
##wage gap: -1.162451 
##error estándar  bootstrap:0.081073   FWL:0.8084172

##BOOTSTRAP CON REGRESIÓN 2 (RESIDUOS)

reg4<-lm(logwResidF~female+maxEducResidF+tiempoTrabajoResidF+estratoResidF+edadResidF+agesqResidF,datos)
#guardamos los coeficientes en un vector
coefsres<- reg4$coef
coefsres
#Coeficientes a escalares
b0res<-coefs[1] #intercepto
b1res<-coefs[2] #female
b2res<-coefs[3] #maxEducativo
b3res<-coefs[4] #tiempoTrabajo
b4res<-coefs[5] #estrato
b5res<-coefs[6] #edad
b6res<-coefs[7] #edad al cuadrado

#estimamos el wage gap condicional
wage_bar<- mean(datos$logw)
wage_gapr<- b1res*wage_bar
wage_gapr
wage_bar

#La brecha salarial es igual

#errores estándar
wp_modelores_fn<-function(data,index,
                        wage_bar=mean(datos$logw)){
  
  
  coefsresd<-lm(logwResidF~female+maxEducResidF+tiempoTrabajoResidF+estratoResidF+edadResidF+agesqResidF,datos, subset=index)$coefficients
  
  
  b1<-coefsresd[2] #coeficiente de la variable female
  
  wage_gapr<- b1*wage_bar
  
  
  #return el gender wage gap condicionado
  return(wage_gapr)
}
wp_modelores_fn(datos,1:nrow(datos))

resultsres<- boot(datos, wp_modelores_fn,R=1000)
resultsres

#el resultado es igual con la descomposición de FWL, el error estándar es 0. 
#error estándar O


#Predicted age-wage profiles 

#Intervalos de confianza
##sexo femenino
IC_inff <- b1*wage_bar - (1.96*0.081073 )
IC_supf <- b1*wage_bar + (1.96*0.081073 )
IC_inff
IC_supf

#sexo masculino
IC_infm <- -b1*wage_bar - (1.96*0.081073 )
IC_supm <- -b1*wage_bar + (1.96*0.081073 )
IC_infm
IC_supm

#Predicciones
#peak ages
#femenino
peak_agef<- -b5/(2*b6)
peak_agef
#43.20267 

#masculino
modelo4 <-lm(salario~sex+ edad+I(edad^2), datos)
coefsm<- modelo4$coefficients
b5m<- coefsm[6]
b6m<- coefsm[7]
peak_agem<- -b5m/(2*b6m)
peak_agem
#53.70463

#Modelo sin interacción entre sexo y edad
modelop <-lm(salario~ female+ edad +I(edad^2), datos)

##Femenino
x_fem <- data.frame(female=1,edad=1:100)
pred_fem<- predict(modelop, newdata=x_fem)
base_f <- data.frame(salario_predicho= pred_fem, x_fem)
##Masculino 
x_masc<-data.frame(female=0,edad=1:100)
pred_masc<- predict(modelop, newdata=x_masc)
base_m <- data.frame(salario_predicho= pred_masc, x_masc)

base <- rbind(base_f, base_m)
#Gráfico

ggplot(base, aes(x=edad, y=salario_predicho, color=as.factor(female) ) ) + geom_line() + theme_bw() +labs(x ="Edad (años)", y = "Salario (predicho)", 
                                                                                                          title= "Edad-salario por sexo sin interacción") 
       
#Modelo con interacción entre sexo y edad
modelop1 <-lm(salario~ female*edad +I(edad^2), datos)

##Femenino
x_fem <- data.frame(female=1,edad=1:100)
pred_fem<- predict(modelop1, newdata=x_fem)
base_f1 <- data.frame(salario_predicho= pred_fem, x_fem)
##Masculino 
x_masc<-data.frame(female=0,edad=1:100)
pred_masc<- predict(modelop1, newdata=x_masc)
base_m1 <- data.frame(salario_predicho= pred_masc, x_masc)

base1 <- rbind(base_f, base_m)
#Gráfico

ggplot(base1, aes(x=edad, y=salario_predicho, color=as.factor(female) ) ) + geom_line() + theme_bw() +labs(x ="Edad (años)", y = "Salario (predicho)", 
                                                                                                          title= "Edad-salario por sexo sin interacción") 



