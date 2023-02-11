
##Punto 3

###Paquetes a instalar
p_load("boot")

###Genero las variables de la regresión ----------------------------


datos<- datos %>% mutate(edad_2=edad^2, 
                         log_salario=log(salario) )

###La regresión ----------------------------

###Necesito manejar los datos que no tengan "." ni "inf" o "-inf", pues genera problemas al correr laregresión-

nuevos_datos <- datos                               # Duplicate data
nuevos_datos[is.na(nuevos_datos) | nuevos_datos == "Inf"] <- NA

nuevos_datos[is.na(nuevos_datos) | nuevos_datos=="-Inf"]= NA

#Estimación log-lin
mod1<-lm(log_salario~edad+edad_2, data = nuevos_datos)
y_predict <- predict(mod1, nuevos_datos)
stargazer(mod1,type="text")

#Gráfica de la relación predicha entre edad y ganancias (en logaritmo).
ggplot(nuevos_datos, aes(x = edad, y=y_predict)) + geom_line() + theme_bw()


#Estimacion en niveles 
mod2<-lm(salario~edad+edad_2, data = nuevos_datos)
salario_predicho <- predict(mod2, nuevos_datos)
stargazer(mod2,type="text")
ggplot(datos, aes(x = edad, y=salario_predicho)) + geom_line() + theme_bw()


coefs<-mod1$coef
coefs

#La función age-earnings se maximiza cuando:
#b2_gorro+2b3_gorro*Edad=0 --> Age*=-(b1_gorro)/2_b3_

b0<-coefs[1]
b1<-coefs[2] 
b2<-coefs[3] 

edad_optima <- -b1/(2*b2)


#De forma automatizada 

result  <- with(nuevos_datos,nuevos_datos[y_predict==max(y_predict),])
result

errores_estandar<-function(nuevos_datos,index ){
  
  #get the coefficients
  coeficientes<-lm(log_salario~edad+edad_2, nuevos_datos, subset = index)$coefficients
  
  
  #put the coefficients in scalars  
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3] 
  
  #calculando la predicción de age-earningg
  age_earnings<-b1+b2*edad+b3*edad_2
  
  #return estimated earnings
  return(earnings_estimated)
}

results <- boot(nuevos_datos, errores_estandar,R=1000)
results


##Nuevo intento: tratando de encintrar la función a maximoxar 

f<- function (edad) (edad + I(edad)^2)
  
derivada <- lm(0~b1+2*b2*edad)

#Mi función
mod1<-lm(log_salario~edad+edad_2, data = nuevos_datos)

age_bar <- mean(nuevos_datos$edad)
age_max <- (-b1/(2*b2))
age_max

boot_age_max <- boot(nuevos_datos, age_max,R=100)

#Donde edad y edad_2 toman los valores: 
#edad:  b1 ; edad_2:b2



eta_mod2_fn<-function(nuevos_datos,index){
  
  #get the coefficients
  coefs<-lm(log_salario~edad+edad_2, data = nuevos_datos, subset = index)$coefficients
  
  #put the coefficients in scalars  
  b1<-coefs[1]
  b2<-coefs[2] 
  
  #calculate the elasticity of demand
  age_max <- (-b1/(2*b2))
  
  #return the elasticty of demand
  return(age_max)
}

results <- boot(nuevos_datos,eta_mod2_fn,R=1000)
results




coefs<-lm(log_salario~edad+edad_2, data = nuevos_datos)$coefficients
b0<-coefs[1]
b1<-coefs[2] 
b2<-coefs[3] 

maximizar <- function(edad) {b0 + b1*edad + b2*(edad^2)}
maximo <- optimize(maximizar, interval = c(18,93), maximum=T)
edad_maxima <- as.numeric(optimize(maximizar, interval = c(18,93), maximum=T)[1])

mod2_fn <- function(data, index, 
                    edad_maxima = as.numeric(optimize(maximizar, interval = c(18,93), maximum=T)[1])) {
  
  coefs <- lm(log_salario~edad+edad_2, data = nuevos_datos, subset = index)$coefficients
  
  b0<-coefs[1]
  b1<-coefs[2] 
  b2<-coefs[3]
  
  age_earnings <- b0 + b1*edad_maxima + b2*(edad_maxima^2)
  
  return(age_earnings)
}

results <- boot(nuevos_datos, mod2_fn, R=1000)
results

