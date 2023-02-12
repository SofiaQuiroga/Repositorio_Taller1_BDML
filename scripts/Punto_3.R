
##Punto 3

###Paquetes a instalar
p_load("boot")

#Fijamos una semilla para el bootstrap 
set.seed(123)

#Genero las variables de la regresión ----------------------------

datos<- datos %>% mutate(edad_2=edad^2, 
                         log_salario=log(salario) )

#La regresión ----------------------------

#Necesito manejar los datos que no tengan "." ni "inf" o "-inf",pues genera problemas al correr laregresión-

nuevos_datos <- datos                               # Duplicate data
nuevos_datos[is.na(nuevos_datos) | nuevos_datos == "Inf"] <- NA

nuevos_datos[is.na(nuevos_datos) | nuevos_datos=="-Inf"]= NA

#Estimación log-lin
mod1<-lm(log_salario~edad+edad_2, data = nuevos_datos)
y_predict <- predict(mod1, nuevos_datos)
stargazer(mod1,type="text")

#Gráfica de la relación predicha entre edad y ganancias (en logaritmo).
ggplot(nuevos_datos, aes(x = edad, y=y_predict)) + geom_line(color = "blue") + theme_bw() + 
  labs(x ="Edad (años)", y = "Logaritmo del salario (predicho)", 
  title= "Gráfico 1. Perfil estimado para la relación edad-ganancias")

#Necesitamos los coeficientes
mod1<-lm(log_salario~edad+edad_2, data = nuevos_datos)

coefs<-mod1$coef
coefs

#La función age-earnings se maximiza cuando:
#b2_gorro+2b3_gorro*Edad=0 --> Age*=-(b1_gorro)/2_b3_

b0<-coefs[1]
b1<-coefs[2] 
b2<-coefs[3]

#Note que la edad máxima se encuentra con la siguiente fórmula:
edad_optima <- -b1/(2*b2)
  
  peak_age<-function(data,index){
    
    #obtenemos los coeficientes
    coefs<-lm(log_salario~edad+edad_2,nuevos_datos, subset = index)$coefficients
    
    #guerdamos los coeficientes  
    b1<-coefs[2]
    b2<-coefs[3] 
    b4<-coefs[5] 
    
    #calculamos la edad que maximiza el ingreso
    edad_optima <- -b1/(2*b2)
    edad_optima
    
    return(edad_optima)
  }

  results <- boot(nuevos_datos, peak_age,R=1000)
  results
  
  
#Calculamos los Intervalos de Confianza con el E.E del Bootrstap
  
  IC_i <- b1 -1.96*(0.4991073) #Intervalo de confianza inferior
  IC_i
  IC_s <- b1 +1.96*(0.4991073) #intervalo de confianza superior 
  IC_s
  
  #Estimador b1 como porcentaje de la media
  y_promedio<- mean(nuevos_datos$log_salario, na.rm = T)
  y_promedio
  
  b1_porcentaje_media <- (0.058/13.97346)*100
  