
##Punto 3
###Genero las variables de la regresión ----------------------------

datos<- datos %>% mutate(edad_2=edad^2, 
                         log_salario=log(salario) )

###La regresión ----------------------------

###Necesito manejar los datos que no tengan "." ni "inf" o "-inf", pues genera problemas al correr laregresión-

nuevos_datos <- datos                               # Duplicate data
nuevos_datos[is.na(nuevos_datos) | nuevos_datos == "Inf"] <- NA

nuevos_datos[is.na(nuevos_datos) | nuevos_datos=="-Inf"]= NA

mod1<-lm(log_salario~edad+edad_2, data = nuevos_datos)
stargazer(mod1,type="text")


#Gráfica de la relación age-earnings

ggplot(data = nuevos_datos , mapping = aes(x = edad , y = log_salario))


ggplot(nuevos_datos, aes(edad, log_salario)) + geom_point(colour = "red")


errores_estandar<-function(nuevos_datos,index ){
  
  #get the coefficients
  coeficientes<-lm(log_salarios~edad+edad_2, nuevos_datos, subset = index)$coefficients
  
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