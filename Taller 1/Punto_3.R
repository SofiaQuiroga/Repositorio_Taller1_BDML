
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
