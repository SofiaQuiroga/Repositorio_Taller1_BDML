
##Punto 3
###Genero las variables de la regresión ----------------------------

datos<- datos %>% mutate(age_2=age^2, 
                         log_income=log(ingtot) )

###La regresión ----------------------------

nuevos_datos <- datos                               # Duplicate data
nuevos_datos[is.na(nuevos_datos) | nuevos_datos == "Inf"] <- NA

nuevos_datos[is.na(nuevos_datos) | nuevos_datos=="-Inf"]= NA

mod1<-lm(log_income~age+age_2, data = nuevos_datos)
stargazer(mod1,type="text")
