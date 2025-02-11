##TALLER 1

### Clean the workspace -----------------------------------------------------
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo


##Punto 2

###Instalamos los paquetes -----------------------------------------------------

require("pacman")
p_load("tidyverse","rvest", "stargazer","vtable") ##cargamos los paquetes.

###Hacemos el web scraping para acceder a los datos ----------------------------

#Tabla 1
Link1<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html")
Tabla1<-html_table(Link1)
Tabla1<-as.data.frame(Tabla1)

#Tabla 2
Link2<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html")
Tabla2<-html_table(Link2)
Tabla2<-as.data.frame(Tabla2)
BaseA<-merge(Tabla1,Tabla2, all = TRUE)

#Tabla 3
Link3<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html")
Tabla3<-html_table(Link3)
Tabla3<-as.data.frame(Tabla3)
BaseA<-merge(BaseA,Tabla3, all = TRUE)

#Tabla 4
Link4<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html")
Tabla4<-html_table(Link4)
Tabla4<-as.data.frame(Tabla4)
BaseA<-merge(BaseA,Tabla4, all = TRUE)

#Tabla 5
Link5<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html")
Tabla5<-html_table(Link5)
Tabla5<-as.data.frame(Tabla5)
BaseA<-merge(BaseA,Tabla5, all = TRUE)

#Tabla 6
Link6<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html")
Tabla6<-html_table(Link6)
Tabla6<-as.data.frame(Tabla6)
BaseA<-merge(BaseA,Tabla6, all = TRUE)

#Tabla 7
Link7<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html")
Tabla7<-html_table(Link7)
Tabla7<-as.data.frame(Tabla7)
BaseA<-merge(BaseA,Tabla7, all = TRUE)

#Tabla 8
Link8<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html")
Tabla8<-html_table(Link8)
Tabla8<-as.data.frame(Tabla8)
BaseA<-merge(BaseA,Tabla8, all = TRUE)

#Tabla 9
Link9<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html")
Tabla9<-html_table(Link9)
Tabla9<-as.data.frame(Tabla9)
BaseA<-merge(BaseA,Tabla9, all = TRUE)

#Tabla 10
Link10<-read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html")
Tabla10<-html_table(Link10)
Tabla10<-as.data.frame(Tabla10)
BaseA<-merge(BaseA,Tabla10, all = TRUE)

##Limpiar la Base -----------------------------------------------------

#LUIS CARLOS
BaseB<-BaseA[BaseA$age>=18, ] #Base con personas mayores a 18 a�os
BaseB<-BaseB[BaseB$ocu==1,]



#variables principales
BaseC<-select(BaseB,age,clase,informal,p6090,sex,totalHoursWorked,y_salary_m,y_salary_m_hu,estrato1,
              p6210,p6426,depto,ingtot,clase)
#Renombramos las variables
BaseC<- BaseC %>%  
  rename(edad=age,
         salud=p6090,
         sexo=sex,
         Horas_trabajadas=totalHoursWorked,
         Salario_mensual=y_salary_m,
         Salario_mensual_horas=y_salary_m_hu,
         estrato=estrato1,
         maxEducativo = p6210,
         tiempoTrabajo = p6426,
         )
BaseD<-na.omit(BaseC)
#Estadisticas descriptivas
sumtable(BaseD)


#SOF�A 
#Limpieza base de datos
datos <- select(BaseA, directorio, sex, estrato1, age, p6210, p6426, p6090, ingtot, clase, ocu)
datos <- na.omit(datos)
datos <- datos %>% filter(age>17, ocu==1)
summary(datos)


#Cambiando nombre de las variables
datos <- datos %>% rename(estrato = estrato1,
                          edad = age, 
                          maxEducativo = p6210, 
                          tiempoTrabajo = p6426,
                          salud = p6090, 
                          salario = ingtot)
summary(datos)
#Estadisticas descriptivas
sumtable(datos)
