##Taller 1

##Punto 2

require(pacman)
p_load(tidyverse,rvest) ##cargamos los paquetes.

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
