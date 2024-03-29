#########################################
########  AN�LISIS DE POK�MON  ##########
#####  Francisco Reyes-V�zquez  #########
############  10-07-2019  ###############
#########################################



#Cargar librerias
library(tidyverse)
library(hrbrthemes)

#Importar la base de datos
data<- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-10/pokemon.csv")

#Estandarizar las variables
data$z1<-scale(data$puntos_vida)
data$z2<-scale(data$ataque)
data$z3<-scale(data$defensa)
data$z4<-scale(data$fuerza_especial_ataque)
data$z5<-scale(data$fuerza_especial_defensa)
data$z6<-scale(data$velocidad)


#Crear el �ndice
data<-data%>%
  mutate(index=(z1+z2+z3+z4+z5+z6)/6)

#Explorar �ndice
summary(data$index)

#-------------------------------------------------
#Calcular el promedio del �ndice por tipo de pokem�n 
data%>%
  group_by(tipo_1)%>%
  summarise(index=mean(index))->tab_index

#Graficar
png("IPG_tipo.png",units="in",height = 7, width = 9,res=300)
ggplot(tab_index,aes(x=reorder(tipo_1,-index),y=index, fill=index))+
  geom_bar(stat="identity",color="white")+
  scale_fill_viridis_c(option = "magma")+
  labs(y="�ndice (-inf= d�bil y +inf= poderoso)",x="",fill="IPG",
       title = "�ndice de Poder Global (IPG) por tipo de pok�mon",
       caption = "Fuente: elaboraci�n propia con datos de Kaggle facilitados por r4ds.
       El �ndice es producto del promedio de las estad�sticas de batalla estandarizadas de 800 pok�mon's.")+
  theme_modern_rc()+
  theme(axis.text.x=element_text(angle=90, vjust=0.3))
dev.off()  
#-----------------------------------------------------------

#Calcular el promedio del �ndice por generaci�n de pokem�n
data$generacion<-as.factor(data$generacion)

data%>%
  group_by(generacion)%>%
  summarise(index=mean(index))->tab_genera

#Graficar
png("IPG_generaci�n.png",units="in",height = 7, width = 9,res=300)
ggplot(tab_genera,aes(x=generacion,y=index, fill=index))+
  geom_bar(stat="identity", color="white")+
  scale_fill_viridis_c(option="magma")+
  labs(x="Generaci�n de Pokem�n",
       y="�ndice (-inf= d�bil y +inf= poderoso)",
       fill="IPG",
       title = "�ndice de Poder Global (IPG) por generaci�n de pok�mon",
       caption = "Fuente: elaboraci�n propia con datos de Kaggle facilitados por r4ds.
       El �ndice es producto del promedio de las estad�sticas de batalla estandarizadas de 800 pok�mon's.")+
  theme_modern_rc()
dev.off()
#----------------------------------------------------------------

#Top 10 de los pok�mon's legendarios m�s poderosos

table(data$es_legendario)   #Identificar cuandos son legendarios y cuantos no

legen_data<-data%>%
  filter(es_legendario=="VERDADERO")   #Filtrar a los pok�mon's legendarios

#Estadarizar las variables 
legen_data$z1<-scale(legen_data$puntos_vida)   
legen_data$z2<-scale(legen_data$ataque)
legen_data$z3<-scale(legen_data$defensa)
legen_data$z4<-scale(legen_data$fuerza_especial_ataque)
legen_data$z5<-scale(legen_data$fuerza_especial_defensa)
legen_data$z6<-scale(legen_data$velocidad)

#Crear nuevo �ndice (IPL)
legen_data<-legen_data%>%
  mutate(index=(z1+z2+z3+z4+z5+z6)/6)

#Seleccionar el top 10 de legendarios m�s poderosos 
legen_data%>%
  filter(index>=0.35)->top_legen

#Gr�ficar
png("IPL_legendarios_nombre.png",units="in",height = 7, width = 9,res=300)
ggplot(top_legen,aes(x=reorder(nombre_ingles,-index),y=index,fill=index))+
  geom_bar(stat="identity", color="white")+
  coord_flip()+
  scale_fill_viridis_c(option = "magma")+
  labs(y="�ndice (-inf= d�bil y +inf= poderoso)",
       x="Nombre del pok�mon",
       fill="IPL",
       title = "�ndice de Poder Legendario (IPL) por pok�mon",
       subtitle = "Los 10 pok�mo's m�s fuertes",
       caption = "Fuente: elaboraci�n propia con datos de Kaggle facilitados por r4ds.
       El �ndice es producto del promedio de las estad�sticas de batalla estandarizadas de 66 pok�mon's 
       considerados legendarios.")+
  theme_modern_rc()
dev.off()  
