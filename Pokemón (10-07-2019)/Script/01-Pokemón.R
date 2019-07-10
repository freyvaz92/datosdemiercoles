#########################################
########  ANÁLISIS DE POKÉMON  ##########
#####  Francisco Reyes-Vázquez  #########
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


#Crear el índice
data<-data%>%
  mutate(index=(z1+z2+z3+z4+z5+z6)/6)

#Explorar índice
summary(data$index)

#-------------------------------------------------
#Calcular el promedio del índice por tipo de pokemón 
data%>%
  group_by(tipo_1)%>%
  summarise(index=mean(index))->tab_index

#Graficar
png("IPG_tipo.png",units="in",height = 7, width = 9,res=300)
ggplot(tab_index,aes(x=reorder(tipo_1,-index),y=index, fill=index))+
  geom_bar(stat="identity",color="white")+
  scale_fill_viridis_c(option = "magma")+
  labs(y="Índice (-inf= débil y +inf= poderoso)",x="",fill="IPG",
       title = "Índice de Poder Global (IPG) por tipo de pokémon",
       caption = "Fuente: elaboración propia con datos de Kaggle facilitados por r4ds.
       El índice es producto del promedio de las estadísticas de batalla estandarizadas de 800 pokémon's.")+
  theme_modern_rc()+
  theme(axis.text.x=element_text(angle=90, vjust=0.3))
dev.off()  
#-----------------------------------------------------------

#Calcular el promedio del índice por generación de pokemón
data$generacion<-as.factor(data$generacion)

data%>%
  group_by(generacion)%>%
  summarise(index=mean(index))->tab_genera

#Graficar
png("IPG_generación.png",units="in",height = 7, width = 9,res=300)
ggplot(tab_genera,aes(x=generacion,y=index, fill=index))+
  geom_bar(stat="identity", color="white")+
  scale_fill_viridis_c(option="magma")+
  labs(x="Generación de Pokemón",
       y="Índice (-inf= débil y +inf= poderoso)",
       fill="IPG",
       title = "Índice de Poder Global (IPG) por generación de pokémon",
       caption = "Fuente: elaboración propia con datos de Kaggle facilitados por r4ds.
       El índice es producto del promedio de las estadísticas de batalla estandarizadas de 800 pokémon's.")+
  theme_modern_rc()
dev.off()
#----------------------------------------------------------------

#Top 10 de los pokémon's legendarios más poderosos

table(data$es_legendario)   #Identificar cuandos son legendarios y cuantos no

legen_data<-data%>%
  filter(es_legendario=="VERDADERO")   #Filtrar a los pokémon's legendarios

#Estadarizar las variables 
legen_data$z1<-scale(legen_data$puntos_vida)   
legen_data$z2<-scale(legen_data$ataque)
legen_data$z3<-scale(legen_data$defensa)
legen_data$z4<-scale(legen_data$fuerza_especial_ataque)
legen_data$z5<-scale(legen_data$fuerza_especial_defensa)
legen_data$z6<-scale(legen_data$velocidad)

#Crear nuevo índice (IPL)
legen_data<-legen_data%>%
  mutate(index=(z1+z2+z3+z4+z5+z6)/6)

#Seleccionar el top 10 de legendarios más poderosos 
legen_data%>%
  filter(index>=0.35)->top_legen

#Gráficar
png("IPL_legendarios_nombre.png",units="in",height = 7, width = 9,res=300)
ggplot(top_legen,aes(x=reorder(nombre_ingles,-index),y=index,fill=index))+
  geom_bar(stat="identity", color="white")+
  coord_flip()+
  scale_fill_viridis_c(option = "magma")+
  labs(y="Índice (-inf= débil y +inf= poderoso)",
       x="Nombre del pokémon",
       fill="IPL",
       title = "Índice de Poder Legendario (IPL) por pokémon",
       subtitle = "Los 10 pokémo's más fuertes",
       caption = "Fuente: elaboración propia con datos de Kaggle facilitados por r4ds.
       El índice es producto del promedio de las estadísticas de batalla estandarizadas de 66 pokémon's 
       considerados legendarios.")+
  theme_modern_rc()
dev.off()  
