
setwd("~/Mi�rcoles de datos/Vino (12-06-2019)")

library(tidyverse)
library(hrbrthemes)
library(broom)

vinos<-readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-12/vinos.csv")

#fILTRAR 
vinos%>%
  filter(pais=="M�xico" | pais=="Estados Unidos")->vinos2


#Precio promedio de los vinos por pa�s
vinos2%>%
  group_by(pais)%>%
  summarize(media=mean(precio,na.rm = TRUE))->precio

#Calificaci�n promedio de los vinos por pa�s
vinos2%>%
  group_by(pais)%>%
  summarize(media=mean(puntos,na.rm = TRUE))->puntos


#Graficar el precio promedio por pais 
png("precio_vino.png",units="in",height = 9, width = 9,res=300)
ggplot(precio,aes(x=pais,y=media,fill=pais))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(media,2)),position=position_stack(vjust = 0.9))+
  scale_y_continuous(limits = c(0,40))+
  labs(fill="",x="",y="Precio promedio (d�lares)",
       title="Precio promedio por botella de vino en Estados Unidos y M�xico",
       subtitle="Actualizaci�n del 15 de junio del 2017",
       caption ="Fuente: Elaboraci�n propia con datos porporcionados por @R4ds.")+
  theme_ipsum_tw()+
  theme(legend.position = "none")
dev.off()

#Graficar la calificaci�n promedio por pa�s
png("puntos_vino.png",units="in",height = 9, width = 9,res=300)
ggplot(puntos,aes(x=pais,y=media,fill=pais))+
  geom_bar(stat="identity")+
  geom_text(aes(label=round(media,2)),position=position_stack(vjust = 0.9))+
  scale_y_continuous(limits = c(0,100))+
  labs(fill="",x="",y="Precio promedio (d�lares)",
       title="Calificaci�n promedio por botella de vino en Estados Unidos y M�xico",
       subtitle="Actualizaci�n del 15 de junio del 2017",
       caption ="Fuente: Elaboraci�n propia con datos porporcionados por @R4ds.
La calificaci�n es una puntuaci�n con escala de 1 a 100.")+
  theme_ipsum_tw()+
  theme(legend.position="none")
dev.off()

#Filtrar por pecio menor de 300 d�lares
vinos3<-filter(vinos2,precio<=300)

#Crear modelo
mod<-lm(precio~puntos+factor(pais),data=vinos3)
summary(mod)
augment(mod)


#Crear grafico de la relaci�n entre precio y calificaci�n
png("relaci�n_vino.png",units="in",height = 8, width = 8,res=300)
ggplot(vinos3,aes(x=puntos,y=precio))+
  geom_point(alpha=0.3)+
  geom_line(data=augment(mod),aes(y=.fitted,color=factor.pais.))+
  labs(color="")+
  theme_ipsum_tw()+
  labs(title="Precio y calificaci�n del vino, 2017",
       subtitle = "Relaci�n por pa�s",
       x="Calificaci�n",
       caption ="Fuente: Elaboraci�n propia con datos porporcionados por @R4ds.
       La informaci�n empleada para este gr�fico est� exenta de algunos valores extremos (>300).")
dev.off()



