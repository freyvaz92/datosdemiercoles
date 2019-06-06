
setwd("~/Miércoles de datos/05-06-2019")

library(readr)
library(ggmap)
library(hrbrthemes)

data<- readr::read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-06-05/cdmx.csv")

register_google(key="key")  #Registrar la llave
geocode("ciudad de méxico") #Conseguir las coordenadas de Ciudad de México

#Obtener el mapa de GoogleMaps
map<-get_map(location=c(-99.1,19.4), zoom=11,maptype="roadmap")

ggmap(map)      #Visualizar mapa
summary(data)   #Sumario de la base de datos

#Converir a factor la variable tipo_alojamiento
data$tipo_alojamiento<-as.factor(data$tipo_alojamiento)

#Gráficar localización por tipo de alojamiento
png("localización.png",units="in",height = 7, width = 10,res=300) #Crear archivo

ggmap(map)+  
  geom_point(data=data,aes(x=longitud,y=latitud,
                          colour="coral2",alpha=0.12,size=.009))+
  facet_grid(~tipo_alojamiento)+
  theme_ipsum_rc()+
  labs(title="Oferta de Airbnb en la Ciudad de México",
       subtitle = "Localización por tipo de alojamiento (5-06-2019)",
       fill="Frecuencia",
       caption="Elaboración propia con datos de Inside Airbnb 
       (http://insideairbnb.com/get-the-data.html).")+
  theme(legend.position = "none")

dev.off()    #Guardar y cerrar archivo


#Gráficar densidad por tipo de alojamiento
png("tipo_densidad.png",units="in",height = 7, width = 10,res=300) #Crear archivo
ggmap(map)+
  geom_density2d(data=data,
                 aes(x=longitud,y=latitud),
                 size=.4,color="gray37",bins=40, alpha=0.6)+
  scale_fill_gradient(low="cyan4",high ="coral2")+ 
  stat_density2d(data=data,
                 aes(x=longitud,y=latitud, 
                     fill=..level..,alpha=..level..),
                 size=0.0009, bins=60, geom="polygon")+
  facet_grid(~tipo_alojamiento)+
  scale_alpha(range = c(0,.7),guide=F) +
  theme_ipsum_rc()+
  labs(title="Oferta de Airbnb en la Ciudad de México",
       subtitle = "Densidad por tipo de alojamiento (5-06-2019)",
       fill="Frecuencia",
       caption="Elaboración propia con datos de Inside Airbnb 
       (http://insideairbnb.com/get-the-data.html).")

dev.off()   #Guardar y cerrrar archivo


#Graficar densidad de los lugares de alojamiento
png("densidad.png",units="in",height = 9, width = 9,res=300)  #Crear archivo
ggmap(map)+
  stat_density2d(data=data,
                 aes(x=longitud,y=latitud, 
                     fill=..level..,alpha=..level..),
                 size=0.0009, bins=60, geom="polygon")+
  geom_density2d(data=data,
                 aes(x=longitud,y=latitud),
                 size=.4,color="gray37",bins=40, alpha=0.6)+
  scale_fill_gradient(low="cyan4",high ="coral2")+ 
  scale_alpha(range = c(0,.7),guide=F) +
  theme_ipsum_rc()+
  labs(title="Oferta de Airbnb en la Ciudad de México",
       subtitle = "Densidad de los lugares de alojamiento (5-06-2019)",
       fill="Frecuencia",
       caption="Elaboración propia con datos de Inside Airbnb 
       (http://insideairbnb.com/get-the-data.html).")
dev.off()   #Guardar y cerrar archivo
