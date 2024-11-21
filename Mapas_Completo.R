# Taller Aprendiendo a Hacer Mapas en R
# Imparte: Dra. Aline Pingarroni FES-Iztacala UNAM
# Organiza: Dra. Driselda Sánchez Aguirre y Azalea Reyes R-Ladies Queretaro
# Lugar: Escuela Nacional de Ciencias de la Tierra UNAM
# Fecha: 21 de Noviembre 2024

# Paso 1 Librerias ####
# Vector de paquetes necesarios
paquetes <- c("ggplot2",    # Visualización de datos
              "sf",         # Archivos vectoriales
              "RColorBrewer", # Paletas de colores
              "terra",      # Maneja archivos raster
              "viridis",    # Contiene paletas de colores
              "dbplyr",     # Manipulación de bases de datos
              "cowplot",    # Unión de gráficas
              "ggspatial",  # Rosa, la escala
              "magick",     # Fotos dentro de gráficos
              "jpeg",       # Carga imágenes
              "patchwork")  # Une gráficas

# Instalar paquetes si no están instalados
#install.packages(paquetes, dependencies = TRUE)

# Cargar librerias
library(ggplot2) 
library(sf) 
library(RColorBrewer)
library(terra)
library(viridis) 
library(dbplyr) 
library(cowplot) 
library(ggspatial) 
library(magick) 
library(jpeg) 
library(patchwork) 


# Paso 2. Importar archivos vectoriales ####
# Cargar la capa de México #####
Mexico <- st_read("destdv250k_2gw.shp")
dim(Mexico)
names(Mexico)
head(Mexico)
class(Mexico)
View(Mexico)

# Leer contenido de una columna
# 
Mexico$ENTIDAD
Mexico$CAPITAL

# Cargar capa con 4 municipios de Michoacán#####
Municipio <- st_read("mun.shp")
dim(Municipio)
names(Municipio)
plot(Municipio)
crs(Municipio, proj = TRUE)


# Cargar capa de puntos de muestreo
Puntos <- st_read("Puntos_muestreo.shp")
dim(Puntos)
names(Puntos)
crs(Municipio, proj = TRUE)


# Importar rasters Modelo Digital de elevacion
elevacion<-rast("Dem_muni.tif")
elevacion
plot(elevacion)
summary(elevacion)
dim(elevacion)
class(elevacion)
elevacion$DEM_muni
crs(Municipio, proj = TRUE)


# Convertir en un data frame
df_Ele<- as.data.frame(elevacion, xy = TRUE)
summary(df_Ele)
class(df_Ele)

# 2 Visualizaciones 
# Visualizar raster
ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
  geom_raster(aes(fill = DEM_muni))


# Haciendo tu propia paleta
ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
  geom_raster(aes(fill = DEM_muni))+
  scale_fill_gradientn(colours = c("darkred", "orange", "yellow"))

# Paleta de Viridis
ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
  geom_raster(aes(fill = DEM_muni))+
  scale_fill_viridis(discrete = FALSE, option = "D")

#Visualizar un archivo vectorial 

#MUNICIPIO
ggplot(data = Municipio) + 
  geom_sf()


#NOMBRES DE MUNICIPIO
Municipio$NOM_MUN
plot(Municipio[4,])
ggplot(data = Municipio) + 
  geom_sf(aes(fill = NOM_MUN))+
  geom_sf_text(aes(label = NOM_MUN))
uruapan<- Municipio[4,]

ggplot(data = Municipio[1,]) + 
  geom_sf(aes(fill = NOM_MUN))+
  geom_sf_text(aes(label = NOM_MUN))


# Usar paleta de colores predeterminada
coul <- brewer.pal(4, "Set1") 
coul
ggplot(data = Municipio) +
  geom_sf(aes(fill = NOM_MUN)) +
  geom_sf_text(aes(label = NOM_MUN))+
  scale_fill_manual(values=coul) 

# Seleccionar un poligono y visualizar
ggplot(data = Municipio[4,]) +
  geom_sf(aes(fill = NOM_MUN)) +
  scale_fill_discrete()

# Visualizar puntos
ggplot(data = Puntos) +
  geom_sf()

# Un solo color
ggplot(data = Puntos) +
  geom_sf(color="red")+
  theme_bw()

# Rampa de color
coul <- brewer.pal(4, "Set2") 
ggplot(data = Puntos) +
  geom_sf(aes(color = NOM_MUN,size = DEM_muni)) +
  scale_colour_manual(values=coul)

#3. Añadir capas#######
#Dos capas shape
names(Mexico)
unique(Mexico$ENTIDAD)
Michoacan <- Mexico[23,]
plot(Michoacan)

ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios focales") +
  geom_sf(data=Municipio, color = 'red')

#tres capas shape
ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios focales") +
  geom_sf(data=Municipio, color = 'red')+
  geom_sf(data=Puntos, color = 'blue')

#Cuatro capas 2 shape y una raster

ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  geom_sf(data=Puntos, color = 'red')+
  geom_sf(data=Municipio, color ="black", fill=NA)

#Zoom al area de estudio
xlim <- c(-103.7383, -101.5)
ylim <- c(18, 20)

ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  geom_sf(data=Puntos, color = 'red')+
  geom_sf(data=Municipio, color ="black", fill=NA)+
  coord_sf(xlim = xlim, ylim = ylim)

#Mejor zoom
xlim <- c(-102.7, -101.5)
ylim <- c(19, 20)

ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  geom_sf(data=Puntos, color = 'red')+
  geom_sf(data=Municipio, color ="black", fill=NA)+
  coord_sf(xlim = xlim, ylim = ylim)

#######escala y rosa de los vientos
ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  geom_sf(data=Puntos, color = 'red')+
  geom_sf(data=Municipio, color ="black", fill=NA)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "cm"), pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = xlim, ylim = ylim)

#Cambiar posición "bl" (bottom-left), "br" (bottom-right), "tl" (top-left), "tr" (top-right)


# Modificar leyenda y color raster

ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  scale_fill_viridis_c(name = "Elevacion")+
  geom_sf(data=Puntos, color = 'red')+
  geom_sf(data=Municipio, color ="black", fill=NA)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = xlim, ylim = ylim)


ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  scale_fill_viridis_c(name = "Elevacion")+
  geom_sf(data=Puntos, color = 'red')+
  geom_sf(data=Municipio, color ="black", fill=NA)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = xlim, ylim = ylim)

# 4. Mapa completo #####
xlim <- c(-102.7, -101.5)
ylim <- c(19, 20)

o<-ggplot(data =Michoacan) +
  geom_sf(color='black', fill=NA) +
  xlab("Longitud") + ylab("Latitud") +
  ggtitle("Zona de estudio",
          subtitle = "Municipios estudiados") +
  geom_raster(data = df_Ele,aes(x = x, 
                                y = y,fill = DEM_muni))+
  scale_fill_viridis_c(name = "Elevacion")+
  geom_sf(data=Puntos, aes(color = 'red'))+
  scale_colour_discrete(name = "Puntos",labels=c("Registros"))+
  geom_sf(data=Municipio, color ="black", fill=NA)+
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering)+
  coord_sf(xlim = xlim, ylim = ylim)+
  theme_linedraw()
o


#Mapa Mexico con relleno de michoacán

#ggplot(data =Mexico) +
#    geom_sf(color='black', fill=NA) +
#    xlab("Longitud") + ylab("Latitud") +
#    ggtitle("Zona de estudio",
#            subtitle = "Municipios focales") +
#   geom_sf(data=Michoacan, fill='black')
#  theme_test()


# Mapa completo 


#Insertar imagen
path <- "mexico_blanco.jpg"

img <- readJPEG(path, native = TRUE)

path <- "Colibri_cola_de_oro.jpeg"

img2 <- readJPEG(path, native = TRUE)

cow_final <- ggdraw() +
  draw_plot(o) +
  draw_image(image = img, x = 0.14, y =  0.24, scale = 0.25) +
  draw_image(image = img2, x = -0.2, y =  0.24, scale = 0.2) 

cow_final

ggsave("Mapa_Final.jpg", plot = cow_final, width = 8, height = 6,dpi = 900)
