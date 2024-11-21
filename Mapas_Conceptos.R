#install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata","terra"))

library(ggplot2)
#theme_set(theme_bw)
library(sf)

#install.packages("sf", dependencies = TRUE)
#install.packages("remotes")
#remotes::install_github("r-spatial/sf")

#El paquete rnaturalearthproporciona un mapa de países de todo el mundo
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
library(rnaturalearthdata)#escalas mayores
library(rnaturalearth)
library(terra)
library(sf)

world <- ne_countries(scale = "medium", returnclass = "sf")
summary(world)
dim(world)
names(world)
class(world)

ggplot(data = world) + 
  geom_sf()

#Etiquetas de título, subtítulo y eje ( ggtitle, xlab, ylab)
ggplot(data = world) +
    geom_sf() +
    xlab("Longitud") + ylab("Latitud") +
    ggtitle("Mapa global", subtitle = "251 paises")

#Color del mapa
ggplot(data = world) + 
    geom_sf(color = "black", fill = "pink")

#Rampa de color
ggplot(data = world) +
    geom_sf(aes(fill = pop_est)) +
    scale_fill_viridis_c(option = "plasma", trans = "sqrt")

#Proyección y extensión
#De forma predeterminada, se utilizará el sistema de coordenadas de la primera capa o, si no lo hay, recurrirá a WGS84

ggplot(data = world) +
    geom_sf() +
    coord_sf(crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs")

ggplot(data = world) +
    geom_sf() +
    coord_sf(crs = st_crs(3035))

?st_crs

#Xlim y Ylim
ggplot(data = world) +
    geom_sf() +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)


#Barra de escala y flecha de norte (paquete ggspatial)
library("ggspatial")
ggplot(data = world) +
    geom_sf() +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
    pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
    style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

#Otra info 
Municipio_cdf <- as.data.frame(Municipio)
names(Municipio_cdf)
elev_cdf <- as(Municipio, "SpatialPolygonsDataFrame")
summary(elev_cdf)
df_Ele <- as.data.frame(elev_cdf)
?as
bdmn<-as(Municipio2, "Spatial")
bdmn2 <- as.data.frame(bdmn,"SpatialPolygonsDataFrame")
df <- Municipio2@data
Municipio_cdf2 <- as.data.frame(Municipio2)
#map_rep<-ggplot(data = Mexico) + geom_sf()

names(Municipio)
pop.df <- data.frame(Municipio,xy=TRUE)
ggplot(data = pop.df) + geom_map(aes(fill = NOM_MUN))+
    geom_text(aes(label = NOM_MUN,x = xy, y = xy))

foo.df <- as(Municipio2, "data.frame")
coul <- colorRampPalette(coul)(100)
map_mun + scale_fill_manual(values=coul)

ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
    geom_raster(aes(fill = DEM_muni))+
    scale_fill_viridis(discrete = FALSE, option = "D")+
    geom_sf(
        data = Municipio, 
        col = NA,
        aes(fill = NOM_MUN))

ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
    geom_raster(aes(fill = DEM_muni))+
    scale_fill_viridis(discrete = FALSE, option = "D")+
    geom_sf(
        data = Municipio, 
        col = NA,
        aes(fill = NOM_MUN))

ggplot(data =Michoacan) +
    geom_sf(color='black', fill=NA) +
    xlab("Longitud") + ylab("Latitud") +
    ggtitle("Zona de estudio",
            subtitle = "Municipios estudiados") +
    geom_sf(data=Municipio, aes(fill=NOM_MUN))+
    scale_fill_discrete("transparent")+
    geom_raster(data = df_Ele,aes(x = x, 
                                  y = y,fill = DEM_muni))+
    geom_sf(data=Puntos, color = 'red')

ggplot(data =Michoacan) +
    geom_sf(color='black', fill=NA) +
    xlab("Longitud") + ylab("Latitud") +
    ggtitle("Zona de estudio",
            subtitle = "Municipios estudiados") +
    geom_sf(data=Municipio, color ="black", fill=NA)+
    geom_raster(data = df_Ele,aes(x = x, 
                                  y = y,fill = DEM_muni))+
    geom_sf(data=Puntos, color = 'red')


ggplot(data = df_Ele,aes(x = x, 
                         y = y)) +
    geom_raster(aes(fill = DEM_muni))+
    scale_fill_viridis_c(name = "Elevacion")

?terrain.colors()