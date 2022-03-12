#  Cargamos las Librerias ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, gtools, tidyverse,rnaturalearth,rnaturalearthdata,
               sf, reticulate,maptools,maps, ggplot2 ,ggspatial, rgeos, ggmap , leaflet)

# Cargammos los SHp del Peru ---------------------------------------------------------------
Peru_Depa   <- getData('GADM', country='Peru', level=2) %>%
  st_as_sf() 
Cusco         <-subset(Peru_Depa , NAME_1 == "Cusco") 

lbl         <- data.frame(month_abb = month.abb, mes = 1:12)
Cusco_xy <- cbind(Cusco, st_coordinates(st_centroid(Cusco$geometry)))
ggplot() + geom_sf(data = Cusco) + theme_bw()

MDD         <- MDD %>% st_transform (crs = 4326)
leaflet(MDD) %>% addTiles() %>% addPolygons()
# Extraemos los datos raster de Precipitacion -----------------------------------------------

Prec        <- getData("worldclim", var = "tmean", res=0.5, lon=-74.8773, lat=-11.54012)

Prec_MDD    <- crop(Prec, Cusco)
Prec_MDD    <- Prec_MDD <- mask(Prec_MDD,Cusco)
summary(Prec_MDD)
PPAnual_MDD = Prec_MDD/10
PPAnual_MD <- do.call("sum", unstack(Prec_MDD))
plot(PPAnual_MD)

# Elaboramos los meses Para precipitacion-----------------------------------------
vls         <- rasterToPoints(PPAnual_MDD) %>% 
  as_tibble() %>% 
  gather(var, value, -x, -y) %>% 
  mutate(mes = parse_number(var)) %>% 
  inner_join(., lbl, by = 'mes') %>% 
  dplyr::select(x, y, month_abb, value) %>% 
  mutate(month_abb = factor(month_abb, levels = month.abb))

vls %>% 
  filter(month_abb == 'Jan')
summary(vls$value)

colores<- c('#d8e2dc', '#8ecae6', '#023e8a', '#03045e', '#184e77', '#40916c', '#80b918',
            '#55a630','#aacc00','#d4d700','#eeef20','#ffff3f','#ff9e00','#ff9100','#ff6d00','#e36414'
            ,'#9a031e')



A=ggplot(vls)  +
  geom_tile(aes(x = x, y =  y, fill = value)) +
  facet_wrap(~ month_abb) +
  scale_fill_gradientn(colours = colores, 
                       breaks = c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30),
                       na.value = 'white',
                       labels = c("[0 -2] ","[2 - 4]", "[4 -6]", "[6 -8]", "[8 -10]", "[10 -12]",
                                  "[12 -14]","[14 - 16]","[16 - 18]","[18 - 20]","[20 - 22]","[22 - 24]"
                                  ,"[24 - 26]","[26 - 28]","[28 - 30]"),
                       name='Temperatura \nPromedio Anual ºC') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data=Cusco, color="white", fill=NA, size=0.3)+
  theme_bw() +
  # geom_sf_label(data = Cusco_xy , aes(x= X, y=Y, label = NAME_2), size =2, color="black",alpha=0.4,fontfamily = "serif",  fontface="italic")+
  scale_x_continuous(breaks = c(-74, -72.5, -71.5, 70.5)) +
  labs(title = 'Precipitación mensual - Madre de Dios', fill = 'Temperatura \nPromedio mensual ºC',  x = 'Longitud', y = 'Latitud', caption = "Gorky Florez") +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face="bold", color="white"),
    legend.key.width = unit(3, 'line'),
    panel.border = element_rect(size = 3, color="white"),
    axis.text.x  = element_text(face="bold", color="white", size=8),
    axis.text.y  = element_text(angle = 90,face="bold", color="white", size=8),
    strip.text=element_text(family='Anton', face='bold', size=14, hjust=0, color='white'),
    strip.background=element_rect(fill='black'),
    plot.title = element_text(size = 16, hjust = 0.5, color = "white", family="serif", face = "italic"),
    plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "white", family="serif"),
    plot.caption = element_text(size = 10, hjust = 0.95, color = "white", family="serif", face = "italic")) +
  guides(shape = guide_legend(override.aes = list(size = 10)))+
  ggtitle("Temperatura Promedio mensual ºC del Cusco ")+
  labs(subtitle="Ing. Gorky Florez Castillo", x="Longitud",y="Latitud",
       caption="Fuente: Data: https://www.worldclim.org")+
  ggspatial::annotation_north_arrow(
    location = "bl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="black"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="black")

ggsave(plot = A,"Mapa/Cusco_mean_temperatura.png", units = "cm", width = 29, #ancho
       height = 29, #Largo
       dpi = 1200)