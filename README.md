# Problem_Set_2
En el siguiente enlace se encuentra el desarrollo de la limpieza, homgenización y analisis de los datos de vivienda de la ciudad de Bogota, por parte del Equipo 11 
#Instalamos paquetes y librerias##

install.packages("pacman") 
library(pacman) 
 install.packages("leaflet")
library(dplyr)
library(sf)
library(leaflet)

install.packages("sf")

install.packages("osmdata")
library(osmdata)
library(tidyverse)
p_load(tidyverse)
library(rio,
       leaflet,
       tmaptools,
       sf,
       osmdata,
       ggplot2,
       scales,
       skimr,
       rvest)
install.packages("plotly")
install.packages("rgeos")
install.packages("tidymodels")
install.packages("ggpubr")
  p_load(rgeos)
  options(repos = c(CRAN = "https://cran.r-project.org/"))
  install.packages("rgeos", type = "source")
  
#### Definimos directorio de donde tomaremos los datos #####
setwd("C:/Users/ncoro/Desktop/2023/BML/taller 2")
dataTrain <- read.csv("train.csv")
dataTest <- read.csv("test.csv")

##Escalamos las descripciones de vivienda de cada base##
library(stringr)

# Pasamos a tener toda la descripcion y titulos en minuscula##

dataTrain$description<-str_to_lower(string=dataTrain$description)
dataTrain$title<-str_to_lower(string=dataTrain$title)

dataTest$description<-str_to_lower(string=dataTest$description)
dataTest$title<-str_to_lower(string=dataTest$title)

# Luego las tildes
dataTrain$description <- iconv(dataTrain$description, from = "UTF-8", to = "ASCII//TRANSLIT")
dataTest$description <- iconv(dataTest$description, from = "UTF-8", to = "ASCII//TRANSLIT")

# Eliminamos caracteres especiales
dataTrain$description <- str_replace_all(dataTrain$description, "[^[:alnum:]]", " ")
dataTest$description <- str_replace_all(dataTest$description, "[^[:alnum:]]", " ")

# Eliminamos espacios extras
dataTrain$description <- gsub("\\s+", " ", str_trim(dataTrain$description))
dataTest$description <- gsub("\\s+", " ", str_trim(dataTest$description))

#Creamos una columna adicional en cada base para mantener la segmenatación###

train<- dataTrain %>% mutate(base = "train") 
test <- dataTest %>% mutate(base="test")

## Juntamos las bases de datos ##

DB <- bind_rows(train, test) %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
class(DB)

#Guardamos Base de datos DB1#
write.table(DB, "DB1.csv",sep=",",dec=".",row.names = FALSE)

#Ahora bien como contabamos con una sola BD, procedemos a limpiar los datos#
leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(data = DB)

library(sf)
Chapinero <- getbb(place_name = "Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon")$multipolygon
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = Chapinero , col = "purple")

#ahora transformamos datos
Chapinero <- st_transform(Chapinero, st_crs(DB))
DChapinero<- DB[Chapinero,]
leaflet() %>% addTiles() %>% addCircles(data = DChapinero, color = "yellow" ) %>% addPolygons(data= Chapinero, col = "grey")
available_tags("amenity","leisure")


#Creacion de varibale dummy tipo de vivienda##
DB <- DB %>%
  filter(property_type == "Apartamento" | property_type == "Casa")
dim(DB)

#idenficamos los NA#
DB %>%
  summarise_all(~sum(is.na(.))) %>%
  t()
#Empecemos a limpiar la base#
# 1. completar variables NA que estan en descripción##
Descripc<-DB$description
pat_1b <- "[:space:]+[:digit:]+[:space:]+baño" ## patrón para baños
pat_2b <- "[:space:]+[:digit:]+[:space:]+baños"
pat_3b<-"[:space:]+[:digit:]+baño"
pat_4b<-"[:space:]+[:digit:]+baños"
pat_5b <- "baño+[:space:]+[:digit:]"
pat_6b <- "baños+[:space:]+[:digit:]"
pat_7b <- "[:space:]+[:digit:]+[:space:]+banio"
pat_8b<-"[:space:]+[:digit:]+[:space:]+banios"
pat_9b <- "[:space:]+[:digit:]+banios"
pat_10b <- "banio+[:space:]+[:digit:]"
pat_11b <- "banios+[:space:]+[:digit:]"
pat_12b <- "[:space:]+[:digit:]+[:space:]+bano"
pat_13b <- "[:space:]+[:digit:]+[:space:]+banos"
pat_14b <- "[:space:]+[:digit:]+bano"
pat_15b <- "[:space:]+[:digit:]+banos"
pat_16b <- "bano+[:space:]+[:digit:]"
pat_17b <- "banos+[:space:]+[:digit:]"
pat_18b <- "[:space:]+[:digit:]+[:space:]+vano"
pat_19b <- "[:space:]+[:digit:]+[:space:]+vanos"
pat_20b <- "[:space:]+[:digit:]+vano"
pat_21b <- "[:space:]+[:digit:]+vanos"
pat_22b <- "vano+[:space:]+[:digit:]"
pat_23b <- "vanos+[:space:]+[:digit:]"
pat_24b <- "[:space:]+[:digit:]+[:space:]+vanio"
pat_25b <- "[:space:]+[:digit:]+[:space:]+vanios"
pat_26b <- "[:space:]+[:digit:]+vanio"
pat_27b<-"[:space:]+[:digit:]+vanios"
pat_28b <- "vanio+[:space:]+[:digit:]"
pat_29b <- "vanios+[:space:]+[:digit:]"
pat_30b <- "[:space:]+[:digit:]+[:space:]+vaño"
pat_31b <- "[:space:]+[:digit:]+[:space:]+vaños"
pat_32b <- "[:space:]+[:digit:]+vaño"
pat_33b<-"[:space:]+[:digit:]+vaños"
pat_34b <- "vaño+[:space:]+[:digit:]"
pat_35b <- "vaños+[:space:]+[:digit:]"
pat_36b <- "[:space:]+[:digit:]+[:space:]+bañio"
pat_37b <- "[:space:]+[:digit:]+[:space:]+bañios"
pat_38b <- "[:space:]+[:digit:]+bañio"
pat_39b<-"[:space:]+[:digit:]+bañios"
pat_40b <- "bañio+[:space:]+[:digit:]"
pat_41b <- "bañios+[:space:]+[:digit:]"
pat_42b <- "[:space:]+[:digit:]+[:space:]+bao"
pat_43b <- "[:space:]+[:digit:]+[:space:]+baos"
pat_44b <- "[:space:]+[:digit:]+bao"
pat_45b<-"[:space:]+[:digit:]+baos"
pat_46b <- "bao+[:space:]+[:digit:]"
pat_47b <- "baos+[:space:]+[:digit:]"

DB <- DB  %>% 
  mutate(WC= str_extract(string=DB$description , pattern= paste0(pat_1b,"|",pat_2b,"|", pat_3b,"|", pat_4b,"|", pat_5b,"|", pat_6b,"|", pat_7b,"|", pat_8b,"|", pat_9b,"|", pat_10b,"|", pat_11b,"|", pat_12b,"|", pat_13b,"|", pat_14b,"|", pat_15b,"|", pat_16b,"|", pat_17b,"|", pat_18b,"|", pat_19b,"|", pat_20b,"|", pat_21b,"|", pat_22b,"|", pat_23b,"|", pat_24b,"|", pat_25b,"|", pat_26b,"|", pat_27b,"|", pat_28b,"|", pat_29b,"|", pat_30b,"|", pat_31b,"|", pat_32b,"|", pat_33b,"|", pat_34b,"|", pat_35b,"|", pat_36b,"|", pat_37b,"|", pat_38b,"|", pat_39b,"|", pat_40b,"|", pat_41b,"|", pat_42b,"|", pat_43b,"|", pat_44b,"|", pat_45b,"|", pat_46b,"|", pat_47b) ))

DB$WC<-str_replace_all(string = DB$WC, pattern = "," , replacement = ".")
DB$WC<-str_replace_all(string = DB$WC, pattern = "baño" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "baños" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "banio" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "banios" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "bano" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "banos" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "vano" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "vanos" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "vanio" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "vanios" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "vaño" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "vaños" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "bañio" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "bañios" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "bao" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "baos" , replacement = "")
DB$WC<-str_replace_all(string = DB$WC, pattern = "\n" , replacement = "")
DB$WC<-as.numeric(DB$WC)
View(DB)
DB$WC[is.na(DB$WC)] = 1 #Se asume este valor debido a la obligación legal de existir al menos 1 baño en una vivienda; Decreto 122 de 2023 Alcaldía Mayor de Bogotá, D.C.
summary(DB$WC) 
filtro2<-is.na(DB$bathrooms)
DB$bathrooms[filtro2]<-DB$WC
table(is.na(DB$bathrooms))

#datos de area#
DB <- DB %>%
  mutate(area = str_extract(description, "\\d+(\\.\\d+)?(?=\\s*(m²|m2|mt2|metros cuadrados|metros2))"))
DB$area <- ifelse(is.na(DB$surface_total) & DB$bedrooms == 2 & DB$property_type == "Apartamento", 35, DB$surface_total) # Art.1 Decreto 2060 de 2004

##Creacion de nuevas variables a partir de descripcion##

#parqueadero
Descripc<-DB$description
pq_aux1<-str_detect( Descripc,"parqueadero") 
pq_aux2<-str_detect( Descripc,"parqueaderos") 
pq_aux3<-str_detect( Descripc,"parqeadero") 
pq_aux4<-str_detect( Descripc,"parqeaderos") 
pq_aux5<-str_detect( Descripc,"garaje") 
pq_aux6<-str_detect( Descripc,"garajes") 
pq_aux7<-str_detect( Descripc,"garage") 
pq_aux8<-str_detect( Descripc,"garages") 
pq_aux9<-str_detect( Descripc,"garjes") 
pq_aux10<-str_detect( Descripc,"garje") 
pq<-ifelse(pq_aux1==TRUE|pq_aux2==TRUE| pq_aux3==TRUE|pq_aux4==TRUE|pq_aux5==TRUE|pq_aux6==TRUE|pq_aux7==TRUE|pq_aux8==TRUE|pq_aux9 == TRUE |pq_aux10==TRUE , 1,0 )
pq<-data.frame(pq)
summary(pq)
pq[is.na(pq)] = 0 #se asume que si no lo describen es porque no cuenta con parqueadero
summary(pq)
DB <- cbind(DB, pq)

#Servicio de gas por redes
DB <- DB %>%
  mutate(gas = grepl('gas', description, ignore.case = TRUE))
DB <- DB %>%
  mutate(gasr = as.integer(gas))

#Tiene chimenea
DB <- DB %>%
  mutate(Chimenea = grepl('Chimenea', description, ignore.case = TRUE))
DB <- DB %>%
  mutate(Chimenea_d = as.integer(Chimenea))

#El inmueble cuenta con Vigilancia o porteria
DB <- DB %>%
  mutate(vigilancia = grepl('porteria|vigilancia', description, ignore.case = TRUE))
DB <- DB %>%
  mutate(vigi = as.integer(vigilancia))

## Creación de nuevas variables a partir de datos de otras fuentes##
#revisar variables disponibles en openmaps ##
available_tags("leisure") 
available_tags("amenity")

Chapinero <- st_transform(Chapinero, st_crs(DB))
DChapinero<- DB[Chapinero,]

DChapinero <- getbb(place_name = "UPZ Chapinero, Bogota",
                    featuretype = "boundary:administrative",
                    format_out = "sf_polygon") %>% .$multipolygon

write.table(DChapinero, "DBc.csv",sep=",",dec=".",row.names = FALSE)
require("tmaptools")

PtsChapi = geocode_OSM("UPZ Chapinero, Bogota", as.sf=T)
leaflet() %>% addTiles() %>% addCircles(data=PtsChapi)

#Coordenadas de chapinero

opq(bbox = getbb("Chapinero Bogotá"))

#Para crear variable parque

parques <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "leisure" , value = "park") 
parques_sf <- osmdata_sf(parques)

parques_geometria <- parques_sf$osm_polygons %>% 
  select(osm_id, name)

# Calculamos el centroide de cada parque para aproximar  ubicacion a un punto

library(sf)

# Convierte tus datos a un objeto sf
parques_sf <- st_as_sf(parques_geometria)

# Calcula los centroides
centroides <- st_centroid(parques_sf)

# Muestra los centroides
head(centroides)

DChapinero_sf <- st_as_sf(DB, coords = c("lon", "lat"))

dist_matrix <- st_distance(x = DChapinero_sf, y = centroides)

# Definimos distancia mas cercana a un parque 
dist_min <- apply(dist_matrix, 1, min)

# La agregamos la distancia al parque como variablea nuestra base de datos original 
DB <- DB %>% mutate(distancia_parque = dist_min)

install.packages("plotly")

install.packages("vctrs")
library(vctrs)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
install.packages("plotly")
library(plotly)

#distribución de parque grafica
p <- ggplot(DB, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p)

#coordenadas y distancia de bares#

library(osmdata)
library(magrittr)

Bar <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key ="amenity" , value = "bar") 
  Bar_sf <- osmdata_sf(Bar)
  
  install.packages("sf")
  library(sf)

Bar_geometria <- Bar_sf$osm_polygons %>% 
  select(osm_id, name)


# Convierte tus datos a un objeto sf
Bar_sf <- st_as_sf(Bar_geometria)

# Calcula los centroides
centroidesb_sf <- st_centroid(Bar_sf)

# Muestra los centroides
head(centroides)

DB_sf <- st_as_sf(DB, coords = c("lon", "lat"))

dist_matrix2 <- st_distance(x = DB_sf, y = centroidesb_sf)

# Encontramos la distancia mínima a un bar
dist_min2 <- apply(dist_matrix2, 1, min)

# La agregamos la distancia al bar como variablea nuestra base de datos original 

DB <- DB %>% mutate(distancia_Bar = dist_min2)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bar") 
class(osmbog)

## Parada de Bus ##

osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osmbog)
osmbog_sf = osmbog %>% osmdata_sf()
View(osmbog_sf)
Transporte_publicoBog = osmbog_sf$osm_points %>% select(osm_id,amenity) 
View(Transporte_publicoBog)

distancia_Pbus<- st_distance(x=DB_sf , y=Transporte_publicoBog)
dist_min4 <- apply(distancia_Pbus, 1 , min)
dist_min4<-data.frame(dist_min4)

DBF<-cbind(DB, dist_min4)

DB <- DB %>% mutate(distancia_Pbus= dist_min4)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bus_station")
class(osmbog)

#coordenadas y distancia a avenidas principales

# Obtener avenidas principales en Bogotá

install.packages("sf")
library(sf)

AV <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key ="highway" , value = "primary")
AV_sf <- osmdata_sf(AV)

AV_geometria <- AV_sf$osm_polygons %>% 
  select(osm_id, name)

# Convierte tus datos a un objeto sf
AV_sf <- st_as_sf(AV_geometria)

DB_sf <- st_as_sf(DB, coords = c("lon", "lat"))

#Calculemos distancia a avenidas principales

distancia_AV<- st_distance(x=DB_sf , y=AV_sf)
dist_min5 <- apply(distancia_AV, 1 , min)
dist_min5<-data.frame(dist_min5)

DBF<-cbind(DB, dist_min5)

DB <- DB %>% mutate(distancia_AV= dist_min5)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="highway" , value="primary")
class(osmbog)


write.table(DBF, "DB2vc.csv",sep=",",dec=".",row.names = FALSE)

setwd("C:/Users/ncoro/Desktop/2023/BML/taller 2")
DB1 <- read.csv("DB2vc.csv")
