# Problem_Set_2
En el siguiente enlace se encuentra el desarrollo de la limpieza, homgenización y analisis de los datos de vivienda de la ciudad de Bogota, por parte del Equipo 11 

Es importante precisar que se trabajaron dos formas de limpieza, homogenización y analisis, pero de acuerdo a los resultados de Kaggel se tomo la base de datos que se encuentra en el archivo "codigo depuracion datos.txt"

#Instalamos paquetes y librerias##

install.packages("pacman") 
library(pacman) 
 install.packages("leaflet")
library(dplyr)

library(leaflet)



install.packages("osmdata")
library(osmdata)
library(tidyverse)
install.packages("plotly")
install.packages("rgeos")
install.packages("tidymodels")
install.packages("ggpubr")
  p_load(rgeos)
  options(repos = c(CRAN = "https://cran.r-project.org/"))
  install.packages("rgeos", type = "source")
  
#### Definimos directorio de donde tomaremos los datos #####
setwd("C:/Users/ncoro/Desktop/2023/BML/taller 2.1")
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


install.packages("dplyr")
library(dplyr)

# Combinar las bases de datos usando
Vivienda <- bind_rows(train,test)

# Guardar el resultado en un nuevo archivo CSV
write.csv(Vivienda, "vivienda.csv", row.names = FALSE)

install.packages("sf")
library(sf)

#Ahora bien como contabamos con una sola BD, procedemos a limpiar los datos#


Vivienda <- read.csv("vivienda.csv")

install.packages("leaflet")
library(leaflet)

install.packages("dplyr")
library(dplyr)

install.packages("osmdata")
library(osmdata)
library(tidyverse)

install.packages("sf")
library(sf)
Bchapi <- getbb(place_name = "UPZ Chapinero, Bogota", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon")$multipolygon

#ahora transformamos datos


# Observamos la primera visualización
leaflet() %>%
  addTiles() %>%
  addCircles(lng = Vivienda$lon, 
             lat = Vivienda$lat)

DB24 <- Vivienda

# Mostrar los límites
chapinero_bounds

chapinero_limits <- list(
  min_lon = -74.063762,
  max_lon = -74.063662,
  min_lat = 4.633122,
  max_lat = 4.633222
)

# Filtrar la base de datos
DB24 <- DB24 %>%
  filter(
    between(lon, chapinero_limits$min_lon, chapinero_limits$max_lon) &
      between(lat, chapinero_limits$min_lat, chapinero_limits$max_lat)
  )

Chapinero_limits <- getbb("UPZ Chapinero, Bogota")
featuretype = "boundary:administrative"


#Creacion de varibale dummy tipo de vivienda##
DB24 <- Vivienda %>%
  filter(property_type == "Apartamento" | property_type == "Casa")
dim(Vivienda)

#idenficamos los NA#
DB24 %>%
  summarise_all(~sum(is.na(.))) %>%
  t()


#Empecemos a limpiar la base#

# 1. completar variables NA que estan en descripción##
Descripc<-DB24$description
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

DB24 <- DB24  %>% 
  mutate(WC= str_extract(string=DB24$description , pattern= paste0(pat_1b,"|",pat_2b,"|", pat_3b,"|", pat_4b,"|", pat_5b,"|", pat_6b,"|", pat_7b,"|", pat_8b,"|", pat_9b,"|", pat_10b,"|", pat_11b,"|", pat_12b,"|", pat_13b,"|", pat_14b,"|", pat_15b,"|", pat_16b,"|", pat_17b,"|", pat_18b,"|", pat_19b,"|", pat_20b,"|", pat_21b,"|", pat_22b,"|", pat_23b,"|", pat_24b,"|", pat_25b,"|", pat_26b,"|", pat_27b,"|", pat_28b,"|", pat_29b,"|", pat_30b,"|", pat_31b,"|", pat_32b,"|", pat_33b,"|", pat_34b,"|", pat_35b,"|", pat_36b,"|", pat_37b,"|", pat_38b,"|", pat_39b,"|", pat_40b,"|", pat_41b,"|", pat_42b,"|", pat_43b,"|", pat_44b,"|", pat_45b,"|", pat_46b,"|", pat_47b) ))

DB24$WC<-str_replace_all(string = DB24$WC, pattern = "," , replacement = ".")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "baño" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "baños" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "banio" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "banios" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "bano" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "banos" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "vano" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "vanos" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "vanio" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "vanios" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "vaño" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "vaños" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "bañio" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "bañios" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "bao" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "baos" , replacement = "")
DB24$WC<-str_replace_all(string = DB24$WC, pattern = "\n" , replacement = "")
DB24$WC<-as.numeric(DB24$WC)
View(DB24)
DB24$WC[is.na(DB24$WC)] = 1 #Se asume este valor debido a la obligación legal de existir al menos 1 baño en una vivienda; Decreto 122 de 2023 Alcaldía Mayor de Bogotá, D.C.
summary(DB24$WC) 
filtro2<-is.na(DB24$bathrooms)
DB24$bathrooms[filtro2]<-DB24$WC
table(is.na(DB24$bathrooms))

write.csv(DB24, "DBbanio.csv", row.names = FALSE)


##datos de area##
DB24 <- DB24 %>%
  mutate(area = str_extract(description, "\\d+(\\.\\d+)?(?=\\s*(m²|m2|mt2|metros cuadrados|metros2))"))
DB24$area <- ifelse(is.na(DB24$surface_covered) & DB24$bedrooms == 2 & DB24$property_type == "Apartamento", 35, DB24$surface_total) # Art.1 Decreto 2060 de 2004
#Teniendo en cuenta el decreto y se asume que un apartaestudio en la localidad de chapinero no seria inferior a 15m2 se elimina estos valores#
DB24 <- DB24 %>% filter( surface_covered > 15)

##Ahora pasamos los datos NA de surface_covered a Area

DB24$area <- ifelse(is.na(DB24$area) & DB24$surface_covered >= 15, DB24$surface_covered, DB24$area)

View(DB24)

write.csv(DB24, "DBBAo.csv", row.names = FALSE)

##Creacion de nuevas variables a partir de descripcion##

#parqueadero
Descripc<-DB24$description
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
DB24 <- cbind(DB24, pq)

#Servicio de gas por redes
DB24 <- DB24 %>%
  mutate(gas = grepl('gas', description, ignore.case = TRUE))
DB24 <- DB24 %>%
  mutate(gasr = as.integer(gas))

#Tiene chimenea
DB24 <- DB24 %>%
  mutate(Chimenea = grepl('Chimenea', description, ignore.case = TRUE))
DB24 <- DB24 %>%
  mutate(Chimenea_d = as.integer(Chimenea))

#El inmueble cuenta con Vigilancia o porteria
DB24 <- DB24 %>%
  mutate(vigilancia = grepl('porteria|vigilancia', description, ignore.case = TRUE))
DB24 <- DB24 %>%
  mutate(vigi = as.integer(vigilancia))

view(DB24)

write.csv(DB24, "DBdESCRIP.csv", row.names = FALSE)

## Creación de nuevas variables a partir de datos de otras fuentes##
#revisar variables disponibles en openmaps ##
available_tags("leisure") 
available_tags("amenity")
available_tags("highway")

leaflet() %>% addTiles() %>% addCircles(data = DB24)
str(DB24)

latitud_central <- mean(DB24$lat)
longitud_central <- mean(DB24$lon)

#Coordenadas de chapinero

opq(bbox = getbb("Chapinero Bogotá"))

#Para crear variable parque

parques <- opq(bbox = getbb("UPZ Chapinero Bogota")) %>%
  add_osm_feature(key = "leisure" , value = "park") 

parques_sf <- osmdata_sf(parques) #Cambiamos a SF el objeto

#verificamos columnas del poligono

names(parques_sf$osm_polygons)

parques_geo <- parques_sf$osm_polygons %>%
  select(osm_id)

# Calculamos el centroide de cada parque para aproximar  ubicacion a un punto

library(sf)

# Convierte tus datos a un objeto sf
parques_sf <- st_as_sf(parques_geo)

# Calcula los centroides
install.packages("rgeos")
library(sf)

centroides <- st_centroid(as(parques_geo, "sf"))

# Muestra los centroides
head(centroides)

#colocamos en el mismo sistema de coordenadas

DChapinero_sf <- st_set_crs(DChapinero_sf, 4326)
centroides <- st_set_crs(centroides, 4326)
DB24_sf <- st_as_sf(DB24, coords = c("lon", "lat"), crs=4326)

centroides_sf <- do.call(rbind, st_geometry(centroides)) %>% 
  as_tibble() %>% setNames(c("lon","lat")) 
centroides_sf <- st_as_sf(centroides_sf, coords = c("lon", "lat"), crs=4326)

# Definimos distancia mas cercana a un parque 
dist_min <- st_nearest_feature(DB24_sf,centroides_sf)

DB24 <- DB24 %>%
  mutate(distancia_parque = st_distance(x = DB24_sf, y = centroides_sf))

# La agregamos la distancia al parque como variablea nuestra base de datos original 
DB24 <- DB24 %>% mutate(distancia_parque = dist_min)

view(DB24)

write.csv(DB24, "DBdESCRIPPAR.csv", row.names = FALSE)

install.packages("plotly")

install.packages("vctrs")
library(vctrs)
install.packages("ggplot2")
library(ggplot2)
install.packages("plotly")
library(plotly)

#distribución de parque grafica

install.packages("ggplot2")
library(ggplot2)


p <- ggplot(DB24, aes(x = distancia_parque)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un parque en metros", y = "Cantidad",
       title = "Distribución de la distancia a los parques") +
  theme_bw()
ggplotly(p)

#Para obtener las nuevas variables espaciales como Bares, Parada de Bus y Cercania a vías principales# --- Se utilizo otra hoja
#se instalan las librerias#
install.packages("pacman") 
library(pacman) 
p_load(caret, 
       Matrix,
       recipes,
       rio, 
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       tidymodels,
       ggplot2,
       scales,
       rvest,
       caret,
       stringr,
       boot,
       caret,
       modeest,
       stargazer,
       sf,
       leaflet,
       tmaptools,
       class,
       rgeos,
       nngeo,
       osmdata,
       randomForest,
       xgboost,
       nnls,
       data.table,
       ranger, SuperLearner, caret)
require("tidyverse")
install.packages("rgeos")

#Definir base#

setwd("C:/Users/ncoro/Desktop/2023/BML/taller 2.1")


DB17<- read.csv("DBdESCRIPPAR.csv")

install.packages("osmdata")
install.packages("magrittr")
library(osmdata)
library(magritt)
library (tidymodels)
## Creación de nuevas variables a partir de datos de otras fuentes##
#revisar variables disponibles en openmaps ##
available_tags("leisure") 
available_tags("amenity")
available_tags ("highway")

install.packages("sf")
library(sf)

install.packages("dplyr")
library(dplyr)
install.packages("leaflet")
library(leaflet)

latitud_central <- mean(DB17$lat)
longitud_central <- mean(DB17$lon)


localidad_chapinero <- opq("Chapinero, Bogota") %>%
  add_osm_feature(key = "admin_level", value = "9") %>%
  osmdata_sf()

library(leaflet)
library(sf)

localidad_chapinero <- st_read("localidad_chapinero.geojson")

PointChapinero = geocode_OSM("UPZ Chapinero, Bogotá", as.sf=T) 

#Coordenadas de chapinero

opq(bbox = getbb("UPZ Chapinero Bogotá"))

#coordenadas y distancia de bares#

library(osmdata)
library(magrittr)

Bares <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key ="amenity" , value = "bar") 
Bares_sf <- osmdata_sf(Bares)

Bar_geo <- Bares_sf$osm_polygons %>% 
  select(osm_id, name)


# Convierte tus datos a un objeto sf
Bares_sf <- st_as_sf(Bar_geo)

# Calcula los centroides
centroidesb_sf <- st_centroid(Bares_sf)

# Muestra los centroides
head(centroidesb_sf)

DB17_sf <- st_as_sf(DB17, coords = c("lon", "lat"), crs=4326)

library(sf)

dist_matrix <- st_distance(x = DB17_sf, y = centroidesb_sf)

# Encontramos la distancia mínima a un bar
dist_min <- apply(dist_matrix, 1, min)

# La agregamos la distancia al bar como variablea nuestra base de datos original 

DB17 <- DB17 %>% mutate(distancia_Bar = dist_min)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bar") 
class(osmbog)
view(DB17)

write.csv(DB17, "DBesP.csv", row.names = FALSE)

## Parada de Bus ##

osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 
class(osmbog)
osmbog_sf = osmbog %>% osmdata_sf()
View(osmbog_sf)
Transporte_publicoBog = osmbog_sf$osm_points %>% select(osm_id,amenity) 
View(Transporte_publicoBog)

distancia_Pbus<- st_distance(x=DB17_sf, y=Transporte_publicoBog)
dist_min2 <- apply(distancia_Pbus, 1 , min)
dist_min2<-data.frame(dist_min2)

DB17<-cbind(DB17, dist_min2)

DB17<- DB17 %>% mutate(distancia_Pbus= dist_min2)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="amenity" , value="bus_station")
class(osmbog)

View(DB17)

write.csv(DB17, "DBes2.csv", row.names = FALSE)

#coordenadas y distancia a avenidas principales

# Obtener avenidas principales en Bogotá

AV <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key ="highway" , value = "primary")
AV_sf <- osmdata_sf(AV)

AV_geometria <- AV_sf$osm_polygons %>% 
  select(osm_id, name)

# Convierte tus datos a un objeto sf
AV_sf <- st_as_sf(AV_geometria)

#Calculemos distancia a avenidas principales

distancia_AV<- st_distance(x=DB17_sf , y=AV_sf)
dist_min3 <- apply(distancia_AV, 1 , min)
dist_min3<-data.frame(dist_min3)

DB25<-cbind(DB17,dist_min3)

DB25 <- DB25 %>% mutate(distancia_AV= dist_min3)
osmbog = opq(bbox = getbb(" Bogotá ")) %>%
  add_osm_feature(key="highway" , value="primary")
class(osmbog)

View(DB25)

write.csv(DB25, "DBes3.csv", row.names = FALSE)

#Revisamos la base de datos homogenizada

cantidad_na <- sapply(DB25, function(x) sum(is.na(x)))
cantidad_na <- data.frame(cantidad_na)
porcentaje_na <- cantidad_na/nrow(DB25)
porcentaje_na <-porcentaje_na*100
porcentaje_na

#Eliminamos variables con NA para limpiar matriz

DB25 <- DB25 %>%
  select(-surface_total)

DB25 <- DB25 %>%
  select(-rooms)

#Para guardar la base de datos

View(DB25)

write.csv(DB25, "DBes4.csv", row.names = FALSE)

#Volvemos a dividir nuestras bases por entrenamiento:

BTRAIN<- DB25[DB25$base=="train",]
BTEST<- DB25[DB25$base=="test",]

#La base de datos de entrenamiento (BTRAIN) cuenta con 8560 observaciones.
#La base de datos de testeo (BTEST) cuenta con 2827 observaciones

#Guardar TRAIN y TEST#

write.csv(BTRAIN, "BTRAINL.csv", row.names = FALSE)
write.csv(BTEST, "BTEST.csv", row.names = FALSE)

##CREAMOS PREDICCION##

#incluir librerias
install.packages("pacman")
library(pacman) 
p_load(caret, 
       Matrix,
       recipes,
       rio, 
       tidyverse,
       glmnet,
       dplyr,
       readr,
       gamlr,
       tidymodels,
       ggplot2,
       scales,
       rvest,
       caret,
       stringr,
       boot,
       caret,
       modeest,
       stargazer,
       sf,
       leaflet,
       tmaptools,
       class,
       rgeos,
       nngeo,
       osmdata,
       randomForest,
       xgboost,
       nnls,
       data.table,
       ranger, SuperLearner, caret)
require("tidyverse")
library(tidymodels)

#definimos directorio

setwd("C:/Users/ncoro/Desktop/2023/BML/taller 2.1")

BTEST_P <- read.csv("BTEST.csv")

BTRAIN_P <- read.csv("BTRAINL.csv")
  

#Se elimina la columna de precio en la base Btest_P

BTEST_P<-BTEST_P%>% mutate(price = NULL)

#para las varibales "property_type" se vuelve dummy, donde 1 = Aparatamneto, 0 = Casa

BTEST_P<-BTEST_P%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))
BTRAIN_P<-BTRAIN_P%>% mutate(Apto = ifelse(property_type=="Apartamento", 1,0))

#Modelo 1 OLS

MD1 <- lm(price ~ factor(Apto) + factor(distancia_parque) + factor(distancia_Bar) + WC+bedrooms+area+distancia_Pbus+distancia_AV, data =BTRAIN_P)

predicciones_MD1 <- predict(MD1, newdata = BTRAIN_P)
Diferencia_MD1 <- (predicciones_MD1 - BTRAIN_P$price)
Diferencia_MD1<-data.frame(Diferencia_MD1)
MSE_MD1<- sqrt(mean((predicciones_MD1 - BTRAIN_P$price)^2))

require(stargazer)
stargazer(MD1)

view(MSE_MD1)

#Modelo 2 LASO

x_train <- model.matrix(price ~ factor(Apto) + factor(distancia_parque) + factor(distancia_Bar) + WC+bedrooms+area+distancia_Pbus+distancia_AV, data =BTRAIN_P)[, -1]
y_train <- BTRAIN_P$price

MD2lass_f <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_MD2Lass_f <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


MD2_Lasso_f <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error_MD2Lass_f$lambda.1se,
  standardize = TRUE
)

predicciones_train_mod2_Lass_f <- predict(MD2_Lasso_f, newx = x_train)
MSE_MD2 <- sqrt(mean((predicciones_train_mod2_Lass_f -y_train)^2))
Diferencia_MD2 <- (predicciones_train_mod2_Lass_f - BTRAIN_P$price)
Diferencia_MD2<-data.frame(Diferencia_MD2)


#Modelo 3 Random Forest

set.seed(10101)

MD3_forest <- ranger(price ~ Apto + distancia_parque + distancia_Bar + WC+bedrooms+area+distancia_Pbus+distancia_AV,
  data = BTRAIN_P,
  num.trees = 5,
  write.forest = TRUE #Para calcular la prediccón
)

MD3_forest_ <- randomForest(
  price ~ Apto + distancia_parque + distancia_Bar + WC+bedrooms+area+distancia_Pbus+distancia_AV,
  data = BTRAIN_P,
  num.trees = 5,
  write.forest = TRUE #Para obtener las variables importantes
)

varImp(MD3_forest_,scale=TRUE)

predicciones_MD3_rf_completo<-predict(MD3_forest, data = BTRAIN_P)$predictions

MSE_MD3 <- sqrt(mean((predicciones_MD3_rf_completo-BTRAIN_P$price)^2))
Diferencia_MD3 <- (predicciones_MD3_rf_completo - BTRAIN_P$price)
Diferencia_MD3<-data.frame(Diferencia_MD3)

#Modelo 4 XGBoost

xgb_train <- xgb.DMatrix(data = x_train, label = y_train)
xgb_test <- xgb.DMatrix(data = x_train, label = y_train) # xgb_test como la misma base train

watchlist <-list(train=xgb_train, test=xgb_test)

MD4<- xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 100)

summary(MD4)
predicciones_MD4 <-predict(MD4, xgb_test) # xgb_test corresponde a la misma base train

MSE_MD4 <- sqrt(mean((predicciones_MD4-BTRAIN_P$price)^2))
Diferencia_MD4 <- (predicciones_MD4 - BTRAIN_P$price)
Diferencia_MD4<-data.frame(Diferencia_MD4)

### Clasificación ###

Precio_compra_MD1<-ifelse(Diferencia_MD1<=-40000000,0, predicciones_MD1)
Precio_compra_MD1<-data.frame(Precio_compra_MD1)
Precio_tot_MD1<-colSums(Precio_compra_MD1)
view(Precio_tot_MD1)
Zeros_MD1<-ifelse(Precio_compra_MD1==0,TRUE, FALSE)
table(Zeros_MD1)


#Modelo OLS para variables relevantes del ejercicio anterior en RF (6 variables)

MD1C <- lm(price ~  distancia_Bar+WC+bedrooms+area+distancia_Pbus+distancia_AV, data =BTRAIN_P)

predicciones_MD1C <- predict(MD1C, newdata = BTRAIN_P)
Diferencia_MD1C <- (predicciones_MD1C - BTRAIN_P$price)
Diferencia_MD1C<-data.frame(Diferencia_MD1C)
MSE_MD1C<- sqrt(mean((predicciones_MD1C - BTRAIN_P$price)^2))


#Modelo 2C LASSO

x_trainb <- model.matrix(price ~distancia_Bar + WC+bedrooms+area+distancia_Pbus+distancia_AV, data =BTRAIN_P)[, -1]
y_trainb <- BTRAIN_P$price

MD2Class_fb <- glmnet(
  x           = x_trainb,
  y           = y_trainb,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_MD2_Lass_fb <- cv.glmnet(
  x      = x_trainb,
  y      = y_trainb,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


MD2_Lasso_fb <- glmnet(
  x           = x_trainb,
  y           = y_trainb,
  alpha       = 1,
  lambda      = cv_error_MD2_Lass_fb$lambda.1se,
  standardize = TRUE
)

predicciones_train_MD2_Lass_fb <- predict(MD2_Lasso_fb, newx = x_trainb)
MSE_MD2C <- sqrt(mean((predicciones_train_MD2_Lass_fb -y_trainb)^2))
Diferencia_MD2C <- (predicciones_train_MD2_Lass_fb - BTRAIN_P$price)
Diferencia_MD2C<-data.frame(Diferencia_MD2C)

#Modelo 3C Random Forest

set.seed(10101)


MD3_forestC <- ranger(
  price ~ distancia_Bar + WC+bedrooms+area+distancia_Pbus+distancia_AV,
  data = BTRAIN_P,
  num.trees = 5,
  write.forest = TRUE
)



predicciones_MD3_rf_completo<-predict(MD3_forestC, data = BTRAIN_P)$predictions

MSE_MD3C <- sqrt(mean((predicciones_MD3_rf_completo-BTRAIN_P$price)^2))
Diferencia_MD3C <- (predicciones_MD3_rf_completo - BTRAIN_P$price)
Diferencia_MD3C<-data.frame(Diferencia_MD3C)

#Modelo 4C XGBoost

xgb_trainb <- xgb.DMatrix(data = x_trainb, label = y_trainb)
xgb_testb <- xgb.DMatrix(data = x_trainb, label = y_trainb)

watchlistC <-list(train=xgb_trainb, test=xgb_testb)

MD4C<- xgb.train(data = xgb_trainb, max.depth = 3, watchlist=watchlistC, nrounds = 100)

summary(MD4C)
predicciones_MD4C <-predict(MD4C, xgb_testb)

MSE_MD4C <- sqrt(mean((predicciones_MD4C-BTRAIN_P$price)^2))
Diferencia_MD4C <- (predicciones_MD4C - BTRAIN_P$price)
Diferencia_MD4C<-data.frame(Diferencia_MD4C)

### Clasificación  de modelos por evaluación de compra

Precio_compra_MD1C<-ifelse(Diferencia_MD1C<=-40000000,0, predicciones_MD1C)
Precio_compra_MD1C<-data.frame(Precio_compra_MD1C)
Precio_tot_MD1C<-colSums(Precio_compra_MD1C)
view(Precio_tot_MD1C)
Zeros_MD1C<-ifelse(Precio_compra_MD1C==0,TRUE, FALSE)
table(Zeros_MD1C)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_MD2C<-ifelse(Diferencia_MD2C<=-40000000,0, predicciones_train_MD2_Lass_fb )
Precio_compra_MD2C<-data.frame(Precio_compra_MD2C)
Precio_tot_MD2C<-colSums(Precio_compra_MD2C)
view(Precio_tot_MD2C)
Zeros_MD2C<-ifelse(Precio_compra_MD2C==0,TRUE, FALSE)
table(Zeros_MD2C)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_MD3C<-ifelse(Diferencia_MD3C<=-40000000,0, predicciones_MD3_rf_completo)
Precio_compra_MD3C<-data.frame(Precio_compra_MD3C)
Precio_tot_MD3C<-colSums(Precio_compra_MD3C)
view(Precio_tot_MD3C)
Zeros_MD3C<-ifelse(Precio_compra_MD3C==0,TRUE, FALSE)
table(Zeros_MD3C)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_MD4C<-ifelse(Diferencia_MD4C<=-40000000,0, predicciones_MD4C  )
Precio_compra_MD4C<-data.frame(Precio_compra_MD4C)
Precio_tot_MD4C<-colSums(Precio_compra_MD4C)
view(Precio_tot_MD4C)
Zeros_MD4C<-ifelse(Precio_compra_MD4C==0,TRUE, FALSE)
table(Zeros_MD4C)


#Modelos con 5 variables

#OLS 1.2

MD12 <- lm(price ~  WC+bedrooms+area+distancia_Pbus+distancia_AV, data =BTRAIN_P)

predicciones_MD12 <- predict(MD12, newdata = BTRAIN_P)
Diferencia_MD12 <- (predicciones_MD12 - BTRAIN_P$price)
Diferencia_MD12<-data.frame(Diferencia_MD12)
MSE_MD12<- sqrt(mean((predicciones_MD12 - BTRAIN_P$price)^2))


#Lasso 2.1

x_trainc <- model.matrix(price ~WC+bedrooms+area+distancia_Pbus+distancia_AV, data =BTRAIN_P)[, -1]
y_trainc <- BTRAIN_P$price

MD2Class_fc <- glmnet(
  x           = x_trainc,
  y           = y_trainc,
  alpha       = 1,
  nlambda     = 200,
  standardize = TRUE
)

set.seed(10101)
cv_error_MD2_Lass_fc <- cv.glmnet(
  x      = x_trainc,
  y      = y_trainc,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)


MD2_Lasso_fc <- glmnet(
  x           = x_trainc,
  y           = y_trainc,
  alpha       = 1,
  lambda      = cv_error_MD2_Lass_fc$lambda.1se,
  standardize = TRUE
)

predicciones_train_MD2_Lass_fc <- predict(MD2_Lasso_fc, newx = x_trainc)
MSE_MD21 <- sqrt(mean((predicciones_train_MD2_Lass_fc -y_trainc)^2))
Diferencia_MD21 <- (predicciones_train_MD2_Lass_fc - BTRAIN_P$price)
Diferencia_MD21<-data.frame(Diferencia_MD21)

#Random Forest 3.1

set.seed(10101)


MD3_forestc <- ranger(
  price ~ WC+bedrooms+area+distancia_Pbus+distancia_AV,
  data = BTRAIN_P,
  num.trees = 5,
  write.forest = TRUE
)



predicciones_MD3_rf_completoc<-predict(MD3_forestc, data = BTRAIN_P)$predictions

MSE_MD31 <- sqrt(mean((predicciones_MD3_rf_completoc-BTRAIN_P$price)^2))
Diferencia_MD31 <- (predicciones_MD3_rf_completoc - BTRAIN_P$price)
Diferencia_MD31<-data.frame(Diferencia_MD31)

#XGBoost 4.1

xgb_trainc <- xgb.DMatrix(data = x_trainc, label = y_trainc)
xgb_testc <- xgb.DMatrix(data = x_trainc, label = y_trainc)

watchlistc <-list(train=xgb_trainc, test=xgb_testc)

MD41<- xgb.train(data = xgb_trainc, max.depth = 3, watchlist=watchlistc, nrounds = 100)

summary(MD41)
predicciones_MD41 <-predict(MD41, xgb_testc)

MSE_MD41 <- sqrt(mean((predicciones_MD41-BTRAIN_P$price)^2))
Diferencia_MD41 <- (predicciones_MD41 - BTRAIN_P$price)
Diferencia_MD41<-data.frame(Diferencia_MD41)

### Clasificación  de modelos por evaluación de compra

Precio_compra_MD12<-ifelse(Diferencia_MD12<=-40000000,0, predicciones_MD12)
Precio_compra_MD12<-data.frame(Precio_compra_MD12)
Precio_tot_MD12<-colSums(Precio_compra_MD12)
view(Precio_tot_MD12)
Zeros_MD12<-ifelse(Precio_compra_MD12==0,TRUE, FALSE)
table(Zeros_MD12)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_MD21<-ifelse(Diferencia_MD21<=-40000000,0, predicciones_train_mod2_Lass_fc )
Precio_compra_MD21<-data.frame(Precio_compra_MD21)
Precio_tot_MD21<-colSums(Precio_compra_MD21)
view(Precio_tot_MD21)
Zeros_MD21<-ifelse(Precio_compra_MD21==0,TRUE, FALSE)
table(Zeros_MD21)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_MD31<-ifelse(Diferencia_MD31<=-40000000,0, predicciones_MD3_rf_completoc  )
Precio_compra_MD31<-data.frame(Precio_compra_MD31)
Precio_tot_MD31<-colSums(Precio_compra_MD31)
view(Precio_tot_MD31)
Zeros_MD31<-ifelse(Precio_compra_MD31==0,TRUE, FALSE)
table(Zeros_MD31)#La cantidad de falsos (diferente de cero) son las propiedades compradas

Precio_compra_MD41<-ifelse(Diferencia_MD41<=-40000000,0, predicciones_MD41  )
Precio_compra_MD41<-data.frame(Precio_compra_MD41)
Precio_tot_MD41<-colSums(Precio_compra_MD41)
view(Precio_tot_MD41)
Zeros_MD41<-ifelse(Precio_compra_MD41==0,TRUE, FALSE)
table(Zeros_MD41) #La cantidad de falsos (diferente de cero) son las propiedades compradas

#===========================================================================================
#Gráfica de MSE

RMSE_modelos<-c(MSE_MD1, MSE_MD2, MSE_MD3, MSE_MD4)
modelos_<-c('modelo1_10var','modelo2_10var', 'MD3_10var', 'modelo4_10var')
RMSE_errores<-data.frame(modelos_,RMSE_modelos)

#Se grafica el resultado
ggplot(data=RMSE_errores, aes(x = modelos_, y = RMSE_modelos, group=1)) + 
  geom_line()+   geom_point()+  labs(title = "Comparación diferentes modelos en términos de RMSE") 

#===========================================================================================
#Se entrena el modelo 3 con las 8 variables. Se realizaron varias iteraciones y se determinó que el número de árboles con el que se obtiene el menor RMSE es 1000, así como menor valor y mayor cantidad de viviendas compradas.



set.seed(10101)
MD3_forest1000 <- ranger(price ~ Apto + distancia_parque + distancia_Bar + WC+bedrooms+area+distancia_Pbus+distancia_AV,
  data = BTRAIN_P,
  num.trees = 1000,
  write.forest = TRUE
)

predicciones_MD3_rf_completo1000<-predict(MD3_forest1000, data = BTRAIN_P)$predictions

MSE_MD31000 <- sqrt(mean((predicciones_MD3_rf_completo1000-BTRAIN_P$price)^2))
Diferencia_MD31000 <- (predicciones_MD3_rf_completo1000 - BTRAIN_P$price)
Diferencia_MD31000<-data.frame(Diferencia_MD31000)

Precio_compra_MD31000<-ifelse(Diferencia_MD31000<=-40000000,0, predicciones_MD3_rf_completo1000  )
Precio_compra_MD31000<-data.frame(Precio_compra_MD31000)
Precio_tot_MD31000<-colSums(Precio_compra_MD31000)
view(Precio_tot_MD31000)
Zeros_MD31000<-ifelse(Precio_compra_MD31000==0,TRUE, FALSE)
table(Zeros_MD31000)#La cantidad de falsos (diferente de cero) son las propiedades compradas

#De acuerdo a la comparación de los modelos, el modelo MD3_forest con 8 variables explicativas es el cual tiene la mejor proporción de dinero invertido/viviendas comparas. 
#Por lo tanto, Se realiza la predicción con el modelo MD3_forest en la base BTEST_P 

Predicciones_Precios1 <- predict(MD3_forest1000,  data = BTEST_P)$predictions #Se realiza predicción sobre la base Test con el modelo RF de 1000 árboles
Predicciones_Precios1 <- data.frame (Predicciones_Precios1)
View(Predicciones_Precios1)
Predicciones_Precios1 <- cbind(BTEST_P$property_id ,Predicciones_Precios1)
View(Predicciones_Precios1)
View(cbind(BTEST_P$property_id, Predicciones_Precios1$`BTEST_P$property_id`))

summary(Predicciones_Precios1)
colnames(Predicciones_Precios1) <- c('property_id','price') #Se renombran las columnas

DTEST <- read.csv("test.csv")

Property_id_o2<-DTEST$property_id #Se importa el id con el orden original de la base Test entregada por Ignacio
Property_id_o2<- data.frame (Property_id_o2)
colnames(Property_id_o2) <- c('property_id') #Se renombran las columnas

Property_id_o2<-left_join(Property_id_o2, Predicciones_Precios1, by="property_id" ) #Se hace left join para que los resultados queden con el mismo orden de la base test original
colnames(Property_id_o2) <- c('property_id','price') #Se renombran las columnas

write.csv (Property_id_or, "../Taller 2.1/predictions_norozc5.csv") #Submission file
