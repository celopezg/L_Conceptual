# Paquetes y versiones requeridas

library(rgdal)        # Versión 1.2-8
library(leaflet)      # Versión 1.1.0 
library(htmlwidgets)  # Versión 1.0 
library(tidyverse)    # Versión 1.2.1
library(rjson)        # Versión 0.2.15
library(readxl)       # Versión 1.0.0
library(viridis)      # Versión 0.4.0 
library(extrafont)    # Versión 0.17


# Lectura del estándar División Política y Administrativa de Colombia - DIVIPOLA

divipola.R <- read.table("Estandares/DIVIPOLA_20160930.csv", sep=";", header=T)


# Base de datos con información de cabeceras municipales del archivo divipola

cabeceras <- divipola.R %>% select(code_dept=Código.Departamento,
                                   code_mun=Código.Municipio,
                                   departamentos=Nombre.Departamento,
                                   municipios=Nombre.Municipio,
                                   tipo_centro=Tipo.Centro.Poblado,
                                   longitud=Longitud,
                                   latitud=Latitud) %>% 
  filter(tipo_centro == "CABECERA MUNICIPAL (CM)")  


# Importar datos con variables de aspirantes de pregrado por lugar de nacimiento

procedencia.R <- read_xlsx("Fuentes Graficos/Aspirantes2019.xlsx", sheet = "I") 
procedencia.R <- procedencia.R %>% filter(!is.na(TIPO_INS))

aspirantes <- procedencia.R %>% select(depart_asp=DEP_NAC,
                                       codept_asp=COD_DEP_NAC,
                                       ciudad_asp=CIU_NAC,
                                       codecity_asp=COD_CIU_NAC,
                                       long_asp=LON_CIU_NAC,
                                       lat_asp=LAT_CIU_NAC) %>%
  filter(depart_asp != "DPTO EXTRANJERO")


# Total de aspirantes de pregrado por departamento de nacimiento 

cant_asp <- aspirantes %>% group_by(codept_asp) %>% summarise(Total=n())


# Total de aspirantes de pregrado por municipio de nacimiento 
# Se eliminan los rgistros con datos faltantes en municipio de nacimiento

cantasp_city <- aspirantes %>% group_by(codecity_asp) %>% summarise(Total=n())
cantasp_city <- cantasp_city %>% filter(!is.na(codecity_asp))
# 1 registro 2019-1

# Función para construcción de información de las capitales de departamento

check.integer <- function(x) {
  x == round(x)
}

# Información DIVIPOLA para capitales de departamento

capitales <- cabeceras %>% filter(check.integer((cabeceras$code_mun-1)/1000)==TRUE) %>% 
  filter(code_mun!="25001")


# convertir variables de longitud y latitud a valores numéricos

options(digits=10)

capitales$longitud <- as.numeric(str_replace(capitales$longitud, ",", "."))
capitales$latitud  <- as.numeric(str_replace(capitales$latitud, ",", "."))

################################################################
# Extraer lista de códigos de los municipios - 1122 - municipios
################################################################

# Archivo json con formato lista

json_file <- "JSON/mpio2.json"
json_data <- fromJSON(paste(readLines(json_file), collapse = "\n")) 

# Cambiar formato de variable MPIOS a integer

for(i in 1:1122){
  json_data$features[[i]]$properties$MPIOS = as.integer(json_data$features[[i]]$properties$MPIOS)
}

# Crear matriz de ceros con dos columnas y 1122 fila (# municipios)

codigos <- matrix(0, nrow=1122,ncol=2)

# Insertar en la matriz el código de municipios del objeto JSON
# Importante conservar el orden de códigos de municipios del JSON

for(i in 1:1122){
  codigos[i,1] = json_data$features[[i]]$properties$MPIOS
}

# Insertar cantidad de aspirantes por municipio de nacimiento a la matriz 
# Importante insertar en el orden de códigos de municipios del JSON

for(i in cantasp_city$codecity_asp){
  codigos[codigos[,1] == i, 2] = cantasp_city$Total[cantasp_city$codecity_asp == i]
}

######### Json por jurisdicciones de municipios

cities_col.R <- rgdal::readOGR("JSON/mpio5.json", use_iconv = T, encoding="UTF-8")

cities_col.R@data <- cities_col.R@data[c(6,8)]

#Agregar información al Spatial Data Frame

cities_col.R@data$CODE_MPI <- codigos[ ,1]
cities_col.R@data$CANT_ASP <- codigos[ ,2]

##############################################################
#Lectura de JSON de Colombia por departamentos
##############################################################

# Archivo json con formato spatialPolygonsDataFrame

colombia.R <- rgdal::readOGR("JSON/depto4.json", use_iconv = T, encoding= "UTF-8")

# Crear matriz de ceros con dos columnas y 33 filas (# departamentos)

codigos2 <- matrix(0, nrow = 33, ncol = 2)

# insertar en la matriz los códigos DIVIPOLA de los departamentos

for(i in 1:33){
  codigos2[i,1] = as.integer(as.character(colombia.R@data$DPTO[i]))
}

# Insertar cantidad de aspirantes por departamento de nacimiento a la matriz 
# Importante insertar en el orden de códigos de departamentos del objeto

for(i in cant_asp$codept_asp){
  codigos2[codigos2[,1] == i, 2] = cant_asp$Total[cant_asp$codept_asp == i]
}

# Eliminar información complementaria

colombia.R@data<-colombia.R@data[2] 

# Insertar en el objeto spatialPoly .. la cantidad de aspirantes por depto de nacimiento

colombia.R@data$CANT_ASP <- codigos2[,2]

##########################################################
# Componente final de mapas
##########################################################


# Ubicar el centroide de cada departamento 

x <- NULL
for(i in 1:33){
  x[i] <- as.character(as.factor(colombia.R@data$NOMBRE_DPT[[i]]))
}

y <- matrix(0, 33, 2)

for(i in 1:33){
  y[i,] <- colombia.R@polygons[[i]]@Polygons[[1]]@labpt
  
}

# Centroides de los departamentos

centro_dept <- data.frame(x,y)
colnames(centro_dept) <- c("dept", "lon", "lat")

# Seleccionar mapas libres de base
# ESRI es un proveedor de bases de mapas con licencia pública
esri <- grep("^Esri", providers, value = T)
esri<- esri[c(11,2,10)]
names.esri <- c("Ligero","Street","Satélite <br>&nbsp;&nbsp;&nbsp;&nbsp; NatGeo")

#Filtrar sedes de la Universidad Nacional de Colombia

Sede <- c("Medellín", "Bogotá", "Manizales", "Tumaco", "Palmira", "Arauca", "Caribe", "Amazonas")

sedes <- cabeceras %>% filter(code_mun %in% c(5001, 11001, 17001, 52835, 76520, 81001, 88001, 91001)) %>% mutate(Sede = Sede)

# Convertir variables de longitud y de latitud a valores numéricos

sedes$longitud <- as.numeric(str_replace(sedes$longitud, ",", "."))
sedes$latitud  <- as.numeric(str_replace(sedes$latitud, ",", "."))

# Parametrización de íconos de la UN
#font_import();n

sedeIcon <- makeAwesomeIcon (markerColor = "green", iconColor = "white", 
                             fontFamily = "Leonardian", text = "un")

# aparece el nombre de la sede en la etiqueta
label_sede <- sprintf("<strong>%s %s</strong>", 
                      "Sede ", sedes$Sede)%>% lapply(htmltools::HTML)

# Función para cambiar mayúsculas a minúsculas

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}