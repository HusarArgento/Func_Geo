# Clase 2. Proyecciones geográficas de Argentina

# Cargamos librerías

library(tidyverse)
library(sf)
library(ggpubr)

# Cargamos los datos (y simplificamos los datos para acelerar el procesamiento)

arg <- read_sf("data/pais_simplificado.json")

# Creamos una función que cambie la proyección y genera un
#   mapa en esta proyección.

gg_proj <- function(epsg, desc = "", db = arg){

  p <- db |>
    st_transform(crs = epsg) |>
    ggplot() +
    geom_sf() +
    labs(title = paste0("EPSG ", epsg, "\n", desc)) +
    theme_void()

  return(p)
}

# CRS posibles
# 4326    WGS-84 reference ellipsoid
# 5346    POSGAR 07 / Argentina 4
# 3031    Antarctic Polar Stereographic
# 32720   WGS 84 / UTM zone 20S

# CRS "poco adecuados"
# 4269    NAD83. Usado para visualizar América del Norte
# 3035    ETRS-LAEA - Lambert Azimuthal Equal-Area projection. Recomendado por la European Commission.

# Otros CRS (no utilizados)
# 3857    Web mercator
# 4573   UTM Zone 33N (~)

epsg = c(4326, 5346, 3031, 32720, 4269, 3035)

desc = c("WGS-84", "POSGAR 07 / Argentina 4", "WGS 84 / Antarctic Polar Stereographic",
         "WGS 84 / UTM zone 20S", "NAD83 (~EEUU)", "ETRS-LAEA (~UE)")

p <- map2(epsg, desc, gg_proj)

ggpubr::ggarrange(plotlist = p)

rm(p, epsg, desc, gg_proj)
rm(arg)
