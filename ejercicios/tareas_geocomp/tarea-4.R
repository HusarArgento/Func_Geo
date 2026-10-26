#Tarea 4

#Librerias
<<<<<<< HEAD
=======
library(tidyverse)

library(sf)
library(geos)

library(ggpubr)
library(leaflet)

library(igraph)
library(tidygraph)

library(sfnetworks)

options(scipen = 999)

# Cargar datos -------------------------------------------------------------------
# buenosaires.data

url_base <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets"
sel <- c(11, 15) # comunas a analizar

COMUNAS <- st_read(paste(url_base, "ministerio-de-educacion",
                         "comunas/comunas.geojson", sep = "/"))  |>
  st_transform(5347) |>
  filter(comuna %in% sel)

COMUNAS_buffer <- COMUNAS |>
  st_buffer(750) # Buffer de 750 metros

BARRIOS <- st_read(paste(url_base, "ministerio-de-educacion",
                         "barrios/barrios.geojson", sep = "/") ) |>
  st_transform(5347) |>
  filter(comuna %in% sel)

CALLE <- st_read(paste(url_base, "jefatura-de-gabinete-de-ministros",
                       "calles/callejero.geojson", sep = "/") ) |>
  st_transform(5347) |>
  st_intersection(COMUNAS_buffer) |>
  filter(sentido %in% c("CRECIENTE", "DECRECIENTE","DOBLE") )

rm(url_base, COMUNAS_buffer)
>>>>>>> 63e207a3e68bfe912af13e100e7f143a0ea14de6
