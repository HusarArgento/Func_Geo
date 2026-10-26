#Tarea 4


#Librerias ---------------------------------------------------------------

library(tidyverse)
library(sf)
library(geos)
library(ggpubr)
library(leaflet)
library(igraph)
library(tidygraph)
library(sfnetworks)
library(patchwork)
options(scipen = 999)

# Cargar datos -------------------------------------------------------------------

# buenosaires.data

url_base <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets"
sel <- c(11, 15) # comunas a analizar


COMUNA15 <- st_read(paste(url_base, "ministerio-de-educacion",
                         "comunas/comunas.geojson", sep = "/"))  |>
  st_transform(5347) |>
  filter(comuna == 15 )

COMUNA_buffer <- COMUNA15 |>
  st_buffer(500) 


CALLE <- st_read(paste(url_base, "jefatura-de-gabinete-de-ministros",
                       "calles/callejero.geojson", sep = "/") ) |>
  st_transform(5347) |>
  st_intersection(COMUNA_buffer)



url_base <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets"

# Transformaciones de datos  -------------------------------------------------------------------

COMUNA15 <- st_read(paste(url_base, "ministerio-de-educacion",
                          "comunas/comunas.geojson", sep = "/"))  |>
  st_transform(5347) |>
  filter(comuna == 15 )

COMUNA_buffer <- COMUNA15 |>
  st_buffer(500) 

CALLE <- paste(url_base,
               "jefatura-de-gabinete-de-ministros",
               "calles/callejero.geojson", sep = "/") |>
  st_read() |>
  st_transform(5347) |>
  st_intersection(st_geometry(COMUNA_buffer))

CALLE2 <- CALLE |>
  filter(st_geometry_type(geometry) %in% c("LINESTRING", "MULTILINESTRING")) |>  # solo líneas
  st_cast("MULTILINESTRING") |>   # ESTANDARIZO
  st_cast("LINESTRING")  #Convierto

table(CALLE2$sentido, useNA = "ifany")

# RECIENTE  DECRECIENTE        DOBLE     PEATONAL PJE. PRIVADO 
#   1750         1657          586          278            1 

CALLE2 <- CALLE2 |>
  filter(sentido %in% c("CRECIENTE", "DECRECIENTE", "DOBLE"))

table(CALLE2$sentido, useNA = "ifany")

#CRECIENTE DECRECIENTE       DOBLE 
#  1750        1657         586 

CALLE_aux1 <- CALLE2 |>
  filter(sentido %in% c("DECRECIENTE", "DOBLE")) |>
  mutate(cod_edge = -1) |>
  st_reverse()

CALLE_aux2 <- CALLE2 |>
  filter(sentido %in% c("CRECIENTE", "DOBLE")) |>
  mutate(cod_edge = 1)

CALLE3 <- rbind(CALLE_aux1, CALLE_aux2)

#Verifico
table(CALLE2$sentido)
table(ORIG = CALLE3$sentido, EDGE = CALLE3$cod_edge)
table(CALLE3$cod_edge)

# Construccion de la Red  -------------------------------------------------------------------

#Matriz de pesos
net <- as_sfnetwork(CALLE3, directed = TRUE,
                    length_as_weight = TRUE)

#Me quedo con el componente mas grande para evitar islotes
net <- net |>
  activate("nodes") |>
  filter(group_components() == 1)

net

#Puntos de interes
POI <- data.frame(
  poi  = c("Facultad de Agronomía (UBA)",
           "Estación Federico Lacroze",
           "Estadio Diego A. Maradona",
           "Estadio de Atlanta"),
  tipo = "Orig",
  x    = c(-58.4826, -58.4553, -58.4766, -58.4494),   # longitud
  y    = c(-34.5930, -34.5874, -34.6063, -34.5937)    # latitud
) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>   # tabla -> objeto sf
  st_transform(st_crs(CALLE))                     # al mismo CRS que las calles

#  Ya no necesitamos los objetos auxiliares
rm(CALLE_aux1, CALLE_aux2, CALLE3)

# Aplicaciones  -------------------------------------------------------------------

# Primer ejercicio  ==================================================================

#Comparar las 2 plazas mas cercanas de la comuna 15 por red  y por distancia euclidiana, a las escuelas de la comuna

#Escuelas
ESC <- st_read(paste(url_base, "ministerio-de-educacion",           # paste() arma la URL
                     "establecimientos-educativos",                 # completa uniendo las
                     "establecimientos_educativos.geojson",         # partes con "/"
                     sep = "/")) |>
  st_transform(5347) |>
  filter(str_detect(nen_mde, "Nivel primario común") & ges == "Estatal") |> 
  st_intersection(st_geometry(COMUNA15))

PLAZAS <- st_read(paste(url_base, "secretaria-de-desarrollo-urbano",
                        "espacios-verdes/espacio_verde_publico.geojson",
                        sep = "/")) |>
  st_transform(5347) |>                   
  filter(clasificac %in% c("PLAZA")) |>          
  st_intersection(st_geometry(COMUNA15)) |>      
  st_centroid()

#blending
new_net <- net |>
  activate("nodes") |>
  st_network_blend(st_as_sf(st_geometry(bind_rows(ESC, PLAZAS)))) |> #destila la geometría de otros datos, y la vuelve a empaquetar en una tabla
  activate("edges") |>
  mutate(weight = edge_length()) |>
  activate("nodes")

#Matriz de costos
cost_matrix  <- st_network_cost(new_net,
                                from = ESC, to = PLAZAS,
                                weights = "weight", direction = "all")
n <- 2
n2 <- 3 

euclidiana <- st_distance(ESC, PLAZAS)

#con n=2
closest <- PLAZAS[apply(cost_matrix, 1, function(x) order(x)[1:n]) |> as.numeric(), ] |>
  st_coordinates()

#con n=3
closest2 <- PLAZAS[apply(cost_matrix, 1, function(x) order(x)[1:n2]) |> as.numeric(), ] |>
  st_coordinates()

matriz_euclidiana <- as.matrix(euclidiana)
indice_tortuosidad <- cost_matrix / matriz_euclidiana

#con n=2
aux_df <- data.frame(escuela = rep(ESC$fna, each = n),                  # nombre de la escuela
                     ori_x = rep(st_coordinates(ESC)[, 1], each = n),   # X origen (escuela)
                     ori_y = rep(st_coordinates(ESC)[, 2], each = n),   # Y origen (escuela)
                     des_x = closest[, 1],                              # X destino (plaza)
                     des_y = closest[, 2])                              # Y destino (plaza)

#con n=3
aux_df2 <- data.frame(escuela = rep(ESC$fna, each = n2),                  # nombre de la escuela
                     ori_x = rep(st_coordinates(ESC)[, 1], each = n2),   # X origen (escuela)
                     ori_y = rep(st_coordinates(ESC)[, 2], each = n2),   # Y origen (escuela)
                     des_x = closest2[, 1],                              # X destino (plaza)
                     des_y = closest2[, 2])                              # Y destino (plaza)

#Graficamos

g1 <- ggplot() +
  geom_sf(data = st_as_sf(new_net, "edges"), color = "grey80") +   # Calles (aristas)
  geom_sf(data = st_as_sf(new_net, "nodes"), color = "grey80", size = 0.3) +  # Nodos
  geom_sf(data = COMUNA15, fill = NA, color = "grey40") +          # Límite de la comuna
  geom_sf(data = ESC, color = "tomato") +                          # Escuelas en rojo
  geom_sf(data = PLAZAS, color = "darkgreen", shape = 2) +         # Plazas: triángulos verdes
  # Flechas desde cada escuela hacia sus plazas más cercanas.
  # OJO: la flecha es recta solo para visualizar; la distancia usada
  # para elegir la plaza se midió por la red.
  geom_segment(data = aux_df, aes(x = ori_x, y = ori_y,
                                  xend = des_x, yend = des_y,
                                  color = escuela),                # Un color por escuela
               alpha = 0.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  guides(color = "none") +   # Ocultamos la leyenda (serían demasiadas escuelas)
  theme_void() +
  ggtitle("2 Plazas más cercanas (n = 2)")


g2 <- ggplot() +
  geom_sf(data = st_as_sf(new_net, "edges"), color = "grey80") +   # Calles (aristas)
  geom_sf(data = st_as_sf(new_net, "nodes"), color = "grey80", size = 0.3) +  # Nodos
  geom_sf(data = COMUNA15, fill = NA, color = "grey40") +          # Límite de la comuna
  geom_sf(data = ESC, color = "tomato") +                          # Escuelas en rojo
  geom_sf(data = PLAZAS, color = "darkgreen", shape = 2) +         # Plazas: triángulos verdes
  # Flechas desde cada escuela hacia sus plazas más cercanas.
  # OJO: la flecha es recta solo para visualizar; la distancia usada
  # para elegir la plaza se midió por la red.
  geom_segment(data = aux_df2, aes(x = ori_x, y = ori_y,
                                  xend = des_x, yend = des_y,
                                  color = escuela),                # Un color por escuela
               alpha = 0.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  guides(color = "none") +   # Ocultamos la leyenda (serían demasiadas escuelas)
  theme_void() +              # Fondo limpio, sin ejes
  ggtitle("3 Plazas más cercanas (n = 3)")

g1 + g2
ggsave("comparacion_plazas.png", width = 12, height = 6)
