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
library(TSP)
options(scipen = 999)

# Cargar datos -------------------------------------------------------------------

# buenosaires.data

url_base <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets"



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


# Segundo ejercicio  ==================================================================

#Tomar ejercicio "El problema del viajante" y agregar dos POI más.

# POIS Origen
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


#POIS destino
new_POI <- data.frame(x = c(-58.47387347223715, -58.4792703267673, -58.477575404481414),  # Longitud
                      y = c(-34.581407985725164, -34.58247346544145, -34.58221973301764),    # Latitud
                      poi = c("Estación De Los Incas - Parque Chas",
                              "Desviador de transito Vicente Chas",
                              "Baldosa de la Memoria de Parque Chas"),
                      tipo = "Dest"
                      ) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>   # Tabla -> objeto sf (EPSG:4326 = lon/lat)
  st_transform(st_crs(CALLE))                     # Lo pasamos al mismo CRS que las calles

new_POI <- bind_rows(POI, new_POI)
net <- activate(net, "nodes")
cost_matrix <- st_network_cost(net, from = new_POI, to = new_POI,
                               weights = "weight", direction = "all")#Empieza en new_POI y termina ahi porque el recorrido empieza y termina en el mismo lugar

rdm_idxs <- st_nearest_feature(new_POI, net)
row.names(cost_matrix) <- rdm_idxs
colnames(cost_matrix)  <- rdm_idxs
cost_matrix

# --- 2.3 Resolver el TSP ---

tour <- solve_TSP(TSP(units::drop_units(cost_matrix))) #esto es una medida puramente matematica ORDEN Y DISTANCIA
tour   # Muestra el orden de visita y la longitud total del recorrido
(tour_idxs <- as.numeric(names(tour))) #convierte en numericos la secuencia de puntos/futuros nodos
tour_idxs

tour_path <- map2_df(tour_idxs[1:(length(tour_idxs))], #esta lleva ese calculo matematico al plano cartografico
                     tour_idxs[c(2:length(tour_idxs), 1)],
                     \(from, to) {          # \(...) es la forma corta de function(...)
                       st_network_paths(net, from = from, to = to,
                                        weights = "weight", mode = "all")
                     })

#Convertimos los índices de NODOS de cada tramo en geometrías.
#  slice() selecciona los nodos por posición; summarise() los une en
#  una sola geometría por tramo.
a_nodes <- map_df(tour_path[["node_paths"]],
                  \(nodes){
                    net |>
                      activate("nodes") |>
                      slice(nodes |> unlist()) |>
                      st_as_sf() |>
                      summarise()
                  })

#Lo mismo con las ARISTAS (los tramos de calle recorridos).
a_edges <- map_df(tour_path[["edge_paths"]],
                  \(edges){
                    net |>
                      activate("edges") |>
                      slice(edges |> unlist()) |>
                      st_as_sf() |>
                      summarise()
                  })

#Aca me tira este error:
# Error in `map()`:
#  ℹ In index: 1.
#Caused by error in `[[<-`:
#  ! Assigned data `geom` must be compatible with existing data.
#✖ Existing data has 1 row.
#✖ Assigned data has 0 rows.
#ℹ Row updates require a list value. Do you need `list()` or `as.list()`?
#  Caused by error in `vectbl_recycle_rhs_rows()`:
#  ! Can't recycle input of size 0 to size 1.
# Run `rlang::last_trace()` to see where the error occurred.



#Esto lo resolvi con IA. 
a_edges <- map(tour_path[["edge_paths"]], \(edges) {
  if (length(edges) == 0) return(NULL) # Si algún tramo está vacío, lo salta
  
  net |>
    activate("edges") |>
    slice(unlist(edges)) |>
    st_as_sf() |>
    summarise()
})

# 2. Filtramos nulos (por seguridad) y unimos todo en un solo objeto sf
a_edges <- bind_rows(compact(a_edges_list))


ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), color = "grey") +   # Red completa de fondo
  geom_sf(data = st_as_sf(net, "nodes"), color = "grey", size = 0.3) +
  geom_sf(data = a_edges, linewidth = 1) +                   # Calles del recorrido (negro)
  geom_sf(data = a_nodes) +                                  # Nodos del recorrido
  geom_sf(data = st_as_sf(net, "nodes")[tour_idxs, ],        # Paradas (nodos de los POI)
          color = "blue", shape = 20) +
  geom_sf(data = st_as_sf(net, "nodes")[tour_idxs[1], ],     # Punto de partida: estrella
          color = "blue", shape = 8, size = 5) +
  geom_sf(data = new_POI, color = "tomato", shape = 7) +     # Ubicación original de los POI
  theme_void()

rm(a_edges, a_edges_list, a_nodes, cost_matrix, new_POI, rdm_idxs, tour, tour_idxs, tour_path)

