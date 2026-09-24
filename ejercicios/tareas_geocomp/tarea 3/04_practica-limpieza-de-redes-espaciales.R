# Redes Espaciales II: limpieza de redes espaciales con datos de redes viales 

library(tidyverse)

library(dbscan)

library(sf)
library(geos)

library(igraph)
library(tidygraph)

library(sfnetworks)

library(osmdata)

library(mapview)

library(leaflet)

# ╠ Manejo de redes direccionales (buenosaires.data) ----------------------------------------

  base_url <- paste("https://cdn.buenosaires.gob.ar",
                    "datosabiertos/datasets", sep = "/")

  CABALLITO <- paste(base_url,
                     "ministerio-de-educacion",
                     "comunas/comunas.geojson", sep = "/") |>
    st_read()  |>
    st_transform(5347) |>
    filter(comuna == 6)

  CABALLITO_buffer <- CABALLITO |>
    st_buffer(750) # Buffer de 750 metros

  CALLE <- paste(base_url,
                 "jefatura-de-gabinete-de-ministros",
                 "calles/callejero.geojson", sep = "/") |>
    st_read() |>
    st_transform(5347) |>
    st_intersection(CABALLITO_buffer)

  table(CALLE$sentido)

  CALLE <- CALLE |>
    filter(sentido %in% c("CRECIENTE", "DECRECIENTE","DOBLE") )

  CALLE_aux1 <- CALLE |> filter(sentido %in% c("DECRECIENTE","DOBLE") ) |> mutate(cod_edge = -1) |> st_reverse()
  CALLE_aux2 <- CALLE |> filter(sentido %in% c("CRECIENTE", "DOBLE") ) |> mutate(cod_edge = 1)
#Aca crea un df para las dobeles y decrecientes (CALLE_aux1) y otro para las dobles y decrecientes
  
    CALLE2 <- rbind(CALLE_aux1, CALLE_aux2)
#Luego del rbind vamos a tener el doble de calles de doble sentido, porque estan duplicadas
    
  table(CALLE$sentido)
  table(ORIG = CALLE2$sentido, EDGE = CALLE2$cod_edge)
  table(CALLE2$cod_edge)
#Aca vamos a tener calles de doble mano repetidas, en una va a a figurar como edge_code 1 y en otra -1

    CALLE2 |> filter(id == 11018) |> st_geometry()
#Filtramos la calle marie curie y extraemos la geometria. Deberían aparecer dos
    #    geometrías con las mismas coordenadas pero en orden inverso
    #    (si es DOBLE). Compará el primer y el último punto de cada una.
    
  net1 <- as_sfnetwork(CALLE2, directed = TRUE,
                       length_as_weight = TRUE)

  rm(base_url)

# ╚ Limpieza de una red real (datos OSM) ---------------------------------------------------------------------

  # 1. Descargar información de OSM --------------------------------------------

  ESCOBAR <- getbb("Belén de Escobar, Provincia de Buenos Aires",
                   format_out = "sf_polygon")

  leaflet() |>
    addTiles() |>
    addPolygons(data = ESCOBAR, color = "black")

  # Consulta
  CALLE_OSM <- getbb("Belén de Escobar, Provincia de Buenos Aires") |>
    # Realizar consulta en OSM
    opq() |>
    add_osm_features(features = list(#"highway" = "construction",
                                     "highway" = "living_street",
                                     "highway" = "residential",
                                     "highway" = "motorway",
                                     "highway" = "motorway_link",
                                     "highway" = "primary",
                                     "highway" = "primary_link",
                                     "highway" = "secondary",
                                     "highway" = "secondary_link",
                                     "highway" = "tertiary",
                                     "highway" = "tertiary_link",
                                     "highway" = "trunk",
                                     "highway" = "trunk_link",
                                     "highway" = "unclassified",
                                     "highway" = "service")) |>
    osmdata_sf()

  # Por qué hay "polygons"?
  summary(CALLE_OSM)

  leaflet() |>
    addTiles() |>
    addPolygons(data = ESCOBAR, color = "black") |>
    addPolylines(data = CALLE_OSM$osm_lines, color = "tomato") |>
    addPolygons(data = CALLE_OSM$osm_polygons, color = "darkblue")

  CALLE_OSM <- CALLE_OSM |>
    # Extraer capas necesarias y unificar en única base
    magrittr::extract(c("osm_lines", "osm_polygons")) |>
    bind_rows() |>
    # Seleccionar variables necesarias
    select(osm_id, name, highway, lanes,oneway, maxspeed, surface) |>
    # Transformar proyección
    st_transform(5347)

  # 2. Preprocesamiento de datos -----------------------

  # a. Castear a LINESTRING (no es necesario usar {geos} porque no hay multilíneas o multupolígonos)
  st_geometry_type(CALLE_OSM) |> fct_drop() |>  table()
  CALLE_OSM <- st_cast(CALLE_OSM, "LINESTRING")
  st_geometry_type(CALLE_OSM) |> fct_drop() |>  table()

  # b. Round coordinates (tener cuidado de estar en proyección plana y considerar la unidad)
  st_geometry(CALLE_OSM) <- st_geometry(CALLE_OSM)  |>
    lapply(function(x) round(x, 0)) |>
    st_sfc(crs = st_crs(CALLE_OSM))

  # c. Manejar bordes bidireccionales (calles "doblemano")

  # Por qué hay calles sin dirección?
  table(CALLE_OSM$highway, DOBLE_MANO = CALLE_OSM$oneway, exclude = NULL)

  factpal <- colorFactor(topo.colors(length(unique(CALLE_OSM$highway))),
                         CALLE_OSM$highway)
  CALLE_OSM |>
    st_transform(4326) |>
    filter(is.na(oneway)) |>
    leaflet() |>
    addTiles() |>
    addPolylines(color = ~factpal(highway),
                 popup = ~ paste0("Tipo: ", highway, "<br>Mano única: ", oneway ) ) |>
    addLegend(pal = factpal, values = ~highway, title = "Tipo de camino")

  rm(factpal)

  # Imputamos a doble mano todas las calles que no están clasificadas
  CALLE_OSM <- CALLE_OSM |>
    mutate(oneway = ifelse(is.na(oneway), "no", oneway))

  # Duplicamos calles con doblevía
  CALLE_aux <- CALLE_OSM |> filter(oneway == "no") |> st_reverse()
  CALLE_OSM <- rbind(CALLE_OSM, CALLE_aux)

  rm(CALLE_aux)

  # Armado red urbana -------------------------------------------

  # convert to sfnetworks
  net2 <- as_sfnetwork(CALLE_OSM, directed = TRUE)

  # Limpieza de la red urbana -------------------------------------------
  # Se modifica orden de las acciones de limpieza.
  # * Si la subdivisión de la red (2) va luego de eliminar bucles en (1) se
  #   pierden rotondas (que tienen nodos internos que pueden coincidir con
  #   otros componentes), aumentando artificialmente número de componentes
  # * Si la eliminación de pseudonodos (3) va antes del armado de la
  #   simplificación de intersecciones mediante clusters (4), se pueden perder,
  #   potenciales intersecciones.
  #
  # Orden:
  # Subdivide (2) -> Simplify (1) -> Simplify intersections (4) -> smooth pseudo nodes (3)

  net_clean <-  net2 |>
    # 2. Subdivide
    convert(to_spatial_subdivision) |>
    # 1. Simplify
    activate("edges") |>
    arrange(edge_length()) |>
    filter(!edge_is_multiple()) |>
    filter(!edge_is_loop()) # ≃ convert(to_spatial_simple)

  # 4. simplify intersections

  # Retener las coordenadas de los nodos.
  clusters <- net_clean %>%
    activate("nodes") %>%
    st_coordinates() |>
    # Nodos dentro de una distancia de 3 (mts) entre sí estarán en el mismo clúster.
    # A un nodo se le asigna un clúster incluso si es el único miembro de ese clúster.
    dbscan(eps = 3, minPts = 1) |>
    pluck("cluster")

  net_clean <- net_clean |>
    activate("nodes") |>
    mutate(cls = clusters,               # Add the cluster information to the nodes of the network.
           cmp = group_components()) |>  # Verificar que los  nodos agrupados estén conectados en la red.
    # Contraer la red (utilizando centroide del clúster y eliminando nodos individuales)
    convert(to_spatial_contracted,
            cls, # cmp,
            simplify = TRUE)

  # 3. smooth pseudo nodes
  net_clean <- net_clean |>
    convert(to_spatial_smooth)

  components(net2)$no
  components(net_clean)$no
  components(net_clean)$csize

  # Visualizamos componetes
  net_aux <- net_clean |>
    st_as_sf("nodes") |>
    mutate(componente = factor(components(net_clean)$membership)) |>
    st_transform(4326)

  factpal <- colorFactor(topo.colors(length(unique(net_aux$componente))),
                         net_aux$componente)

  net_aux |>
    leaflet() |>
    addTiles() |>
    addPolygons(data = st_transform(st_as_sf(net_clean, "edges"), 4326)) |>
    addCircles(color = ~factpal(componente))

  rm(net_aux)

  # 5. Retenemos el componente principal de la red y ponderamos la red por la distancia del eje
  net_clean <- net_clean |>
    filter(group_components() == 1) |>    # Select 1st component
    activate("edges") |>
    mutate(weight = edge_length())        # Add weights to edges

  net_clean <- net_clean |>
    activate("nodes") |>
    mutate(comp = group_components())    # Add component information to nodes
