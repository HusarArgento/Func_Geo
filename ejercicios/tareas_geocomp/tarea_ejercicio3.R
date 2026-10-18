#Tarea

#Librerias
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
library(tmap)

#Descarga
CHAS <- getbb("Parque Chas, Ciudad Autónoma de Buenos Aires",
                 format_out = "sf_polygon")
leaflet() |>
  addTiles() |>
  addPolygons(data = st_transform (CHAS, 4326), color = "black")


callejero_CHAS <- getbb("Parque Chas, Ciudad Autónoma de Buenos Aires") |>
  opq(timeout = 180) |>
  add_osm_feature(
    key = "highway",
    value = c(
      "living_street", "residential",
      "motorway", "motorway_link",
      "primary", "primary_link",
      "secondary", "secondary_link",
      "tertiary", "tertiary_link",
      "trunk", "trunk_link",
      "unclassified", "service"
    )
  ) |>
  osmdata_sf()

summary(callejero_CHAS)

#Verficacion de las calles
calles <- callejero_CHAS$osm_lines
nrow(calles)                          # ¿cuántos segmentos?
st_crs(calles)                        # ¿4326?
table(st_geometry_type(calles))       # ¿todos LINESTRING?
table(calles$highway, useNA = "ifany")# distribucion x tipos de vía

leaflet() |>
  addTiles() |>
  addPolygons(data = st_transform (CHAS,4326), 
              color = "black") |>
  addPolylines(data = callejero_CHAS$osm_lines, color = "tomato") |>
  addPolygons(data = callejero_CHAS$osm_polygons, color = "darkblue")

#Extracción
callejero_CHAS <- callejero_CHAS |>
  magrittr::extract(c("osm_lines", "osm_polygons")) |>
  bind_rows() |>
  # Seleccionar variables necesarias
  select(osm_id, name, highway, lanes,oneway, maxspeed, surface) |>
  # Transformar proyección 
  st_transform(5347)

#Paso todo a linestring
st_geometry_type(callejero_CHAS) |> fct_drop() |>  table()  #reviso tipos de goemetria
callejero_CHAS <- st_cast(callejero_CHAS, "LINESTRING")  #casteo a linestring todo
st_geometry_type(callejero_CHAS) |> fct_drop() |>  table() #verifico que haya pasado bien

#Redondeo
st_geometry(callejero_CHAS) <- st_geometry(callejero_CHAS)  |>
  lapply(function(x) round(x, 0)) |>
  st_sfc(crs = st_crs(callejero_CHAS))

#Me fijo las calles de dos y una mano
table(callejero_CHAS$highway, DOBLE_MANO = callejero_CHAS$oneway, exclude = NULL)

#Grafico
#Preparacicon
factpal <- colorFactor(terrain.colors(length(unique(callejero_CHAS$highway))),
                       callejero_CHAS$highway)
#Leafleet
callejero_CHAS |>
  st_transform(4326) |>
  filter(is.na(oneway)) |>
  leaflet() |>
  addTiles() |>
  addPolylines(color = ~factpal(highway),
               popup = ~ paste0("Tipo: ", highway, "<br>Mano única: ", oneway ) ) |>
  addLegend(pal = factpal, values = ~highway, title = "Tipo de camino")
rm(factpal)

#TRATAMIENTO DOBLE MANO

#Imputar implica reemplazar valores que no tenemos por otros, en este caso si oneway es NA pasamos a doble mano
callejero_CHAS <- callejero_CHAS |>
  mutate(oneway = ifelse(is.na(oneway), "no", oneway))

#reverso: filtrame todas las que no son de una mano y revertilas
CALLE_ghost <- callejero_CHAS |> filter(oneway == "no") |> st_reverse()

callejero_CHAS <- rbind(callejero_CHAS, CALLE_ghost)

rm(CALLE_ghost)


#LA LIMPIEZA

#A la red!
red_CHAS <- as_sfnetwork(callejero_CHAS, directed = TRUE)

CHAS_limpia <-  red_CHAS |> 
  convert(to_spatial_subdivision) |> # 1 Subdivide
    activate("edges") |>
    arrange(edge_length()) |>
    filter(!edge_is_multiple()) |>
    filter(!edge_is_loop()) #2 Simplifico: descarto lo multiple y los loops

#Simplifico intersecciones
#Aca clasifico para clusterizar el grupo de nodos por esquinas
clusters <- CHAS_limpia %>%
  activate("nodes") %>%
  st_coordinates() |>
  dbscan(eps = 3, minPts = 1) |>
  pluck("cluster")

#Fusiono
CHAS_limpia <- CHAS_limpia |>
  activate("nodes") |>
  mutate(cls = clusters,
         cmp = group_components()) |>
  convert(to_spatial_contracted,
          cls,
          simplify = TRUE)
#Smooth
CHAS_limpia <- CHAS_limpia |>
  convert(to_spatial_smooth)

components(red_CHAS)$no
components(CHAS_limpia)$no
components(CHAS_limpia)$csize


# Visualizamos componetes
CHAS_aux <- CHAS_limpia |>
  st_as_sf("nodes") |>
  mutate(componente = factor(components(CHAS_limpia)$membership)) |>
  st_transform(4326)

factpal <- colorFactor(topo.colors(length(unique(CHAS_aux$componente))),
                       CHAS_aux$componente)

CHAS_aux |>
  leaflet() |>
  addTiles() |>
  addPolygons(data = st_transform(st_as_sf(CHAS_limpia, "edges"), 4326)) |>
  addCircles(color = ~factpal(componente))

rm(CHAS_aux)

#Dejamos una sola red conectada, tomando el componenete principal

components(CHAS_limpia)$csize #verifico componente principal

#filtro
CHAS_limpia <- CHAS_limpia |>
  filter(group_components() == 1) |>    # Select 1st component
  activate("edges") |>
  mutate(weight = edge_length())        # Add weights to edges
