# Redes Espaciales I: Construcción de redes espaciales

# Basado en: https://luukvdmeer.github.io/sfnetworks/articles/sfn01_structure.html#spatial-information

library(tidyverse)

library(sf)

library(igraph)
library(tidygraph)

library(sfnetworks)

# Creación de una red espacial ----------------------------------------

### ╠ Creación "desde cero" -----------------------------------------------

  p1 <- st_point(c(7, 51))
  p2 <- st_point(c(7, 52))
  p3 <- st_point(c(8, 52))
  p4 <- st_point(c(8, 51.5))

  p <- st_sfc(p1, p2, p3, p4) |>
    st_as_sf() |>
    mutate(id = 1:n()) |>
    ggplot() +
    geom_sf() +
    geom_sf_text(aes(label = id), nudge_x = 0.05) +
    theme_void()

  p

  l1 <- st_sfc(st_linestring(c(p1, p2)))
  l2 <- st_sfc(st_linestring(c(p1, p4, p3)))
  l3 <- st_sfc(st_linestring(c(p3, p2)))

  p +
    geom_sf(data = st_as_sf(l1), col = "tomato") +
    geom_sf(data = st_as_sf(l2), col = "darkblue") +
    geom_sf(data = st_as_sf(l3), col = "green")

  edges <- st_as_sf(c(l1, l2, l3), crs = 4326)
  nodes <- st_as_sf(c(st_sfc(p1), st_sfc(p2), st_sfc(p3)), crs = 4326)

  edges$from <- c(1, 1, 3)
  edges$to <- c(2, 3, 2)

  # Red dirigida
  net <- sfnetwork(nodes, edges)
  net

  plot(net)

  # Red No dirigida
  net <- sfnetwork(nodes, edges, directed = FALSE)
  net

  # Agrega atributos
  nodes$name <- c("city", "village", "farm")
  nodes

  edges$from <- c("city", "city", "farm")
  edges$to <- c("village", "farm", "village")
  edges$transito <- c(10, 20, 30)
  edges

  net <- sfnetwork(nodes, edges, node_key = "name")

  # Generar ejes sf
  st_geometry(edges) <- NULL

  other_net <- sfnetwork(nodes, edges, edges_as_lines = TRUE)

  par(mfrow=c(1,2))
  plot(net, cex = 2, lwd = 2, main = "Geometría original")
  plot(other_net, cex = 2, lwd = 2, main = "Geometría reconstruida")
  par(mfrow=c(1,1))

### ╚ Creación desde un objeto {sf} --------------------------------------

  base_url <- paste("https://cdn.buenosaires.gob.ar",
                     "datosabiertos/datasets", sep = "/")

  CABALLITO <- st_read(paste(base_url, "ministerio-de-educacion",
                             "comunas/comunas.geojson", sep = "/"))  |>
    st_transform(5347) |>
    filter(comuna == 6)

  CABALLITO_buffer <- CABALLITO |>
    st_buffer(500)

  CALLE <- st_read(paste(base_url, "jefatura-de-gabinete-de-ministros",
                         "calles/callejero.geojson", sep = "/") ) |>
    st_transform(5347) |>
    st_intersection(CABALLITO_buffer)

  net <- as_sfnetwork(CALLE, directed = FALSE)

  plot(net)

  net

# Características generales de las redes espaciales ----------------------------

### ╠ Activación y modificación con {tidyverse} --------------------------------

  net

  net |> filter(tipo_c == "AVENIDA")

  active(net)
  # es necesario previamente "activar" la capa de ejes

  net |>
    activate("edges") |>
    filter(tipo_c == "AVENIDA")

  net <- net |>
    activate("edges") |>
    # Calculamos el "peso" de los bordes en función de su longitud
    mutate(weight = edge_length()) |>
    activate("nodes") |>
    # Calculamos la "centralidad" de los nodos
    mutate(bc = centrality_betweenness(weights = weight, directed = FALSE))

  net

  net |>
    activate("edges") |>
    select(long, weight)

### ╠ Extracción de los elementos {sf} ------------------------------------

  # Es igual a: st_as_sf(net, "nodes")
  esquinas <- net  |>
    activate("nodes")  |>
    st_as_sf()

  esquinas |> class()
  esquinas

  # Es igual a: st_as_sf(net, "edges")
  calles <- net |>
    activate("edges") |>
    st_as_sf()

  calles |> class()
  calles

### ╠ Visualización -------------------------------------

  plot(net)

  autoplot(net) + ggtitle("Red de calles del barrio de Caballito (CABA)") + theme_void()

  ggplot() +
    geom_sf(data = CABALLITO, fill = NA, linewidth = 2, color = "grey20") +
    geom_sf(data = st_as_sf(net, "edges"), color = "grey50") +
    geom_sf(data = st_as_sf(net, "nodes"), alpha = 0.7,
            aes(color = bc, size = bc)) +
    scale_color_viridis_c() +
    ggtitle("Red de calles del barrio de Caballito (CABA)") +
    labs(size = "Centralidad \nde intermediación", color = NULL ) +
    theme_void()

  ggplot() +
    geom_sf(data = CABALLITO, fill = NA, linewidth = 2, color = "grey20") +
    geom_sf(data = st_as_sf(net, "edges"), aes(color = tipo_c) ) +
    geom_sf(data = st_as_sf(net, "nodes"), aes(size = bc),
            color = "grey", alpha = 0.7) +
    ggtitle("Red de calles del barrio de Caballito (CABA)") +
    labs(size = "Centralidad de\n intermediación",
         color = "Tipo de vía \nde comunicación") +
    theme_void()

### ╠ Datos resúmenes de la red -------------------------

  # Cantidad de ejes de la red
  # Es igual a: ecount(net)
  igraph::gsize(net)

  # Cantidad de nodos de la red
  # Es igual a: vcount(net)
  igraph::gorder(net)

### ╚ Componentes ----------------------------------------

  comp <- igraph::components(net)
  comp

  comp$membership |> table()

  net <- net |>
    activate("nodes") |>
    mutate(component = factor(comp$membership))

  ggplot() +
    geom_sf(data = CABALLITO, fill = NA, linewidth = 2, color = "grey20") +
    geom_sf(data = st_as_sf(net, "edges"), col = "grey" ) +
    geom_sf(data = st_as_sf(net, "nodes"), aes(color = component)) +
    ggtitle("Componentes de la red vial de Caballito") +
    labs(color = "Componente") +
    theme_void()

  # Quitar los componentes minoritarios
  new_net <- net |>
    activate("nodes") |>
    filter(component == 1)

  igraph::components(new_net)
