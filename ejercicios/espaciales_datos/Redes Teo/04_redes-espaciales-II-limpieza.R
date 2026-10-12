# Redes Espaciales II: Preprocesamiento y limpieza de redes espaciales

# Basado en: https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html

library(tidyverse)

library(dbscan)

library(sf)
library(geos)

library(ggpubr)

library(igraph)
library(tidygraph)

library(sfnetworks)

# Datos de ejemplo --------------------------------------------------------

p1 = st_point(c(0, 10))
p2 = st_point(c(10, 10))
p3 = st_point(c(20, 10))
p4 = st_point(c(30, 10))
p5 = st_point(c(40, 10))
p6 = st_point(c(30, 20))
p7 = st_point(c(30, 0))
p8 = st_point(c(40, 30))
p9 = st_point(c(40, 20))
p10 = st_point(c(40, 00))
p11 = st_point(c(50, 20))
p12 = st_point(c(50, 00))
p13 = st_point(c(50, -10))
p14 = st_point(c(57.7, 10)) # Valor real es 58, 10
p14_ = st_point(c(58.4, 10))
p15 = st_point(c(60, 12))
p16 = st_point(c(62, 10))
p17 = st_point(c(60, 8))
p18 = st_point(c(60, 20))
p19 = st_point(c(60, -10))
p20 = st_point(c(70, 10))

l1 = st_sfc(st_linestring(c(p1, p2, p3)))
l2 = st_sfc(st_linestring(c(p3, p4, p5)))
l3 = st_sfc(st_linestring(c(p6, p4, p7)))
l4 = st_sfc(st_linestring(c(p8, p11, p9)))
l5 = st_sfc(st_linestring(c(p9, p5, p10)))
l6 = st_sfc(st_linestring(c(p8, p9)))
l7 = st_sfc(st_linestring(c(p10, p12, p13, p10)))
l8 = st_sfc(st_linestring(c(p5, p14)))
l9 = st_sfc(st_linestring(c(p15, p14_)))
l10 = st_sfc(st_linestring(c(p16, p15)))
l11 = st_sfc(st_linestring(c(p14_, p17)))
l12 = st_sfc(st_linestring(c(p17, p16)))
l13 = st_sfc(st_linestring(c(p15, p18)))
l14 = st_sfc(st_linestring(c(p17, p19)))
l15 = st_sfc(st_linestring(c(p16, p20)))

ml1 <- st_sfc(st_multilinestring(c(l1, l2)))

lines <- st_as_sf(c(ml1, l3, l4, l5, l6, l7, l8, l9, l10, l11, l12, l13, l14, l15))
points <- st_as_sf(st_sfc(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p14_, p15, p16, p17, p18, p19, p20))

lines$id <- 1:nrow(lines)

# 1. Preprocesamiento y creación de la red -------------------------------------

### ╠ Convertir datos a tipo LINESTRING --------------------------

  g1 <- ggplot() +
    geom_sf(data = lines, aes(color = factor(id))) +
    geom_sf(data = points, color = "grey60") +
    guides(color = "none")
  g1

  net <- as_sfnetwork(lines) # Falla!

  # Convertir a LINESTRING con {sf}
  lines_lost <- st_cast(lines, "LINESTRING")

  g2 <- ggplot() +
    geom_sf(data = lines_lost,
            aes(color = factor(1:14))) +
    geom_sf(data = points, color = "grey60") +
    guides(color = "none")

  ggarrange(g1, g2, nrow = 1, common.legend = T) # Perdimos algunas líneas!

  # Convertir a LINESTRING con {geos}
  lines_sfc <- st_geometry(lines)
  lines_sfc_corrected <- geos::geos_unnest(lines_sfc, keep_multi = FALSE)
  lines_sfc_corrected |> attributes()

  lines_corr <- lines[rep(1:nrow(lines), times = attr(lines_sfc_corrected, which = "lengths")), ]
  st_geometry(lines_corr) <- st_as_sfc(lines_sfc_corrected)

  g3 <- ggplot() +
    geom_sf(data = lines_corr,
            aes(color = factor(1:15))) +
    geom_sf(data = points, color = "grey60") +
    guides(color = "none")

  ggarrange(g1, g2, g3, nrow = 1) # OK!

  lines <- lines_corr

  rm(lines_lost, lines_sfc, lines_sfc_corrected, lines_corr)
  rm(g1, g2, g3)

### ╠ Redondear coordenadas --------------------------------------

  ggplot() +
    geom_sf(data = lines) +
    geom_sf(data = l8, color = "tomato") +
    geom_sf(data = l9, color = "chartreuse3") +
    geom_sf(data = l11, color = "chartreuse3") +
    geom_sf(data = points, color = "grey40") +
    geom_sf(data = p14, color = "tomato4")  +
    geom_sf(data = p14_, color = "#458B00") +
    theme_minimal()

  net <- as_sfnetwork(lines)
  igraph::components(net) # Tenemos muchos componentes!

  # Función para definir un color para los ejes de la red
  edge_colors <- function(x) rep(sf.colors(12, categorical = TRUE)[-2], 2)[c(1:ecount(x))]

  # Visualicemos los ejes de la red
  g4 <- ggplot() +
    geom_sf(data = st_as_sf(net, "edges"), aes(color = edge_colors(net))) +
    geom_sf(data = points, color = "grey60", fill = NA, shape = 21) +
    geom_sf(data = st_as_sf(net, "nodes")) +
    guides(color = "none") +
    theme_void()
  g4

  # Función para hacer el redondeo de las coordenadas
  st_geometry(lines) <- st_geometry(lines) %>%
    lapply(function(x) round(x, 0)) %>%
    st_sfc(crs = st_crs(lines))

  net <- as_sfnetwork(lines)
  igraph::components(net) # Tenemos muchos componentes!

  g5 <- ggplot() +
    geom_sf(data = st_as_sf(net, "edges"), aes(color = edge_colors(net))) +
    geom_sf(data = points, color = "grey60", fill = NA, shape = 21) +
    geom_sf(data = st_as_sf(net, "nodes")) +
    guides(color = "none") +
    theme_void()

  ggarrange(g4, g5, nrow = 1)

  rm(g4, g5)

### ╠ Manejar bordes unidireccionales ------------------------

  lines$dobleway <- c(rep(F, nrow(lines)-2), T, T)

  ggplot() +
    geom_sf(data = lines, aes(color = dobleway)) +
    theme_void()

  lines_aux <- lines |> filter(dobleway) |> st_reverse() |> mutate(dobleway = F)
  lines_bidirec <- rbind(lines, lines_aux)

  ggplot() +
    geom_sf(data = lines_bidirec,
            aes(color = dobleway, linetype = dobleway),
            alpha = 0.5, linewidth = 1,
            arrow = arrow(ends = "last", type = "closed", length = unit(0.3, "cm"))
             ) +
    theme_void()

  lines$dobleway <- NULL

  rm(lines_aux, lines_bidirec)

### ╚ Limpieza del espacio de trabajo --------------------------------------

  rm(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11,
     p12, p13, p15, p16, p17, p18, p19, p20)
  rm(l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, l11,
     l12, l13, l14, l15)
  rm(p14, p14_, ml1)

# 2. Crear la red base --------------------------------------

  net  <- as_sfnetwork(lines, directed = FALSE)

  # Agregamos atributos aleatorios
  set.seed(123)
  net <- net |>
    activate("edges") |>
    mutate(flow = sample(1:10, n(), replace = TRUE),
           type = c(rep("path", 5 ), rep("road", 5), rep("highway", 5)),
           foo = sample(c(1:n()), n()),
           bar = sample(letters, n()))

  # Función para graficar las aristas de la red con diferentes colores
  ggnet <- function(n){

    ggplot() +
      geom_sf(data = st_as_sf(n, "edges"), aes(color = edge_colors(n))) +
      geom_sf(data = st_as_sf(n, "nodes"), color = "grey60", fill = "white", shape = 21) +
      geom_sf_text(data = st_as_sf(n, "nodes") |> mutate(ID = row_number()),
                   aes(label = ID), nudge_y = 0.5, nudge_x = 0.5) +
      guides(color = "none") +
      theme_minimal()
  }

  (p1_base <- ggnet(net))

# 3. Limpieza de la red ----------------------------------------

### ╠ Simplify -------------------------------------

  # Ejes con múltiples caminos
  net |>
    activate("edges") |>
    filter(edge_is_between(6, 7)) |>
    st_as_sf()

  # Bucles
  net |>
    activate("edges") |>
    filter(from == to) |>
    st_as_sf()

  # Simplificar: edge_is_multiple() + edge_is_loop()
  net |>
    activate("edges") |>
    filter(!edge_is_multiple()) |>
    filter(!edge_is_loop()) |>
    ggnet()

  net |>
    activate("edges") |>
    arrange(edge_length()) |>
    filter(!edge_is_multiple()) |>
    filter(!edge_is_loop()) |>
    ggnet()

  # Simplificar: to_spatial_simple()
  # Cuando sus ejes tienen atributos, es mejor "fusionar" los ejes en un único
  #   eje que tenga la geometría del primer eje, pero sus valores de atributos
  #   sean una combinación de los atributos de todos los ejes. Para ello tenemos
  #   to_spatial_simple()

  combinations = list(
    flow = "sum",
    type = function(x) if (length(unique(x)) == 1) x[1] else "unknown",
    foo = "first",
    "ignore" # Todo lo demás lo va a ignorar
  )

  simple <- net |>
    activate("edges") |>
    arrange(edge_length()) |>
    convert(to_spatial_simple, summarise_attributes = combinations)

  simple
  (p2_simple <- simple |> ggnet())

  ggarrange(p1_base, p2_simple, nrow = 1,
            labels = c("Base", "Simple"))

  # plot(st_geometry(simple, "edges"), col = edge_colors(simple), lwd = 4)
  # plot(st_geometry(simple, "nodes"), pch = 20, cex = 1.5, add = TRUE)

### ╠ Subdivision ------------------------------------------

  igraph::components(simple)
  subdivision <- convert(simple, to_spatial_subdivision)

  (p3_subdivision <- subdivision |> ggnet())

  ggarrange(p1_base, p2_simple, p3_subdivision, nrow = 1,
            labels = c("Base", "Simple", "Subdivision"))

  # plot(st_geometry(subdivision, "edges"), col = edge_colors(simple), lwd = 4)
  # plot(st_geometry(subdivision, "nodes"), pch = 20, cex = 1.5, add = TRUE)

  igraph::components(subdivision)

### ╠ Smoot pseudo nodes ------------------------------------------------

  convert(subdivision, to_spatial_smooth) |> ggnet()

  # combinations = list(
  #   flow = "sum",
  #   type = function(x) if (length(unique(x)) == 1) x[1] else "unknown",
  #   "ignore"
  # )

  smoothed <- convert(subdivision, to_spatial_smooth,
                      # require_equal = "type",
                      summarise_attributes = combinations)

  smoothed
  (p4_smoothed <- smoothed |> ggnet())

  ggarrange(p1_base, p2_simple,
            p3_subdivision, p4_smoothed,
            labels = c("Base", "Simple", "Subdivision", "Smoothed"),
            nrow = 2, ncol = 2)

### ╚ Simplify intersections ----------------------------------------------------

  node_coords <- smoothed |> activate("nodes") |> st_coordinates()
  clusters <- dbscan(node_coords, eps = 5, minPts = 1)$cluster
  clustered <- smoothed |> activate("nodes") |> mutate(cls = clusters, cmp = group_components())

  # Hacer la transformación
  contracted <- convert(clustered, to_spatial_contracted, cls, cmp, simplify = TRUE)

  (p5_contracted <- contracted |> ggnet())

  ggarrange(p1_base, p2_simple,
            p3_subdivision, p4_smoothed,
            p5_contracted,
            labels = c("Base", "Simple", "Subdivision", "Smoothed", "Contracted"),
            nrow = 2, ncol = 3)

  ggarrange(p1_base, p5_contracted, nrow = 1, labels = c("Base", "Final"))

  rm(p1_base, p2_simple, p3_subdivision, p4_smoothed, p5_contracted)
  rm(node_coords, clusters, clustered, contracted)

  rm(net, simple, smoothed, subdivision)
  rm(lines)

