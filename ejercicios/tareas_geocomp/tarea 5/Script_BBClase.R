#===============================================================================
# Práctico 4 — Transformaciones morfológicas de redes espaciales
# Geocomputación | Caso de estudio: red vial de Bahía Blanca, Buenos Aires, AR
#===============================================================================

paquetes <- c("sf", "osmdata", "sfnetworks", "tidygraph", "igraph",
              "ggplot2", "dplyr", "tibble", "viridis", "mapview")
instalar_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar_faltantes) > 0) install.packages(instalar_faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# mapview(grilla_hex, map.types = "OpenStreetMap")

CRS_GEO <- 4326
CRS_PROYECTADO <- 5343

# --- Escala de rojos por cuantiles -------------------------------------------------
# Clasifica una medida muy asimétrica (como la intermediación) en clases de igual
# cantidad de elementos, sobre log10. Devuelve la clase de cada valor, la paleta de
# rojos y las etiquetas de la leyenda en unidades originales.
clasificar_rojos <- function(valores, n_clases = 8) {
  logv <- log10(valores + 1)
  cortes <- unique(quantile(logv, probs = seq(0, 1, length.out = n_clases + 1), na.rm = TRUE))
  k <- length(cortes) - 1
  list(
    clase = cut(logv, breaks = cortes, include.lowest = TRUE),
    paleta = colorRampPalette(
      c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15", "#67000D")
    )(k),
    etiquetas = paste0(
      format(round(10^cortes[-length(cortes)] - 1), big.mark = "."), " – ",
      format(round(10^cortes[-1] - 1), big.mark = ".")
    )
  )
}

# --- 0. Reconstrucción de la red limpia -----------------------------------------

bb <- osmdata::getbb("Bahía Blanca, Argentina")
margen <- 0.02
bb_recortado <- rbind(
  x = c(min = bb["x", "min"] + margen, max = bb["x", "max"] - margen),
  y = c(min = bb["y", "min"] + margen, max = bb["y", "max"] - margen)
)

vias_sf <- osmdata::opq(bbox = bb_recortado, timeout = 1200) |>
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf() |>
  (\(x) x$osm_lines)() |>
  filter(!is.na(highway),
         !highway %in% c("proposed", "construction", "raceway", "escape", "corridor")) |>
  select(osm_id, name, highway, geometry)

red <- sfnetworks::as_sfnetwork(vias_sf, directed = FALSE) |>
  tidygraph::convert(to_spatial_subdivision) |>
  tidygraph::convert(to_spatial_smooth) |>
  tidygraph::convert(to_spatial_simple) |>
  activate("nodes") |>
  mutate(comp = group_components())

comp_principal <- red |> activate("nodes") |> st_as_sf() |> st_drop_geometry() |>
  count(comp) |> slice_max(n, n = 1) |> pull(comp)
red <- red |> activate("nodes") |> filter(comp == comp_principal)

cat("Red primal — orden:", igraph::gorder(red), "| tamaño:", igraph::gsize(red), "\n")

# --- 1. Grafo dual / line graph (Ejercicio 1) -------------------------------------

red_dual <- tidygraph::convert(red, to_linegraph)

cat("Grafo dual — orden:", igraph::gorder(red_dual), "| tamaño:", igraph::gsize(red_dual), "\n")
cat("(Referencia: tamaño de la red primal =", igraph::gsize(red), "-> debería coincidir",
    "con el orden del grafo dual)\n")

# --- 2. Centralidad en ambas representaciones (Ejercicio 2) -----------------------

red <- red |> activate("nodes") |> mutate(betweenness_primal = centrality_betweenness())
red_dual <- red_dual |> activate("nodes") |> mutate(betweenness_dual = centrality_betweenness())

nodos_primal_sf <- red |> activate("nodes") |> st_as_sf()

# Escala secuencial de rojos por cuantiles sobre log10(intermediación).
# Cada clase agrupa la misma cantidad de nodos, para que se distingan los valores medios.
nodos_primal_sf$inter_log <- log10(nodos_primal_sf$betweenness_primal + 1)
cortes <- unique(quantile(nodos_primal_sf$inter_log,
                          probs = seq(0, 1, length.out = 9), na.rm = TRUE))
n_clases <- length(cortes) - 1
paleta_rojos <- colorRampPalette(
  c("#FEE5D9", "#FCAE91", "#FB6A4A", "#DE2D26", "#A50F15", "#67000D")
)(n_clases)
nodos_primal_sf$clase <- cut(nodos_primal_sf$inter_log, breaks = cortes, include.lowest = TRUE)

# Leyenda en unidades originales de intermediación (se deshace el log10)
etiquetas_clases <- paste0(
  format(round(10^cortes[-length(cortes)] - 1), big.mark = "."), " – ",
  format(round(10^cortes[-1] - 1), big.mark = ".")
)

mapa_betweenness_primal <- ggplot() +
  geom_sf(data = red |> activate("edges") |> st_as_sf(), color = "grey85", linewidth = 0.2) +
  geom_sf(data = nodos_primal_sf, aes(color = clase), size = 0.8) +
  scale_color_manual(values = paleta_rojos, labels = etiquetas_clases,
                     name = "Intermediación\n(nodo primal, cuantiles)", drop = FALSE) +
  labs(title = "Centralidad de intermediación — red primal (intersecciones)") +
  theme_minimal()
print(mapa_betweenness_primal)
ggsave("figura_ejercicio2a_betweenness_primal.png", mapa_betweenness_primal,
       width = 8, height = 7, dpi = 200)

# Traer la centralidad dual de vuelta a la geometría de las calles originales:
# el orden de nodos del grafo dual coincide con el orden de aristas de la red primal
aristas_primal_sf <- red |> activate("edges") |> st_as_sf()
# CORRECCIÓN: to_linegraph() devuelve un tbl_graph sin geometría sf, así que no se puede
# usar st_as_sf(). Se extrae la tabla de nodos directamente.
betweenness_dual_vec <- red_dual |> activate("nodes") |> tibble::as_tibble() |>
  dplyr::pull(betweenness_dual)

# Nota: to_linegraph preserva el orden original de las aristas primales como
# orden de nodos duales, por lo que el mapeo directo es válido en la gran
# mayoría de los casos; verificar cardinalidad antes de asignar.
# CORRECCIÓN: si la cardinalidad no coincide, el mapeo por posición sería incorrecto,
# así que se detiene la ejecución en lugar de solo advertir.
stopifnot(length(betweenness_dual_vec) == nrow(aristas_primal_sf))
aristas_primal_sf$betweenness_dual <- betweenness_dual_vec

esc_calles <- clasificar_rojos(aristas_primal_sf$betweenness_dual)
aristas_primal_sf$clase <- esc_calles$clase

mapa_betweenness_dual <- ggplot() +
  geom_sf(data = aristas_primal_sf, aes(color = clase), linewidth = 0.6) +
  scale_color_manual(values = esc_calles$paleta, labels = esc_calles$etiquetas,
                     name = "Intermediación\n(nodo dual = calle, cuantiles)", drop = FALSE) +
  labs(title = "Centralidad de intermediación — grafo dual, mapeada sobre las calles") +
  theme_minimal()
print(mapa_betweenness_dual)
ggsave("figura_ejercicio2b_betweenness_dual.png", mapa_betweenness_dual,
       width = 8, height = 7, dpi = 200)

top5_calles <- aristas_primal_sf |> st_drop_geometry() |>
  filter(!is.na(name)) |>
  distinct(name, .keep_all = TRUE) |>
  slice_max(betweenness_dual, n = 5) |>
  select(name, highway, betweenness_dual)
print(top5_calles)

# --- 3. Contracción espacial (Ejercicio 3) -----------------------------------------

# CORRECCIÓN: la grilla debe construirse en un CRS métrico (EPSG 5343).
# Con EPSG 4326 (grados), cellsize = 500 generaba una sola celda para toda la ciudad.
area_estudio <- st_as_sfc(st_bbox(vias_sf)) |> st_transform(CRS_PROYECTADO)
grilla_hex_geom <- sf::st_make_grid(area_estudio, cellsize = 2000, square = FALSE)
grilla_hex <- sf::st_sf(zona_id = seq_along(grilla_hex_geom), geometry = grilla_hex_geom)
mapview(grilla_hex, map.types = "OpenStreetMap")

# Los nodos se transforman al mismo CRS antes del join espacial
nodos_proyectados <- nodos_primal_sf |> st_transform(CRS_PROYECTADO)
nodos_con_zona <- st_join(nodos_proyectados, grilla_hex, join = st_within)

# Verificación: cada nodo debe caer en una celda, y debe haber más de una zona
cat("Nodos sin zona asignada:", sum(is.na(nodos_con_zona$zona_id)), "\n")
cat("Zonas con al menos un nodo:", dplyr::n_distinct(nodos_con_zona$zona_id, na.rm = TRUE), "\n")
stopifnot(nrow(nodos_con_zona) == igraph::gorder(red))

red_zonas <- red |>
  activate("nodes") |>
  mutate(zona_id = nodos_con_zona$zona_id)

red_contraida <- tidygraph::convert(
  red_zonas, to_spatial_contracted, zona_id, simplify = TRUE
)

# CORRECCIÓN: si la contracción devuelve una lista de estados modificados en lugar de un grafo,
# se toma el primer estado. Luego se verifica que el objeto sea un grafo antes de medirlo.
cat("Clase de red_contraida:", paste(class(red_contraida), collapse = ", "), "\n")
if (is.list(red_contraida) && !inherits(red_contraida, "tbl_graph")) {
  red_contraida <- red_contraida[[1]]
}
stopifnot(inherits(red_contraida, "tbl_graph"))

cat("Red contraída por zonas — orden:", igraph::gorder(red_contraida),
    "| tamaño:", igraph::gsize(red_contraida), "\n")

nodos_contraidos_sf <- red_contraida |> activate("nodes") |> st_as_sf()
aristas_contraidas_sf <- red_contraida |> activate("edges") |> st_as_sf()

mapa_contraccion <- ggplot() +
  geom_sf(data = red |> activate("edges") |> st_as_sf(), color = "grey90", linewidth = 0.15) +
  geom_sf(data = aristas_contraidas_sf, color = "darkorange", linewidth = 0.8, alpha = 0.7) +
  geom_sf(data = nodos_contraidos_sf, color = "darkred", size = 1.5) +
  labs(title = "Red contraída por zonas (grilla hexagonal ~500 m)",
       subtitle = "Naranja: conexiones agregadas entre zonas") +
  theme_minimal()
print(mapa_contraccion)
ggsave("figura_ejercicio3_contraccion_espacial.png", mapa_contraccion,
       width = 8, height = 7, dpi = 200)


# MAPA CENTRALIDAD CONTRAIDA PONDERADO

# Intermediación calculada sobre la red contraída (un valor por zona)
red_contraida <- red_contraida |>
  activate("nodes") |>
  mutate(betweenness_contraida = tidygraph::centrality_betweenness())

nodos_contraidos_sf <- red_contraida |> activate("nodes") |> st_as_sf()
aristas_contraidas_sf <- red_contraida |> activate("edges") |> st_as_sf()

# Escala de rojos por cuantiles (helper clasificar_rojos, definido al inicio del script)
esc_contraida <- clasificar_rojos(nodos_contraidos_sf$betweenness_contraida)
nodos_contraidos_sf$clase <- esc_contraida$clase

mapa_contraccion <- ggplot() +
  geom_sf(data = red |> activate("edges") |> st_as_sf(), color = "grey90", linewidth = 0.15) +
  geom_sf(data = aristas_contraidas_sf, color = "grey60", linewidth = 0.6, alpha = 0.6) +
  geom_sf(data = nodos_contraidos_sf, aes(color = clase), size = 2) +
  scale_color_manual(
    values = esc_contraida$paleta,
    labels = esc_contraida$etiquetas,
    name = "Intermediación\n(zona contraída, cuantiles)",
    drop = FALSE
  ) +
  labs(title = "Red contraída por zonas (grilla hexagonal ~500 m)",
       subtitle = "Cada punto es una zona, coloreado por su intermediación") +
  theme_minimal()

print(mapa_contraccion)
ggsave("figura_ejercicio3_contraccion_espacial.png", mapa_contraccion,
       width = 8, height = 7, dpi = 200)

# --- 4. Efecto sobre la densidad (Ejercicio 4) ---------------------------------------

# CORRECCIÓN: las funciones de igraph no aceptan directamente objetos tbl_graph/sfnetwork
# en versiones recientes (error ensure_igraph). Se convierten explícitamente a igraph.
g_primal <- igraph::as.igraph(red)
g_contraida <- igraph::as.igraph(red_contraida)

comparacion_densidad <- tibble(
  red = c("Primal completa", "Contraída por zonas"),
  nodos = c(igraph::gorder(g_primal), igraph::gorder(g_contraida)),
  aristas = c(igraph::gsize(g_primal), igraph::gsize(g_contraida)),
  densidad = c(igraph::edge_density(g_primal), igraph::edge_density(g_contraida)),
  grado_promedio = c(mean(igraph::degree(g_primal)), mean(igraph::degree(g_contraida)))
)
print(comparacion_densidad)
write.csv(comparacion_densidad, "tabla_ejercicio4_densidad_comparada.csv", row.names = FALSE)

cat("\nInterpretación esperada: la densidad de la red contraída debería ser",
    "sustancialmente mayor a la de la red primal, como efecto casi mecánico",
    "de la reducción en el número de nodos (ver solucion.md).\n")

# --- 5. Síntesis (Ejercicio 5) -----------------------------------------------------

cat("\n--- Insumos para el informe de síntesis (Ejercicio 5) ---\n")
cat("Top 5 calles con mayor centralidad dual:\n")
print(top5_calles)
cat("\nResumen red contraída por zonas:\n")
print(comparacion_densidad)

cat("\n--- Fin del script: Práctico 4 (cierre de la unidad de redes espaciales) ---\n")