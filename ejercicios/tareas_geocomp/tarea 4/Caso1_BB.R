#===============================================================================
# Práctico 3 — Enrutamiento mediante redes espaciales
# Geocomputación | Caso de estudio: red vial de Bahía Blanca, Buenos Aires, AR
#===============================================================================

paquetes <- c("sf", "osmdata", "sfnetworks", "tidygraph", "igraph",
              "ggplot2", "dplyr", "tibble", "units", "viridis")
instalar_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar_faltantes) > 0) install.packages(instalar_faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

CRS_GEO <- 4326
CRS_PROYECTADO <- 5343

# --- 0. Reconstrucción y limpieza rápida de la red -----------------------------

bb <- osmdata::getbb("Bahía Blanca, Argentina")
margen <- 0.02
bb_recortado <- rbind(
  x = c(min = bb["x", "min"] + margen, max = bb["x", "max"] - margen),
  y = c(min = bb["y", "min"] + margen, max = bb["y", "max"] - margen)
)

vias <- osmdata::opq(bbox = bb_recortado, timeout = 120) |>
  osmdata::add_osm_feature(key = "highway") |>
  osmdata::osmdata_sf()

tipos_excluidos <- c("proposed", "construction", "raceway", "escape", "corridor")
vias_sf <- vias$osm_lines |>
  filter(!is.na(highway), !highway %in% tipos_excluidos) |>
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

cat("Red limpia lista. Orden:", igraph::gorder(red), "Tamaño:", igraph::gsize(red), "\n")

# --- 1. Pesos de arista (Ejercicio 1) --------------------------------------------

tabla_velocidades <- tibble(
  highway = c("motorway", "trunk", "primary", "secondary", "tertiary",
              "residential", "living_street", "unclassified", "service",
              "pedestrian", "footway", "path", "track"),
  velocidad_kmh = c(80, 70, 60, 50, 40, 30, 15, 30, 15, 5, 5, 5, 20)
)

red <- red |>
  activate("edges") |>
  mutate(longitud_m = as.numeric(st_length(st_transform(geometry, CRS_PROYECTADO)))) |>
  left_join(tabla_velocidades, by = "highway") |>
  mutate(
    velocidad_kmh = ifelse(is.na(velocidad_kmh), 30, velocidad_kmh),  # valor por defecto
    tiempo_min = (longitud_m / 1000) / velocidad_kmh * 60
  )

resumen_pesos <- red |> activate("edges") |> st_as_sf() |> st_drop_geometry() |>
  summarise(long_total_km = sum(longitud_m) / 1000,
            tiempo_total_estimado_h = sum(tiempo_min) / 60)
print(resumen_pesos)

# --- 2. Camino más corto vs. más rápido (Ejercicio 2) -----------------------------

nodos_sf <- red |> activate("nodes") |> st_as_sf()

set.seed(42)
muestra_puntos <- nodos_sf[sample(nrow(nodos_sf), 2), ]
origen <- muestra_puntos[1, ]
destino <- muestra_puntos[2, ]

ruta_distancia <- sfnetworks::st_network_paths(
  red, from = origen, to = destino, weights = "longitud_m"
)
ruta_tiempo <- sfnetworks::st_network_paths(
  red, from = origen, to = destino, weights = "tiempo_min"
)

extraer_ruta_sf <- function(red, ruta) {
  red |> activate("edges") |>
    slice(ruta$edge_paths[[1]]) |>
    st_as_sf()
}

ruta_distancia_sf <- extraer_ruta_sf(red, ruta_distancia)
ruta_tiempo_sf <- extraer_ruta_sf(red, ruta_tiempo)

mapa_rutas <- ggplot() +
  geom_sf(data = red |> activate("edges") |> st_as_sf(), color = "grey85", linewidth = 0.2) +
  geom_sf(data = ruta_distancia_sf, aes(color = "Más corta (distancia)"), linewidth = 1.1) +
  geom_sf(data = ruta_tiempo_sf, aes(color = "Más rápida (tiempo)"), linewidth = 1.1,
          linetype = "dashed") +
  geom_sf(data = origen, color = "black", size = 2) +
  geom_sf(data = destino, color = "black", size = 2, shape = 17) +
  scale_color_manual(name = "Ruta", values = c("Más corta (distancia)" = "steelblue",
                                                "Más rápida (tiempo)" = "firebrick")) +
  labs(title = "Comparación: camino más corto vs. camino más rápido") +
  theme_minimal()
print(mapa_rutas)
ggsave("figura_ejercicio2_comparacion_rutas.png", mapa_rutas, width = 8, height = 7, dpi = 200)

long_ruta_distancia <- sum(ruta_distancia_sf$longitud_m)
long_ruta_tiempo <- sum(ruta_tiempo_sf$longitud_m)
tiempo_ruta_distancia <- sum(ruta_distancia_sf$tiempo_min)
tiempo_ruta_tiempo <- sum(ruta_tiempo_sf$tiempo_min)

cat("Ruta por distancia:", round(long_ruta_distancia), "m /", round(tiempo_ruta_distancia, 1), "min\n")
cat("Ruta por tiempo:   ", round(long_ruta_tiempo), "m /", round(tiempo_ruta_tiempo, 1), "min\n")

# --- 3. Matriz de costos origen-destino (Ejercicio 3) -------------------------------

pois <- osmdata::opq(bbox = bb_recortado, timeout = 120) |>
  osmdata::add_osm_feature(key = "amenity", value = c("hospital", "clinic", "school")) |>
  osmdata::osmdata_sf()

hospitales <- pois$osm_points |> filter(amenity %in% c("hospital", "clinic")) |>
  select(osm_id, name, amenity, geometry) |> na.omit()
escuelas <- pois$osm_points |> filter(amenity == "school") |>
  select(osm_id, name, amenity, geometry) |> na.omit()

cat("Hospitales/clínicas encontrados:", nrow(hospitales), "| Escuelas encontradas:", nrow(escuelas), "\n")

if (nrow(hospitales) > 0 && nrow(escuelas) > 0) {
  matriz_costos <- sfnetworks::st_network_cost(
    red, from = escuelas, to = hospitales, weights = "tiempo_min"
  )

  tiempo_al_mas_cercano <- apply(matriz_costos, 1, min, na.rm = TRUE)
  escuelas$tiempo_min_a_salud <- tiempo_al_mas_cercano

  peor_servida <- escuelas |> slice_max(tiempo_min_a_salud, n = 1)
  print(peor_servida)

  mapa_matriz <- ggplot() +
    geom_sf(data = red |> activate("edges") |> st_as_sf(), color = "grey90", linewidth = 0.2) +
    geom_sf(data = escuelas, aes(color = tiempo_min_a_salud), size = 2) +
    geom_sf(data = hospitales, color = "black", shape = 4, size = 2) +
    geom_sf(data = peor_servida, color = "red", size = 3, shape = 1, stroke = 1.5) +
    scale_color_viridis_c(option = "inferno", name = "Min. al\ncentro de salud\nmás cercano") +
    labs(title = "Accesibilidad de escuelas a centros de salud",
         subtitle = "Círculo rojo: establecimiento peor servido") +
    theme_minimal()
  print(mapa_matriz)
  ggsave("figura_ejercicio3_matriz_costos.png", mapa_matriz, width = 8, height = 7, dpi = 200)
} else {
  cat("No se encontraron suficientes POIs de ambas categorías en el área; ",
      "ampliar el bbox o revisar disponibilidad de datos en OSM para esta zona.\n")
}

# --- 4. Isócronas (Ejercicio 4) --------------------------------------------------

if (nrow(hospitales) > 0) {
  punto_referencia <- hospitales[1, ]

  costos_desde_ref <- sfnetworks::st_network_cost(
    red, from = punto_referencia, weights = "tiempo_min"
  )
  nodos_sf$tiempo_desde_ref <- as.numeric(costos_desde_ref[1, ])

  umbrales <- c(5, 10, 15)
  isocronas <- lapply(umbrales, function(u) {
    subset_nodos <- nodos_sf |> filter(tiempo_desde_ref <= u)
    if (nrow(subset_nodos) < 4) return(NULL)
    poligono <- sf::st_concave_hull(st_union(subset_nodos), ratio = 0.3)
    st_sf(umbral_min = u, geometry = poligono)
  })
  isocronas_sf <- do.call(rbind, Filter(Negate(is.null), isocronas))

  mapa_isocronas <- ggplot() +
    geom_sf(data = red |> activate("edges") |> st_as_sf(), color = "grey90", linewidth = 0.15) +
    geom_sf(data = isocronas_sf, aes(fill = factor(umbral_min)), alpha = 0.35, color = NA) +
    geom_sf(data = punto_referencia, color = "black", size = 2) +
    scale_fill_viridis_d(option = "viridis", name = "Isócrona\n(minutos)") +
    labs(title = "Isócronas de acceso vial desde un centro de salud de referencia") +
    theme_minimal()
  print(mapa_isocronas)
  ggsave("figura_ejercicio4_isocronas.png", mapa_isocronas, width = 8, height = 7, dpi = 200)
}

cat("\n--- Fin del script: Práctico 3 ---\n")
