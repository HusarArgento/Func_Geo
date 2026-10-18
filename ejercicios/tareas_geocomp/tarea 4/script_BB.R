#===============================================================================
# Práctico 4 — Transformaciones morfológicas de redes espaciales
# Geocomputación | Casos: Viedma (Río Negro) y Carmen de Patagones (Buenos Aires), AR
#
# Mismo análisis que el script de Bahía Blanca, con estas diferencias:
#   - Toda la red se transforma a un CRS métrico (UTM 20S, WGS 84) antes de medir
#     y de construir la grilla. El CRS se calcula a partir del bbox de cada ciudad.
#   - La asignación de nodos a zonas usa st_intersects() y no st_within(), para que
#     un nodo sobre el borde de una celda no quede sin zona.
#   - Las conversiones a igraph son explícitas (igraph::as.igraph()).
#   - El top 5 toma el valor máximo por calle (group_by + slice_max) antes del
#     ranking global, en lugar de la primera fila de cada calle.
#===============================================================================

paquetes <- c("sf", "osmdata", "sfnetworks", "tidygraph", "igraph",
              "ggplot2", "dplyr", "tibble", "viridis")
instalar_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar_faltantes) > 0) install.packages(instalar_faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

# --- Configuración ----------------------------------------------------------------

lugares <- list(
  viedma = list(nombre = "Viedma, Argentina", etiqueta = "Viedma"),
  carmen_de_patagones = list(nombre = "Carmen de Patagones, Argentina",
                             etiqueta = "Carmen de Patagones")
)

# UTM hemisferio sur (WGS 84): código 327 + número de zona.
# Para ambas ciudades la zona es 20 (meridiano central −63°), es decir EPSG 32720.
crs_utm_para <- function(bb) {
  lon <- mean(bb["x", c("min", "max")])
  zona <- floor((lon + 180) / 6) + 1
  as.integer(32700 + zona)
}

# --- Descarga de OSM con reintentos y mirrors -------------------------------------
# Un 504 significa que el servidor Overpass no llegó a responder a tiempo. Subir el
# timeout del cliente no lo resuelve, así que se prueban otros servidores con pausas.

endpoints_overpass <- c(
  "https://overpass-api.de/api/interpreter",
  "https://overpass.kumi.systems/api/interpreter",
  "https://overpass.private.coffee/api/interpreter"
)

descargar_osm <- function(bb, intentos_por_servidor = 2) {
  for (url in endpoints_overpass) {
    osmdata::set_overpass_url(url)
    for (i in seq_len(intentos_por_servidor)) {
      cat("Descargando OSM desde", url, "(intento", i, ")\n")
      res <- tryCatch({
        osmdata::opq(bbox = bb, timeout = 300) |>
          osmdata::add_osm_feature(key = "highway") |>
          osmdata::osmdata_sf() |>
          (\(x) x$osm_lines)()
      }, error = function(e) {
        message("  Falló: ", conditionMessage(e))
        Sys.sleep(20)
        NULL
      })
      if (!is.null(res)) return(res)
    }
  }
  stop("Ningún servidor Overpass respondió. Volver a intentar más tarde; ",
       "la descarga queda en caché cuando tiene éxito.")
}

# --- Función principal: el mismo flujo para cada ciudad ----------------------------

analizar_lugar <- function(lugar_nombre, etiqueta) {
  slug <- tolower(gsub(" ", "_", etiqueta))
  cat("\n==================== ", etiqueta, " ====================\n", sep = "")
  
  # --- 0. Reconstrucción de la red limpia -----------------------------------------
  
  bb <- osmdata::getbb(lugar_nombre)
  margen <- 0.02
  bb_recortado <- rbind(
    x = c(min = bb["x", "min"] + margen, max = bb["x", "max"] - margen),
    y = c(min = bb["y", "min"] + margen, max = bb["y", "max"] - margen)
  )
  # Si el bbox del partido es muy grande (zonas rurales) la descarga puede tardar.
  # En ese caso conviene recortar a mano bb_recortado con coordenadas del área urbana.
  
  crs_proj <- crs_utm_para(bb)
  cat("CRS proyectado: EPSG", crs_proj, "(UTM 20S, WGS 84)\n")
  
  # Caché: si la descarga ya se hizo, se reutiliza el archivo y no se vuelve a consultar OSM.
  cache_osm <- paste0("osm_vias_", slug, ".rds")
  if (file.exists(cache_osm)) {
    lineas_osm <- readRDS(cache_osm)
  } else {
    lineas_osm <- descargar_osm(bb_recortado)
    saveRDS(lineas_osm, cache_osm)
  }
  
  vias_sf <- lineas_osm |>
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
  
  # CORRECCIÓN: la red completa pasa a metros antes de medir y de crear la grilla.
  red <- st_transform(red, crs_proj)
  
  # --- 1. Grafo dual (Ejercicio 1) ----------------------------------------------
  
  red_dual <- tidygraph::convert(red, to_linegraph)
  
  cat("Red primal — orden:", igraph::gorder(red), "| tamaño:", igraph::gsize(red), "\n")
  cat("Grafo dual  — orden:", igraph::gorder(red_dual), "| tamaño:", igraph::gsize(red_dual), "\n")
  cat("(El orden del dual debe coincidir con el tamaño de la primal)\n")
  
  # --- 2. Centralidad en ambas representaciones (Ejercicio 2) -------------------
  
  red <- red |> activate("nodes") |> mutate(betweenness_primal = centrality_betweenness())
  red_dual <- red_dual |> activate("nodes") |> mutate(betweenness_dual = centrality_betweenness())
  
  nodos_primal_sf <- red |> activate("nodes") |> st_as_sf()
  aristas_primal_sf <- red |> activate("edges") |> st_as_sf()
  
  mapa_2a <- ggplot() +
    geom_sf(data = aristas_primal_sf, color = "grey85", linewidth = 0.2) +
    geom_sf(data = nodos_primal_sf, aes(color = betweenness_primal), size = 0.6) +
    scale_color_viridis_c(option = "plasma", name = "Intermediación\n(nodo primal)") +
    labs(title = paste("Centralidad de intermediación — red primal,", etiqueta)) +
    theme_minimal()
  print(mapa_2a)
  ggsave(paste0("figura_", slug, "_ejercicio2a_betweenness_primal.png"), mapa_2a,
         width = 8, height = 7, dpi = 200)
  
  # Mapeo de la centralidad dual: los nodos del dual siguen el orden de las aristas primales.
  # Se verifica la cardinalidad y se detiene si no coincide.
  betweenness_dual_vec <- red_dual |> activate("nodes") |> tibble::as_tibble() |>
    dplyr::pull(betweenness_dual)
  stopifnot(length(betweenness_dual_vec) == nrow(aristas_primal_sf))
  aristas_primal_sf$betweenness_dual <- betweenness_dual_vec
  
  mapa_2b <- ggplot() +
    geom_sf(data = aristas_primal_sf, aes(color = betweenness_dual), linewidth = 0.6) +
    scale_color_viridis_c(option = "inferno", name = "Intermediación\n(nodo dual = calle)") +
    labs(title = paste("Centralidad de intermediación — grafo dual sobre las calles,", etiqueta)) +
    theme_minimal()
  print(mapa_2b)
  ggsave(paste0("figura_", slug, "_ejercicio2b_betweenness_dual.png"), mapa_2b,
         width = 8, height = 7, dpi = 200)
  
  # CORRECCIÓN: el máximo por calle se toma antes del ranking global.
  top5 <- aristas_primal_sf |> st_drop_geometry() |>
    filter(!is.na(name)) |>
    group_by(name) |>
    slice_max(betweenness_dual, n = 1, with_ties = FALSE) |>
    ungroup() |>
    slice_max(betweenness_dual, n = 5, with_ties = FALSE) |>
    select(name, highway, betweenness_dual)
  cat("\nTop 5 calles con mayor centralidad dual:\n")
  print(top5)
  
  # --- 3. Contracción espacial (Ejercicio 3) ------------------------------------
  
  # La grilla se crea en metros porque la red ya está en UTM.
  area_estudio <- st_as_sfc(st_bbox(nodos_primal_sf))
  grilla_hex_geom <- st_make_grid(area_estudio, cellsize = 500, square = FALSE)
  grilla_hex <- st_sf(zona_id = seq_along(grilla_hex_geom), geometry = grilla_hex_geom)
  
  # Un nodo puede caer sobre el borde de dos celdas: se toma la primera que lo contiene.
  idx_zonas <- st_intersects(nodos_primal_sf, grilla_hex)
  zona_nodo <- vapply(idx_zonas,
                      function(z) if (length(z) > 0) z[1] else NA_integer_,
                      integer(1))
  cat("Nodos sin zona:", sum(is.na(zona_nodo)),
      "| zonas ocupadas:", length(unique(na.omit(zona_nodo))), "\n")
  
  red_zonas <- red |>
    activate("nodes") |>
    mutate(zona_id = zona_nodo) |>
    filter(!is.na(zona_id))
  
  red_contraida <- tidygraph::convert(
    red_zonas, to_spatial_contracted, zona_id, simplify = TRUE
  )
  
  # Si la contracción devuelve una lista de estados, se toma el primero.
  if (is.list(red_contraida) && !inherits(red_contraida, "tbl_graph")) {
    red_contraida <- red_contraida[[1]]
  }
  stopifnot(inherits(red_contraida, "tbl_graph"))
  
  cat("Red contraída por zonas — orden:", igraph::gorder(igraph::as.igraph(red_contraida)),
      "| tamaño:", igraph::gsize(igraph::as.igraph(red_contraida)), "\n")
  
  nodos_contraidos_sf <- red_contraida |> activate("nodes") |> st_as_sf()
  aristas_contraidas_sf <- red_contraida |> activate("edges") |> st_as_sf()
  
  mapa_3 <- ggplot() +
    geom_sf(data = aristas_primal_sf, color = "grey90", linewidth = 0.15) +
    geom_sf(data = aristas_contraidas_sf, color = "darkorange", linewidth = 0.8, alpha = 0.7) +
    geom_sf(data = nodos_contraidos_sf, color = "darkred", size = 1.5) +
    labs(title = paste("Red contraída por zonas (grilla hexagonal ~500 m),", etiqueta),
         subtitle = "Naranja: conexiones agregadas entre zonas") +
    theme_minimal()
  print(mapa_3)
  ggsave(paste0("figura_", slug, "_ejercicio3_contraccion_espacial.png"), mapa_3,
         width = 8, height = 7, dpi = 200)
  
  # --- 4. Efecto sobre la densidad (Ejercicio 4) --------------------------------
  
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
  write.csv(comparacion_densidad,
            paste0("tabla_ejercicio4_densidad_", slug, ".csv"), row.names = FALSE)
  
  invisible(list(etiqueta = etiqueta, top5 = top5, densidad = comparacion_densidad))
}

# --- Ejecución para ambas ciudades --------------------------------------------------

resultados <- lapply(lugares, function(l) analizar_lugar(l$nombre, l$etiqueta))

cat("\n--- Insumos para el informe de síntesis (Ejercicio 5) ---\n")
for (r in resultados) {
  cat("\n", r$etiqueta, "\n", sep = "")
  cat("Top 5 calles con mayor centralidad dual:\n")
  print(r$top5)
  cat("Densidad primal vs. contraída:\n")
  print(r$densidad)
}

cat("\n--- Fin del script: Práctico 4, Viedma y Carmen de Patagones ---\n")

