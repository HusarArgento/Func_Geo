#===============================================================================
# Práctico 6 — Dependencia espacial local (variante demográfica)
# Geocomputación | Caso de estudio: departamentos de la República Argentina
# Variable de análisis: densidad de población (habitantes por km²)
#===============================================================================

# Paquetes que usa el script:
# sf (datos espaciales), spdep (autocorrelación espacial), geodata (descarga de
# datos), terra (rásteres y extracción), dplyr (manipulación), ggplot2 (mapas),
# tibble (tablas).
paquetes <- c("sf", "spdep", "geodata", "terra", "dplyr", "ggplot2", "tibble")

# Se instalan solo los paquetes que todavía no están en la computadora.
instalar_faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(instalar_faltantes) > 0) {
  install.packages(instalar_faltantes, repos = "https://cloud.r-project.org")
}

# Se cargan todos los paquetes de la lista.
invisible(lapply(paquetes, library, character.only = TRUE))

# Nivel de significancia: se rechaza la hipótesis nula si p < alfa.
alfa <- 0.05

# Carpeta temporal donde se guardan los datos descargados (GADM, población).
dir_datos <- file.path(tempdir(), "geodata_ar")
dir.create(dir_datos, showWarnings = FALSE, recursive = TRUE)

# --- 0. Dataset: polígonos de departamentos (GADM nivel 2) -------------------------
# La API de Georef devuelve centroides (puntos) y no contornos: todas las geometrías
# fueron descartadas por no ser polígonos. Los polígonos de departamentos/partidos
# se toman de GADM nivel 2. Esto cambia el origen respecto del Práctico 5.

# Descarga los polígonos de departamentos/partidos de Argentina (nivel 2 de GADM).
gadm_v <- geodata::gadm(country = "ARG", level = 2, path = dir_datos)

# Convierte el objeto de terra a sf: primero a data.frame con la geometría en texto
# (WKT) y después se reconstruye como objeto espacial sf con el mismo sistema de coordenadas.
# Luego se corrige cualquier polígono inválido (autointersecciones, anillos mal cerrados).
departamentos <- sf::st_as_sf(
  as.data.frame(gadm_v, geom = "WKT"),
  wkt = "geometry",
  crs = terra::crs(gadm_v)
) |>
  sf::st_make_valid()

# Elimina filas sin geometría (vacías), que no pueden participar del análisis.
departamentos <- departamentos[!sf::st_is_empty(departamentos), ]

# st_make_valid() a veces devuelve colecciones que mezclan polígonos con líneas o puntos.
# Si existen, se extraen solo las partes poligonales.
if (any(as.character(sf::st_geometry_type(departamentos)) == "GEOMETRYCOLLECTION")) {
  departamentos <- sf::st_collection_extract(departamentos, "POLYGON")
}

# Se calcula el tipo de geometría de cada fila y se informa cuántas se descartan
# por no ser polígonos (debería dar 0 con los datos de GADM).
tipos_finales <- as.character(sf::st_geometry_type(departamentos))
cat("Geometrías descartadas por no ser polígonos:",
    sum(!tipos_finales %in% c("POLYGON", "MULTIPOLYGON")), "\n")

# Se conservan solo las filas que son polígonos o multipolígonos.
departamentos <- departamentos[tipos_finales %in% c("POLYGON", "MULTIPOLYGON"), ]

# Verifica que GADM trajo las columnas esperadas antes de renombrarlas.
stopifnot(all(c("GID_2", "NAME_2", "NAME_1") %in% names(departamentos)))

# Selecciona solo las columnas necesarias y les da nombres en español:
# identificador, nombre del departamento y nombre de la provincia.
departamentos <- departamentos |>
  dplyr::select(depto_id = GID_2, depto_nombre = NAME_2, provincia = NAME_1)

# --- 0.b Variable demográfica: densidad de población --------------------------------
# Se descarga el raster de población de WorldPop (cada celda tiene la cantidad
# estimada de habitantes). Año 2020, resolución de 2.5 minutos de arco.
# La función y sus argumentos deben verificarse con la versión instalada de geodata
# (ver ?geodata::population).
r_pob <- geodata::population(year = 2020, res = 2.5, path = dir_datos)

# Se recorta el raster al rectángulo que cubre al país, para que los cálculos sean más rápidos.
departamentos_vect <- terra::vect(departamentos["depto_id"])
r_pob <- terra::crop(r_pob, terra::ext(departamentos_vect))

# Para cada departamento se suma la población de las celdas que caen dentro de su polígono.
# Se usa sum (no mean) porque lo que interesa es el total de habitantes, no un promedio.
pob_total <- terra::extract(r_pob, departamentos_vect,
                            fun = sum, na.rm = TRUE, ID = FALSE)

# Se guarda la población total de cada departamento como columna de la tabla.
departamentos$pob_total <- pob_total[[1]]

# Superficie de cada departamento en km². terra::expanse() calcula el área sobre la
# superficie terrestre (coordenadas geográficas) y devuelve metros cuadrados,
# por eso se divide por 1e6.
departamentos$area_km2 <- as.numeric(terra::expanse(departamentos_vect)) / 1e6

# Densidad de población: habitantes por km². Es la variable que se analiza.
departamentos$densidad_pob <- departamentos$pob_total / departamentos$area_km2

# Se eliminan los departamentos sin dato de población (valores faltantes o área nula).
departamentos <- departamentos |>
  dplyr::filter(!is.na(densidad_pob), is.finite(densidad_pob))

# Cantidad de departamentos que quedan para el análisis.
cat("Departamentos con datos válidos:", nrow(departamentos), "\n")

# Matriz de vecinos por contigüidad "reina": dos departamentos son vecinos si
# comparten al menos un punto de borde (lado o vértice).
vecinos_reina <- spdep::poly2nb(departamentos, queen = TRUE)

# Matriz de pesos espaciales: estandarización "W" (cada fila suma 1), de modo que
# el rezago de un departamento es el promedio de sus vecinos.
# zero.policy = TRUE permite que las unidades sin vecinos (islas) no generen error.
pesos_reina <- spdep::nb2listw(vecinos_reina, style = "W", zero.policy = TRUE)

# --- 1. I de Moran local / LISA (Ejercicio 1) --------------------------------------

# Se guarda la variable de interés (densidad de población) en un vector auxiliar.
densidad <- departamentos$densidad_pob

# Calcula el I de Moran local (LISA) para cada departamento:
# devuelve, entre otras columnas, el valor Ii y su p-valor.
lisa <- spdep::localmoran(densidad, pesos_reina, zero.policy = TRUE)

# Se guarda el estadístico Ii de cada departamento.
departamentos$lisa_Ii <- lisa[, "Ii"]

# Se guarda el p-valor de cada departamento (prueba de dos colas).
departamentos$lisa_p  <- lisa[, "Pr(z != E(Ii))"]

# Valor estandarizado (z-score) de la densidad: ¿cuánto se aleja cada departamento
# de la media nacional?
z_var <- as.numeric(scale(densidad))

# Rezago espacial: el promedio de la densidad de los vecinos de cada departamento,
# estandarizado con el mismo criterio.
z_lag <- as.numeric(scale(spdep::lag.listw(pesos_reina, densidad, zero.policy = TRUE)))

# Asigna a cada departamento su cuadrante del diagrama de Moran, según el signo
# de z_var (valor propio) y de z_lag (valor de sus vecinos).
departamentos$cuadrante <- dplyr::case_when(
  z_var > 0 & z_lag > 0 ~ "Alto-Alto",
  z_var < 0 & z_lag < 0 ~ "Bajo-Bajo",
  z_var > 0 & z_lag < 0 ~ "Alto-Bajo",
  z_var < 0 & z_lag > 0 ~ "Bajo-Alto",
  TRUE ~ NA_character_
)

# Solo se conserva el cuadrante de los departamentos significativos (p < alfa);
# el resto queda como "No significativo".
departamentos$cluster_lisa <- ifelse(departamentos$lisa_p < alfa,
                                     departamentos$cuadrante, "No significativo")

# Paleta de colores del mapa de clusters: rojo = Alto-Alto, azul = Bajo-Bajo,
# rosa = Alto-Bajo, celeste = Bajo-Alto, gris = no significativo.
colores_cluster <- c("Alto-Alto" = "#d7191c", "Bajo-Bajo" = "#2c7bb6",
                     "Alto-Bajo" = "#f4b6b6", "Bajo-Alto" = "#b9e0f0",
                     "No significativo" = "grey85")

# MAPA 1: clusters LISA sin corregir.
# Se dibuja cada departamento como polígono, coloreado según su cluster LISA.
# El resultado muestra dónde hay agrupamientos de densidad significativos antes de
# ajustar por comparaciones múltiples: rojo = departamentos densos rodeados de
# departamentos densos; azul = despoblados rodeados de despoblados.
mapa_lisa <- ggplot(departamentos) +
  geom_sf(aes(fill = cluster_lisa), color = NA) +
  scale_fill_manual(values = colores_cluster, name = "Cluster LISA\n(densidad de población)") +
  labs(title = "Clusters LISA sin corregir (p < 0.05)") +
  theme_minimal()

# Muestra el mapa en la ventana de gráficos.
print(mapa_lisa)

# Guarda el mapa como imagen PNG (7 x 10 pulgadas, 200 puntos por pulgada).
ggsave("figura_ejercicio1_lisa_sin_corregir.png", mapa_lisa, width = 7, height = 10, dpi = 200)

# Cuenta cuántos departamentos son significativos sin ninguna corrección.
n_sig_sin_corregir <- sum(departamentos$lisa_p < alfa)
cat("Departamentos significativos SIN corregir:", n_sig_sin_corregir,
    "de", nrow(departamentos), "\n")

# --- 2. Corrección por comparaciones múltiples (Ejercicio 2) -----------------------

# Ajusta los p-valores por Bonferroni: multiplica cada p por la cantidad de tests.
# Es muy conservadora.
departamentos$lisa_p_bonferroni <- p.adjust(departamentos$lisa_p, method = "bonferroni")

# Ajusta los p-valores por FDR (Benjamini-Hochberg): controla la proporción esperada
# de falsos descubrimientos entre los significativos. Es menos estricta.
departamentos$lisa_p_fdr        <- p.adjust(departamentos$lisa_p, method = "BH")

# Cantidad de departamentos significativos con cada corrección.
n_sig_bonferroni <- sum(departamentos$lisa_p_bonferroni < alfa)
n_sig_fdr        <- sum(departamentos$lisa_p_fdr < alfa)

# Tabla resumen: cantidad y proporción de departamentos significativos en cada caso.
tabla_correccion <- tibble(
  metodo           = c("Sin corregir", "Bonferroni", "FDR (Benjamini-Hochberg)"),
  n_significativos = c(n_sig_sin_corregir, n_sig_bonferroni, n_sig_fdr),
  proporcion       = round(c(n_sig_sin_corregir, n_sig_bonferroni, n_sig_fdr) / nrow(departamentos), 3)
)

# Muestra la tabla en consola y la guarda como CSV para la entrega.
print(tabla_correccion)
write.csv(tabla_correccion, "tabla_ejercicio2_correccion_multiple.csv", row.names = FALSE)

# Aplica la corrección FDR para definir los clusters finales: se conserva el cuadrante
# solo si el p-valor corregido es menor que alfa.
departamentos$cluster_lisa_fdr <- ifelse(departamentos$lisa_p_fdr < alfa,
                                         departamentos$cuadrante, "No significativo")

# MAPA 2: clusters LISA con corrección FDR.
# Se compara visualmente con el mapa 1: el patrón se "encoge" cuando se
# eliminan los falsos positivos y quedan los clusters más robustos.
mapa_lisa_fdr <- ggplot(departamentos) +
  geom_sf(aes(fill = cluster_lisa_fdr), color = NA) +
  scale_fill_manual(values = colores_cluster, name = "Cluster LISA\n(FDR, densidad)") +
  labs(title = "Clusters LISA con corrección FDR (Benjamini-Hochberg)") +
  theme_minimal()

# Muestra el mapa corregido.
print(mapa_lisa_fdr)

# Guarda el mapa corregido como PNG con el mismo tamaño que el anterior.
ggsave("figura_ejercicio2_lisa_corregido_fdr.png", mapa_lisa_fdr, width = 7, height = 10, dpi = 200)

# --- 3. Gi* de Getis-Ord (Ejercicio 3) ---------------------------------------------

# Incluye a cada departamento como vecino de sí mismo (esto es lo que convierte a
# Gi en Gi*).
vecinos_reina_self <- spdep::include.self(vecinos_reina)

# Matriz de pesos binaria ("B"): vale 1 si hay vecindad (incluida la propia unidad) y 0 si no.
pesos_gi <- spdep::nb2listw(vecinos_reina_self, style = "B", zero.policy = TRUE)

# Calcula el estadístico Gi* para cada departamento: es un z-score que compara el
# promedio de su entorno (incluido él mismo) con el promedio nacional de densidad.
gi_star <- as.numeric(spdep::localG(densidad, pesos_gi, zero.policy = TRUE))

# Se guarda el z-score de cada departamento.
departamentos$gi_star <- gi_star

# Convierte el z-score en p-valor de dos colas (distribución normal estándar).
departamentos$gi_p    <- 2 * pnorm(-abs(gi_star))

# Clasifica los departamentos: hot spot si z > 0 y es significativo, cold spot si z < 0
# y es significativo, y no significativo en los demás casos.
departamentos$cluster_gi <- dplyr::case_when(
  gi_star > 0 & departamentos$gi_p < alfa ~ "Hot spot (p<0.05)",
  gi_star < 0 & departamentos$gi_p < alfa ~ "Cold spot (p<0.05)",
  TRUE ~ "No significativo"
)

# Paleta del mapa de Gi*: rojo = hot spot, azul = cold spot, gris = no significativo.
colores_gi <- c("Hot spot (p<0.05)" = "#d7191c",
                "Cold spot (p<0.05)" = "#2c7bb6",
                "No significativo" = "grey85")

# MAPA 3: hot spots y cold spots de Gi*.
# Se dibuja el mapa de departamentos coloreado por la clasificación de Gi*.
# Muestra zonas de alta densidad concentrada (hot spots, por ejemplo el AMBA
# y los grandes centros urbanos) y zonas de baja densidad concentrada (cold spots,
# la Patagonia y el oeste árido).
mapa_gi <- ggplot(departamentos) +
  geom_sf(aes(fill = cluster_gi), color = NA) +
  scale_fill_manual(values = colores_gi, name = "Gi* Getis-Ord\n(densidad de población)") +
  labs(title = "Hot spots y cold spots — estadístico Gi* de Getis-Ord") +
  theme_minimal()

# Muestra el mapa de Gi*.
print(mapa_gi)

# Guarda el mapa de Gi* como PNG.
ggsave("figura_ejercicio3_gi_star.png", mapa_gi, width = 7, height = 10, dpi = 200)

# --- 4. LISA vs. Gi* (Ejercicio 4) --------------------------------------------------

# Tabla de contingencia: cruza el cluster LISA (FDR) con la clasificación de Gi*,
# para ver cuántos departamentos coinciden en cada combinación.
tabla_comparacion_lisa_gi <- departamentos |>
  sf::st_drop_geometry() |>
  dplyr::count(cluster_lisa_fdr, cluster_gi) |>
  dplyr::arrange(dplyr::desc(n))

# Muestra la tabla y la guarda como CSV.
print(tabla_comparacion_lisa_gi)
write.csv(tabla_comparacion_lisa_gi, "tabla_ejercicio4_comparacion_lisa_gi.csv", row.names = FALSE)

# Porcentaje de departamentos Alto-Alto (LISA con FDR) que también son hot spot en Gi*.
# Es una medida directa de cuánto coinciden ambos indicadores.
coincidencia_hh_hotspot <- departamentos |>
  sf::st_drop_geometry() |>
  dplyr::filter(cluster_lisa_fdr == "Alto-Alto") |>
  dplyr::summarise(pct_tambien_hotspot = round(100 * mean(cluster_gi == "Hot spot (p<0.05)"), 1))

# Muestra ese porcentaje en consola.
cat("De los departamentos Alto-Alto (LISA-FDR), % que también son hot spot en Gi*:",
    coincidencia_hh_hotspot$pct_tambien_hotspot, "%\n")

# Mensaje de cierre y repetición de la tabla de corrección.
cat("\n--- Fin del script: Práctico 6 (variante demográfica) ---\n")
print(tabla_correccion)
