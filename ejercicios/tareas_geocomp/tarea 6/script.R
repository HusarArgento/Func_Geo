#===============================================================================
# Práctico 6 — Dependencia espacial local
# Geocomputación | Caso de estudio: departamentos de la República Argentina
#===============================================================================

paquetes <- c("sf", "spdep", "geodata", "terra", "dplyr", "ggplot2", "tibble")
instalar_faltantes <- paquetes[!paquetes %in% installed.packages()[, "Package"]]
if (length(instalar_faltantes) > 0) install.packages(instalar_faltantes)
invisible(lapply(paquetes, library, character.only = TRUE))

dir_datos <- file.path(tempdir(), "geodata_ar")
dir.create(dir_datos, showWarnings = FALSE, recursive = TRUE)

# --- 0. Reconstrucción del dataset (igual que Práctico 5) --------------------------

url_departamentos <- paste0(
  "https://apis.datos.gob.ar/georef/api/departamentos",
  "?campos=completo&max=600&formato=geojson"
)
departamentos <- sf::read_sf(url_departamentos) |>
  st_make_valid() |>
  select(any_of(c("id", "nombre", "provincia_nombre"))) |>
  rename(depto_id = id, depto_nombre = nombre, provincia = provincia_nombre)

r_temp <- geodata::worldclim_country("Argentina", var = "tavg", res = 10, path = dir_datos)
temp_media_anual <- terra::app(r_temp, mean, na.rm = TRUE)

departamentos_vect <- terra::vect(departamentos)
ex_temp <- terra::extract(temp_media_anual, departamentos_vect, fun = mean,
                           na.rm = TRUE, exact = TRUE)
departamentos$temp_media_c <- ex_temp[, 2]
departamentos <- departamentos |> filter(!is.na(temp_media_c))

cat("Departamentos con datos válidos:", nrow(departamentos), "\n")

vecinos_reina <- spdep::poly2nb(departamentos, queen = TRUE)
pesos_reina <- spdep::nb2listw(vecinos_reina, style = "W", zero.policy = TRUE)

# --- 1. I de Moran local / LISA (Ejercicio 1) --------------------------------------

lisa <- spdep::localmoran(departamentos$temp_media_c, pesos_reina, zero.policy = TRUE)
departamentos$lisa_Ii <- lisa[, "Ii"]
departamentos$lisa_p <- lisa[, "Pr(z != E(Ii))"]

z_variable <- as.numeric(scale(departamentos$temp_media_c))
z_rezago <- as.numeric(scale(spdep::lag.listw(pesos_reina, departamentos$temp_media_c,
                                               zero.policy = TRUE)))

departamentos$cuadrante <- dplyr::case_when(
  z_variable > 0 & z_rezago > 0 ~ "Alto-Alto",
  z_variable < 0 & z_rezago < 0 ~ "Bajo-Bajo",
  z_variable > 0 & z_rezago < 0 ~ "Alto-Bajo",
  z_variable < 0 & z_rezago > 0 ~ "Bajo-Alto",
  TRUE ~ NA_character_
)

alfa <- 0.05
departamentos$cluster_lisa <- ifelse(departamentos$lisa_p < alfa,
                                      departamentos$cuadrante, "No significativo")

colores_cluster <- c("Alto-Alto" = "#d7191c", "Bajo-Bajo" = "#2c7bb6",
                      "Alto-Bajo" = "#fdae61", "Bajo-Alto" = "#abd9e9",
                      "No significativo" = "grey85")

mapa_lisa <- ggplot(departamentos) +
  geom_sf(aes(fill = cluster_lisa), color = NA) +
  scale_fill_manual(values = colores_cluster, name = "Cluster LISA\n(temperatura)") +
  labs(title = "Clusters LISA sin corregir (p < 0.05)") +
  theme_minimal()
print(mapa_lisa)
ggsave("figura_ejercicio1_lisa_sin_corregir.png", mapa_lisa, width = 7, height = 10, dpi = 200)

n_sig_sin_corregir <- sum(departamentos$lisa_p < alfa)
cat("Departamentos significativos SIN corregir:", n_sig_sin_corregir,
    "de", nrow(departamentos), "\n")

# --- 2. Corrección por comparaciones múltiples (Ejercicio 2) -----------------------

departamentos$lisa_p_bonferroni <- p.adjust(departamentos$lisa_p, method = "bonferroni")
departamentos$lisa_p_fdr <- p.adjust(departamentos$lisa_p, method = "BH")

n_sig_bonferroni <- sum(departamentos$lisa_p_bonferroni < alfa)
n_sig_fdr <- sum(departamentos$lisa_p_fdr < alfa)

tabla_correccion <- tibble(
  metodo = c("Sin corregir", "Bonferroni", "FDR (Benjamini-Hochberg)"),
  n_significativos = c(n_sig_sin_corregir, n_sig_bonferroni, n_sig_fdr),
  proporcion = round(c(n_sig_sin_corregir, n_sig_bonferroni, n_sig_fdr) / nrow(departamentos), 3)
)
print(tabla_correccion)
write.csv(tabla_correccion, "tabla_ejercicio2_correccion_multiple.csv", row.names = FALSE)

departamentos$cluster_lisa_fdr <- ifelse(departamentos$lisa_p_fdr < alfa,
                                          departamentos$cuadrante, "No significativo")

mapa_lisa_fdr <- ggplot(departamentos) +
  geom_sf(aes(fill = cluster_lisa_fdr), color = NA) +
  scale_fill_manual(values = colores_cluster, name = "Cluster LISA\n(FDR corregido)") +
  labs(title = "Clusters LISA con corrección FDR (Benjamini-Hochberg)") +
  theme_minimal()
print(mapa_lisa_fdr)
ggsave("figura_ejercicio2_lisa_corregido_fdr.png", mapa_lisa_fdr, width = 7, height = 10, dpi = 200)

# --- 3. Gi* de Getis-Ord (Ejercicio 3) ---------------------------------------------

vecinos_reina_self <- spdep::include.self(vecinos_reina)
pesos_gi <- spdep::nb2listw(vecinos_reina_self, style = "B", zero.policy = TRUE)

gi_star <- spdep::localG(departamentos$temp_media_c, pesos_gi, zero.policy = TRUE)
departamentos$gi_star <- as.numeric(gi_star)

departamentos$cluster_gi <- dplyr::case_when(
  departamentos$gi_star >= 1.96 ~ "Hot spot (p<0.05)",
  departamentos$gi_star <= -1.96 ~ "Cold spot (p<0.05)",
  TRUE ~ "No significativo"
)

colores_gi <- c("Hot spot (p<0.05)" = "#d7191c",
                 "Cold spot (p<0.05)" = "#2c7bb6",
                 "No significativo" = "grey85")

mapa_gi <- ggplot(departamentos) +
  geom_sf(aes(fill = cluster_gi), color = NA) +
  scale_fill_manual(values = colores_gi, name = "Gi* Getis-Ord\n(temperatura)") +
  labs(title = "Hot spots y cold spots — estadístico Gi* de Getis-Ord") +
  theme_minimal()
print(mapa_gi)
ggsave("figura_ejercicio3_gi_star.png", mapa_gi, width = 7, height = 10, dpi = 200)

# --- 4. LISA vs. Gi* (Ejercicio 4) --------------------------------------------------

tabla_comparacion_lisa_gi <- departamentos |>
  st_drop_geometry() |>
  count(cluster_lisa_fdr, cluster_gi) |>
  arrange(desc(n))
print(tabla_comparacion_lisa_gi)
write.csv(tabla_comparacion_lisa_gi, "tabla_ejercicio4_comparacion_lisa_gi.csv", row.names = FALSE)

coincidencia_hh_hotspot <- departamentos |> st_drop_geometry() |>
  filter(cluster_lisa_fdr == "Alto-Alto") |>
  summarise(pct_tambien_hotspot = round(100 * mean(cluster_gi == "Hot spot (p<0.05)"), 1))
cat("De los departamentos Alto-Alto (LISA-FDR), % que también son hot spot en Gi*:",
    coincidencia_hh_hotspot$pct_tambien_hotspot, "%\n")

cat("\n--- Fin del script: Práctico 6 ---\n")
print(tabla_correccion)
