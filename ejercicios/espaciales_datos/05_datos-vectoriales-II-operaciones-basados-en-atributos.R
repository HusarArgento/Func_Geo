# Datos Vectoriales II: Operaciones espaciales basadas en atributos

# Cargar librerías --------------------------------------------

library(fs)

library(tidyverse)
library(sf)

library(nngeo)
# library(ggpubr)

# Cargar los datos de la clase anterior -----------------------

partidos <- st_read(fs::path("data", "limite_partidos", ext = "geojson")) |>
  st_transform(5347)

any(!st_is_valid(partidos))
sum(!st_is_valid(partidos))

partidos <- partidos |> st_make_valid()

uni <- st_read(fs::path("data", "universidades", ext = "csv"),
               options = "GEOM_POSSIBLE_NAMES=geom",
               crs = 4326) |>
  st_transform(st_crs(partidos))

escuelas <- read.csv(fs::path("data", "establecimientos-educativos_13112023",
                              ext = "csv"),
                     colClasses = c("cue" = "character") ) |>
  st_as_sf(wkt = "the_geom", crs = 4326) |>
  st_transform(st_crs(partidos))

# Operaciones sobre datos espaciales basadas en atributos -----------------------------------
# Ver: https://r.geocompx.org/attr#vector-attribute-manipulation

### ╠ Subconjunto de atributos vectoriales ----------------------------

  # `[` Para seleccionar filas
  escuelas[escuelas$sector == "Privado", ]

  # filter()
  esc_privadas <- escuelas |>
    filter(sector == "Privado" & matricula > 100 & nivel == "Nivel Secundario")

  # slice()
  esc_privadas |>
    slice_max(order_by = matricula, n = 5)

  # `[` Para seleccionar columnas
  escuelas["modalidad"] |> head()
  escuelas[c("sector", "modalidad")] |> head()

  escuelas$modalidad |> head()
  escuelas[["modalidad"]] |> head()

  # select()
  esc_privadas |>
    select(cue, modalidad, sector)

  #Agregamos información sobre pertenencia a GBA (24 partidos)
  GBA_code <- paste0("06", c("490", "658", "434", "371", "840", "035", "568", "756",
                             "410", "515", "861", "760", "412", "408", "427", "028",
                             "539", "560", "274", "805", "091", "260", "749", "270") )

  partidos_GBA <- partidos |>
    mutate(GBA = cde %in% GBA_code) |>
    filter(GBA)
  # filter(GBA & !str_detect(fna, "Islas"))

  rm(esc_privadas, GBA_code)

### ╠ Encadenamiento ("pipe") ----------------------------

  # `[` Para seleccionar filas y columnas simultaneamente
  escuelas[escuelas$sector == "Privado", c("modalidad", "cue")]

  # con tidyverse
  escuelas |>
    filter(sector == "Privado" & nivel == "Nivel Secundario") |>
    select(cue, nombre, subvencion, matricula:turnos) |>
    arrange(-matricula) |>
    as_tibble()

### ╠ Agregación ----------------------------------------

  # summarise()
  escuelas |> summarise(alumnos = sum(matricula))

  escuelas |> summarise(across(matricula:secciones,
                               \(.x) sum(.x, na.rm = T) ) )

  escuelas |> st_drop_geometry() |> summarise(alumnos = sum(matricula))

  PBA <- partidos |>  summarise(area = sum(ara3))
  PBA

  ggplot(PBA) + geom_sf() + theme_void()

  PBA <- PBA |> nngeo::st_remove_holes()

  ggplot(PBA) + geom_sf() + theme_void()

  GBA <- partidos_GBA |>
    summarise(n_partidos = n()) |>
    nngeo::st_remove_holes()

  GBA

  ggplot(GBA) + geom_sf() + theme_void()

  # group_by() |> summarize()
  # Corregimos los partidos "dobles" (zona islas)

  partidos_GBA <- partidos_GBA |>
    group_by(cde) |>
    summarise(nombre = str_remove(first(fna), "Islas "),
              ara3 = sum(ara3, na.rm = T),
              arl = sum(arl, na.rm = T)) |>
    nngeo::st_remove_holes()

  ggplot(partidos_GBA) + geom_sf(aes(fill = nombre)) + theme_void()

### ╚ Vincular tablas de atributos con datos espaciales ------------------------
  # Recuperamos los datos de la clase 2 (quitamos la parte de regiones)

  # datos_depart <- read.csv(fs::path("data",
  #                                   "indicadores_de_personas_departamentos_2010",
  #                                   ext = "csv")) |>
  #   rename(PROV = Código.de.provincia,
  #          PROV_lab = Nombre.de.provincia,
  #          DEP = Código.de.departamentos,
  #          DEP_lab = Nombre.de.departamentos.comuna,
  #
  #          HOG = Total.de.hogares,
  #          POB = Población.total..en.hogares.familiares..,
  #          POB2 = Población.total,
  #          MUJ = Total.de.mujeres..en.hogares.familiares..,
  #
  #          POB_00_03 = Población.de.0.a.3.años.,
  #          POB_04_05 = Población.de.4.a.5.años.,
  #          POB_06_12 = Población.de.6.a.12.años.,
  #          POB_13_17 = Población.de.13.a.17.años.,
  #
  #          POB_NIN = Población.de.0.a.17.años.,
  #
  #          ASI_INI = Niños.de.4.y.5.años.que.asisten.a.jardín,
  #          ASI_PRI = Población.de.6.a.12.que.asiste,
  #
  #          POB_ADU = Población.de.18.años.y.más.,
  #          POB_ADU_SINPRI = Población.de.18.y.más.con.primaria.incompleta.o.menos,
  #          POB_ADU_CONPRI = Población.de.18.y.más.con.primaria.completa,
  #          POB_ADU_CONSEC = Población.de.18.y.más.con.secundaria.completa,
  #          POB_ADU_SINSEC = Población.de.18.y.más.con.primaria.completa.sin.secundario.completo,
  #          POB_ADU_SEC = Población.de.18.y.más.con.secundaria.completa.sin.superior.completo,
  #          POB_ADU_TER = Población.de.18.y.más.con.terciario.completo,
  #          POB_ADU_UNI = Población.de.18.y.más.con.universitario.completo.o.más,
  #
  #          AREA = Superficie.en.km2
  #   ) |>
  #   mutate(POB_ADU_PRI = POB_ADU - POB_ADU_CONSEC,
  #          PROV = str_pad(PROV, 2, pad = "0"),
  #          DEP = str_pad(DEP, 5, pad = "0")) |>
  #   select(-POB_ADU_CONPRI, -POB_ADU_SINPRI, -POB_ADU_SINSEC,
  #          -starts_with("Varones"), -starts_with("Mujeres"),
  #          -Código.de.departamentos.comuna,
  #          -Total.de.varones..en.hogares.familiares..,
  #          -ends_with(".del.centroide")) |>
  #   select(# Información geográfica
  #     PROV, PROV_lab, DEP, DEP_lab,
  #     # Información demográfica global
  #     HOG, POB, POB_TOT = POB2, MUJ,
  #     # Información demográfica de población infantil
  #     POB_NIN, POB_00_03:POB_13_17,
  #     # Información de educación de adultos
  #     POB_ADU, POB_ADU_CONSEC, POB_ADU_PRI, POB_ADU_SEC, POB_ADU_TER, POB_ADU_UNI,
  #     # Resto de variables
  #     everything()) |>
  #   relocate(starts_with("ASI_"), .before = POB_ADU) |>
  #   mutate(AREA = round(AREA, 1),          # km^2
  #
  #          DEN = POB / (AREA * 100),       # Población por ha (100ha = 1 km^2)
  #          FEM = MUJ / (POB-MUJ) * 100,    # Mujeres por cada 100 varones
  #
  #          ASI_INI_p = ASI_INI/ POB_04_05 * 100,
  #          ASI_PRI_p = ASI_PRI/ POB_06_12 * 100,
  #
  #          across(.cols = starts_with("POB_ADU_"),
  #                 .fns = ~.x / POB_ADU * 100,
  #                 .names = "p_{.col}") ) |>
  #   rename_with(.cols = starts_with("p_POB_ADU_"),
  #               .fn = ~str_remove(.x, "POB_ADU_"))

  source(fs::path("scripts", "cargar_data_departamentos", ext = "R"))

  str(datos_depart)

  partidos_GBA <- partidos_GBA |> rename(DEP = cde)

  # datos_depart <- datos_depart |>
  #   filter(PROV == "06")

  left_join(partidos_GBA, datos_depart, by = "DEP")  |> class()
  right_join(datos_depart, partidos_GBA, by = "DEP")  |> class()

  # OJO CON CASOS COMO ESTE:
  # datos |>
  #   # mutate(muchas cosas) |>
  #   # select(seleccionamos variables) |>
  #   right_join(partidos_GBA, by = "DEP")

  partidos_GBA <- left_join(partidos_GBA, datos_depart, by = "DEP")

  partidos_GBA

  ggplot(partidos_GBA) +
    geom_sf(aes(fill = p_UNI)) +
    scale_fill_viridis_c("Porcentaje de personas adultas\ncon nivel universitario completo",
                         option = "E") +
    theme_void() +
    theme(legend.position = "bottom")

  rm(datos_depart)

# ► Consigna práctica de aplicación (Parte II) --------------------------------------------

# 8. Recuperando los datos y el análisis hecho en la clase anterior, visualizar
#     conjuntamente en un mapa los radios censales, las líneas y estaciones
#     de FFCC. Agregar una capa que permita distinguir las dos jurisdicciones
#     principales que componen el AMBA y los partidos (sólo para PBA).
# 9. Descargar datos sobre la cantidad de población de los radios radios del
#     portal poblaciones (poblaciones.org) referidos al AMBA (24 Partidos + CABA).
#     Los datos que se descarguen deben ser datos sin geografía (csv).
# 10. Generar una unión de los datos de población con los datos de radios.
# 11. Visualizar la densidad de población de los radios (agregar una capa con
#     los límites jurisdiccionales).
# 12. Visualizar la densidad de población infantil (0-17 años) y adulta mayor
#     (+70 años)de los radios.
# 13. Visualice conjuntamente los puntos 11 y 12. ¿Existen diferencias en
#     la distribución de estas poblaciones y la población general?
# 14. Agregar a la visualización anterior (punto 13) el mapa de las estaciones y
#       líneas de FFCC. ¿Cree que existe una buena conectividad para estas personas?
