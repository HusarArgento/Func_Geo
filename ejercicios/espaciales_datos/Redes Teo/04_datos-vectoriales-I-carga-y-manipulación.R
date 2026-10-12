# Datos Vectoriales I: Manipulación de datos vectoriales

# Cargamos librerías

library(tidyverse)
library(sf)
library(fs)

# Fuentes de datos usadas
# -----------------------*
# Limite partidos:    https://catalogo.datos.gba.gob.ar/dataset/partidos/archivo/2cc73f96-98f7-42fa-a180-e56c755cf59a
# Escuelas:           https://catalogo.datos.gba.gob.ar/dataset/establecimientos-educativos/archivo/3951210e-7e0e-4fed-bbf1-0183e704c9ae
# Universidades:      https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG

# Operaciones introductorias para datos vectoriales en R -----------------------

### ╠ Lectura y escritura de datos espaciales ----------------------------------

  # st_read()
  # Permite leer datos espaciales desde diferentes fuentes.
  partidos <- st_read(dsn = fs::path("data", "limite_partidos", ext = "geojson"))

  # exploramos la estrcutrura del objeto importado
  print(partidos)
  class(partidos)

  dim(partidos)
  names(partidos)
  attributes(partidos)

  # plot(partidos)
  plot(partidos[NULL])

  # read_sf()
  # Es un alias de st_read() con algunas particularidades
  # -> st_read(..., quiet = TRUE, stringsAsFactors = FALSE, as_tibble = TRUE)
  partidos <- read_sf(fs::path("data", "limite_partidos.geojson"))

  # Lectura con consulta SQL: Es posible leer los datos incluyendo una
  #   consulta SQL, mediante el parámetro 'query'.
  escobar <- st_read(fs::path("data", "limite_partidos.geojson"),
                     query = "SELECT * FROM limite_partidos WHERE nam = 'Escobar'")

  # st_write()
  # Permite guardar datos espaciales en diferentes formatos.
  st_write(obj = escobar, dsn = fs::path("data", "escobar.geojson"),
           append = FALSE, delete_layer = TRUE)

  # Limpiamos el espacio de trabajo y el directorio.
  rm(escobar)

  file.remove(list.files(fs::path("data"),
                         pattern = "escobar.*",
                         full.names = T))

  # Si los datos están en csv es posible usar el parámetro 'options' para indicar
  #   la columna donde está la 'geometría'. El parámetro crs nos permite indicar
  #   el crs de varias maneras (si es un número supondrá que es ESRI, sino como
  #   cadena WKT). Volveremos más adelante a esto.
  # Se debe indicar:
  #   * "GEOM_POSSIBLE_NAMES=NOMBRE"                  si tiene una WKT
  #   * c("X_POSSIBLE_NAMES=X", "Y_POSSIBLE_NAMES=Y") si es un par de coordenadas
  uni <- st_read(fs::path("data","universidades.csv"),
                 options = "GEOM_POSSIBLE_NAMES=geom",
                 crs = 4326)

  # st_as_sf()
  # A veces es necesario convertir datos tabulares (data.frames), que tienen
  #   geometrías implícitas, en objetos sf. Necesitamos  indicar qué columna
  #    tiene la geometría (mediante los parámetros "wkt" o "coords").
  escuelas <- read.csv(fs::path("data",
                                "establecimientos-educativos_13112023.csv"),
                       colClasses = c("cueanexo" = "character"))
  class(escuelas)
  names(escuelas)

  # ¿Dónde están las coordenadas?
  escuelas[c("latitud", "longitud", "the_geom")] |> head()

  escuelas <- escuelas |>
    as_tibble() |>
    st_as_sf(wkt = "the_geom")
  # escuelas <- st_as_sf(escuelas, coords =  c("longitud", "latitud"))

  # Alternativamente: Cargar directamente con {sf}
  # escuelas <- st_read(fs::path("data",
  #                                "establecimientos-educativos_13112023.csv"),
  #                     options = c("X_POSSIBLE_NAMES=longitud",
  #                                 "Y_POSSIBLE_NAMES=latitud"))

  class(escuelas)
  escuelas

### ╠ sf y los sistemas de coordenadas --------------------------

  # st_crs(): Permite obtener el crs de un objeto sf
  st_crs(partidos)
  st_crs(escuelas)

  # st_set_crs(): Asignar crs a un objeto sf.
  st_set_crs(escuelas, value = 4326) |> head()
  st_crs(escuelas) <- 4326

  # Ojo, esto no reproyecta! (ver warning)
  st_set_crs(partidos, 5347) |> head()

  # st_transform(): Reproyectar/transformar entre crs.
  partidos <- st_transform(partidos, crs = 5347)
  partidos

  st_crs(partidos)

  # Unificamos el CRS de las capas
  uni <- st_transform(uni, crs = st_crs(partidos))
  escuelas <- st_transform(escuelas, st_crs(partidos))

### ╚ La columna de geometría --------------------------------------

  # La columna geometry es "pegajosa"
  escuelas |> select(sector)

  # st_drop_geometry(): Quitar ("despegar") la geometría.
  escuelas |> st_drop_geometry() |> select(sector)

  # Alternativamente podemos usar st_set_geometry(NULL), que tiene el mismo efecto que st_drop_geometry()
  partidos |> st_set_geometry(NULL) |> select(cca)

  # st_coordinates(): Permite obtener las coordenadas como una matriz.
  st_coordinates(escuelas) |> head()
  class(st_coordinates(escuelas))

  # st_is_valid(): Comprobar si geometrías son válidas y no corruptas
  st_is_valid(partidos)

  geo_invalida <- partidos |> filter(!st_is_valid(geometry))
  st_is_valid(geo_invalida, reason = TRUE)
  plot(geo_invalida[NULL])

  # st_make_valid(): Corrige geometrías inválidas
  partidos <- partidos |> st_make_valid()
  sum(!st_is_valid(partidos))

  # st_bbox(): Devuelve una "caja contenedora" (es un vector numérico con
  #   nombres y un crs implícito)
  st_bbox(escuelas)
  st_bbox(uni)

  st_bbox(escuelas) |> class()
  st_bbox(escuelas) |> as.numeric()

  # Podemos transformarlo a un objeto sfc -> sf
  st_bbox(escuelas) |> st_as_sfc() |> st_as_sf() |> plot()

# La estructura de los datos vectoriales en la librería {sf} -----------------------------
  # la librería {sf} presenta los objetos de clase “sf” como una subclase de
  #   los “data.frame”, con una “lista-columna” de clase “sfc” en la cual se
  #    almacena la geometría como un objeto de la clase “sfg”.

### ╠ sf: Simple feature -------------------------------------
  # Los objetos de clase “sf” son una subclase de los "data.frame" con una
  #   “lista-columna” donde se almacena la geometría (de clase "sfc").
  # La clase "sf" contiene como atributos:
  #   * el nombre de la columna con la geometría activa ($sf_column)
  #   * información sobre la relación atributo-geometría ($agr)       -> st_agr()

  partidos[1]
  uni[1]

  class(uni)

  st_geometry_type(partidos)[1]
  st_geometry_type(uni)[1]

### ╠ sfc: simple feature geometry list-column ----------------------
  # Los objetos de clase “sfc” son una lista con las coordenadas de cada una de
  #   las geometrías. Los objetos de clase sfc tienen otra clase que indica el
  #   tipo de geometría que almacenan (sfc_POINT, sfc_MULTIPOINT, sfc_LINESTRING,
  #   sfc_POLYGON, etc.).
  # La clase "sfc" contiene como atributos:
  #   * el sistema de referencia de coordenadas ($crs)  -> st_crs()
  #   * el cuadro delimitador ($bbox)                   -> st_bbox()
  #   * la precisión ($precision)                       -> st_presicion()
  #   * el número de geometrías vacías ($n_empty)

  # st_geometry()
  # Permite obtener de un objeto de clase "sf" la columna de la clase "sfc" con la geometría.
  geom <- st_geometry(uni)
  geom

  class(geom)
  typeof(geom)

  length(geom)

  attributes(geom)

### ╚ sfg: simple feature geometry ---------------------------

  # La clase “sfg” almacena las geometrías individuales. Los objetos de clase "sfg"
  #   tienen también una sublcase que indica el tipo de geometría y las dimensiones
  #   consideradas (XY, XYZ -altura-, XYM -measure-).
  geom_el <- st_geometry(geom)[[1]]
  geom_el

  class(geom_el)
  typeof(geom_el)

  length(geom_el)
  as.numeric(geom_el)

  rm(geom, geom_el)

# Visualizaciones de datos espaciales en ggplot2 ---------------------------

### ╠ Visualización de una capa ------------------------------------

  partidos |>
    ggplot() +
    geom_sf(color = "grey60", fill = "tomato2") +
    labs(title = "Provincia de Buenos Aires") +
    theme_void()

  partidos |>
    mutate(BB = ifelse(nam == "Bahía Blanca", "Bahia Blanca", "Resto de la Provincia") )  |>
    ggplot() +
    geom_sf(color = "grey60", aes(fill = BB)) +
    scale_fill_manual("", values = c("darkblue", "tomato2")) +
    labs(title = "Ubicación del Partido de Bahía Blanca en la Provincia de Buenos Aires") +
    theme_void()

### ╠ Visualización de más de una capa ------------------------------------

  partidos |>
    filter(nam == "Tandil") |>
    ggplot() +
    geom_sf(color = "grey60", fill = "grey", alpha = 0.3) +
    geom_sf(data = filter(escuelas, distrito == "Tandil"),
            aes(color = sector),
            shape = 16, alpha = 0.5) +
    scale_color_manual("", values = c("tomato4", "darkblue")) +
    labs(title = "Escuelas del partido de Tandil",
         color = "Sector") +
    theme_void()

### ╚ Visualizando facetados ---------------------------------------------

  ggplot() +
    geom_sf(color = "grey60", fill = "white",
            data = partidos) +
    geom_sf(data = escuelas,
            aes(color = sector),
            shape = 16, alpha = 0.5) +
    facet_wrap(~sector) +
    labs(title = "Escuelas de la Provincia de Buenos Aires\ndiferenciadas por sector") +
    scale_fill_manual("", values = c("tomato4", "darkblue")) +
    theme_void()

# ► Consigna práctica de aplicación (Parte I) ---------------------------------

# 1. Buscar en el portal Buenos Aires Data (https://data.buenosaires.gob.ar/dataset/)
#     los data sets que contienen el trayecto de las vías y estaciones de ferrocarril.
# 2. Además, en el portal de Datos Argentina (https://datos.gob.ar/) buscar los
#     Radios censales del AMBA.
# 3. Con estos data sets homogeneizar la proyección entre ambos, eligiendo una que
#     sea adecuada para el el área de estudio (AMBA).
# 4. Asegurarse que las geometrías sean válidas (corregir en caso de ser necesario).
# 5. Obtener un "bounding box" de la zona de trabajo. ¿Cómo podemos interpretar
#     esta información?
# 6. Describir brevemente los atributos que acompañan estos datasets. Para ello
#     utilizar un gráfico hecho con ggplot que utilice al menos dos capas de
#     datos espaciales.
# 7. Pensar alguna pregunta que pueda ser interesante responder con estos datos.
#     ¿Es necesario un nuevo data set para responder esta pregunta?
