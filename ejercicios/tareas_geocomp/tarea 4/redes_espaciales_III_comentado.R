# =============================================================================
#  REDES ESPACIALES III: OPERACIONES DE FILTRADO Y RUTEO
# =============================================================================
#
#  Basado en las viñetas oficiales del paquete sfnetworks:
#   * https://luukvdmeer.github.io/sfnetworks/articles/sfn03_join_filter.html
#   * https://luukvdmeer.github.io/sfnetworks/articles/sfn04_routing.html
#
#  ¿QUÉ VAMOS A HACER?
#  Primero vamos a CONSTRUIR una red de calles (un grafo espacial) para la
#  Comuna 15 de CABA. Después vamos a USAR esa red para responder tres
#  preguntas típicas del análisis de redes:
#
#   1. Infraestructura más cercana: ¿cuáles son las 2 plazas más cercanas
#      a cada escuela, caminando por las calles (no en línea recta)?
#   2. Problema del viajante (TSP): ¿en qué orden conviene visitar un
#      conjunto de puntos para recorrer la menor distancia posible?
#   3. Isocronas: ¿hasta dónde puedo llegar en 1, 3, 6 o 10 minutos
#      desde un punto de origen?
#
#  El script es AUTÓNOMO: descarga todos los datos del portal de datos
#  abiertos de la Ciudad (https://data.buenosaires.gob.ar), así que necesita
#  conexión a internet. Está pensado para sfnetworks 0.6.x (versión de CRAN).
# =============================================================================


# -----------------------------------------------------------------------------
# 0. LIBRERÍAS
# -----------------------------------------------------------------------------
#  Si te falta alguna, instalala una sola vez con install.packages("nombre").

library(tidyverse)   # Manipulación de datos (dplyr, purrr, stringr) y gráficos (ggplot2)

library(sf)          # Datos vectoriales espaciales: puntos, líneas y polígonos

library(ggpubr)      # Utilidades extra para ggplot2 (combinar gráficos, etc.)
library(leaflet)     # Mapas web interactivos

library(igraph)      # Motor de análisis de grafos (caminos mínimos, componentes...)
library(tidygraph)   # Permite manipular grafos con la sintaxis de dplyr

library(sfnetworks)  # Une sf + tidygraph: grafos con geometría (redes espaciales)

library(TSP)         # Resuelve el "Problema del Viajante" (Traveling Salesperson Problem)


# =============================================================================
#  PREPARACIÓN: CONSTRUCCIÓN DE LA RED DE CALLES
# =============================================================================
#  Antes de analizar una red, hay que construirla. Los pasos son:
#   P1. Obtener el polígono de la Comuna 15 y agrandarlo con un buffer.
#   P2. Descargar las calles y recortarlas con ese buffer.
#   P3. Tener en cuenta el SENTIDO de circulación de cada calle.
#   P4. Convertir las calles en un objeto sfnetwork (la red).
#   P5. Definir los puntos de interés (POI) que usaremos más adelante.
# -----------------------------------------------------------------------------

# Dirección base del portal de datos abiertos de la Ciudad de Buenos Aires.
# La guardamos en una variable para no repetirla en cada descarga.
url_base <- "https://cdn.buenosaires.gob.ar/datosabiertos/datasets"

# --- P1. Polígono de la Comuna 15 y su buffer ---
#  El dataset "Comunas" trae un polígono por comuna. El número de comuna
#  está en la columna "comuna". as.numeric() asegura que la comparación
#  funcione tanto si viene como texto ("15") como si viene como número.
COMUNA15 <- st_read("https://data.buenosaires.gob.ar/dataset/comunas/resource/b0b627ac-5b47-4574-89ac-6999b63598ee/download") |>
  filter(as.numeric(comuna) == 15) |>
  # Reproyectamos a EPSG:5347 (POSGAR 2007 / Argentina 5), un sistema
  # proyectado en METROS. Es imprescindible para medir distancias reales.
  st_transform(5347)

#  ¿Por qué un BUFFER de 500 m?
#  Si cortamos las calles exactamente en el límite de la comuna, los puntos
#  cerca del borde quedarían "encerrados": el camino más corto real a veces
#  sale de la comuna y vuelve a entrar. El buffer agrega un margen de 500 m
#  alrededor del polígono para evitar ese "efecto borde".
COMUNA15_buffer <- st_buffer(COMUNA15, dist = 500)   # dist en metros (unidad del CRS)

# --- P2. Calles (callejero oficial) recortadas al buffer ---
CALLE <- paste(url_base,
               "jefatura-de-gabinete-de-ministros",
               "calles/callejero.geojson", sep = "/") |>
  st_read() |>
  st_transform(5347) |>
  # st_intersection() recorta cada calle con el buffer.
  # st_geometry(...) pasa solo la geometría, sin columnas, para no sumar
  # los atributos de la comuna a cada calle.
  st_intersection(st_geometry(COMUNA15_buffer))

#  Al recortar pueden aparecer geometrías raras (por ejemplo, un PUNTO donde
#  una calle apenas toca el borde) o MULTILÍNEAS (una calle que sale y vuelve
#  a entrar al buffer). sfnetworks necesita que cada arista sea una LINESTRING
#  simple, así que:
CALLE <- CALLE |>
  filter(st_geometry_type(geometry) %in% c("LINESTRING", "MULTILINESTRING")) |>  # solo líneas
  st_cast("MULTILINESTRING") |>   # primero todo a MULTILINESTRING...
  st_cast("LINESTRING")           # ...y después cada parte a LINESTRING

# --- P3. Sentido de circulación ---
#  ¿Qué valores tiene la columna "sentido"? table() cuenta cuántas calles hay
#  de cada tipo.
table(CALLE$sentido, useNA = "ifany")

#  Nos quedamos con las calles que tienen un sentido definido:
#   - CRECIENTE   : se circula en el mismo sentido en que está dibujada la línea
#   - DECRECIENTE : se circula en sentido contrario al dibujo
#   - DOBLE       : se circula en ambos sentidos
CALLE <- CALLE |>
  filter(sentido %in% c("CRECIENTE", "DECRECIENTE", "DOBLE"))

#  En una red DIRIGIDA cada arista va de su primer punto a su último punto.
#  Por eso armamos dos grupos:
#   - CALLE_aux1: calles DECRECIENTES y DOBLES, con la línea INVERTIDA
#                 (st_reverse) para que apunte en el sentido "de vuelta".
#   - CALLE_aux2: calles CRECIENTES y DOBLES, tal como están dibujadas.
#  Las calles DOBLES quedan en los dos grupos: una arista por cada sentido.
#  La columna cod_edge marca el sentido: 1 = como está dibujada, -1 = invertida.
CALLE_aux1 <- CALLE |>
  filter(sentido %in% c("DECRECIENTE", "DOBLE")) |>
  mutate(cod_edge = -1) |>
  st_reverse()

CALLE_aux2 <- CALLE |>
  filter(sentido %in% c("CRECIENTE", "DOBLE")) |>
  mutate(cod_edge = 1)

CALLE2 <- rbind(CALLE_aux1, CALLE_aux2)   # unimos ambos grupos

#  Controles: las DOBLES deberían aparecer con -1 y con 1;
#  las CRECIENTES solo con 1 y las DECRECIENTES solo con -1.
table(CALLE$sentido)
table(ORIG = CALLE2$sentido, EDGE = CALLE2$cod_edge)
table(CALLE2$cod_edge)

# --- P4. Construir la red ---
#  as_sfnetwork() convierte las líneas en un grafo:
#   - cada extremo de calle se transforma en un NODO (si dos calles comparten
#     un extremo, comparten el nodo: así se conectan);
#   - cada calle se transforma en una ARISTA (edge).
#  directed = TRUE         -> red dirigida (respeta el sentido de las calles).
#  length_as_weight = TRUE -> crea la columna "weight" con el largo de cada
#                             arista en metros. La usaremos como costo.
net <- as_sfnetwork(CALLE2, directed = TRUE,
                    length_as_weight = TRUE)

#  Nos quedamos con el componente conectado más grande (el 1). Así evitamos
#  "islas" de calles sueltas desde las que no se puede llegar a ningún lado.
net <- net |>
  activate("nodes") |>
  filter(group_components() == 1)

net   # Inspeccionamos la red: cantidad de nodos, aristas y columnas

# --- P5. Puntos de interés (POI) ---
#  Los creamos a partir de coordenadas geográficas (lon/lat, EPSG:4326).
#  El PRIMERO es la Facultad de Agronomía: lo usamos como origen de las
#  isocronas (sección 3).
POI <- data.frame(
  poi  = c("Facultad de Agronomía (UBA)",
           "Estación Federico Lacroze",
           "Estadio Diego A. Maradona",
           "Estadio de Atlanta"),
  tipo = "Orig",
  x    = c(-58.4826, -58.4553, -58.4766, -58.4494),   # longitud
  y    = c(-34.5930, -34.5874, -34.6063, -34.5937)    # latitud
) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>   # tabla -> objeto sf
  st_transform(st_crs(CALLE))                     # al mismo CRS que las calles

#  Ya no necesitamos los objetos auxiliares
rm(CALLE_aux1, CALLE_aux2, CALLE2)


# =============================================================================
#  APLICACIONES
# =============================================================================


# -----------------------------------------------------------------------------
# 1. ANÁLISIS DE INFRAESTRUCTURA MÁS CERCANA
# -----------------------------------------------------------------------------
#  Pregunta: ¿cuáles son las 2 plazas más cercanas a cada escuela primaria
#  estatal de la Comuna 15, medidas POR LA RED DE CALLES?
#
#  Idea clave: la distancia "en línea recta" (euclidiana) no refleja lo que
#  realmente camina una persona. Por eso calculamos la distancia a lo largo
#  de la red: el camino más corto siguiendo las calles.
# -----------------------------------------------------------------------------

# --- 1.1 Cargar las ESCUELAS ---
ESC <- st_read(paste(url_base, "ministerio-de-educacion",           # paste() arma la URL
                     "establecimientos-educativos",                 # completa uniendo las
                     "establecimientos_educativos.geojson",         # partes con "/"
                     sep = "/")) |>
  st_transform(5347) |>
  # Nos quedamos solo con escuelas de nivel primario común y de gestión estatal.
  # str_detect() busca el texto dentro de la columna "nen_mde" (niveles).
  filter(str_detect(nen_mde, "Nivel primario común") & ges == "Estatal") |>
  # Recortamos a la Comuna 15 (sin el buffer: solo las escuelas de la comuna).
  st_intersection(st_geometry(COMUNA15))

# --- 1.2 Cargar las PLAZAS ---
PLAZAS <- st_read(paste(url_base, "secretaria-de-desarrollo-urbano",
                        "espacios-verdes/espacio_verde_publico.geojson",
                        sep = "/")) |>
  st_transform(5347) |>                          # Mismo sistema de coordenadas que ESC
  filter(clasificac %in% c("PLAZA")) |>          # Solo espacios clasificados como "PLAZA"
  st_intersection(st_geometry(COMUNA15)) |>      # Recortamos a la Comuna 15
  # Las plazas son polígonos; para conectarlas a la red las representamos
  # con un único punto: su centroide (centro geométrico).
  st_centroid()

# --- 1.3 Incorporar escuelas y plazas a la red ("blend") ---
#  st_network_blend() "inserta" cada punto en la red: busca el tramo de
#  calle más cercano, lo corta en ese lugar y crea un NUEVO NODO allí.
#  Así, cada escuela y cada plaza queda representada exactamente sobre la red.
new_net <- net |>
  activate("nodes") |>
  # Alternativa: hacer el blend en dos pasos separados
  # st_network_blend(ESC) |>
  # st_network_blend(PLAZAS)
  # bind_rows() une escuelas y plazas en un solo objeto para hacerlo de una vez.
  # st_geometry() + st_as_sf() deja solo la geometría (no hace falta el resto).
  st_network_blend(st_as_sf(st_geometry(bind_rows(ESC, PLAZAS)))) |>
  # Al cortar las calles, los tramos nuevos heredan el "weight" del tramo
  # original. Lo recalculamos para que cada tramo tenga su largo real.
  activate("edges") |>
  mutate(weight = edge_length()) |>
  activate("nodes")

# --- 1.4 Matriz de costos (distancias por la red) ---
#  st_network_cost() calcula el costo del camino más corto entre cada par
#  origen-destino. El resultado es una MATRIZ:
#    - filas    = escuelas (from)
#    - columnas = plazas   (to)
#    - valor    = distancia en metros por la red
#  weights = "weight"  -> usa el largo de cada tramo como costo.
#  direction = "all"   -> ignora el sentido de circulación (análisis PEATONAL:
#                         una persona caminando puede ir "a contramano").
cost_matrix  <- st_network_cost(new_net,
                                from = ESC, to = PLAZAS,
                                weights = "weight", direction = "all")

# --- 1.5 Seleccionar las N plazas más cercanas a cada escuela ---
n <- 2   # Cantidad de plazas a buscar por escuela (podés cambiarlo)

#  Paso a paso de la línea siguiente:
#   a) apply(cost_matrix, 1, ...) recorre la matriz FILA POR FILA (cada escuela).
#   b) order(x) devuelve las posiciones de las plazas ordenadas de la más
#      cercana a la más lejana; [1:n] se queda con las n primeras.
#   c) as.numeric() convierte el resultado en un vector largo:
#      plazas de la escuela 1, luego las de la escuela 2, etc.
#   d) PLAZAS[ ... , ] selecciona esas plazas y st_coordinates() extrae
#      sus coordenadas X e Y.
closest <- PLAZAS[apply(cost_matrix, 1, function(x) order(x)[1:n]) |> as.numeric(), ] |>
  st_coordinates()

#  Armamos una tabla auxiliar para dibujar flechas escuela -> plaza.
#  rep(..., each = n) repite cada escuela n veces, para que quede alineada
#  con sus n plazas en el vector "closest".
aux_df <- data.frame(escuela = rep(ESC$fna, each = n),                  # nombre de la escuela
                     ori_x = rep(st_coordinates(ESC)[, 1], each = n),   # X origen (escuela)
                     ori_y = rep(st_coordinates(ESC)[, 2], each = n),   # Y origen (escuela)
                     des_x = closest[, 1],                              # X destino (plaza)
                     des_y = closest[, 2])                              # Y destino (plaza)

# --- 1.6 Mapa del resultado ---
ggplot() +
  geom_sf(data = st_as_sf(new_net, "edges"), color = "grey80") +   # Calles (aristas)
  geom_sf(data = st_as_sf(new_net, "nodes"), color = "grey80", size = 0.3) +  # Nodos
  geom_sf(data = COMUNA15, fill = NA, color = "grey40") +          # Límite de la comuna
  geom_sf(data = ESC, color = "tomato") +                          # Escuelas en rojo
  geom_sf(data = PLAZAS, color = "darkgreen", shape = 2) +         # Plazas: triángulos verdes
  # Flechas desde cada escuela hacia sus plazas más cercanas.
  # OJO: la flecha es recta solo para visualizar; la distancia usada
  # para elegir la plaza se midió por la red.
  geom_segment(data = aux_df, aes(x = ori_x, y = ori_y,
                                  xend = des_x, yend = des_y,
                                  color = escuela),                # Un color por escuela
               alpha = 0.6,
               arrow = arrow(length = unit(0.3, "cm"))) +
  guides(color = "none") +   # Ocultamos la leyenda (serían demasiadas escuelas)
  theme_void()               # Fondo limpio, sin ejes

# Limpiamos objetos temporales para no saturar la memoria ni confundirnos
rm(aux_df, closest, n, cost_matrix, new_net, ESC, PLAZAS, url_base)


# -----------------------------------------------------------------------------
# 2. OPTIMIZACIÓN DE RUTAS: PROBLEMA DEL VIAJANTE (TSP)
# -----------------------------------------------------------------------------
#  Pregunta: tengo que pasar por varios puntos y volver al inicio.
#  ¿En qué ORDEN los visito para que el recorrido total sea el más corto?
#
#  Es un problema clásico de optimización. Con pocos puntos se podrían probar
#  todas las combinaciones, pero crecen muy rápido (con 10 puntos hay más de
#  180.000 recorridos posibles). Por eso se usan algoritmos heurísticos, que
#  encuentran soluciones muy buenas en poco tiempo.
#
#  Estrategia en 3 pasos:
#   1) Matriz de distancias por la red entre todos los puntos.
#   2) Resolver el TSP con esa matriz -> obtenemos el ORDEN de visita.
#   3) Reconstruir el camino real por las calles entre cada par consecutivo.
# -----------------------------------------------------------------------------

# --- 2.1 Agregar un nuevo punto de interés ---
#  Creamos un punto a partir de coordenadas geográficas (lon/lat, WGS84).
new_POI <- data.frame(x = -58.47387347223715,                 # Longitud
                      y = -34.581407985725164,                # Latitud
                      poi = "Estación De Los Incas - Parque Chas",
                      tipo = "Dest") |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>   # Tabla -> objeto sf (EPSG:4326 = lon/lat)
  st_transform(st_crs(CALLE))                     # Lo pasamos al mismo CRS que las calles

#  Lo sumamos a los POI que ya teníamos
new_POI <- bind_rows(POI, new_POI)

#  Activamos los nodos de la red (las funciones siguientes trabajan sobre nodos)
net <- activate(net, "nodes")

# --- 2.2 Matriz de costos entre todos los puntos ---
#  Ahora "from" y "to" son los mismos puntos: obtenemos una matriz
#  cuadrada con la distancia por la red entre cada par de POI.
#  Usamos direction = "all" (recorrido a pie), así la matriz es simétrica.
cost_matrix <- st_network_cost(net, from = new_POI, to = new_POI,
                               weights = "weight", direction = "all")

#  st_nearest_feature() busca, para cada POI, el NODO de la red más cercano
#  y devuelve su número (índice). Lo necesitamos para después trazar caminos.
rdm_idxs <- st_nearest_feature(new_POI, net)

#  Ponemos esos índices como nombres de filas y columnas de la matriz.
#  Así, cuando el TSP nos devuelva el orden, sabremos a qué nodo
#  corresponde cada parada.
row.names(cost_matrix) <- rdm_idxs
colnames(cost_matrix)  <- rdm_idxs

cost_matrix   # Inspeccionamos la matriz en consola

# --- 2.3 Resolver el TSP ---
#  units::drop_units() quita las unidades (metros) porque el paquete TSP
#  necesita números "puros". TSP() crea el problema y solve_TSP() lo resuelve.
#  set.seed() fija el azar: algunos métodos arrancan desde un punto aleatorio,
#  y así todos obtenemos el mismo resultado.
set.seed(15)
tour <- solve_TSP(TSP(units::drop_units(cost_matrix)))
tour   # Muestra el orden de visita y la longitud total del recorrido

#  Los nombres del resultado son los índices de nodo, ya ordenados.
#  (El paréntesis exterior hace que el resultado se imprima al asignarse.)
(tour_idxs <- as.numeric(names(tour)))

# --- 2.4 Reconstruir el recorrido por las calles ---
#  Necesitamos el camino de cada parada a la siguiente:
#    parada 1 -> 2, 2 -> 3, ..., última -> 1 (volvemos al inicio).
#  - tour_idxs[1:length(tour_idxs)]            = los "desde"
#  - tour_idxs[c(2:length(tour_idxs), 1)]      = los "hasta" (corridos un lugar,
#                                                el último vuelve al primero)
#  map2_df() aplica la función a cada par (from, to) y une los resultados.
#  st_network_paths() devuelve los nodos y aristas del camino más corto.
#  mode = "all": igual que en la matriz, ignoramos el sentido de las calles.
tour_path <- map2_df(tour_idxs[1:(length(tour_idxs))],
                     tour_idxs[c(2:length(tour_idxs), 1)],
                     \(from, to) {          # \(...) es la forma corta de function(...)
                       st_network_paths(net, from = from, to = to,
                                        weights = "weight", mode = "all")
                     })

#  Convertimos los índices de NODOS de cada tramo en geometrías.
#  slice() selecciona los nodos por posición; summarise() los une en
#  una sola geometría por tramo.
a_nodes <- map_df(tour_path[["node_paths"]],
                  \(nodes){
                    net |>
                      activate("nodes") |>
                      slice(nodes |> unlist()) |>
                      st_as_sf() |>
                      summarise()
                  })

#  Lo mismo con las ARISTAS (los tramos de calle recorridos).
a_edges <- map_df(tour_path[["edge_paths"]],
                  \(edges){
                    net |>
                      activate("edges") |>
                      slice(edges |> unlist()) |>
                      st_as_sf() |>
                      summarise()
                  })

# --- 2.5 Mapa del recorrido óptimo ---
ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), color = "grey") +   # Red completa de fondo
  geom_sf(data = st_as_sf(net, "nodes"), color = "grey", size = 0.3) +
  geom_sf(data = a_edges, linewidth = 1) +                   # Calles del recorrido (negro)
  geom_sf(data = a_nodes) +                                  # Nodos del recorrido
  geom_sf(data = st_as_sf(net, "nodes")[tour_idxs, ],        # Paradas (nodos de los POI)
          color = "blue", shape = 20) +
  geom_sf(data = st_as_sf(net, "nodes")[tour_idxs[1], ],     # Punto de partida: estrella
          color = "blue", shape = 8, size = 5) +
  geom_sf(data = new_POI, color = "tomato", shape = 7) +     # Ubicación original de los POI
  theme_void()

rm(a_edges, a_nodes, cost_matrix, new_POI, rdm_idxs, tour, tour_idxs, tour_path)


# -----------------------------------------------------------------------------
# 3. ISOCRONAS
# -----------------------------------------------------------------------------
#  Una ISOCRONA es el área a la que se puede llegar desde un origen en un
#  tiempo dado (ej.: "todo lo que está a 5 minutos de la facultad").
#
#  Hasta ahora el costo era la DISTANCIA (metros). Para hablar de TIEMPO
#  necesitamos asignar una VELOCIDAD a cada calle:  tiempo = distancia / velocidad
#
#  Acá pensamos en un VEHÍCULO: por eso SÍ respetamos el sentido de las calles
#  (la red es dirigida y node_distance_from() sigue solo las aristas "de salida").
# -----------------------------------------------------------------------------

# --- 3.1 Calcular el tiempo de recorrido de cada tramo ---
net <- activate(net, "edges") |>      # Trabajamos sobre las aristas (calles)
  mutate(
    # Velocidad supuesta según el tipo de calle (en km/h)
    speed = case_when(tipo_c == "AVENIDA" ~ 60,
                      tipo_c == "CALLE"   ~ 40,
                      TRUE                ~ 20),   # Cualquier otro caso (pasajes, etc.)
    # El paquete "units" nos permite trabajar con unidades y convertirlas
    # automáticamente, evitando errores de cálculo.
    speed  = units::set_units(speed, "km/h"),     # Le asignamos la unidad km/h
    speed  = units::set_units(speed, "m/s"),      # La convertimos a metros por segundo
    weight = edge_length(),                       # Largo del tramo, en metros
    # Tiempo = distancia / velocidad -> resultado en segundos (m / (m/s))
    time   = weight / speed,
    time   = units::set_units(time, "minutes"),   # Lo pasamos a minutos
    # igraph necesita números "puros": guardamos el tiempo sin unidades
    time   = units::drop_units(time)
  ) |>
  activate("nodes")                    # Volvemos a dejar activos los nodos

# --- 3.2 Definir el punto de origen ---
ori      <- st_geometry(POI[1, ])          # Primer POI: Facultad de Agronomía
ori_node <- st_nearest_feature(ori, net)   # Nodo de la red más cercano al origen

# --- 3.3 Tiempo desde el origen hasta cada nodo ---
iso <- net |>
  mutate(
    # node_distance_from() calcula, para CADA nodo, el costo del camino más
    # corto desde el nodo de origen. Como usamos weights = time, el
    # resultado es el TIEMPO de viaje en minutos.
    DIS_FAC_AGRO = node_distance_from(ori_node, weights = time),
    # cut() clasifica ese tiempo continuo en rangos (categorías):
    #   [0,1] -> "1 min" | (1,3] -> "3 min" | (3,6] -> "6 min" | (6,10] -> ">6 min"
    # include.lowest = TRUE hace que el propio origen (tiempo 0) entre en
    # "1 min". Los nodos a más de 10 minutos quedan como NA.
    iso = cut(DIS_FAC_AGRO, breaks = c(0, 1, 3, 6, 10),
              labels = c("1 min", "3 min", "6 min", ">6 min"),
              include.lowest = TRUE)
  )
# filter(DIS_FAC_AGRO <= 1)   # (alternativa: quedarse solo con lo alcanzable en 1 min)

# --- 3.4 Polígono de la isocrona de 1 minuto ---
#  Tomamos los nodos alcanzables en 1 minuto o menos y dibujamos la
#  "envolvente convexa" (convex hull): el polígono más chico que los contiene,
#  como si estiráramos una banda elástica alrededor de ellos.
iso_poly <- iso |>
  filter(DIS_FAC_AGRO <= 1) |>   # Nodos a 1 minuto o menos
  st_geometry() |>               # Solo la geometría
  st_combine() |>                # Unimos todos los puntos en un MULTIPOINT
  st_convex_hull()               # Calculamos la envolvente convexa

# --- 3.5 Mapa: isocrona de 1 minuto ---
ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), color = "grey") +       # Red de fondo
  geom_sf(data = st_as_sf(net, "nodes"), color = "grey", size = 0.3) +
  # Calles alcanzables: al filtrar los nodos, tidygraph elimina también las
  # aristas que pierden alguno de sus extremos. Quedan las calles "internas".
  geom_sf(data = iso |>
            activate("nodes") |>
            filter(DIS_FAC_AGRO <= 1) |>
            st_as_sf("edges"),
          color = "darkgreen") +
  geom_sf(data = st_as_sf(iso, "nodes") |> filter(DIS_FAC_AGRO <= 1),   # Nodos alcanzables
          color = "darkgreen") +
  geom_sf(data = iso_poly, color = "darkblue", fill = NA, linewidth = 1) + # Polígono isocrona
  geom_sf(data = ori, color = "tomato", shape = 8, size = 5) +          # Origen (estrella)
  labs(title = "Isocrona de 1 minuto") +
  theme_void()

# --- 3.6 Mapa: todas las isocronas por categoría ---
ggplot() +
  geom_sf(data = st_as_sf(net, "edges"), color = "grey") +
  geom_sf(data = st_as_sf(net, "nodes"), color = "grey", size = 0.3) +
  # Cada nodo se colorea según su rango de tiempo (columna "iso").
  # Dibujamos solo los que tienen categoría (los NA quedan en gris).
  geom_sf(data = st_as_sf(iso, "nodes") |> filter(!is.na(iso)), aes(color = iso)) +
  geom_sf(data = ori, color = "tomato", shape = 8, size = 5) +
  # Paleta viridis: ordenada y legible para personas con daltonismo
  scale_color_viridis_d("Distancia a la\nFacultad de\nAgronomía") +
  labs(title = "Isocrona de Facultad de Agronomia") +
  theme_void()

# =============================================================================
#  PARA PENSAR / EJERCICIOS
# =============================================================================
#  1. Cambiá n <- 2 por n <- 3. ¿Cambian mucho las plazas asignadas?
#  2. Compará la distancia por red con la euclidiana (st_distance) para
#     escuelas y plazas. ¿Dónde difieren más? ¿Por qué?
#  3. En el TSP, agregá dos POI más. ¿Cambia el orden de todo el recorrido?
#  4. Modificá las velocidades por tipo de calle (ej.: 5 km/h para todas,
#     simulando una caminata). ¿Cómo cambian las isocronas?
#  5. Calculá las isocronas con mode = "all" en node_distance_from().
#     ¿Qué cambia al ignorar el sentido de las calles?
#  6. Cambiá el buffer de 500 m por 0 m. ¿Cambian las distancias de las
#     escuelas que están cerca del borde de la comuna?
#  7. ¿Qué limitaciones tiene usar el convex hull para representar una
#     isocrona? ¿Qué alternativa propondrías? (pista: st_concave_hull)
# =============================================================================