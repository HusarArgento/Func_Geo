# Guía de `tmap` en R
### Material de cátedra — Tecnicatura Universitaria en Ciencia de Datos Espaciales
### Elaborado a partir de la documentación oficial del paquete (r-tmap.github.io, CRAN, versión 4.4-1)

---

## 0. Cómo usar esta guía

Esta guía está pensada para leerse **con RStudio abierto al lado**. No alcanza con leerla: cada sección tiene código para copiar, ejecutar, romper y volver a arreglar. Ese "romper y arreglar" es, de hecho, la parte más importante del aprendizaje: `tmap` da mensajes de error y de aviso muy informativos, y aprender a leerlos es una habilidad tan valiosa como aprender la sintaxis.

La guía sigue una lógica de **capas que se van agregando con `+`**, igual que en `ggplot2`. Si ya conocen `ggplot2`, gran parte de la lógica mental ya la tienen incorporada; lo que cambia es el vocabulario (hablamos de "shapes" y "aesthetics/variables visuales" en vez de "data" y "aes").

**Aviso importante sobre versiones.** En 2024–2025 `tmap` pasó de la versión 3 a la versión 4, y el cambio de sintaxis fue grande (no es un simple parche). Muchísimos tutoriales, foros de Stack Overflow y videos de YouTube todavía muestran sintaxis de la versión 3. Esta guía enseña **la sintaxis de tmap v4** (la versión estable actual, 4.4-1), que es la que se instala por defecto hoy desde CRAN, pero en la Sección 10 van a encontrar una tabla comparativa v3 → v4 para que puedan leer material viejo sin confundirse.

---

## 1. ¿Qué es `tmap` y por qué lo usamos?

`tmap` es un paquete de R para crear **mapas temáticos**: mapas que no solo muestran "dónde está algo" sino que representan visualmente una variable (población, PBI, exportaciones, uso del suelo, etc.) sobre un territorio.

Según la documentación oficial, el paquete ofrece *"un enfoque flexible, basado en capas y fácil de usar para crear mapas temáticos, como coropletas y mapas de burbujas"*, y su diseño está inspirado en la misma filosofía de "gramática de gráficos por capas" que usa `ggplot2`.

Dos ideas para retener desde el primer día:

1. **`tmap` no dibuja los datos: dibuja objetos espaciales.** Antes de mapear algo con `tmap` ese algo tiene que ser un objeto espacial válido en R —típicamente de clase `sf` (simple features), aunque también soporta `stars` (rásters) y, cada vez más, objetos de `terra` (`SpatVector`, `SpatRaster`). Si vienen del mundo QGIS, piensen en un objeto `sf` como el equivalente de una capa vectorial cargada en la Tabla de Contenidos.
2. **La sintaxis es aditiva.** Se arma un mapa sumando piezas con el operador `+`: primero se declara el objeto espacial (`tm_shape()`), después una o más capas de dibujo (`tm_polygons()`, `tm_lines()`, `tm_dots()`, etc.), y opcionalmente elementos de diseño (título, leyenda, brújula, escala gráfica).

```r
library(tmap)
library(sf)

# Estructura mínima de cualquier mapa en tmap:
tm_shape(objeto_espacial) +
  tm_polygons()
```

Esto ya es, técnicamente, un mapa completo y funcional.

---

## 2. Instalación y puesta en marcha

### 2.1 Instalar

Desde CRAN (versión estable):

```r
install.packages("tmap")
```

La documentación oficial también permite instalar la versión de desarrollo desde GitHub, algo útil si necesitan una función que todavía no llegó a CRAN:

```r
# install.packages("remotes")
remotes::install_github("r-tmap/tmap")

# alternativa vía pak
# install.packages("pak")
pak::pak("r-tmap/tmap")
```

`tmap` depende de otros paquetes espaciales del ecosistema R (principalmente `sf`), que se instalan automáticamente. En Windows las librerías del sistema (GDAL, PROJ, GEOS, s2) vienen empaquetadas; en Linux/Mac a veces hay que instalarlas aparte a nivel de sistema operativo antes de que `install.packages("sf")` funcione.

### 2.2 Cargar el paquete

```r
library(tmap)
```

Si tenían instalada una versión vieja (v3) y actualizan a v4, verán un mensaje del estilo:

```
## Breaking News: tmap 3.x is retiring. Please test v4...
```

Eso es normal y esperable: es el propio paquete avisando del cambio de versión mayor.

### 2.3 El dataset `World`: nuestro "Hola, mundo"

`tmap` incluye datasets de ejemplo listos para practicar. El más usado en la documentación oficial es `World`, un objeto `sf` con un polígono por país y columnas como `iso_a3`, `name`, `continent`, `pop_est`, `economy`, `income_grp`, `life_exp`, `HPI` (Happy Planet Index), entre otras.

```r
data("World")
names(World)
class(World)   # debería mostrar "sf" y "data.frame"
```

Noten algo clave: **un objeto `sf` es, ante todo, un `data.frame`** con una columna especial llamada `geometry`. Todo lo que ya saben de `dplyr` (`filter()`, `mutate()`, `select()`) sigue funcionando sobre estos objetos.

---

## 3. El primer mapa: `tm_shape()` + una capa

La función `tm_shape()` no dibuja nada por sí sola: solo **declara qué objeto espacial vamos a usar**. Es el equivalente al `ggplot(data = ...)` de `ggplot2`. Recién cuando le sumamos una capa de dibujo aparece algo en pantalla.

```r
tm_shape(World) +
  tm_polygons("HPI")
```

Esto crea una coropleta mundial coloreando cada país según su Happy Planet Index. Fíjense la lógica: **shape primero, capa(s) después, todo unido con `+`**.

### 3.1 Las funciones de capa más comunes

| Función | Qué dibuja | Tipo de geometría típica |
|---|---|---|
| `tm_polygons()` | Polígonos con relleno **y** borde | polígonos (países, provincias, departamentos) |
| `tm_fill()` | Solo el relleno de los polígonos, sin borde | polígonos |
| `tm_borders()` | Solo los bordes/contornos, sin relleno | polígonos |
| `tm_lines()` | Líneas (rutas, ríos, límites) | líneas |
| `tm_dots()` | Puntos pequeños | puntos |
| `tm_bubbles()` / `tm_symbols()` | Símbolos cuyo tamaño y/o color varían según una variable | puntos |
| `tm_squares()` / `tm_markers()` | Variantes de símbolos puntuales | puntos |
| `tm_raster()` | Datos ráster (grillas continuas: elevación, temperatura, NDVI) | raster |
| `tm_text()` / `tm_labels()` | Etiquetas de texto | cualquiera |
| `tm_graticules()` | Grilla de coordenadas (paralelos/meridianos) | — |

Un dato pedagógico importante: `tm_polygons()` es en realidad una combinación conveniente de `tm_fill()` + `tm_borders()`. Prueben esto para verlo con sus propios ojos:

```r
tm_shape(World) + tm_fill()       # solo relleno gris
tm_shape(World) + tm_borders()    # solo contornos
tm_shape(World) + tm_polygons()   # ambos combinados
```

### 3.2 Combinar varias capas y varios "shapes"

Se pueden apilar tantas capas como se quiera, e incluso combinar **distintos objetos espaciales** en un mismo mapa, cada uno con su propio `tm_shape()`:

```r
data(World, metro, rivers, land)

tm_shape(land) +
  tm_raster("elevation", palette = terrain.colors(10)) +
tm_shape(World) +
  tm_borders("white", lwd = .5) +
  tm_text("iso_a3", size = "AREA") +
tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020", scale = .5) +
  tm_legend(show = FALSE)
```

Este ejemplo (tomado de la documentación oficial "tmap: get started!") combina en un solo mapa: un ráster de elevación de fondo, los países del mundo con sus bordes y su código ISO como etiqueta, y las ciudades metropolitanas como símbolos rojos escalados por población. Es exactamente la lógica que van a usar en su proyecto Mercosur: un `tm_shape()` para los países, otro para flujos comerciales, otro para puntos de interés, etc.

**Ejercicio 1.** Carguen `World`, filtren solo los países de `continent == "South America"` y hagan un `tm_polygons()` con relleno constante celeste y borde gris oscuro.

---

## 4. Variables visuales: el corazón de tmap v4

Esta es, probablemente, la sección conceptual más importante de toda la guía, porque acá es donde vive el cambio grande entre v3 y v4.

### 4.1 La idea de "variable visual" (visual variable)

Cada capa de dibujo tiene un conjunto de **variables visuales** (fill, col, size, lwd, lty, shape...) que se pueden asignar de dos maneras distintas:

- **Valor constante**: todos los objetos se dibujan igual. Ejemplo: `fill = "red"`, `lwd = 2`.
- **Variable de datos**: el valor de una columna del objeto espacial determina el color/tamaño/forma de cada elemento. Ejemplo: `fill = "HPI"`.

```r
data(World)
s <- tm_shape(World)  # lo guardamos para reutilizarlo

# (a) valores constantes
s + tm_polygons(
  fill = "#ffce00",   # color de relleno
  col  = "black",     # color de línea/borde
  lwd  = 0.5,          # ancho de línea
  lty  = "dashed")     # tipo de línea

# (b) variable de datos: cada país se colorea según su income_grp
s + tm_polygons(fill = "income_grp")
```

Esta distinción —constante vs. variable de datos— es la misma que en `ggplot2` entre poner un argumento **fuera** de `aes()` (constante) o **dentro** de `aes()` (mapeado a datos), aunque en `tmap` v4 no hay un `aes()` explícito: el propio nombre del argumento (`fill`, `col`, `size`...) hace de mapeo directo.

### 4.2 Variables visuales por tipo de capa

| Capa | Variables visuales principales |
|---|---|
| `tm_polygons()` | `fill`, `col` (borde), `fill_alpha`, `col_alpha`, `lwd`, `lty` |
| `tm_symbols()` / `tm_bubbles()` / `tm_dots()` | `size`, `fill`, `col`, `shape`, `fill_alpha` |
| `tm_lines()` | `col`, `lwd`, `lty`, `col_alpha` |
| `tm_raster()` | `col` (o el nombre de la banda) |
| `tm_text()` | `size`, `col`, `fontface` |

Se pueden combinar varias variables visuales de datos **distintas** en una misma capa, algo que en v3 no era posible del mismo modo:

```r
s + tm_symbols(
  size  = "pop_est",     # tamaño según población
  fill  = "well_being",  # color según bienestar
  shape = "income_grp")  # forma del símbolo según grupo de ingreso
```

### 4.3 Cada variable visual tiene su propia escala y su propia leyenda

Acá está la clave conceptual de v4: **por cada variable visual de datos existen dos argumentos hermanos**, con el sufijo `.scale` y `.legend`:

```r
s + tm_polygons(
  fill        = "HPI",
  fill.scale  = tm_scale_intervals(values = "purple_green"),
  fill.legend = tm_legend(title = "Happy Planet Index")
)
```

Es decir: `fill` dice **qué** variable mapear; `fill.scale` dice **cómo** transformar esos valores en colores/tamaños; `fill.legend` dice **cómo mostrar** la leyenda correspondiente. Si usan `lwd` como variable visual, existirán `lwd.scale` y `lwd.legend`; si usan `size`, existirán `size.scale` y `size.legend`; y así con cualquier variable visual.

Esto reemplaza a los argumentos sueltos que existían en v3 (`palette`, `style`, `breaks`, `title`, todos mezclados dentro de `tm_polygons()`), y es justamente lo que verán detallado en la Sección 10.

---

## 5. Escalas: `tm_scale_*()`

Una **escala** define la regla de transformación entre el valor de una columna de datos y el valor visual (color, tamaño, forma) que efectivamente se dibuja. `tmap` ofrece varias familias de escalas, cada una pensada para un tipo de dato distinto.

### 5.1 `tm_scale_intervals()` — para datos numéricos continuos agrupados en clases (coropletas clásicas)

Es la escala más usada para coropletas: divide el rango numérico en intervalos (clases) y asigna un color por clase.

```r
s + tm_polygons(
  fill = "HPI",
  fill.scale = tm_scale_intervals(
    style    = "fisher",     # método de clasificación
    n        = 7,            # cantidad de clases
    values   = "pu_gn_div"   # paleta de color
  )
)
```

El argumento `style` acepta, entre otros, los métodos clásicos de clasificación cartográfica que probablemente ya conocen de QGIS: `"pretty"`, `"equal"`, `"quantile"`, `"jenks"` (rupturas naturales), `"fisher"`, `"kmeans"`, `"sd"` (desvíos estándar), y `"fixed"` para definir los cortes manualmente:

```r
s + tm_polygons(
  fill = "HPI",
  fill.scale = tm_scale_intervals(
    n      = 6,
    style  = "fixed",
    breaks = c(0, 10, 20, 30, 40, 50, 60),  # se necesitan n+1 cortes
    values = "pu_gn_div"
  )
)
```

### 5.2 `tm_scale_continuous()` — gradiente continuo, sin clases

Para cuando **no** queremos agrupar en clases discretas sino usar un degradé continuo de color:

```r
s + tm_polygons(
  fill = "HPI",
  fill.scale = tm_scale_continuous(
    limits = c(10, 60),
    values = "scico.hawaii"
  )
)
```

### 5.3 `tm_scale_categorical()` — para variables categóricas sin orden

Ideal para variables cualitativas como el tipo de economía o el uso del suelo:

```r
s + tm_polygons(fill = "economy", fill.scale = tm_scale_categorical())
```

### 5.4 `tm_scale_ordinal()` — para variables categóricas **con** orden

Cuando las categorías tienen un orden lógico (por ejemplo, niveles de ingreso: bajo/medio/alto):

```r
s + tm_polygons(
  fill = "income_grp",
  fill.scale = tm_scale_ordinal(values = "matplotlib.summer")
)
```

### 5.5 Sobre las paletas de color (`values`)

En v4 el argumento que fija la paleta de colores se llama `values` (en v3 se llamaba `palette`), y hay una oferta mucho más amplia de paletas provenientes del paquete `cols4all`. La propia documentación recomienda explorar visualmente las opciones disponibles con:

```r
cols4all::c4a_gui()
```

Esto abre una interfaz interactiva donde pueden probar paletas y copiar el nombre exacto que después usan en `values = "..."`.

**Ejercicio 2.** Tomen la variable `pop_est_dens` (densidad de población) de `World` y prueben tres escalas distintas (`tm_scale_intervals` con `style = "quantile"`, `tm_scale_intervals` con `style = "jenks"`, y `tm_scale_continuous`). Comparen visualmente cómo cambia la lectura del mapa según la escala elegida. Esta comparación es, de hecho, una discusión metodológica clásica en cartografía temática: la elección del método de clasificación **no es neutral** y puede cambiar drásticamente el mensaje visual de un mapa.

---

## 6. Leyendas: `tm_legend()`

El argumento `.legend` de cada variable visual recibe un objeto construido con `tm_legend()`, donde se configuran título, orientación, marco, tamaño, etc.

```r
tm_shape(World, crs = "+proj=robin") +
  tm_polygons(
    fill = "HPI",
    fill.scale = tm_scale_continuous(values = "matplotlib.rd_yl_bu"),
    fill.legend = tm_legend(
      title       = "Happy Planet Index",
      orientation = "landscape",
      frame       = FALSE
    )
  )
```

Algunos argumentos útiles de `tm_legend()`:

- `title`: el texto del título de la leyenda.
- `orientation`: `"portrait"` (vertical, por defecto) o `"landscape"` (horizontal).
- `frame`: si dibuja o no un marco alrededor de la leyenda.
- `item.height` / `item.width`: tamaño de cada ítem de la leyenda.
- `show = FALSE` (dentro de `tm_legend()` sin variable asociada, ver más abajo): oculta la leyenda completamente.

Para ocultar **todas** las leyendas de un mapa se usa `tm_legend(show = FALSE)` como una capa independiente:

```r
tm_shape(metro) +
  tm_symbols(col = "red", size = "pop2020") +
  tm_legend(show = FALSE)
```

---

## 7. Diseño general del mapa: `tm_layout()` y componentes cartográficos

Un mapa profesional necesita más que polígonos de colores: título, escala gráfica, brújula (norte), créditos/fuente, y un layout prolijo. `tmap` separa estos elementos en funciones específicas que se suman como capas.

### 7.1 `tm_layout()`

Controla el aspecto general: colores de fondo, márgenes, tamaños de fuente, si se muestra el marco, etc.

```r
tm_shape(World) +
  tm_polygons("HPI") +
  tm_layout(
    bg.color = "lightblue",
    inner.margins = c(0.1, 0.1, 0.1, 0.1),
    frame = FALSE
  )
```

### 7.2 Componentes típicos de un mapa completo

```r
tm_shape(World) +
  tm_polygons("HPI", fill.scale = tm_scale_intervals(values = "pu_gn")) +
  tm_title("Happy Planet Index por país") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_credits("Fuente: World dataset, paquete tmap", position = c("left", "bottom"))
```

- `tm_title()`: título del mapa.
- `tm_compass()`: brújula/flecha de norte.
- `tm_scalebar()`: escala gráfica (barra de escala).
- `tm_credits()`: texto de créditos o fuente de datos (¡fundamental en cualquier entrega académica o profesional!).
- `tm_logo()`: para insertar un logo institucional.
- `tm_minimap()`: un mapa de referencia pequeño (modo interactivo).

### 7.3 Posicionamiento de componentes

La posición de estos elementos se define con `tm_pos_in()` / `tm_pos_out()`, o de forma abreviada con combinaciones de palabras clave: horizontal (`"left"`, `"center"`, `"right"`) y vertical (`"top"`, `"center"`, `"bottom"`).

```r
tm_compass(position = c("right", "top"))
```

### 7.4 Unidades de layout: todo se mide en líneas de texto

Un detalle útil que aparece en la documentación oficial (vignette "foundations: units"): el ancho y alto de componentes como la leyenda, la brújula o los títulos se expresan en **alturas de línea de texto**, de modo que todo el layout escala naturalmente si cambian el tamaño de fuente general. Esto se controla con el argumento `scale` de `tmap_options()` o `tm_layout()`, que multiplica de una sola vez todos los tamaños (símbolos, anchos de línea, fuentes):

```r
tmap_options(scale = 0.75)
```

Es muy útil cuando exportan el mismo mapa a distintos tamaños (por ejemplo, una miniatura para un dashboard de Power BI versus una lámina A3 para imprimir).

---

## 8. Proyecciones cartográficas (CRS)

`tm_shape()` acepta un argumento `crs` para reproyectar "al vuelo" sin modificar el objeto `sf` original:

```r
tm_shape(World, crs = "+proj=robin") +   # proyección de Robinson
  tm_polygons("HPI")

tm_shape(World, crs = "+proj=eck4") +    # proyección de Eckert IV
  tm_polygons("HPI")
```

Para un país o región específica (como haría cualquiera de ustedes con Mercosur), lo recomendable es usar un CRS proyectado adecuado a la zona en lugar de coordenadas geográficas sin proyectar, exactamente el mismo criterio que aplicarían en QGIS. Pueden pasar tanto un string PROJ4 como un objeto `crs` de `sf::st_crs()` o un código EPSG:

```r
tm_shape(paises_mercosur, crs = 5346) +  # ejemplo: Argentina POSGAR / Gauss-Krüger
  tm_polygons("exportaciones_china")
```

---

## 9. Facetas: varios mapas pequeños en una sola instrucción

Las facetas (*small multiples*) permiten repetir el mismo mapa una vez por cada categoría de una variable —el equivalente espacial de `facet_wrap()` en `ggplot2`. Esto es exactamente lo que van a necesitar para comparar, por ejemplo, el mismo mapa de flujos comerciales en distintos años, o un panel por país del Mercosur.

### 9.1 Faceta automática por múltiples variables

Si le pasan **un vector de nombres de columnas** a una variable visual, `tmap` genera un panel con un mapa por cada columna:

```r
tm_shape(World) +
  tm_polygons(fill = c("well_being", "life_exp"))
```

### 9.2 Faceta explícita con `tm_facets_wrap()` / `tm_facets_grid()`

Cuando la faceta depende de una variable categórica dentro de los mismos datos (por ejemplo, un panel por continente, o por año si tienen los datos en formato largo):

```r
data(NLD_muni)
NLD_muni$perc_men <- NLD_muni$pop_men / NLD_muni$population * 100

tm_shape(NLD_muni) +
  tm_polygons("perc_men", fill.scale = tm_scale_intervals(values = "brewer.rd_yl_bu")) +
  tm_facets_wrap(by = "province")
```

Para su proyecto Mercosur, esta es probablemente la herramienta más directa para mostrar, por ejemplo, un panel 2015 / 2019 / 2023 de participación de China vs. EE. UU. en el comercio de cada país miembro.

### 9.3 Sincronizar facetas en modo interactivo

En modo `"view"` (ver Sección 11), se puede sincronizar el zoom/paneo entre varios mapas pequeños con `sync = TRUE`:

```r
tmap_mode("view")
tm_shape(World) +
  tm_polygons(c("HPI", "economy")) +
  tm_facets(sync = TRUE, ncol = 2)
```

---

## 10. Los dos modos de tmap: estático (`"plot"`) e interactivo (`"view"`)

Una de las características distintivas de `tmap` frente a otros paquetes de mapeo en R es que **el mismo código produce, indistintamente, un mapa estático o uno interactivo**, según el modo activo. Esto se controla globalmente con `tmap_mode()`.

```r
tmap_mode("plot")   # mapas estáticos (motor: grid graphics) — ideal para imprimir/exportar a PNG, PDF
tmap_mode("view")   # mapas interactivos (motor: leaflet) — ideal para explorar y para HTML/Shiny
```

En modo `"view"` pueden hacer zoom, paneo, hover con popups de información, y elegir mapas base (OpenStreetMap, satelital, etc.) con `tm_basemap()`. Es el modo natural para incorporar en un dashboard Shiny.

```r
tmap_mode("view")
tm_shape(World) +
  tm_polygons("HPI")
```

Para volver al modo estático simplemente se llama de nuevo a `tmap_mode("plot")`. El modo queda activo para **toda la sesión de R**, no solo para el próximo mapa, así que es buena práctica dejarlo explícito al principio del script.

---

## 11. Guardar y exportar mapas

### 11.1 Guardar un mapa como objeto de R

Los mapas de `tmap` se pueden asignar a un objeto, lo cual permite construirlos por partes, reutilizarlos o combinarlos después:

```r
mapa_base <- tm_shape(World) + tm_polygons("HPI")
class(mapa_base)   # "tmap"

# se le pueden seguir agregando capas más tarde
mapa_completo <- mapa_base + tm_shape(rivers) + tm_lines(col = "blue")
```

### 11.2 Exportar a archivo

```r
tmap_save(mapa_completo, filename = "mapa_hpi.png", width = 2000, height = 1500, dpi = 300)
tmap_save(mapa_completo, filename = "mapa_hpi.html")   # exportación interactiva si el modo es "view"
```

### 11.3 Combinar varios mapas en un panel con `tmap_arrange()`

A diferencia de las facetas (que dividen **un mismo** mapa según una variable), `tmap_arrange()` combina mapas **distintos**, ya generados por separado, en una sola grilla:

```r
tm1 <- tm_shape(NLD_muni) + tm_polygons("population")
tm2 <- tm_shape(NLD_muni) + tm_bubbles(size = "population")

tmap_arrange(tm1, tm2)
```

Esto es especialmente útil cuando quieren mostrar, lado a lado, dos representaciones distintas de la misma variable (por ejemplo, coropleta vs. símbolos proporcionales), algo muy pedagógico para una presentación o un informe.

---

## 12. De la versión 3 a la versión 4: tabla de equivalencias

Como en Internet conviven materiales de ambas versiones, esta tabla —elaborada a partir del anuncio oficial de migración— les va a servir de "diccionario" cuando encuentren código viejo.

| Concepto | Sintaxis tmap v3 | Sintaxis tmap v4 |
|---|---|---|
| Color de relleno con paleta | `tm_polygons("HPI", palette = "PRGn", style = "jenks")` | `tm_polygons(fill = "HPI", fill.scale = tm_scale_intervals(values = "purple_green", style = "jenks"))` |
| Título de leyenda | `tm_polygons("HPI", title = "Índice")` | `tm_polygons(fill = "HPI", fill.legend = tm_legend(title = "Índice"))` |
| Nombre de la paleta | `palette = "..."` | `values = "..."` (dentro de `*.scale`) |
| Configuración de leyenda general | `tm_layout(legend.title.size = 0.8)` | `tm_legend(title.size = 0.8)` como argumento de `fill.legend`/`*.legend` |
| Tamaño de símbolo | `tm_bubbles(size = "pop", scale = 0.5)` | `tm_symbols(size = "pop", size.scale = tm_scale_continuous(values.scale = 0.5))` (aprox.) |

Un detalle tranquilizador de la documentación oficial: **el código escrito en sintaxis v3 sigue funcionando en v4** (retrocompatibilidad), y cuando lo detecta, `tmap` imprime en la consola un mensaje explicativo del tipo:

```
── tmap v3 code detected ──────────────────────────────
[v3->v4] `tm_polygons()`: instead of `style = "jenks"`,
use fill.scale = `tm_scale_intervals()`.
```

Les recomiendo, como ejercicio de clase, tomar deliberadamente un fragmento de código v3 encontrado en un tutorial viejo y dejar que `tmap` les muestre el "traductor" automático en la consola: es una forma muy directa de internalizar el cambio.

---

## 13. Ejemplo integrador aplicado (orientado a un proyecto de geoeconomía regional)

Vamos a armar, paso a paso, un mapa coropleta de una región (pensado como plantilla general, adaptable a un análisis de comercio exterior de los países del Mercosur) que integra todo lo visto:

```r
library(tmap)
library(sf)
library(dplyr)

# 1. Supongamos un objeto sf con los países de la región
#    y una columna con, por ejemplo, participación de China en el comercio total (%)
# paises_mercosur <- st_read("data/raw/mercosur.gpkg")

tmap_mode("plot")

mapa_comercio <- tm_shape(paises_mercosur, crs = "+proj=eqearth") +
  tm_polygons(
    fill        = "participacion_china_pct",
    fill.scale  = tm_scale_intervals(
                    style  = "jenks",
                    n      = 5,
                    values = "brewer.blues"),
    fill.legend = tm_legend(title = "Participación de China\nen el comercio total (%)")
  ) +
  tm_text("iso_a3", size = 0.7) +
  tm_title("Reconfiguración geoeconómica del Mercosur (2023)") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom")) +
  tm_credits("Fuente: UN Comtrade | Elaboración propia", position = c("left", "bottom")) +
  tm_layout(frame = FALSE)

mapa_comercio

tmap_save(mapa_comercio, "outputs/mapa_comercio_mercosur.png", dpi = 300)
```

Noten cómo cada pieza de la guía aparece reflejada acá: `tm_shape()` con reproyección, una variable visual de datos (`fill`) con su escala y su leyenda explícitas, una capa de texto adicional, y los componentes cartográficos de rigor (título, brújula, escala, fuente). Esta misma estructura, con pequeñas variantes, les va a servir tanto para el análisis de comercio como para el futuro proyecto de pesca ilegal en el Atlántico Sur.

**Ejercicio 3 (integrador).** A partir de su extracción de UN Comtrade ya guardada en `data/raw/`, generen un panel de tres mapas (2015, 2019, 2023) usando `tm_facets_wrap()`, mostrando la participación relativa de China vs. Estados Unidos en las exportaciones de cada país del Mercosur. Prueben al menos dos paletas de color distintas y decidan, con criterio propio, cuál comunica mejor el mensaje del "giro hacia el este" en el comercio regional.

---

## 14. Errores y avisos frecuentes (y cómo leerlos)

`tmap` es particularmente verborrágico con sus mensajes, lo cual —lejos de ser un defecto— es una ventaja pedagógica: casi siempre el propio mensaje les dice qué hacer.

- **`old-style crs object detected; please recreate object with a recent sf::st_crs()`**: el objeto espacial tiene metadatos de proyección en un formato desactualizado. Solución: `st_crs(objeto) <- st_crs(objeto)` o releer el archivo fuente con una versión reciente de `sf`.
- **`Some components or legends are too "high" and are therefore rescaled`**: la leyenda no entra en el espacio disponible y `tmap` la reescala automáticamente. Pueden desactivar este comportamiento con `tmap_options(component.autoscale = FALSE)` si prefieren controlar el tamaño manualmente.
- **Mensajes `[v3->v4]`**: no son errores, son sugerencias de migración de sintaxis, como se explicó en la Sección 12.
- **Un mapa "en blanco" o solo con fondo**: la causa más común es haber invocado `tm_shape()` sin ninguna capa de dibujo después, o haber usado el nombre de columna equivocado (recuerden que `tmap` es sensible a mayúsculas/minúsculas, igual que R en general).
- **Colores "genéricos" pese a haber puesto `fill = "nombre_columna"`**: revisen que la columna sea del tipo esperado (`numeric` para escalas continuas/de intervalos, `factor` o `character` para categóricas) — `class(objeto$nombre_columna)` es su mejor amigo acá.

---

## 15. Buenas prácticas de cartografía temática (más allá de la sintaxis)

Aprender la sintaxis de `tmap` es la parte más fácil; diseñar un buen mapa temático requiere criterio cartográfico, que en una tecnicatura de ciencia de datos espaciales conviene tratar con el mismo rigor que el código:

1. **Elijan el método de clasificación con criterio, no por defecto.** `"quantile"` reparte la misma cantidad de observaciones por clase pero puede ocultar outliers; `"jenks"` (rupturas naturales) busca minimizar la varianza dentro de cada clase y suele ser más honesto con la distribución real de los datos; `"equal"` es fácil de leer pero puede dejar clases vacías si los datos están sesgados.
2. **La paleta de color debe corresponder al tipo de variable**: secuencial para variables que van de "menos" a "más" (ej. población), divergente cuando hay un punto medio significativo (ej. saldo comercial positivo/negativo), y cualitativa (sin orden perceptual) para categorías nominales (ej. tipo de economía).
3. **Siempre citen la fuente de los datos** (`tm_credits()`) y **siempre incluyan una escala gráfica y una orientación** cuando el mapa vaya a usarse fuera de un contexto puramente exploratorio.
4. **Elijan una proyección apropiada a la escala del mapa.** Un mapa mundial en coordenadas geográficas sin proyectar distorsiona brutalmente las áreas cerca de los polos; para una región como el Mercosur, una proyección cónica o equivalente centrada en Sudamérica es preferible a heredar por defecto el CRS de origen del archivo.
5. **Menos es más.** La tentación de mapear cinco variables visuales a la vez (fill + size + shape + lwd + lty) casi siempre resulta en un mapa ilegible. `tmap` v4 lo permite técnicamente, pero eso no significa que deban hacerlo.

---

## 16. Recursos oficiales para seguir profundizando

Estos son los recursos que la propia documentación de `tmap` recomienda, en orden de utilidad para quienes recién empiezan:

- **Vignettes oficiales** en `r-tmap.github.io` — cubren desde lo básico (shapes, variables visuales, escalas, leyendas, facetas) hasta temas avanzados (unidades, márgenes, extensiones).
- **Capítulo "Making Maps with R"** del libro *Geocomputation with R* (Lovelace, Nowosad y Muenchow), disponible gratis online, que trata `tmap` en el contexto más amplio del análisis espacial con R.
- **Libro en desarrollo** *Elegant and Informative Maps with tmap* (`tmap.geocompx.org`), pensado específicamente como manual extendido del paquete.
- **Documentación de referencia de funciones** en CRAN (`tmap.pdf`) para consultar todos los argumentos posibles de cada función cuando la vignette no alcance.

---

## 17. Resumen visual de la sintaxis (para pegar en el escritorio)

```r
tmap_mode("plot")                          # o "view" para interactivo

tm_shape(objeto_espacial, crs = "...") +   # declara el objeto y, opcional, reproyecta
  tm_polygons(                             # capa de dibujo
    fill        = "variable",              # variable visual ← variable de datos
    fill.scale  = tm_scale_intervals(...), # CÓMO se transforma el dato en color
    fill.legend = tm_legend(...)           # CÓMO se muestra la leyenda
  ) +
  tm_facets_wrap(by = "categoria") +       # paneles pequeños (opcional)
  tm_title("...") +
  tm_compass() +
  tm_scalebar() +
  tm_credits("Fuente: ...") +
  tm_layout(frame = FALSE)
```

Esa estructura de siete piezas —modo, shape, capa con sus tres sub-argumentos, facetas, y componentes de layout— cubre, en la práctica, más del 90% de los mapas que van a necesitar producir a lo largo de la carrera.

---

*Guía elaborada para uso didáctico, con base en la documentación oficial del paquete `tmap` (r-tmap.github.io) y su código fuente en CRAN, versión 4.4-1.*
