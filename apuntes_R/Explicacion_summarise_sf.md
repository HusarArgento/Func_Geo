# Explicación del uso de `summarise()` en `sf`

## El contexto

Partimos de un objeto `sf` llamado `puntos` con tres filas:

  nombre   geometry
  -------- ----------
  A        POINT
  B        POINT
  C        POINT

Visualmente:

``` text
        B
       ●

●               ●
A               C
```

## El problema

Un polígono **no puede construirse a partir de tres filas
independientes**.

`st_cast("POLYGON")` necesita una **única geometría** que contenga todos
los puntos.

Actualmente R tiene:

``` text
Fila 1 → POINT A
Fila 2 → POINT B
Fila 3 → POINT C
```

Necesita algo como:

``` text
MULTIPOINT(A,B,C)
```

## ¿Qué hace `st_combine()`?

``` r
st_combine(geometry)
```

Toma varias geometrías y las junta en una sola colección.

Antes:

``` text
POINT A
POINT B
POINT C
```

Después:

``` text
MULTIPOINT(A,B,C)
```

Sin embargo, esto por sí solo **no reduce el número de filas** del
objeto `sf`.

## ¿Qué hace `summarise()`?



``` r
summarise(
  geometry = st_combine(geometry)
)
```

`summarise()` reduce varias filas a una sola.

Con datos numéricos ocurre algo similar:

``` r
ventas |>
  summarise(total = sum(ventas))
```

Tres filas de ventas pasan a una única fila con el total.

Con geometrías ocurre lo mismo.

Antes:

  geometry
  ----------
  POINT A
  POINT B
  POINT C

Después:

  geometry
  -------------------
  MULTIPOINT(A,B,C)

Es decir:

> **"Reducí todas las filas a una sola y, como geometría de esa fila,
> usá la combinación de todos los puntos."**

## Finalmente

``` r
st_cast("POLYGON")
```

Ahora sí puede convertir esa geometría en un polígono.

``` text
      B
     / \
    /   \
   /     \
  A-------C
```

## Analogía

Imaginá tres hojas de papel.

``` text
Hoja 1 → Punto A
Hoja 2 → Punto B
Hoja 3 → Punto C
```

-   `st_combine()` es como sujetar las tres hojas con un clip.
-   `summarise()` es como copiar los tres puntos en una **única hoja
    nueva**.

Recién sobre esa hoja única puede dibujarse el polígono.

## Idea clave

En `sf`, `summarise()` no se usa únicamente para obtener promedios o
sumas.

También sirve para **condensar varias entidades espaciales en una
sola**, permitiendo luego realizar operaciones como disolver polígonos,
combinar líneas o construir nuevas geometrías a partir de varias
existentes. Bien
