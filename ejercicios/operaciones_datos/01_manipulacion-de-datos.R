# Clase 1. Repaso de herramientas para la manipulación de datos en R

# - [ ] Armado de proyectos y estructura de carpetas en R y RStudio.
# - [ ] Lectura de datos externos rectangulares (data frames) provenientes de archivos .csv y .xlsx.
# - [ ] Manipulación de datos (filtrar, seleccionar, ordenar, modificar, resumir, agrupar unir, etc.).
# - [ ] Visualización de datos con ggplot2.

# - [ ] Manejo básico de listas y matrices.
# - [ ] Listas-columnas en data frames.

# - [ ] Bucles (for).
# - [ ] Condicionales (if-else).
# - [ ] Funciones.
# - [ ] Uso básico de funcionales (familia map).

# Cargar librerías --------------------------------------------------------

# No usaremos librerías externas para esta clase, pero es importante
# recordar que utilizamos librerías y es conveniente cargarlas al principio
# del script.
#library(tidyverse)

# Vectores atómicos ------------------------------------------

### ╠ Tipo y coerción entre tipos ---------------------------

  # Tipo y longitud
  # El tipo de un vector se obtiene con typeof()
  # La longitud de un vector se obtiene con length()

  vector1 <- c(1L, 3L, 56L, 100:103)
  typeof(vector1)
  length(vector1)

  # Coerción / forzado entre tipos
  # Los tipos se coercionan siguiendo el siguiente orden:
  # logic -> numeric (integer -> double) -> character

  vector2 <- c(vector1, 0.5)
  typeof(vector2)

  vector3 <- c(1, 3L, 56.)
  typeof(vector3)

  vector4 <- c("a", 1, FALSE, T)
  typeof(vector4)

  T + F + 10

  rm(vector1, vector2, vector3, vector4)

### ╠ Atributos --------------------------

  # Puede considerarse como una lista con nombres de metadatos arbitrarios

  (vector1 <- sample(1:3, 12, replace = T))

  attributes(vector1)
  attributes(vector1)$mi_atributo <- "d"
  attributes(vector1)

  vector2 <- c("a" = "es una letra", "diez" = "Maradona")
  vector2
  # attributes(vector2)

  attributes(vector2)$names[2] <- "D10s"
  # attributes(vector2)
  vector2

  names(vector2)[2] <- c("diez")
  # attributes(vector2)
  vector2

  unname(vector2) |> attributes()

  rm(vector1, vector2)

### ╚ El atributo dim() y las matrices -------------------
  # Una matrix no es más que un vector atómico con un atributo "dim"

  vector1 <- 1:12
  (m1 <- matrix(vector1, nrow = 3, byrow = F))

  typeof(m1)
  attributes(m1)
  dim(m1)
  length(m1)

  (m2 <- matrix(letters[1:15], nrow = 3, byrow = T))

  typeof(m2)
  attributes(m2)
  dim(m2)

  m3 <- as.numeric(vector1)

  dim(m3) <- c(3, 4)
  m3

  structure(1:12, dim = 3:4) |> class()

  # La extracción de elementos de una matriz tiene dos notaciones posibles.

  # Si sólo utilizamos un índice extraeremos el elemento i (como si fuera un vector atómico)
  m1[3]

  # Si utilizamos dos índices (i, j) podemos extraer el elemento de la fila i y la columna j
  m1[1, 3]
  m1[ , 3]
  m1[1, ]

  rm(vector1, vector2)
  rm(m1, m2, m3)

# Listas ------------------------------------------

### ╠ Construcción de listas ----------------------------------
  # Las listas son, básicamente, un tipo de objeto que permite guardar
  #   elementos heterogéneos en una misma estructura de datos.
  #   Adicionalmente, las listas permiten nombrar estos elementos
  #   (no es obligatorio hacerlo).

  l1 = list(description = "esto es la descripcion de lo que hay en esta lista",
            db = data.frame(a = 1:3, b = letters[1:3]),
            numeros = 1:10,
            c("esta es una cadena que no tiene nombre en la lista"),
            una_lista_vacia = list(),
            una_lista_mas = list(a = c("hola", "mundo"),
                                 b = \(x) x + 1))

### ╚ Operaciones básicas con listas -------------------------

  # Identificamos la clase
  class(l1)
  typeof(l1)

  # Vemos la cantidad de elementos
  length(l1)

  # Obtenemos los nombres de cada uno de los elementos
  names(l1)

  # Indexación: Podemos seleccionar uno de los elementos mediante su nombre o su índice
  l1$description
  l1[4]

  # Existe una diferencia entre seleccionar por índice y por nombre respecto a
  #   los resultados que obtenemos
  l1$numeros
  l1[[3]]
  l1[3]

  l1$numeros |> class()
  l1[[3]] |> class()
  l1[3] |> class()

  # Podemos seleccionar de forma anidada
  l1[[6]][[1]][1]

  # Combinar listas
  l2 <- list(ultimo = 1:3)

  c(l1[1:2], l2)

  rm(l1, l2)

# Data frames -------------------------------------------------------------

### ╠ Dataframes como listas y como "matrices" ----------------------------
  # Un data.frame es la estructura de datos más popular para el procesamiento de datos
  #   Un data.frame es una _lista nombrada de vectores_ con atributos para
  #   names (nombre de columna), row.names (nombre de filas) y la clase.
  #   A diferencia de una lista regular, un data.frame tiene como restricción que
  #   la longitud de cada uno de sus vectores (columnas) debe ser la misma.
  #   Esto le da a los data.frame una estructura rectangular y hace que tenga
  #   propiedades comunes con las matrices y las listas.

  df <- data.frame(a = 1:4,
                   b = letters[1:4])

  class(df)
  typeof(df)

  attributes(df)

  rownames(df)
  colnames(df)
  names(df)

  nrow(df)
  ncol(df)
  length(df)

  class(df[1:2])
  class(df[[1]])
  class(df[ , 1])
  class(df[ , 1:2])

### ╚ Listas-columnas en data-frames -------------------

  l1 <- list(1:4, 5:10, 12, "a")

  df$lista <- l1

  df |> class()
  df$lista |> class()

  df |> as_tibble()

  rm(l1, df)

# Control de flujos, funciones y funcionales ----------------------

### ╠ Condicionales ----------------------------------

  # El manejo de estructuras condicionales if() permite elegir entre diferentes
  # "trozos" de codigo en funcion del valor de una prueba o condición.

  # 1. Condicional simple (if)
  # if(cond) cons.expr

  x <- 5
  if(x > 0){
    print("Número positivo")
  }

  # Siempre deberemos ingresar un valor único a evaluar, si existen más
  #    de un valor a evaluar nos indicara un error.
  x <- 1:3

  if(x > 0){
    print("Número positivo")
  }

  # 2. Condicional completo (if-else)
  # if(cond) cons.expr  else  alt.expr

  x <- -5
  if(x >= 0){
    print("Número Positivo")
  } else {
    print("Número Negativo")
  }

  # 3 Anidado de condicionales if-else

  x <- 0
  if(x > 0){
    print("Número Positivo")
  } else {
    if(x == 0){
      print("CERO")
    }else{
      print("Número Negativo")
    }
  }

  x <- sample(1:10, 1)
  if(x >= 9){
    cat("Sobresaliente: ")
    cat(x)
  }else{
    if(x >= 7){
      cat("Muy bien. Aprobado: ")
      cat(x)
    }else{
      if(x >= 4){
        cat("Regular. Recuperatorio: ")
        cat(x)
      }else{
        cat("Desaprobado: ")
        cat(x)
      }
    }
  }

  # Una manera similar es con else if
  if(x >= 9){
    cat("Sobresaliente: ")
    cat(x)
  }else if(x >= 7){
    cat("Muy bien. Aprobado: ")
    cat(x)
  }else if(x >= 4){
    cat("Regular. Recuperatorio: ")
    cat(x)
  }else{
    cat("Desaprobado: ")
    cat(x)
  }

  rm(x)

### ╠ Bucles -----------------------------------------
  # for(var in vector) expr

  for(i in 1:3){
    cat(i)
    cat("\n Es par:")
    cat(i %% 2 == 0)
    cat("\n")
  }

  # Nuestro vector no tiene porque ser numerico ni ordenado, siendo posible
  #   incluir caracteres o vectores aleatorios

  # Con caracteres
  for (value in c("Mi", "segundo", "for", "loop")) {
    cat(value)
    cat(" ")
  }

  # Con numeros que van de forma inversa
  for(i in 3:(-10) ){
    print(i)
  }

  # Con valores desordenados
  valor <- sample(1:5)
  for(i in valor){
    print(i)
  }

  # Podemos aprovechar esto para elegir variables por su nombre...
  df <- data.frame(SEXO = sample(c("v", "m"), 10, replace = T),
                   EDAD = sample(10:70, 10, replace = T),
                   PLAN = sample(c("Si", "No"), 10, replace = T))

  df$EDAD_CAT <- cut(df$EDAD, breaks = c(9, 29, 49, 69, 70),
                     labels = c("10-29", "30-49", "50-69", "NS/NC"))

  class(df$SEXO)

  variables <- c("SEXO", "EDAD_CAT", "PLAN" )

  for(i in variables){
    niveles <- unique(df[[i]])
    df[[i]] <- factor(df[[i]], levels = niveles)
  }
  class(df$SEXO)

  # También podemos usar un bucle para elegir una fila y actuar sobre ella...
  a <- 0
  for(i in 1:nrow(df)){
    if(df$SEXO[i] == "v"){
      a <- a + 1
    }
  }
  a
  table(df$SEXO)

  # Podemos generar bucles adentro de bucles... Imaginemos que queremos tener el
  # nombre de todos los elementos de una matriz 3x5, cuyas filas se representan
  # por numeros (1 a 3) y sus columnas por letras (a, b, c, d, e).

  for (i in 1:3) {
    cat(paste0("\nFila: ", i, "\n" ))
    for(j in letters[1:5])
      print(paste0(i, j))
  }

  rm(a, i, j, niveles, valor, df, value, variables)

### ╚ Funciones --------------------------------------
  # Permite sintetizar código

  # Función sin argumento
  devuelvo_1 <- function(){

    res <- "uno"

    return(res)
    # return("uno")
  }

  # Función con argumentos
  nombre_foo <- function(arg1, arg2, arg3 = T){

    res <- arg1 + arg2

    if(arg3){
      res <- res^2
    }

    return(res)
  }

  rm(nombre_foo, devuelvo_1)

# Funcionales ------------------------------------
  # Toma un vector y una función, llama a la función una vez para cada elemento
  # del vector y devuelve los resultados en una lista.

  triple_raro <- function(x){
    if(x > 2){
      res <- x * 3
    }else{
      res <- 0
    }
    return(res)
  }

  map(1:3, triple_raro)

  triple <- function(x) x * 3

  map(1:3, triple)
  # Equivalente a:
  map(1:3, ~.x*3)
  map(1:3, \(x) x*3)

  # Puedo definir el tipo de salida
  map_int(1:3, \(x) x*3)
  map_chr(1:3, \(x) LETTERS[x*3])
  map_lgl(1:3, \(x) x*3 > 0)

  # Puedo incorporar más variables que iteren conjuntamente
    xs <- map(1:8, ~ runif(10))
    ws <- map(1:8, ~ rpois(10, 5) + 1)

  map2_dbl(xs, ws, weighted.mean)

  # Un ejemplo que sea más realista...
  by_cyl <- split(mtcars, mtcars$cyl)

  res_lm <- by_cyl |>
    map(~lm(mpg ~ wt, data = .x))

  # ...extraer la pendiente...
  res_lm |>
    map(coef) %>%
    map_dbl(2)

  # Extra: agregamos el modelo a una columna de un data frame resumen.
  mtcars_cyl <- mtcars |>
    group_by(cyl) |>
    summarise(across(where(is.numeric), mean)) |>
    mutate(modelo_lm = res_lm)

  mtcars_cyl
  mtcars_cyl$modelo_lm[1]
  mtcars_cyl$modelo_lm[2]

  rm(by_cyl, res_lm, mtcars_cyl, triple, triple_raro)
