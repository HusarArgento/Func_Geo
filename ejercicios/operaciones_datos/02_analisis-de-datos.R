# Clase 2. Geocomputación y el ecosistema de datos espaciales en R

# Cargar librerías ---------------------------------------

# Manejo de archivos
library(fs)

# Son todos paquetes que integran el mega paquete {tidyverse}
# library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(ggplot2)

# Lectura y manipulación de data frames ---------------------------------------------------

### ╠ 0. Lectura y escritura de datos tabulares (dataframes) -----------------------------

db_en_disco <- read.csv(text = "id,edad,estado,sexo
                                 1,35,ocupado,v
                                 2,40,desocupado,m
                                 3,42,inactivo,m
                                 4,27,desocupado,m
                                 5,52,desocupado,v
                                 6,63,inactivo,m
                                 7,37,ocupado,m
                                 8,24,ocupado,v
                                 9,21,ocupado,m")

fs::dir_create(fs::path("data"), recurse = TRUE)

write.csv(db_en_disco,
          fs::path("data", "db_intro", ext = "csv"),
          row.names = F)

ls()
rm(db_en_disco)
ls()

db <- read.csv(fs::path("data", "db_intro", ext = "csv"),
               sep = ",", dec = ".", encoding = "UTF-8")

db

fs::file_delete(fs::path("data", "db_intro", ext = "csv"))

### ╠ 1. Seleccionar columnas ------------------------
# Retenemos las columnas id y sexo

db[c("id", "sexo")]
db[ , c("id", "sexo")]

# con {dplyr} de {tidiverse}
db |>  dplyr::select(id, sexo)

### ╠ 2. Filtrar filas -------------------------------
# Retenemos las filas donde la edad sea mayor o igual a 40

db[db$edad >= 40, ]

# con {dplyr} de {tidiverse}
db |>  dplyr::filter(edad >= 40)

### ╠ 3. Ordenar filas -------------------------------

db[order(db$edad), ]

# con {dplyr} de {tidiverse}
db |>  dplyr::arrange(edad)

### ╠ 4. Modificar y transformar ---------------------
# Modificamos una columna (“estado” pasa a mayúsculas) y creamos dos
# columnas nuevas (año de nacimiento y una combinación de sexo y edad)

# db$estado <- str_to_upper(db$estado)
# db$nac <- 2023 - db$edad
# db$sex_edad <- paste(db$sexo, db$edad, sep = "_")

# con {dplyr} de {tidiverse}
db |>
  dplyr::mutate(estado = stringr::str_to_upper(estado),
                nac = 2023 - edad,
                sex_edad = paste(sexo, edad, sep = "_"))

### ╠ 5. Resumir -------------------------------------
# Toma los valores de una (o más) columnas y genera indicadores resúmenes
# sobre estas (cantidad de casos, cantidad de varones y edad promedio)

db |>
  summarise(n = n(),
            varones = sum(sexo == "v"),
            edad_prom = mean(edad, na.rm = T))

### ╠ 6. Divide, aplica y combina -------------------
# La estrategia consiste en:
#   (1) Dividimos la base a partir de los valores de una variable (“estado”)
#         obteniendo una base agrupada;
#   (2) aplicamos una transformación/modificación/resumen a cada grupo;
#   (3) unimos los resultados en una nueva base.

# Contamos, para cada valor de estado, el número de registros,
#     la cantidad de varones y la edad promedio

db |>
  group_by(estado) |>
  mutate(n = n(),
         varones = sum(sexo == "v"),
         edad_prom = mean(edad, na.rm = T))

db |>
  group_by(estado) |>
  summarise(n = n(),
            varones = sum(sexo == "v"),
            edad_prom = mean(edad, na.rm = T))

### ╠ 7. Unir bases --------------------------------
# Con los comandos *_join podemos unir dos bases en función de algún índice común.

(db1 <- db |> select(id, sexo, edad) |> filter(edad >= 36))
(db2 <- db |> select(id, estado) |> filter(id != 2))

db1 |> left_join(db2, by = "id")
db1 |> right_join(db2, by = "id")
db1 |> inner_join(db2, by = "id")
db1 |> full_join(db2, by = "id")

# Podemos indicar cómo emparejar los índices si tienen diferente nombre
db1 |> rename(id2 = id) |> left_join(db2, by = c("id2" = "id"))

# Podemos utilizar más de una columna como índice para el emparejamiento
db |> select(-sexo) |> left_join(db1, by = c("id", "edad"))

rm(db1, db2)

### ╚ 8. Reorganizar (reshape) -------------------

db_aux <- db |>
  group_by(estado, sexo) |>
  summarise(edad_prom = mean(edad))

db_aux

tabla1 <- db_aux |>
  pivot_wider(id_cols = estado, names_from = sexo,
              values_from = edad_prom, values_fill = NA)

tabla1

tabla2 <- tabla1  |>
  pivot_longer(cols = m:v, names_to = "sexo", values_to = "edad_prom", values_drop_na = T)

tabla2

rm(db_aux, tabla1, tabla2)

# Visualización de datos con {ggplot2} ----------------------------

# Llamada inicial (ggplot)
ggplot(data = db) +
  # Agregar geometrías (geom_*)
  geom_boxplot(aes(x = sexo, y = edad, fill = sexo),
               color = "tomato4", alpha = 0.3) +
  # Etiquetas (lab)
  labs(title = "Esto es un gráfico de cajas",
       caption = "Fuente: Datos inventados",
       x = "Sexo de las personas",
       y = "Edad") +
  # Escalas (scale_*)
  scale_y_continuous(n.breaks = 10, limits = c(0, NA)) +
  # Facetados (facet_*)
  # facet_wrap(~estado) +
  # Definir tema (theme_* / theme)
  theme_minimal()

rm(db)

# Ejemplo de aplicación -------------------------------------------

# Tenemos datos referidos a población y educación de los departamentos de la
#   Argentina. Los datos fueron descargados del portal poblaciones.org en csv.
#
# ¿Qué preguntas podríamos "hacerle" a estos datos?
# ¿Es necesario modificar / limpiar estos datos de alguna manera?
# ¿Cómo podríamos visualizar nuestros resultados?


### ╠ Cargar datos -------------------------------------------

  # Descargado de poblaciones.org
  # https://mapa.poblaciones.org/map/#/@-34.163682,-61.831055,7z/l=8601!v2!a1!i1!w0,0,0
  depart <- read.csv(fs::path("data",
                              "indicadores_de_personas_departamentos_2010", ext = "csv"),
                     colClasses = c("Código.de.departamentos" = "character"))

### ╠ Limpieza de datos (data wrangling) --------------------

  # ¿Cuál es la estructura de los datos?

    class(depart)

    dim(depart)
    # nrow(depart)
    # ncol(depart)

    # View(depart)

  # ¿Qué información contiene el dataset?

    names(depart)

    head(depart)
    # head(as_tibble(depart))

    str(depart)

    summary(depart)

  # Simplificamos los nombres de algunas columnas
  # (para que sea más fácil de trabajar)
  # rename()

    depart <- rename(depart,
                     PROV = Código.de.provincia,
                     PROV_lab = Nombre.de.provincia,
                     DEP = Código.de.departamentos,
                     DEP_lab = Nombre.de.departamentos.comuna,

                     HOG = Total.de.hogares,
                     POB = Población.total..en.hogares.familiares..,
                     POB2 = Población.total,
                     MUJ = Total.de.mujeres..en.hogares.familiares..,

                     POB_00_03 = Población.de.0.a.3.años.,
                     POB_04_05 = Población.de.4.a.5.años.,
                     POB_06_12 = Población.de.6.a.12.años.,
                     POB_13_17 = Población.de.13.a.17.años.,

                     POB_NIN = Población.de.0.a.17.años.,

                     ASI_INI = Niños.de.4.y.5.años.que.asisten.a.jardín,
                     ASI_PRI = Población.de.6.a.12.que.asiste,

                     POB_ADU = Población.de.18.años.y.más.,
                     POB_ADU_SINPRI = Población.de.18.y.más.con.primaria.incompleta.o.menos,
                     POB_ADU_CONPRI = Población.de.18.y.más.con.primaria.completa,
                     POB_ADU_CONSEC = Población.de.18.y.más.con.secundaria.completa,
                     POB_ADU_SINSEC = Población.de.18.y.más.con.primaria.completa.sin.secundario.completo,
                     POB_ADU_SEC = Población.de.18.y.más.con.secundaria.completa.sin.superior.completo,
                     POB_ADU_TER = Población.de.18.y.más.con.terciario.completo,
                     POB_ADU_UNI = Población.de.18.y.más.con.universitario.completo.o.más,

                     AREA = Superficie.en.km2
    )

  # Analicemos algunas columnas que pueden contener información ambigua...

  # ¿Por qué tenemos dos datos de población?
  # ¿Cuál es el que deberíamos retener para analizar los datos de educación
  #  y de distribución de edad?
  # transmute() == mutate(..., .keep = "none")

    depart |>
      mutate(POB_CAL = POB_ADU + POB_NIN,
             POB1 = POB,
             POB2,
             .keep = "none") |>
      # summarise(si = sum(POB_CAL != POB1) )
      head()

  # Analicemos la información referidas al nivel educativo de los adultos.
  # ¿Qué contienen estas columnas?

  # Visualizamos las columnas referidas al nivel educativo de los adultos
  #   y analizamos esta información para entender qué contiene
  # select()

    depart |>
      as_tibble() |>
      select(starts_with("POB_ADU")) |>
      head()

    # POB_ADU POB_ADU_SINPRI POB_ADU_CONPRI POB_ADU_CONSEC POB_ADU_SINSEC POB_ADU_SEC POB_ADU_TER POB_ADU_UNI
    #  152671           6758         144199         103834          40365       66674       10217       26943
    #  131806           1620         129503         114740          14763       59994       11103       43643
    #  148893           4824         142720         103705          39015       70641       11183       21881
    #  161400          10634         148419          85455          62964       63227        9241       12987
    #  144539           3775         139550         106552          32998       67289       12609       26654
    #  143110           2406         139787         115121          24666       65698       14020       35403

  # Calculamos algunas columnas para analizar nuestras hipótesis sobre los datos
  # (retenemos sólo las columnas modificadas)
  # transmute()

    depart |>
      transmute(POB_ADU_CONSEC,
                POB_ADU_CONSEC2 = POB_ADU_SEC + POB_ADU_TER + POB_ADU_UNI,
                POB_ADU,
                POB_ADU1 = POB_ADU_CONSEC  + POB_ADU_SINSEC + POB_ADU_SINPRI,
                POB_ADU2 = POB_ADU_CONPRI  + POB_ADU_SINPRI,
                POB_ADU_SEC ,
                POB_ADU_TER ,
                POB_ADU_UNI,
                POB_ADU_PRI = POB_ADU - POB_ADU_CONSEC
                ) |>
      head()

    # POB_ADU_CONSEC = POB_ADU_SEC + POB_ADU_TER + POB_ADU_UNI
    # TOTAL = POB_ADU_CONSEC  + POB_ADU_SINSEC + POB_ADU_SINPRI (+ E)
    # TOTAL = POB_ADU_CONPRI  + POB_ADU_SINPRI (+ E)

  # Hay un error en la información tal cómo fue calculada.
  # ¿Qué puede haber ocurrido? ¿Cómo podríamos solucionarlo?

  # Recalculamos los datos de educación que faltan para corregir la base
  # mutate()

    depart <- mutate(depart,
                     POB_ADU_PRI = POB_ADU - POB_ADU_CONSEC)

  # Nos interesa tener las regiones

    regiones <- readr::read_csv(fs::path("data", "regiones", ext = "csv"),
                                show_col_types = TRUE)

    regiones

    depart <- depart |>
      mutate(PROV = str_pad(PROV, width = 2, pad = "0", side = "left"),
             DEP = str_pad(DEP, 5, pad = "0")) |>
      left_join(regiones, by = "PROV")

  # ¿Hay columnas que podríamos descartar?
  # Descartamos las variables que no son de interés
  # select() usando selectores negativos

    depart <- depart |>
      select(-POB_ADU_CONPRI, -POB_ADU_SINPRI, -POB_ADU_SINSEC,
             -starts_with("Varones"), -starts_with("Mujeres"),
             -Código.de.departamentos.comuna,
             -Total.de.varones..en.hogares.familiares..,
             -ends_with(".del.centroide"))

    names(depart)

  # Sería posible reordenar las variables para que sea más fácil de manejar y
  #   visualizar esta información?
  # select() usando diferentes selectores

    depart <- select(depart,
                     # Información de códigos geográficos
                     PROV, PROV_lab, DEP, DEP_lab, REGION,
                     # Información demográfica global
                     HOG, POB, POB_TOT = POB2, MUJ,
                     # Información demográfica de población infantil
                     POB_NIN, POB_00_03:POB_13_17,
                     # Información de educación de adultos
                     POB_ADU, POB_ADU_CONSEC, POB_ADU_PRI,
                     POB_ADU_SEC, POB_ADU_TER, POB_ADU_UNI,
                     # Resto de variables
                     everything())

    names(depart)

  # Podíamos aún ordenar diferente las variables ASI_INI y ASI_PRI
  # relocate()

    depart <- depart |>
      relocate(starts_with("ASI_"), .before = POB_ADU)

  # ¿Tiene sentido comparar estos datos de esta manera?
  # Creemos algunas variables más que nos permitan comparar los resultados.

    depart <- depart |>
      mutate(AREA = round(AREA, 1),           # km^2

             DEN = POB/ (AREA * 100),         # Población por ha (100ha = 1 km^2)
             FEM = MUJ / (POB-MUJ) * 100,     # Mujeres por cada 100 varones

             ASI_INI_p = ASI_INI/ POB_04_05 * 100,
             ASI_PRI_p = ASI_PRI/ POB_06_12 * 100,

             # p_CONSEC = POB_ADU_CONSEC/ POB_ADU * 100,
             # p_PRI =    POB_ADU_PRI/ POB_ADU * 100,
             # p_SEC =    POB_ADU_SEC/ POB_ADU * 100,
             # p_TER =    POB_ADU_TER/ POB_ADU * 100,
             # p_UNI =    POB_ADU_UNI/ POB_ADU * 100

             across(.cols = starts_with("POB_ADU_"),
                    # .fns = ~.x / POB_ADU * 100,
                    # .fns = \(.variable) .variable / POB_ADU * 100,
                    .fns = function(.variable) .variable / POB_ADU * 100,
                    .names = "p_{.col}") ) |>
      # rename(p_CONSEC = p_POB_ADU_CONSEC,
      #        p_PRI =    p_POB_ADU_PRI,
      #        p_SEC =    p_POB_ADU_SEC,
      #        p_TER =    p_POB_ADU_TER,
      #        p_UNI =    p_POB_ADU_UNI) |>
      rename_with(.cols = starts_with("p_POB_ADU_"),
                  .fn = ~str_remove(.x, "POB_ADU_"))

### ► Actividad -------------

  # Resumir este proceso mediante un encadenamiento (pipe)

  depart2 <- depart

  regiones <- read_csv(here::here("data", "regiones.csv"),
                       show_col_types = T)

  depart <- read.csv(here::here("data",
                                "indicadores_de_personas_departamentos_2010.csv")) |>
    rename(PROV = Código.de.provincia,
           PROV_lab = Nombre.de.provincia,
           DEP = Código.de.departamentos,
           DEP_lab = Nombre.de.departamentos.comuna,

           HOG = Total.de.hogares,
           POB = Población.total..en.hogares.familiares..,
           POB2 = Población.total,
           MUJ = Total.de.mujeres..en.hogares.familiares..,

           POB_00_03 = Población.de.0.a.3.años.,
           POB_04_05 = Población.de.4.a.5.años.,
           POB_06_12 = Población.de.6.a.12.años.,
           POB_13_17 = Población.de.13.a.17.años.,

           POB_NIN = Población.de.0.a.17.años.,

           ASI_INI = Niños.de.4.y.5.años.que.asisten.a.jardín,
           ASI_PRI = Población.de.6.a.12.que.asiste,

           POB_ADU = Población.de.18.años.y.más.,
           POB_ADU_SINPRI = Población.de.18.y.más.con.primaria.incompleta.o.menos,
           POB_ADU_CONPRI = Población.de.18.y.más.con.primaria.completa,
           POB_ADU_CONSEC = Población.de.18.y.más.con.secundaria.completa,
           POB_ADU_SINSEC = Población.de.18.y.más.con.primaria.completa.sin.secundario.completo,
           POB_ADU_SEC = Población.de.18.y.más.con.secundaria.completa.sin.superior.completo,
           POB_ADU_TER = Población.de.18.y.más.con.terciario.completo,
           POB_ADU_UNI = Población.de.18.y.más.con.universitario.completo.o.más,

           AREA = Superficie.en.km2
    ) |>
    mutate(POB_ADU_PRI = POB_ADU - POB_ADU_CONSEC,
           PROV = str_pad(PROV, 2, pad = "0"),
           DEP = str_pad(DEP, 5, pad = "0")) |>
    left_join(regiones, by = "PROV") |>
    select(-POB_ADU_CONPRI, -POB_ADU_SINPRI, -POB_ADU_SINSEC,
           -starts_with("Varones"), -starts_with("Mujeres"),
           -Código.de.departamentos.comuna,
           -Total.de.varones..en.hogares.familiares..,
           -ends_with(".del.centroide")) |>
    select(# Información geográfica
           PROV, PROV_lab, DEP, DEP_lab, REGION,
           # Información demográfica global
           HOG, POB, POB_TOT = POB2, MUJ,
           # Información demográfica de población infantil
           POB_NIN, POB_00_03:POB_13_17,
           # Información de educación de adultos
           POB_ADU, POB_ADU_CONSEC, POB_ADU_PRI, POB_ADU_SEC, POB_ADU_TER, POB_ADU_UNI,
           # Resto de variables
           everything()) |>
    relocate(starts_with("ASI_"), .before = POB_ADU) |>
    mutate(AREA = round(AREA, 1),          # km^2

           DEN = POB / (AREA * 100),       # Población por ha (100ha = 1 km^2)
           FEM = MUJ / (POB-MUJ) * 100,    # Mujeres por cada 100 varones

           ASI_INI_p = ASI_INI/ POB_04_05 * 100,
           ASI_PRI_p = ASI_PRI/ POB_06_12 * 100,

           across(.cols = starts_with("POB_ADU_"),
                  .fns = ~.x / POB_ADU * 100,
                  .names = "p_{.col}") ) |>
    rename_with(.cols = starts_with("p_POB_ADU_"),
                .fn = ~str_remove(.x, "POB_ADU_"))

  all.equal(depart, depart2)

  rm(depart2)

### ╠ Análisis exploratorio de datos ------------------------------------------

  # ¿Podemos comparar cuatro departamentos? ¿Qué pasa en cuatro
  #   departamentos intermedias de la PBA?
  # filter() + transmute()

  depart |>
    as_tibble() |>
    filter(PROV_lab == "Buenos Aires" &
             str_detect(DEP_lab, "Zárate|Junín|Tandil|Necochea")) |>
    select(DEP, DEP_lab, POB, POB_ADU, AREA,
           DEN, FEM, ends_with("_p"),
           starts_with("p_"),
           -p_PRI, -p_CONSEC)

  # Podemos seleccionar para analizar aquellos departamentos (ciudades) intermedios

  int_cities <-  depart |>
    as_tibble() |>
    filter(POB > 50000 & POB < 150000) |>
    mutate(SIZE = cut(POB, breaks = c(50, 75, 100, 150) * 1000,
                       include.lowest = T,
                       labels = c("Chicas (<75)", "Intermedia (e/75-100)",
                                  "Grande (>100)") ))

  # Veamos los departamentos con mayor/menor porcentaje de universitarios
  int_cities |>
    arrange(-p_UNI) |>
    select(DEP, PROV_lab, DEP_lab, AREA, POB, DEN, p_SEC, p_TER, p_UNI) |>
    head(6)

  int_cities |>
    arrange(-p_UNI) |>
    select(DEP, PROV_lab, DEP_lab, AREA, POB, DEN, p_SEC, p_TER, p_UNI) |>
    tail(8)

  # ¿Es posible que el tamaño del departamento (en superficie) esté modificando
  # el análisis? Veamos qué pasa si consideramos un rango de área.
  int_cities |>
    filter(AREA > 2000 & AREA < 3000) |>
    select(DEP, PROV_lab, DEP_lab, POB, POB_ADU, AREA,
           DEN, FEM, ends_with("_p"),
           starts_with("p_"),
           -p_PRI, -p_CONSEC) |>
    arrange(p_UNI)

  # ¿El tamaño del municipio y la región pueden estar modificando el análisis?
  int_cities |>
    group_by(SIZE, REGION) |>
    summarise(POB_ADU = sum(POB_ADU),
              POB_UNI = sum(POB_ADU_UNI),
              POB_UNI_p = POB_UNI/POB_ADU * 100) |>
    pivot_wider(id_cols = REGION, names_from = SIZE, values_from = POB_UNI_p)

  # ¿Existe una relación entre la cantidad de personas con secundario y la
  #   cantidad de personas con estudios universitarios?

  int_cities |>
    ggplot(aes(x = p_SEC, y = p_UNI)) +
    geom_point(aes(color = REGION, size = DEN)) +
    geom_smooth(se = F, method = "glm", formula = y ~ poly(x, 2)) +
    labs(title = paste0("Relación entre personas adultas con nivel secundario y universitario",
                        "\ndiferenciado por tamaño y región de la ciudad"),
         x = "Porcentaje de personas adultas\ncon nivel secundario (sin nivel superior)") +
    scale_y_continuous("Porcentaje de personas adultas\ncon nivel universtiario",
                       breaks = seq(0, 20, 2.5)) +
    facet_wrap(~SIZE) +
    theme_minimal()

### ╚ Armado de base de provincias --------------------------------------------

  # ¿Cómo podríamos resumir los datos a nivel de provincias?

  # group_by() + summarise()
  prov <- depart |>
    group_by(PROV, PROV_lab, REGION) |>
    summarise(n = n(),
              across(c(HOG:POB_ADU_UNI, AREA),
                     .fns = sum)) |>
    mutate(DEN = POB / (AREA * 100),
           FEM = MUJ / (POB-MUJ) * 100,
           ASI_INI_p = ASI_INI/ POB_04_05 * 100,
           ASI_PRI_p = ASI_PRI/ POB_06_12 * 100,
           across(.cols = starts_with("POB_ADU_"),
                  .fns = ~.x / POB_ADU * 100,
                  .names = "p_{.col}") ) |>
    rename_with(.cols = starts_with("p_POB_ADU_"),
                .fn = ~str_remove(.x, "POB_ADU_"))

  # ¿Podríamos mantener los datos de las provincias en una columna del dataset?
  dep_x_prov <- depart |>
    split(~PROV)

  prov$dep <- dep_x_prov

### ► Actividad ------------------------------------------------------------

  # En grupos, propongan y realicen algún análisis sobre el conjunto de
  #   departamentos utilizados.
