names(ciberseg1a) #nombres
dim(ciberseg1b) #dimensiones (filas x campos) 
str(ciberseg1b) #estructura
unique(ciberseg1b$FREQ) #categorias
distinct #lo mismo pero en formato dataframe
indicadores <- unique(ciberseg1b$INDICATOR_LABEL)

view(indicadores)

obs <- unique(ciberseg1b$OBS_VALUE)
view(obs)
nrow #cuenta filas
count #cuenta filas por categorias 

# JOINS:
left_join(): #mantiene todos de la izquierda
inner_join(): #solo los que coinciden
semi_join(): #filtra izquierda según derecha (sin agregar columnas)
anti_join(): #lo opuesto a semi_join
#  1️⃣ Detectar problemas de datos
#  2️⃣ Ver quiénes no cumplen una condición
#  3️⃣ Chequeo de joins (regla de oro):👉 Si da 0 → join completo
                                     #👉 Si da >0 → hay claves sin match
#✅ Joins con múltiples claves: by = c("col1", "col2")
#✅ Diferencia entre semi_join (solo filtra) e inner_join (trae columnas)
#✅ El group_by() debe ir después del join
#✅ El truco de mean(condicion == valor) para calcular porcentajes
#✅ arrange(desc()) para ordenar descendente

# AGREGACIONES:
group_by() + summarise():# resume datos por grupos
# - all(): TRUE si TODOS cumplen
# - any(): TRUE si AL MENOS UNO cumple
# - n(): cuenta filas

  hogares %>% 
  anti_join(trabajos, by = c("folioviv", "foliohog")) %>%
  summarise(
    cantidad_hogares = n(),
    gasto_promedio = mean(gasto_mon, na.rm = TRUE),
    gasto_mediano = median(gasto_mon, na.rm = TRUE),
    gasto_maximo = max(gasto_mon, na.rm = TRUE),
    ingreso_promedio = mean(ing_cor, na.rm = TRUE)
  )

viviendas |>
  count(folioviv) |>
  filter(n > 1)

Tablas

tabla_resumen <- bind_rows(
  hogares %>%
    distinct(folioviv, foliohog) %>%
    summarise(n = n()) %>%
    mutate(fuente = "hogares"),
  personas %>%
    distinct(folioviv, foliohog) %>%
    summarise(n = n()) %>%
    mutate(fuente = "personas")
)

tabla_resumen <- bind_rows(
  hogares %>%
    distinct(folioviv, foliohog) %>%
    summarise(n = n()) %>%
    mutate(fuente = "hogares"),
  personas %>%
    distinct(folioviv, foliohog) %>%
    summarise(n = n()) %>%
    mutate(fuente = "personas")
)


#Pregunta                        Código correcto
#¿Todos tienen el mismo sexo?    n_distinct(sexo) == 1
#¿Todos son hombres?             all(sexo == 1)
#¿Todos alfabetizados?           all(alfabetism == 1)
#¿Todos tienen la misma edad?    n_distinct(edad) == 1
#¿Cuántas edades diferentes hay? n_distinct(edad)

Resumen:
  
  #✅ n_distinct(variable) == 1 → ¿Todos tienen el mismo valor (sin importar cuál)?
#✅ all(variable == valor) → ¿Todos tienen un valor específico?
  #❌ n_distinct(variable == 1) → No hace lo que esperás