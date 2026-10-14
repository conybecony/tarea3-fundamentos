
# Constanza Pinilla
# Octubre 2026
# consiste en dos partes, A Y B, 

# cargar paquetes
library(dplyr)
library(broom)
library(modelsummary)

## Parte A — CASEN (45 puntos)
### A1. Dos modelos
casen <- read.csv("data/raw/casen_reducido.csv") |>
  mutate(experiencia = pmax(edad - educ - 6, 0))

m1 <- lm(ingreso ~ educ, data = casen)
m2 <- lm(ingreso ~ educ + experiencia + genero, data = casen)

### A2. La tabla de coeficientes, con `broom` + `dplyr`
tidy(m2, conf.int = TRUE) |> 
  filter(term != "(Intercept)") |>
  mutate(estimate = round((estimate), 3),
         conf.low = round(conf.low, 3),
         conf.high = round(conf.high, 3),
         p.value    = round(p.value, 3),
         significativo = p.value < 0.05) |>
  select(term, estimate, p.value, significativo, conf.low, conf.high) |> 
  arrange(p.value)

### A3. Comparar modelos
m3 <- lm(ingreso ~ educ * genero, data = casen) 

modelos <- list(simple = m1, controles = m2, mio = m3)
lapply(modelos, glance) |> bind_rows(.id = "modelo")  

# Mi modelo explica un poco más la variación del ingreso, ya que el R² aumenta de 0.458 a 0.478.
# Sin embargo, el R² ajustado se mantiene en 0.448, por lo que la mejora del modelo es mínima.

### A4. ¿Cambió el coeficiente de educación?
bind_rows(simple    = tidy(modelos$simple),
          controles = tidy(modelos$controles),
          mio       = tidy(modelos$mio), .id = "modelo") |>
  filter(term == "educ") |>
  select(modelo, estimate, std.error, p.value)

# El coeficiente de educación cambia poco entre m1 y m2, porque el control
# agregado no modifica mucho la relación entre educación e ingreso.
# En m3, el coeficiente aumenta porque se incorpora la interacción entre educación
# con género, siendo que para que un control mueve un coeficiente solo si se relaciona A LA VEZ con la
# educación y con el ingreso.

### A5. Diagnóstico
# 1
augment(m3) |>
  select(ingreso, educ, genero, .fitted, .resid) |>
  mutate(across(c(.fitted, .resid), round)) |>
  arrange(desc(abs(.resid))) |>
  head(3)
# Las tres personas tienen ingresos reales menores a los predichos por el
# modelo, por lo que presentan residuos negativos de gran magnitud.
# Esto indica que el modelo sobreestima sus ingresos.

#2
augment(m2) |>
  summarise(
    cook_max = max(.cooksd, na.rm = TRUE),
    hat_max = max(.hat, na.rm = TRUE))

augment(m2) |>
  slice_max(.cooksd, n = 3) |> 
  select(.rownames, ingreso, educ, genero, .cooksd)  

augment(m2) |>
  slice_max(.hat, n = 3) |> 
  select(.rownames, ingreso, educ, genero, .hat)   

#3
m2_sin <- casen |>
  filter(row_number() != 43 ) |> 
  lm(ingreso ~ educ + experiencia + genero, data = _)

bind_rows(todos = tidy(m2), sin_43 = tidy(m2_sin), .id = "muestra") |> 
  filter(term == "educ") |>
  select(muestra, estimate, std.error, p.value)
# sin la observación de mayor distancia de Cook, el coeficiente de educación
# aumenta en 2.972. Esto indica que, sin esa observación, la relación estimada
# entre educación e ingreso es mayor.

### A6. Interpretación de la Parte A
# En el modelo con controles, cada año adicional de educación se asocia con
# $42246 más de ingreso mensual, en promedio, manteniendo experiencia y
# género constantes (IC 95 %: $29487 a $55004).

# El coeficiente de `generoM` es $55686. Esto significa que los hombres tienen
# un ingreso mensual estimado $55.686 mayor que la categoría base que es femenino,
# manteniendo las demás variables constantes. Sin embargo, esta diferencia
# no es estadísticamente significativa.

# El coeficiente de generoM es grande, pero no significativo.
# El género presenta una diferencia estimada de $55.686 mensuales, lo que
# podría ser relevante. Sin embargo, como el coeficiente no
# es estadísticamente significativo, no podemos asegurar que exista una
# diferencia real de ingresos entre los géneros en la población.
# Como la muestra es pequeña (n = 55), puede que no haya suficientes
# observaciones para detectar con claridad la diferencia de ingresos
# según género. Por eso, aunque el coeficiente es grande, no es
# estadísticamente significativo y no podemos asegurar que exista una
# diferencia real en los ingresos entre hombres y la categoría de referencia.
 
## Parte B — ENE:¿Existen diferencias en la tasa de desocupación según el grupo etario de las 
# personas en Chile durante el trimestre móvil mayo-julio de 2026?

### B1. Declara tu pregunta
# ¿Existen diferencias en la tasa de desocupación según el grupo etario y genero de las 
# personas en Chile durante el trimestre móvil mayo-julio de 2026?

### B2. Prepara la base
ene <-  read.csv2("data/raw/ene-2026-06-mjj.csv")
ene_procesada <- read.csv("data/processed/ene_a3.csv")

### B3. Al menos dos modelos
desocupacion_edad <- select(ene, edad, activ, sexo, starts_with("fact"))

desocupacion <- desocupacion_edad |>
  mutate(
    sexo = case_when(
      sexo == 1 ~ "Hombre",
      sexo == 2 ~ "Mujer"
    ),
    grupo_edad = case_when(
      edad >= 15 & edad <= 24 ~ "15-24 años",
      edad >= 25 & edad <= 34 ~ "25-34 años",
      edad >= 35 & edad <= 44 ~ "35-44 años",
      edad >= 45 & edad <= 54 ~ "45-54 años",
      TRUE                    ~ "55 años o más"
    ),
    situacion = case_when(
      activ == 1 ~ "Ocupado",
      activ == 2 ~ "Desocupado",
      activ == 3 ~ "Inactivo",
      TRUE       ~ "Fuera de edad de trabajar"  
    ),
    desocupado = ifelse(activ == 2, 1, 0)
  )

desocupacion|>
  filter(!is.na(grupo_edad), !is.na(activ), !is.na(fact_cal), !is.na(sexo), !is.na(desocupado)) |>
  group_by(grupo_edad, sexo) |>
  summarise(
    N = n(),
    desocupados = sum(fact_cal[activ == 2]),
    fuerza_trabajo = sum(fact_cal[activ %in% c(1, 2)]),
    tasa_desocupacion = desocupados / fuerza_trabajo * 100 
  )|> 
  arrange(desc(desocupados))

dir.create("data/processed", showWarnings = FALSE)
write.csv(desocupacion, "data/processed/ene_a3.csv", row.names = FALSE)
file.exists("data/processed/ene_a3.csv")

ene_procesada <- read.csv("data/processed/ene_a3.csv")
names(ene_procesada)

ene_modelo <- ene_procesada |>
  filter(
    activ %in% c(1, 2)
  ) |>
  mutate(
    desocupado = ifelse(activ == 2, 1, 0),
    sexo = factor(sexo)
  )

m1_ene <- lm (desocupado ~ edad, data = ene_modelo, weights = fact_cal)
  
m2_ene <- lm (desocupado ~ edad + sexo, data = ene_modelo, weights = fact_cal)

### B4. Extrae y compara con `broom` + `dplyr`
modelos <- list(simple = m1_ene, controles = m2_ene, nuevo = m3_ene)

# 1
bind_rows(simple    = tidy(modelos$simple),
          controles = tidy(modelos$controles), .id = "modelos") |>
  filter(term == "edad") |>
  select(modelos, term, estimate, std.error, p.value)

#2
glance(m1_ene) |> select(r.squared, adj.r.squared, sigma, AIC, nobs)
glance(m2_ene) |> select(r.squared, adj.r.squared, sigma, AIC, nobs)

#3
m3_ene <- lm(desocupado ~ edad * sexo, data = ene_modelo, weights = fact_cal)

bind_rows(simple    = tidy(modelos$simple),
          controles = tidy(modelos$controles),
          nuevo     = tidy(modelos$nuevo), .id = "modelos") |>
  filter(term == "edad") |>
  select(modelos, term, estimate, std.error, p.value)

lapply(modelos, glance) |>
  bind_rows(.id = "modelos") |>
  select(modelos, r.squared, adj.r.squared, sigma, AIC, nobs)


### B5. Sensibilidad

# sin ponderar
ene_sin_ponderar <- lm(
  desocupado ~ edad + sexo,
  data = ene_modelo
)

# con ponderación
ene_con_ponderar <- lm(
  desocupado ~ edad + sexo,
  data = ene_modelo,
  weights = fact_cal
)
# Al comparar el modelo con y sin ponderación, el coeficiente de la edad pasó
# de 0.00272 a -0,002684, una variación de 0,000036. Esto significa que la 
# asociación entre edad y desocupación se mantiene negativa, pero se vuelve 
# un poco más cercana a cero al utilizar los factores de expansión. En ambos 
# modelos, un año adicional de edad se asocia con una disminución de aproximadamente 
# 0,27% en la probabilidad estimada de desocupación, manteniendo constante
# el sexo. Por lo tanto, la decisión de ponderar modifica muy poco el 
# resultado principal.


### B6. Interpretación de la Parte B
# El modelo simple analiza la asociación entre edad y desocupación, el segundo controla por sexo.
# El modelo nuevo agrega la interacción educ * sexo para evaluar diferencias según el sexo.
# El coeficiente de edad pasa de -0,00270 a -0,00268 y luego a -0,00252.
# Esto equivale a disminuciones de 0,270, 0,268 y 0,252 puntos porcentuales por año.
# La asociación se mantiene negativa y cambia poco al agregar controles.
# Aunque es estadísticamente significativa, la magnitud del efecto de la edad es pequeña.
# El R² aumenta de 0,0153 a 0,0157, por lo que los modelos explican poco de las diferencias.
# Una limitación es que lm() con ponderaciones no considera el diseño muestral de la ENE.
# Otra limitación es que pueden faltar variables relevantes, como experiencia laboral.
# Por ello, los resultados muestran asociaciones, pero no permiten afirmar causalidad.

## Parte C — Comunicar
# 1
modelsummary(list(simple = m1_ene, controles = m2_ene, nuevo = m3_ene),
             stars   = TRUE,
             gof_map = c("nobs", "r.squared"))
# 2
dir.create("data/processed", showWarnings = FALSE)
modelsummary(modelos, output = "data/processed/t3_tabla_ene.csv")
tidy(m2_ene, conf.int = TRUE) |>
  write.csv("data/processed/t3_coeficientes_casen.csv", row.names = FALSE)

