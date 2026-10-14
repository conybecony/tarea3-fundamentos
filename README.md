# tarea3-fundamentos

# T3 — Regresión económica: CASEN y ENE
- **Parte A — CASEN.** La ecuación de ingresos de la Semana 8, con todo el flujo de
  `broom` + `dplyr`. Es igual para todo el curso: se evalúa que domines el flujo.
- **Parte B — ENE.** Tu propia pregunta sobre el mercado laboral chileno, con la
  base que limpiaste en la A3. Se evalúa tu juicio: qué modelo, qué controles,
  cómo se lee y qué **no** se puede concluir.

Pregunta parte B: ¿Existen diferencias en la tasa de desocupación según la edad
y genero de las personas en Chile durante el trimestre móvil mayo-julio de 2026?
La desocupación se asocia negativamente con la edad y es mayor en mujeres.
Las diferencias por edad y género son pequeñas y no implican causalidad.
Los resultados sugieren diferencias en la desocupación en Chile durante mayo-julio de 2026.

## Datos

Archivos en `data/raw/casen_reducido.csv` 
            `data/raw/ene-2026-06-mjj.csv`
            `data/processed/ene_a3.csv`
            
Datos creados en el final del script
            `data/processed/t3_tabla_ene.csv`
            `data/processed/t3_coeficientes_casen.csv`

## Cómo correrlo

Abrir `tarea3-fundamentos.Rproj` y ejecutar `scripts/tarea_t3.R`.

## Estructura

```
 tarea2-fundamentos/
   ├── tarea2-fundamentos.Rproj
   ├── data/
       ├── raw/ 
       └── processed/
   ├── scripts/
   └── README.md
```
## Autor

Constanza Pinilla — Octubre 2026

## Declaración de autoría y uso de IA
- Herramienta utilizada: Chat GPT (GPT-6.)
- Para qué la usé: la usé para ver porque me daba error los modelos y veríficar 
si las formulas que hacía estaban bien.
- Qué hice yo: modifique la pregunta de la anterior tarea, los controles y escribí 
la interpretación, entre otras cosas.
- Verificación: confirmo que entiendo y puedo explicar todo lo que entrego.
