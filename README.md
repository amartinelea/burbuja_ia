# boom_ia

Efecto de un ajuste en la burbuja de la inteligencia artificial (IA) sobre el comercio exterior de electrónicos de México.

## Objetivo

Estimar cuánto cambiarían las exportaciones de electrónicos de México a Estados Unidos y las importaciones de electrónicos de México (y la recaudación de IVA e IGI que generan) si la valuación en bolsa de los hiperescaladores cae, se mantiene o sube.

El modelo responde tres preguntas en cadena:

| Etapa | Pregunta | Elasticidad | Script |
|---|---|---|---|
| 1 | Si cae la bolsa, ¿cuánto recortan su gasto de capital los hiperescaladores? | θ | 07 |
| 2a | Si invierten menos, ¿cuánto dejan de importar en equipo de EUA? | ε | 10 |
| 2b | Si EUA compra menos, ¿cuánto cambia el comercio de México? | η | 09 (importaciones), 11 (exportaciones) |
| 3 | Simulación de escenarios (A a F) con intervalos Monte Carlo | | 10 (importaciones), 12 (exportaciones) |

## Requisitos

**R** (4.3 o más reciente) con estos paquetes:

```r
install.packages(c("tidyverse", "data.table", "fixest", "sandwich", "glue", "crayon",
                   "httr", "jsonlite", "readxl", "writexl", "zoo", "stringi"))
```

**Python 3** con `pandas`, `numpy` y `matplotlib` para las gráficas (pabloplot, en `scripts/1_funciones/`). R llama a Python como proceso aparte (`Sys.setenv()` + `system()`); no se usa reticulate. `scour` es opcional (solo optimiza los SVG).

```bash
python3 -m pip install pandas numpy matplotlib scour
```

**Internet** para las descargas: SEC EDGAR y Yahoo Finance (01), Census (06), FRED y Banxico (01 y 19).

El código corre igual en Mac y Windows: los scripts no llevan acentos en comentarios ni en cadenas y leen todo en UTF-8.

## Claves de API (archivo `.env`)

Cada persona necesita sus propias claves. Se guardan en un archivo `.env` en la raíz del proyecto, que **no se sube al repositorio** (está en `.gitignore`). **Nunca se escriben las claves en los scripts.**

```
CENSUS_API_KEY=tu_clave_del_census
BANXICO_TOKEN=tu_token_de_banxico
```

| Clave | Para qué se usa | Cómo obtenerla |
|---|---|---|
| `CENSUS_API_KEY` | Importaciones de EUA por subpartida y país (script 06) | Solicitarla en https://api.census.gov/data/key_signup.html. Llega por correo y hay que activarla con el enlace que mandan. |
| `BANXICO_TOKEN` | Tipo de cambio y series macro (01 y 19); INPC solo si las slides de contexto se corren en pesos | Generarlo en el portal de la API del SIE de Banxico: https://www.banxico.org.mx/SieAPIRest/service/v1/token |

Los scripts que se identifican ante portales externos (por ejemplo, el user agent de la SEC) usan un correo personal, no el institucional.

## Insumos (no se suben al repositorio)

En `input/`:

- `raw/consulta_electro_imp.csv` y `raw/consulta_electro_exp.csv`: consultas de la ANAM (importaciones y exportaciones de electrónicos, todas las formas de pago).
- `Catálogo LIGIE 2025 NICO - <fecha>.xlsx`: catálogo de fracciones y NICO.
- `canasta_ia_subpartidas.csv` y `canasta_ia_ajustes_fraccion.csv`: regla de la canasta de bienes de IA por subpartida y sus excepciones por fracción o NICO.
- `claves_pedimento.csv` y `empresas_ia_clasificacion.csv`.

## Cómo correr el proyecto

1. Abrir `boom_ia.Rproj` (la carpeta de trabajo debe ser la raíz del proyecto).
2. Revisar que estén los insumos y el `.env`.
3. Abrir `scripts/00_correr_todo.R`, ajustar los parámetros del inicio y correrlo completo:

| Parámetro | Qué hace |
|---|---|
| `correr_descargas` | `TRUE` baja de nuevo SEC, Yahoo, FRED y Banxico (01 y 19). `FALSE` usa lo ya descargado. |
| `correr_complementarias` | `TRUE` corre las pruebas de robustez lentas (08 y 14 a 22). |
| `correr_slides_contexto` | `TRUE` genera las slides de contexto (revisar antes `FECHA_CORTE` en `1_tabulados_importaciones.R`). |
| `desde_script` | Para retomar desde un script si algo falló, por ejemplo `"07"`. |

El script se detiene si falla uno obligatorio y deja la bitácora en `output/00_bitacora_corrida.csv`. Al terminar, `25_revisa_outputs.R` deja un resumen de todas las salidas en `output/revision_outputs.txt`.

El 06 solo vuelve a bajar del Census los últimos 9 meses (`refrescar_meses`); lo demás lo toma de `datos/census/`.

## Qué hace cada script

### 1. Datos y limpieza

| Script | Qué hace |
|---|---|
| `01_construye_datos_empresas.R` | Baja estados financieros de la SEC (gasto de capital, flujo operativo, deuda, ingresos), precios de Yahoo Finance y variables macro. Arma índices de valuación en bolsa por grupo de empresas. |
| `02_prepara_panel_anam.R` | Limpia la consulta de importaciones: una fila por celda (la base trae una línea por contribución), valor en aduana convertido a dólares con el tipo de cambio de cada operación, solo flujo físico (A1, C1, IN y AF). Arma la recaudación de IVA e IGI pagada en efectivo (forma de pago 0). |
| `03_equivalencia_clasificacion.R` | Clasifica cada NICO en la canasta: núcleo, infraestructura, mixta o resto de electrónicos, y su eslabón. |
| `04_verifica_cobertura_catalogo.R` | Diagnóstico: qué fracciones de los capítulos 84, 85 y 90 trae la consulta de la ANAM. |
| `05_panel_exportaciones_anam.R` | Limpia la consulta de exportaciones (A1, C1 y RT; valor comercial en dólares). |
| `06_census_eua_por_producto.R` | Baja de Census las importaciones de EUA por subpartida y país y calcula la demanda de EUA sin México. |
| `06b_diagnostico_census.R` | Opcional, solo si falla la descarga del 06. No lo corre `00_correr_todo.R`. |

### 2. Etapa 1 y escenarios

| Script | Qué hace |
|---|---|
| `07_analisis_choque_ia_anam.R` | Etapa 1: θ, respuesta del gasto de capital de los hiperescaladores a su valuación en bolsa. Define los escenarios A a F. |
| `08_robustez_calibracion.R` | Robustez de la etapa 1 y calibración de los escenarios por fecha (lento). |

### 3. Canal 1: importaciones de México

| Script | Qué hace |
|---|---|
| `09_robustez_census.R` | Etapa 2b: η de importaciones por subpartida y sus pruebas (placebo, sin cada año, sin las 5 mayores, adelanto y rezago). |
| `10_escenarios_census.R` | Etapa 2a (ε por nivel) y etapa 3: escenarios con intervalos, en dólares y pesos, con IVA e IGI. |

### 4. Canal 2: exportaciones de México a EUA

| Script | Qué hace |
|---|---|
| `11_eta_exportaciones.R` | Etapa 2b: η de exportaciones por partida y por nivel, con pruebas. |
| `12_escenarios_exportaciones.R` | Etapa 3: escenarios sobre las exportaciones a EUA. |

### 5. Contexto, pruebas complementarias y gráficas

| Script | Qué hace |
|---|---|
| `13_industria_hoy.R` | Cifras del punto de partida (gasto de capital, valuación, comercio, recaudación). |
| `14` a `22` | Pruebas complementarias de la versión anterior del modelo (sectores cercanos, variables alternativas, ceros, proyecciones locales, bootstrap, valores unitarios). |
| `23_graficas_escenarios.R` | Trayectorias por escenario. |
| `24_graficas_resultados.R` (+ `.py`) | Gráficas de resultados para la presentación (pabloplot). |
| `25_revisa_outputs.R` | Resumen de todas las salidas para revisión. |
| `diagnostico_*.R` | Diagnósticos puntuales (Data México 2024; valor en aduana contra valor en dólares). |

### 6. Slides de contexto (`scripts/slides_contexto/`)

| Script | Qué hace |
|---|---|
| `1_tabulados_importaciones.R` | Tabulados y textos de tarjetas de importaciones y exportaciones. Por omisión en millones de dólares corrientes (`MONEDA <- "usd"`); con `"mxn"`, en pesos reales. Las tarjetas reportan la mayor caída o, si no hay caídas, el mayor aumento; las listas van de mayor a menor cambio. |
| `2_graficas_importaciones.R` (+ `.py`) | Gráficas de las slides con pabloplot. |

Las salidas quedan en `output/slides_contexto/<ddmmaa>/<flujo>/`; los textos de las tarjetas están en `tabulados/cifras_tarjetas_<ddmmaa>.csv`.

## Convenciones

- **Términos:** gasto de capital (no capex en textos), valuación en bolsa, semiconductores (no chips), efecto de arrastre (no contagio), canasta de bienes de IA, niveles (núcleo, infraestructura, mixta, resto de electrónicos) y eslabones.
- **Moneda:** los modelos se estiman en dólares corrientes. Las cifras que vienen de la ANAM se reportan en dólares y, entre paréntesis, en pesos; el resto, solo en dólares.
- **Claves de pedimento:** importaciones definitivas A1 y C1, temporales IN y AF (IMMEX); exportaciones definitivas A1 y C1, retornos RT.
- **Recaudación:** solo pagos en efectivo (forma de pago 0); el IVA cubierto con crédito fiscal IMMEX no cuenta como ingreso.

## Más documentación

- [Documentación detallada](./docs/Documentación.md): salidas de cada script, insumos y notas.

