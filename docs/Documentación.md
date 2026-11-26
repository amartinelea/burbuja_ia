# Documentación - boom_ia

Para ver el esquema general, vea el [README](../README.md) del proyecto.

## Estructura del proyecto

Los scripts están numerados en el orden en que se corren (renumerados el 24/11/2026; la numeración anterior está respaldada en `temp/respaldo_scripts_2026-11-24/`). Para correr todo desde cero: abrir `boom_ia.Rproj` y correr `scripts/00_correr_todo.R`. Ese script llama a cada uno con `source()` en orden, se detiene si falla uno obligatorio y deja la bitácora en `output/00_bitacora_corrida.csv`. Si algo falla, se corrige y se retoma con `desde_script <- "NN"`.

### 1. Datos y limpieza

| Script | Qué hace | Salidas |
|---|---|---|
| 01_construye_datos_empresas.R | Baja estados financieros de la SEC (capex, flujo operativo, deuda), precios de Yahoo Finance y variables macro (tasas, VIX, S&P 500). Arma índices de valuación en bolsa por grupo de empresas. Necesita internet. | datos/empresas/ |
| 02_prepara_panel_anam.R | Limpia la consulta de importaciones de la ANAM: deja una fila por celda (la base trae una línea por contribución), convierte a dólares con el tipo de cambio de cada línea, conserva el flujo físico (A1, C1, IN, AF) y quita el mes en curso. También arma la recaudación por fracción. | output/panel/ |
| 03_equivalencia_clasificacion.R | Clasifica cada NICO en la canasta de bienes de IA (núcleo, infraestructura, mixta, resto) con la regla por subpartida y los ajustes por fracción. | input/canasta_ia_fracciones.csv, output/choque_ia/equivalencia_clasificacion.xlsx |
| 04_verifica_cobertura_catalogo.R | Compara la consulta de la ANAM contra el catálogo LIGIE: qué fracciones de los capítulos 84, 85 y 90 vienen y cuáles faltan. Diagnóstico. | output/cobertura_catalogo/ |
| 05_panel_exportaciones_anam.R | Limpia la consulta de exportaciones (A1, C1 y RT; valor comercial entre tipo de cambio) y la compara con Census. | output/panel_exportaciones/ |
| 06_census_eua_por_producto.R | Baja de Census las importaciones de EUA por subpartida y país (requiere CENSUS_API_KEY). Calcula la demanda de EUA sin México y las compras a México. Primeras regresiones por producto. Solo vuelve a bajar los últimos meses. | datos/census/, output/census/ |
| 06b_diagnostico_census.R | Opcional. Solo si la descarga del 06 falla: prueba qué tamaño de consulta acepta la API. No lo corre 00_correr_todo. | output/census/00_diagnostico_api.csv |

### 2. Etapa 1 y escenarios

| Script | Qué hace | Salidas |
|---|---|---|
| 07_analisis_choque_ia_anam.R | Etapa 1: θ, respuesta del capex de los hiperescaladores a su valuación en bolsa (panel con razón capex/flujo, Driscoll y Kraay). Define los escenarios A a F. También trae la cadena agregada anterior. | output/choque_ia/ |
| 08_robustez_calibracion.R | Corre el 07 con varias especificaciones (controles, muestras) y calibra los escenarios por fecha. Lento. | output/robustez/ |

### 3. Canal 1: importaciones de México

| Script | Qué hace | Salidas |
|---|---|---|
| 09_robustez_census.R | Etapa 2b del canal 1: η por subpartida (Poisson, efectos fijos y tendencia) y sus pruebas: placebo, sin cada año, sin las 5 mayores, rezagos, por partida. | output/census_robustez/ |
| 10_escenarios_census.R | Etapa 2a (ε por nivel con subpartidas de serie completa) y etapa 3: simulación de escenarios con intervalos Monte Carlo, valor y recaudación. También calcula la prueba directa para el canal 2. | output/choque_ia_census/ |

### 4. Canal 2: exportaciones de México a EUA

| Script | Qué hace | Salidas |
|---|---|---|
| 11_eta_exportaciones.R | Etapa 2b del canal 2: η de exportaciones a EUA por partida y por nivel, con pruebas y la serie espejo de Census. | output/eta_exportaciones/ |
| 12_escenarios_exportaciones.R | Etapa 3 del canal 2: escenarios sobre lo exportado a EUA. Principal: η por nivel; también η única, prueba directa con el capex y Census. | output/choque_ia_exportaciones/ |

### 5. Contexto

| Script | Qué hace | Salidas |
|---|---|---|
| 13_industria_hoy.R | Cifras del punto de partida: capex, valuación, participación de México en Census, importaciones y recaudación de la ANAM. | output/industria_hoy/ |

### 6. Pruebas complementarias (versión agregada anterior)

| Script | Qué hace | Salidas |
|---|---|---|
| 14_sectores_cercanos_ia.R | Respuesta de otros sectores a la valuación en bolsa. | output/sectores_ia/ |
| 15_pruebas_identificacion_paso2.R | Pruebas de identificación de la versión agregada. | output/identificacion/ |
| 16_margenes_chen_roth.R | Ceros y margen extensivo (Chen y Roth). | output/margenes/ |
| 17_proyecciones_locales.R | Proyecciones locales mensuales del núcleo (las usa el 23). | output/proyecciones_locales/ |
| 18_bootstrap_paso2.R | Bootstrap por bloques de trimestres. | output/bootstrap_paso2/ |
| 19_descarga_variables_alternativas.R | Baja variables alternativas de inversión (FRED y otras). Necesita internet. | datos/alternativas/ |
| 20_pruebas_variables_alternativas.R | η con las variables alternativas. | output/alternativas/ |
| 21_confirma_pedidos_computadoras.R | Confirmación con pedidos de computadoras. | output/confirmacion/ |
| 22_valores_unitarios.R | Separa precio y cantidad. | output/valores_unitarios/ |

### 7. Gráficas y revisión

| Script | Qué hace | Salidas |
|---|---|---|
| 23_graficas_escenarios.R | Gráficas de escenarios en ggplot (cadena census por defecto). | output/graficas_escenarios/ |
| 24_graficas_resultados.R (+ .py) | Gráficas de la presentación con pabloplot (llama a Python con Sys.setenv() + system()). | output/graficas_resultados/ |
| 25_revisa_outputs.R | Imprime un resumen de todas las salidas para revisarlas. | output/revision_outputs.txt |

### 8. Slides de contexto (scripts/slides_contexto/)

| Script | Qué hace | Salidas |
|---|---|---|
| 1_tabulados_importaciones.R | Tabulados y cifras de tarjetas de importaciones y exportaciones, en millones de dólares corrientes por omisión (MONEDA; en pesos reales usa el INPC de Banxico), promedio diario al día de corte (FECHA_CORTE). | output/slides_contexto/<ddmmaa>/ |
| 2_graficas_importaciones.R (+ .py) | Gráficas de las slides con pabloplot. | output/slides_contexto/<ddmmaa>/<flujo>/figuras/ |

## Insumos

- input/raw/consulta_electro_imp.csv y consulta_electro_exp.csv (ANAM)
- input/Catálogo LIGIE 2025 NICO.xlsx, canasta_ia_subpartidas.csv, canasta_ia_ajustes_fraccion.csv, claves_pedimento.csv, empresas_ia_clasificacion.csv
- .env con CENSUS_API_KEY y BANXICO_TOKEN (nunca en los scripts)

## Entregables y resultados

- nota_metodologica_burbuja_ia_5.docx, burbuja_ia_v11.pptx

## Notas y consideraciones

- El 02 quita el mes en curso según la fecha del equipo: si se corre en un mes posterior, revisar que el último mes de la consulta esté completo.
- La consulta de importaciones trae todas las formas de pago; la recaudación usa solo los pagos en efectivo (forma de pago 0).

## Referencias
