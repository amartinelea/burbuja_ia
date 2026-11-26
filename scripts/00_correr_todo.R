# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/00_correr_todo.R
# Objetivo:       Corre todo el proyecto desde cero, en orden, con source().
#                 Cada script se ejecuta en un entorno propio (local = new.env())
#                 para que su rm(list = ls()) no borre las variables de este
#                 script. Al final deja un resumen en output/00_bitacora_corrida.csv.
#
# Uso:            abrir boom_ia.Rproj (la carpeta de trabajo debe ser la raiz
#                 del proyecto) y correr este script completo.
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          24/11/2026
#
# Requisitos antes de correr:
#   - input/raw/consulta_electro_imp.csv y consulta_electro_exp.csv (ANAM)
#   - input/Catalogo LIGIE ... NICO ... .xlsx, input/canasta_ia_subpartidas.csv,
#     input/canasta_ia_ajustes_fraccion.csv, input/claves_pedimento.csv,
#     input/empresas_ia_clasificacion.csv
#   - .env con CENSUS_API_KEY y BANXICO_TOKEN
#   - Python 3 con pandas, numpy y matplotlib (graficas del 24 y slides)
#   - Internet: 01 (SEC y Yahoo), 06 (Census), 19 (FRED y otros), slides (Banxico)
# _____________________________________________________________________________

# PREAMBULO ___________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8")

# ---- PARAMETROS ----------------------------------------------------------------
correr_descargas      <- FALSE  # 01 y 19: bajan empresas, mercado y macro (FALSE si ya se bajaron hoy)
correr_complementarias <- TRUE  # 08 y 14 a 22: robustez y pruebas de la version anterior (lentas)
correr_slides_contexto <- TRUE  # slides_contexto/1 y 2: graficas de contexto (revisar FECHA_CORTE)
desde_script          <- "02"   # p. ej. "07" para retomar desde ese script si algo fallo
# ----------------------------------------------------------------------------------

if (!file.exists("boom_ia.Rproj") || !dir.exists("scripts")) {
  stop("Corre este script parado en la raiz de boom_ia (abre boom_ia.Rproj). Carpeta actual: ", getwd())
}

# obligatorio = TRUE: si falla, se detiene la corrida (los siguientes lo necesitan).
# obligatorio = FALSE: si falla, avisa y sigue.
plan <- data.frame(rbind(
  # 1. Datos y limpieza
  c("01_construye_datos_empresas.R",        "descargas",      TRUE),
  c("02_prepara_panel_anam.R",              "base",           TRUE),
  c("03_equivalencia_clasificacion.R",      "base",           TRUE),
  c("04_verifica_cobertura_catalogo.R",     "base",           FALSE),
  c("05_panel_exportaciones_anam.R",        "base",           TRUE),
  c("06_census_eua_por_producto.R",         "base",           TRUE),
  # 2. Etapa 1 (theta) y escenarios
  c("07_analisis_choque_ia_anam.R",         "base",           TRUE),
  c("08_robustez_calibracion.R",            "complementaria", FALSE),
  # 3. Canal 1: importaciones
  c("09_robustez_census.R",                 "base",           TRUE),
  c("10_escenarios_census.R",               "base",           TRUE),
  # 4. Canal 2: exportaciones
  c("11_eta_exportaciones.R",               "base",           TRUE),
  c("12_escenarios_exportaciones.R",        "base",           TRUE),
  # 5. Contexto
  c("13_industria_hoy.R",                   "base",           FALSE),
  # 6. Pruebas complementarias
  c("14_sectores_cercanos_ia.R",            "complementaria", FALSE),
  c("15_pruebas_identificacion_paso2.R",    "complementaria", FALSE),
  c("16_margenes_chen_roth.R",              "complementaria", FALSE),
  c("17_proyecciones_locales.R",            "complementaria", FALSE),
  c("18_bootstrap_paso2.R",                 "complementaria", FALSE),
  c("19_descarga_variables_alternativas.R", "descargas",      FALSE),
  c("20_pruebas_variables_alternativas.R",  "complementaria", FALSE),
  c("21_confirma_pedidos_computadoras.R",   "complementaria", FALSE),
  c("22_valores_unitarios.R",               "complementaria", FALSE),
  # 7. Graficas y revision
  c("23_graficas_escenarios.R",             "base",           FALSE),
  c("24_graficas_resultados.R",             "base",           FALSE),
  c("25_revisa_outputs.R",                  "base",           FALSE),
  # 8. Slides de contexto (importaciones y exportaciones, pesos reales)
  c("slides_contexto/1_tabulados_importaciones.R", "slides", FALSE),
  c("slides_contexto/2_graficas_importaciones.R",  "slides", FALSE)
), stringsAsFactors = FALSE)
names(plan) <- c("script", "grupo", "obligatorio")
plan$obligatorio <- as.logical(plan$obligatorio)

plan$correr <- with(plan, grupo == "base" |
                      (grupo == "descargas" & correr_descargas) |
                      (grupo == "complementaria" & correr_complementarias) |
                      (grupo == "slides" & correr_slides_contexto))
# 19 descarga variables que solo usan 20 a 22
plan$correr[plan$script == "19_descarga_variables_alternativas.R"] <- correr_descargas && correr_complementarias
if (!is.null(desde_script)) {
  i0 <- which(startsWith(plan$script, desde_script))[1]
  if (is.na(i0)) stop("No encontre el script ", desde_script)
  plan$correr[seq_len(i0 - 1)] <- FALSE
}

faltan <- plan$script[plan$correr & !file.exists(file.path("scripts", plan$script))]
if (length(faltan)) stop("No encontre: ", paste(faltan, collapse = ", "))

# CORRIDA _____________________________________________________________________

bitacora <- data.frame()
inicio_total <- Sys.time()
for (i in which(plan$correr)) {
  s <- plan$script[i]
  message("\n==== ", s, " | ", format(Sys.time(), "%H:%M:%S"))
  t0 <- Sys.time()
  error <- tryCatch({
    source(file.path("scripts", s), local = new.env(), encoding = "UTF-8")
    NA_character_
  }, error = function(e) conditionMessage(e))
  minutos <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  bitacora <- rbind(bitacora, data.frame(script = s, minutos = minutos,
                                         estado = if (is.na(error)) "ok" else "error",
                                         mensaje = ifelse(is.na(error), "", error)))
  write.csv(bitacora, file.path("output", "00_bitacora_corrida.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  if (is.na(error)) {
    message("==== ", s, " termino en ", minutos, " min")
  } else if (plan$obligatorio[i]) {
    print(bitacora)
    stop("Fallo ", s, " (obligatorio): ", error,
         "\nCorrige y vuelve a correr con desde_script <- \"", substr(s, 1, 2), "\"")
  } else {
    warning("Fallo ", s, " (se sigue con los demas): ", error, immediate. = TRUE)
  }
}

message("\n==== Listo en ", round(as.numeric(difftime(Sys.time(), inicio_total, units = "mins")), 1), " min")
print(bitacora)
message("Bitacora en output/00_bitacora_corrida.csv")
