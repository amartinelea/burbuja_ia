# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/24_graficas_resultados.R
# Objetivo:       Graficas de resultados para la presentacion con pabloplot (Python), como
#                 proceso aparte: Sys.setenv() para pasar rutas + system() para correr
#                 24_graficas_resultados.py. No usa reticulate.
#                 01 impacto total por escenario (B a D): canasta de IA + efecto de arrastre
#                 02 eta del paso 2 por prueba (script 09)
#                 03 epsilon por nivel (capex -> lo que EUA importa)
#                 04 impacto por eslabon (una grafica por escenario: B, C, D)
#                 05 escenarios de anexo (E y F): total y nucleo
#                 06 version anterior del paso 2: eta del nucleo (anexo 8)
#                 Las graficas no llevan titulo: el titulo y la lectura van en la lamina.
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          18/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Hace las graficas de resultados de la presentacion (impacto por escenario
#     y por eslabon, anexos).
#   - Llama a Python como proceso aparte; necesita Python 3 con pandas, numpy y
#     matplotlib.
#   - Si Python no esta en el PATH, poner PYTHON_PATH en el .env.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Requiere: 10 (output/choque_ia_census), 09 (output/census_robustez),
#             08 (output/robustez, solo para la 06); Python 3 con pandas, numpy y
#             matplotlib; funciones de pabloplot y fuentes Montserrat
#   Salidas : output/graficas_resultados/*.png y *_scour.svg
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

paquetes <- c("dplyr", "stringr", "purrr", "readr")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base      <- getwd()                                         # raiz de boom_ia (abrir boom_ia.Rproj)
dir_funciones <- file.path(dir_base, "scripts", "1_funciones")    # barras_horizontales.py y funciones.py
dir_fuentes   <- file.path(dir_base, "scripts", "0_fonts")        # Montserrat
script_py     <- file.path(dir_base, "scripts", "24_graficas_resultados.py")
salida        <- "graficas_resultados"     # carpeta dentro de output/
version       <- "principal"               # version_beta del script 19

# ---- Validaciones -----------------------------------------------------------
for (d in c(dir_funciones, dir_fuentes))
  if (!dir.exists(d)) stop("No existe ", d, ". Ajusta dir_funciones / dir_fuentes.")
if (!file.exists(script_py)) stop("No existe ", script_py)
for (f in c("output/choque_ia_census/04_resultado_total.csv", "output/census_robustez/01_pruebas.csv"))
  if (!file.exists(f)) stop("No existe ", f, ": corre antes los scripts 09 y 10.")

# ---- Python -----------------------------------------------------------------
# Orden: PYTHON_PATH del .env, luego python3 y python del PATH (sin el alias de
# la tienda de Windows)
leer_env <- function(ruta = ".env") {
  if (!file.exists(ruta)) return(character(0))
  lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
  lineas <- str_trim(lineas[str_detect(lineas, "^\\s*[A-Za-z_]+\\s*=")])
  setNames(str_remove_all(str_remove(lineas, "^[^=]+=\\s*"), "^[\"']|[\"']$"), str_trim(str_remove(lineas, "=.*$")))
}
encontrar_python <- function() {
  candidatos <- c(leer_env()["PYTHON_PATH"], Sys.getenv("PYTHON_PATH"), Sys.which("python3"), Sys.which("python"), Sys.which("py"))
  candidatos <- unique(candidatos[!is.na(candidatos) & candidatos != ""])
  candidatos <- candidatos[!str_detect(candidatos, "WindowsApps")]
  for (py in candidatos) {
    v <- suppressWarnings(tryCatch(system2(py, "--version", stdout = TRUE, stderr = TRUE), error = function(e) ""))
    if (any(str_detect(v, "Python 3"))) return(normalizePath(py, winslash = "/", mustWork = FALSE))
  }
  stop("No encontre Python 3. Instalalo o pon su ruta en .env como PYTHON_PATH=...")
}
python <- encontrar_python()
message("Python: ", python)
faltan <- keep(c("pandas", "numpy", "matplotlib"),
               ~ system2(python, c("-c", shQuote(paste("import", .x))), stdout = FALSE, stderr = FALSE) != 0)
if (length(faltan)) stop("A Python le faltan paquetes: ", paste(faltan, collapse = ", "),
                         ". Instalalos con: ", python, " -m pip install ", paste(faltan, collapse = " "))

# ---- Graficas ---------------------------------------------------------------
Sys.setenv(
  BIA_RAIZ          = normalizePath(dir_base, winslash = "/"),
  BIA_DIR_FUNCIONES = normalizePath(dir_funciones, winslash = "/"),
  BIA_DIR_FUENTES   = normalizePath(dir_fuentes, winslash = "/"),
  BIA_DIR_19        = "output/choque_ia_census",
  BIA_DIR_18        = "output/census_robustez",
  BIA_DIR_04        = "output/robustez",
  BIA_SALIDA        = salida,
  BIA_VERSION       = version,
  PYTHONUTF8        = "1",          # acentos bien en Windows
  PYTHONIOENCODING  = "utf-8",
  MPLBACKEND        = "Agg"
)
message("Generando graficas...")
estado <- system(paste(shQuote(python), shQuote(script_py)))
if (estado != 0) stop("Python termino con error (codigo ", estado, "). Revisa el mensaje de arriba.")

pngs <- list.files(file.path("output", salida), pattern = "\\.png$")
message("Listo: ", length(pngs), " graficas en output/", salida, "/")
print(pngs)
