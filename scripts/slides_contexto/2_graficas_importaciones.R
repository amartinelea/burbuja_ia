# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/slides_contexto/2_graficas_importaciones.R
# Objetivo:       Genera las graficas de importaciones (PNG + SVG) desde R,
#                 llamando a las funciones de pabloplot en Python
#                 (scripts/1_funciones) como PROCESO APARTE:
#                 Sys.setenv() para pasar rutas y parametros + system() para
#                 correr scripts/slides_contexto/2_graficas_importaciones.py.
#                 Copia de proyecto_textiles_anam, apuntada a la carpeta
#                 slides_contexto de boom_ia. No usa reticulate.
#
#                 Se corre todo desde R (Positron), parado en la raiz del
#                 proyecto (abrir boom_ia.Rproj).
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          19/11/2026
#
# REQUISITOS:
#   - Correr antes scripts/slides_contexto/1_tabulados_importaciones.R (escribe el manifiesto).
#   - Python 3 con pandas, numpy y matplotlib. Si falta algo, el script avisa
#     y dice que instalar. (scour es opcional: solo optimiza los SVG.)
#   - Si Python no esta en el PATH, poner su ruta en el .env del proyecto:
#       PYTHON_PATH=C:/Users/<usuario>/AppData/Local/Programs/Python/Python312/python.exe
#
# SALIDAS: output/slides_contexto/<ddmmaa>/<flujo>/figuras/slide_XX_<seccion>_<descripcion>.png y
#          _scour.svg (mismo nombre que su tabulado en output/slides_contexto/<ddmmaa>/<flujo>/tabulados/)
# _____________________________________________________________________________

# PREAMBULO ___________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

paquetes <- c("tidyverse", "glue", "crayon")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- PARAMETROS ----
FECHA_CORTE   <- as.Date("2026-11-17")    # el mismo de 1_tabulados_importaciones.R
FLUJOS        <- c("importaciones", "exportaciones")
SOLO_SLIDES   <- NULL                     # p. ej. c(4, 6) para rehacer solo esas; NULL = todas
DIR_FUNCIONES <- "scripts/1_funciones"
SCRIPT_PY     <- "scripts/slides_contexto/2_graficas_importaciones.py"
# --------------------

SUFIJO <- format(FECHA_CORTE, "%d%m%y")

# VALIDACIONES ________________________________________________________________

RAIZ <- normalizePath(getwd(), winslash = "/")
if (!file.exists(file.path(RAIZ, SCRIPT_PY)) || !dir.exists(file.path(RAIZ, DIR_FUNCIONES))) {
    stop("Corre este script parado en la ra\u00edz de boom_ia (abre el .Rproj).\n",
         "  Carpeta actual: ", RAIZ)
}

# PYTHON ______________________________________________________________________
# Orden de busqueda: PYTHON_PATH del .env, luego python3 y python del PATH.
# En Windows se descarta el alias de Microsoft Store (WindowsApps), que no es
# un Python real.

leer_env <- function(ruta = ".env") {
    if (!file.exists(ruta)) return(character(0))
    lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
    lineas <- str_trim(lineas[str_detect(lineas, "^\\s*[A-Za-z_]+\\s*=")])
    setNames(str_remove_all(str_remove(lineas, "^[^=]+=\\s*"), "^[\"']|[\"']$"),
             str_trim(str_remove(lineas, "=.*$")))
}

encontrar_python <- function() {
    candidatos <- c(leer_env()["PYTHON_PATH"], Sys.getenv("PYTHON_PATH"),
                    Sys.which("python3"), Sys.which("python"), Sys.which("py"))
    candidatos <- unique(candidatos[!is.na(candidatos) & candidatos != ""])
    candidatos <- candidatos[!str_detect(candidatos, "WindowsApps")]
    for (py in candidatos) {
        version <- suppressWarnings(tryCatch(
            system2(py, "--version", stdout = TRUE, stderr = TRUE), error = function(e) ""))
        if (any(str_detect(version, "Python 3"))) return(normalizePath(py, winslash = "/", mustWork = FALSE))
    }
    stop("No encontr\u00e9 Python 3. Inst\u00e1lalo (python.org) o pon su ruta en .env como PYTHON_PATH=...")
}

PYTHON <- encontrar_python()
message("Python: ", PYTHON, " (", system2(PYTHON, "--version", stdout = TRUE, stderr = TRUE), ")")

faltan <- c("pandas", "numpy", "matplotlib") |>
    keep(~ system2(PYTHON, c("-c", shQuote(glue("import {.x}"))), stdout = FALSE, stderr = FALSE) != 0)
if (length(faltan) > 0) {
    stop("A Python le faltan paquetes: ", paste(faltan, collapse = ", "), ".\n",
         "  Inst\u00e1lalos desde la terminal de Positron con:\n  ",
         PYTHON, " -m pip install ", paste(faltan, collapse = " "))
}

# GRAFICAS ____________________________________________________________________
# Todo se pasa por variables de entorno; el .py no usa os.chdir() y guarda en
# output/slides_contexto/<ddmmaa>/figuras/ relativo a la raiz (el proceso hereda la carpeta
# de trabajo de R).

for (FLUJO in FLUJOS) {
    cat(bold$magenta(glue("\n######## {str_to_upper(FLUJO)} ########\n", .trim = FALSE)))
    CARPETA_TAB <- glue("output/slides_contexto/{SUFIJO}/{FLUJO}/tabulados/")
    MANIFIESTO  <- glue("{CARPETA_TAB}manifiesto_graficas.csv")
    EVENTOS     <- glue("{CARPETA_TAB}eventos_graficas.csv")
    if (!file.exists(MANIFIESTO)) {
        stop("No existe ", MANIFIESTO, ". Corre antes scripts/slides_contexto/1_tabulados_importaciones.R ",
             "con la misma FECHA_CORTE.")
    }

    Sys.setenv(
        IMP_RAIZ          = RAIZ,
        IMP_DIR_FUNCIONES = normalizePath(DIR_FUNCIONES, winslash = "/"),
        IMP_MANIFIESTO    = normalizePath(MANIFIESTO, winslash = "/"),
        IMP_EVENTOS       = normalizePath(EVENTOS, winslash = "/"),
        IMP_SOLO_SLIDES   = paste(SOLO_SLIDES, collapse = ","),
        IMP_EJE_Y         = readLines(glue("{CARPETA_TAB}eje_y.txt"), encoding = "UTF-8")[1],
        PYTHONUTF8        = "1",        # acentos bien en Windows
        PYTHONIOENCODING  = "utf-8",
        MPLBACKEND        = "Agg"
    )

    message("Generando gr\u00e1ficas...")
    estado <- system(paste(shQuote(PYTHON), shQuote(SCRIPT_PY)))

    if (estado != 0) {
        stop("Python termin\u00f3 con error (c\u00f3digo ", estado, "). Revisa el mensaje de arriba.")
    }

    # Verificacion: cada renglon del manifiesto debe tener su PNG
    manifiesto <- read_csv(MANIFIESTO, show_col_types = FALSE)
    if (!is.null(SOLO_SLIDES)) manifiesto <- filter(manifiesto, slide %in% SOLO_SLIDES)
    revision <- manifiesto |>
        mutate(png = glue("output/{grafica}.png"), existe = file.exists(png)) |>
        select(slide, seccion, titulo, png, existe)
    print(revision, n = Inf, width = Inf)

    if (!all(revision$existe)) {
        stop("Faltan gr\u00e1ficas: ", paste(revision$png[!revision$existe], collapse = ", "))
    }
    cat(green$bold(glue("\nListo: {nrow(revision)} gr\u00e1ficas en output/slides_contexto/{SUFIJO}/{FLUJO}/figuras/\n", .trim = FALSE)))
}
