# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/06b_diagnostico_census.R
# Objetivo:       Mide cuanto tarda la API del Census con distintos tamanos de consulta y dice
#                 que valores poner en el 06 (unidad_tiempo, nivel_filtro, por_pais).
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
#   - Opcional. Solo sirve si la descarga del 06 se atora; no lo corre
#     00_correr_todo.
#   - Tarda entre 1 y 15 minutos.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Por que: la consulta minima responde en ~3 s, pero un capitulo completo por
#   trimestre con 12 paises no responde en 90 s. Aqui se prueban 6 tamanos con el
#   mismo mes (ene 2024) y se estima el tiempo total de cada opcion. Tarda entre
#   1 y 15 minutos (cada prueba espera hasta 150 s).
#   Requiere: .env con CENSUS_API_KEY y output/panel (para contar partidas)
#   Salidas : output/census/00_diagnostico_api.csv
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 200)
paquetes <- c("dplyr", "tidyr", "stringr", "readr", "lubridate", "httr", "jsonlite")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

dir_base <- getwd()                                     # AJUSTAR si hace falta
dir_out  <- file.path(dir_base, "output", "census")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
readRenviron(file.path(dir_base, ".env"))
clave_census <- Sys.getenv("CENSUS_API_KEY")
if (clave_census == "") stop("Falta CENSUS_API_KEY en el .env")

espera     <- 150
paises     <- c("-", "2010", "5700", "5830", "5800", "5880", "5520", "5570", "5490", "5590", "5650", "5330", "5820")
capitulos  <- c("84", "85", "90")
anio_panel <- 2021
anio_desde <- 2013
url_base   <- "https://api.census.gov/data/timeseries/intltrade/imports/hs"

# Cuantas partidas (HS4) tiene el panel de Mexico y cuantas son del nucleo (aprox.)
pfm <- read_csv(file.path(dir_base, "output", "panel", "panel_fraccion_pais_mes.csv"),
                col_types = cols(fraccion = col_character(), .default = col_guess()),
                col_select = c("fraccion"), show_col_types = FALSE)
hs4 <- unique(str_sub(str_pad(pfm$fraccion, 8, pad = "0"), 1, 4))
n_hs4 <- sum(str_sub(hs4, 1, 2) %in% capitulos)
n_hs4_nucleo <- 4                                       # 8471, 8473, 8517, 8542 (aprox.)
n_meses <- length(seq(as.Date(paste0(anio_panel, "-01-01")), floor_date(Sys.Date(), "month"), by = "month"))
n_meses_hist <- 12 * (anio_panel - anio_desde)

prueba <- function(etiqueta, filtro, tiempo, ctys) {
  url <- paste0(url_base, "?get=I_COMMODITY,CTY_CODE,CTY_NAME,GEN_VAL_MO&COMM_LVL=HS6&I_COMMODITY=", filtro,
                "&time=", tiempo, paste0("&CTY_CODE=", ctys, collapse = ""), "&key=", clave_census)
  message("Probando: ", etiqueta, " ...")
  t0 <- Sys.time()
  r <- tryCatch(GET(url, timeout(espera)), error = function(e) NULL)
  seg <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  txt <- if (is.null(r)) "" else content(r, as = "text", encoding = "UTF-8")
  ok <- !is.null(r) && status_code(r) == 200 && startsWith(trimws(txt), "[")
  filas <- if (ok) nrow(fromJSON(txt)) - 1 else NA_integer_
  estado <- if (is.null(r)) sprintf("sin respuesta en %.0f s", seg) else if (ok) "ok" else
    paste0("HTTP ", status_code(r), ": ", substr(gsub("<[^>]+>", " ", txt), 1, 120))
  message(sprintf("  %s | %.1f s | filas: %s", estado, seg, ifelse(is.na(filas), "-", filas)))
  tibble(prueba = etiqueta, ok = ok, segundos = round(seg, 1), filas = filas, estado = estado)
}

res <- bind_rows(
  prueba("capitulo, mes, todos los paises",       "84*",   "2024-01",                 paises),
  prueba("capitulo, mes, un pais",                "84*",   "2024-01",                 "-"),
  prueba("capitulo, trimestre, un pais",          "84*",   "from+2024-01+to+2024-03", "-"),
  prueba("partida, mes, todos los paises",        "8471*", "2024-01",                 paises),
  prueba("partida, trimestre, todos los paises",  "8471*", "from+2024-01+to+2024-03", paises),
  prueba("partida, mes, un pais",                 "8471*", "2024-01",                 "-")
)

# Opciones para el 06: consultas totales y tiempo estimado con lo medido.
# Con "capitulo", la historia del nucleo va por partida (n_hs4_nucleo).
n_p <- length(paises)
opciones <- tribble(
  ~prueba,                                ~unidad_tiempo, ~nivel_filtro, ~por_pais, ~consultas,
  "capitulo, mes, todos los paises",      "mes",          "capitulo",    FALSE,     3 * n_meses + n_hs4_nucleo * n_meses_hist,
  "capitulo, mes, un pais",               "mes",          "capitulo",    TRUE,      (3 * n_meses + n_hs4_nucleo * n_meses_hist) * n_p,
  "capitulo, trimestre, un pais",         "trimestre",    "capitulo",    TRUE,      (3 * n_meses + n_hs4_nucleo * n_meses_hist) * n_p / 3,
  "partida, mes, todos los paises",       "mes",          "hs4",         FALSE,     (n_hs4 * n_meses + n_hs4_nucleo * n_meses_hist),
  "partida, trimestre, todos los paises", "trimestre",    "hs4",         FALSE,     (n_hs4 * n_meses + n_hs4_nucleo * n_meses_hist) / 3,
  "partida, mes, un pais",                "mes",          "hs4",         TRUE,      (n_hs4 * n_meses + n_hs4_nucleo * n_meses_hist) * n_p
) %>%
  mutate(incluir_asia = TRUE)
# Las opciones "un pais" tambien se pueden correr solo con total y Mexico (2 paises)
opciones <- bind_rows(opciones, opciones %>% filter(por_pais) %>% mutate(consultas = consultas * 2 / n_p, incluir_asia = FALSE)) %>%
  left_join(res, by = "prueba") %>%
  mutate(consultas = round(consultas), minutos_estimados = if_else(ok & segundos < 100, round(consultas * segundos / 60), NA_real_)) %>%
  arrange(is.na(minutos_estimados), minutos_estimados)

write_excel_csv(opciones, file.path(dir_out, "00_diagnostico_api.csv"))
cat("\nResultado de las pruebas y tiempo estimado de cada opcion para el 17:\n")
print(as.data.frame(opciones %>% select(prueba, incluir_asia, estado, segundos, filas, consultas, minutos_estimados)),
      row.names = FALSE, right = FALSE)

recomienda <- function(m, nota) {
  cat(sprintf("\n%s\nPon en el 17 (seccion Parametros):\n  unidad_tiempo <- \"%s\"\n  nivel_filtro  <- \"%s\"\n  por_pais      <- %s\n  incluir_asia  <- %s\nTiempo estimado de la primera descarga: unos %d minutos.\n",
              nota, m$unidad_tiempo, m$nivel_filtro, m$por_pais, m$incluir_asia, as.integer(m$minutos_estimados)))
}
validas  <- opciones %>% filter(!is.na(minutos_estimados))
completa <- validas %>% filter(incluir_asia) %>% slice(1)
reducida <- validas %>% filter(!incluir_asia) %>% slice(1)
if (nrow(completa)) recomienda(completa, "Opcion completa (con todos los paises):")
if (nrow(reducida) && (!nrow(completa) || completa$minutos_estimados > 120) &&
    (!nrow(completa) || reducida$minutos_estimados < completa$minutos_estimados))
  recomienda(reducida, "Opcion mas rapida (solo total y Mexico, sin la prueba fuera de Asia):")
if (nrow(validas)) {
} else {
  cat("\nNinguna opcion respondio a tiempo. La alternativa es bajar los datos a mano de USITC DataWeb (ver la conversacion).\n")
}
