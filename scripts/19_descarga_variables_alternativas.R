# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/19_descarga_variables_alternativas.R
# Objetivo:       Descarga variables candidatas para reemplazar al capex en el paso 2.
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
#   - Baja variables alternativas al gasto de capital (FRED, Banxico, INEGI,
#     Census). Solo las usan 20 a 22.
#   - Necesita internet y BANXICO_TOKEN en el .env; si una fuente falla, busca
#     el archivo local en input/raw/.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Dos grupos de variables:
#     EUA (demanda de hardware de IA, con decadas de historia):
#       inv_computadoras        BEA, inversion privada en computadoras y perifericos (nominal)  trimestral
#       inv_computadoras_real   BEA, la misma en cantidad real (indice 2017=100)               trimestral
#       pedidos_electronicos    Census M3, pedidos nuevos de computadoras y electronicos       mensual
#       pedidos_computadoras    Census M3, pedidos nuevos de computadoras y relacionados        mensual
#       pedidos_comp_electronicas Census M3, pedidos de fabricacion de computadoras             mensual
#       prod_computadoras       Fed, produccion industrial de computadoras (NAICS 3341)          mensual
#       prod_semiconductores    Fed, produccion industrial de semiconductores (NAICS 3344)       mensual
#       construccion_centros_datos  Census C30, construccion de centros de datos (sin equipo)   mensual
#       construccion_electrica      Census C30, construccion privada electrica (generacion y red) mensual
#     Mexico (demanda final de estos bienes en Mexico):
#       tipo_cambio             Banxico FIX (SF43718), promedio mensual
#       fbcf_maq_importada      INEGI, formacion bruta de capital fijo en maquinaria importada   mensual
#       construccion_no_residencial INEGI, formacion bruta de capital fijo en construccion no residencial
#       consumo_bienes_importados   INEGI, consumo privado en el mercado interior: bienes importados
#       igae_informacion        INEGI, IGAE sector 51 (informacion en medios masivos)            mensual
#
#   Como se obtienen:
#     FRED: se descarga sola; si falla, lee input/raw/<CLAVE>.csv (fecha, valor), por
#           ejemplo input/raw/B935RC1Q027SBEA.csv bajado a mano de fred.stlouisfed.org
#     Banxico: con BANXICO_TOKEN del .env; si falla, input/raw/SF43718.csv
#     INEGI: con INEGI_TOKEN del .env y la clave del indicador en el BIE (llenar
#            id_bie abajo); si no, input/raw/inegi_<nombre>.csv (fecha, valor)
#     Taiwan y SIA (segunda tanda, opcionales, archivo local con fecha, valor):
#       input/raw/taiwan_pedidos_ict.csv   pedidos de exportacion de Taiwan de productos de
#                                          informacion y comunicacion (servidores, redes), MOEA, mensual
#       input/raw/taiwan_pedidos_eua.csv   pedidos de exportacion de Taiwan con origen en EUA, MOEA, mensual
#       input/raw/sia_ventas_semiconductores.csv  ventas mundiales de semiconductores (SIA/WSTS), mensual
#     Census: no esta en FRED. Bajar de
#            https://www.census.gov/construction/c30/historical_data.html el archivo
#            Monthly > Private > Seasonally Adjusted (privsatime.xlsx) y dejarlo tal
#            cual en input/raw/. Se leen las columnas "Data center" (desde 2014) y
#            "Electric". Las fechas vienen como "Sep-26p" (p preliminar, r revisada).
#   Salidas: datos/alternativas/variables_mensuales.csv, variables_trimestrales.csv,
#            00_fuentes.csv (que se obtuvo, de donde y que periodo cubre)
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "httr", "jsonlite", "readxl")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_out   <- file.path(dir_base, "datos", "alternativas")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
fecha_ini <- "1995-01-01"   # historia larga: sirve tambien para el paso 1 con estas variables

archivo_env <- file.path(dir_base, ".env")
if (file.exists(archivo_env)) readRenviron(archivo_env)

series_fred <- tribble(
  ~nombre,                     ~id,               ~frecuencia,  ~descripcion,
  "inv_computadoras",          "B935RC1Q027SBEA", "trimestral", "EUA: inversion privada en computadoras y perifericos, nominal (miles de millones, tasa anual)",
  "inv_computadoras_real",     "B935RA3Q086SBEA", "trimestral", "EUA: inversion privada en computadoras y perifericos, cantidad real (2017=100)",
  "pedidos_electronicos",      "A34SNO",          "mensual",    "EUA: pedidos nuevos de computadoras y productos electronicos (millones, desestacionalizado)",
  "pedidos_computadoras",      "ACRPNO",          "mensual",    "EUA: pedidos nuevos de computadoras y productos relacionados",
  "pedidos_comp_electronicas", "A34ANO",          "mensual",    "EUA: pedidos nuevos de fabricacion de computadoras electronicas",
  "prod_computadoras",         "IPG3341S",        "mensual",    "EUA: produccion industrial de computadoras y perifericos (2017=100)",
  "prod_semiconductores",      "IPG3344S",        "mensual",    "EUA: produccion industrial de semiconductores y componentes (2017=100)",
  # Segunda tanda: precios (ciclo de semiconductores y memoria 2022 a 2026)
  "precio_export_computo",     "IQ213",           "mensual",    "EUA: indice de precios de exportacion, computadoras, perifericos y semiconductores (2000=100)",
  "precio_export_computadoras","IQ213COM",        "mensual",    "EUA: indice de precios de exportacion, computadoras y perifericos sin semiconductores",
  "precio_import_computo",     "IR213",           "mensual",    "EUA: indice de precios de importacion, computadoras, perifericos y semiconductores (2000=100)",
  "ppi_semiconductores",       "PCU33443344",     "mensual",    "EUA: precios al productor, semiconductores y componentes electronicos")

# Claves del Banco de Informacion Economica (BIE) de INEGI: llenar id_bie con la
# clave numerica del indicador (en el BIE, al consultar la serie). Usar las
# series desestacionalizadas. Si se deja vacio, se busca el archivo local.
series_inegi <- tribble(
  ~nombre,                       ~id_bie, ~descripcion,
  "fbcf_maq_importada",          "",      "Mexico: formacion bruta de capital fijo, maquinaria y equipo importado (indice)",
  "construccion_no_residencial", "",      "Mexico: formacion bruta de capital fijo, construccion no residencial (indice)",
  "consumo_bienes_importados",   "",      "Mexico: consumo privado en el mercado interior, bienes importados (indice)",
  "igae_informacion",            "",      "Mexico: IGAE, 51 informacion en medios masivos (indice)")

`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# 1. Funciones de descarga
# =============================================================================
lee_local <- function(...) {
  ids <- c(...)
  f <- file.path(dir_base, "input", rep(c("raw", "macro"), each = length(ids)), rep(paste0(ids, ".csv"), times = 2))
  f <- f[file.exists(f)][1]
  if (is.na(f)) return(NULL)
  message("  leo ", f)
  d <- read_csv(f, show_col_types = FALSE, na = c(".", "", "N/E", "NA"), locale = locale(encoding = "UTF-8"))
  names(d)[1:2] <- c("fecha", "valor")
  d %>% transmute(fecha = as.Date(parse_date_time(as.character(fecha), c("Ymd", "Ym", "dmY", "mY"))),
                  valor = suppressWarnings(as.numeric(gsub(",", "", valor)))) %>%
    filter(!is.na(fecha), !is.na(valor)) %>% mutate(origen = "archivo local")
}

baja_fred <- function(id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id, "&cosd=", fecha_ini)
  # HTTP/1.1: con HTTP/2 FRED corta la conexion en algunos equipos
  r <- tryCatch(GET(url, config(http_version = 2), timeout(60)), error = function(e) NULL)
  d <- if (!is.null(r) && status_code(r) == 200)
    tryCatch(read_csv(content(r, as = "text", encoding = "UTF-8"), show_col_types = FALSE, na = c(".", "")),
             error = function(e) NULL) else NULL
  if (is.null(d)) { message("FRED ", id, ": fallo la descarga; busco archivo local"); return(lee_local(id)) }
  names(d)[1:2] <- c("fecha", "valor")
  d %>% transmute(fecha = as.Date(fecha), valor = as.numeric(valor), origen = "FRED") %>% filter(!is.na(valor))
}

baja_banxico <- function(id) {
  token <- Sys.getenv("BANXICO_TOKEN")
  if (token == "") { message("Sin BANXICO_TOKEN; busco input/raw/", id, ".csv"); return(lee_local(id)) }
  url <- sprintf("https://www.banxico.org.mx/SieAPIRest/service/v1/series/%s/datos/%s/%s", id, fecha_ini, Sys.Date())
  r <- tryCatch(GET(url, add_headers(`Bmx-Token` = token, Accept = "application/json"), timeout(60)),
                error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) { message("Banxico ", id, ": fallo; busco archivo local"); return(lee_local(id)) }
  x <- fromJSON(content(r, as = "text", encoding = "UTF-8"))$bmx$series$datos[[1]]
  tibble(fecha = dmy(x$fecha), valor = suppressWarnings(as.numeric(gsub(",", "", x$dato))), origen = "Banxico") %>%
    filter(!is.na(valor))
}

baja_inegi <- function(id, nombre) {
  token <- Sys.getenv("INEGI_TOKEN")
  if (token != "" && id != "") {
    url <- sprintf("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/%s/es/00/false/BIE/2.0/%s?type=json",
                   id, token)
    d <- tryCatch({
      r <- GET(url, config(http_version = 2), timeout(60))
      j <- fromJSON(content(r, as = "text", encoding = "UTF-8"))
      obs <- j$Series$OBSERVATIONS[[1]]
      tibble(fecha = ymd(paste0(gsub("/", "-", obs$TIME_PERIOD), "-01")),
             valor = suppressWarnings(as.numeric(obs$OBS_VALUE)), origen = "INEGI API") %>% filter(!is.na(valor))
    }, error = function(e) NULL)
    if (!is.null(d) && nrow(d) > 0) return(d)
    message("INEGI ", id, ": fallo la descarga; busco archivo local")
  }
  lee_local(paste0("inegi_", nombre))
}

# Census C30 (privsatime.xlsx): una columna por tipo de construccion, una fila
# por mes. Meses en ingles con mapa fijo (no depende del idioma del equipo).
lee_census <- function(columna) {
  f <- list.files(file.path(dir_base, "input", "raw"), pattern = "^privsatime.*\\.xlsx$", full.names = TRUE)[1]
  if (is.na(f)) { message("  no esta input/raw/privsatime.xlsx"); return(NULL) }
  x <- read_excel(f, skip = 3, col_types = "text")
  nombres <- str_squish(gsub("[\r\n]|_x000D_", " ", names(x)))
  k <- which(tolower(nombres) == tolower(columna))[1]
  if (is.na(k)) { message("  privsatime.xlsx no trae la columna ", columna); return(NULL) }
  meses_en <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6, Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
  fecha_txt <- str_remove(str_trim(x[[1]]), "[pr]$")
  mm <- unname(meses_en[str_sub(fecha_txt, 1, 3)]); yy <- suppressWarnings(as.integer(str_sub(fecha_txt, 5, 6)))
  tibble(fecha = make_date(if_else(yy >= 90, 1900L + yy, 2000L + yy), mm, 1L),
         valor = suppressWarnings(as.numeric(x[[k]])), origen = "Census C30 (privsatime.xlsx)") %>%
    filter(!is.na(fecha), !is.na(valor)) %>% arrange(fecha)
}

# =============================================================================
# 2. Descarga
# =============================================================================
lista <- list()
for (k in seq_len(nrow(series_fred))) {
  message("FRED: ", series_fred$id[k])
  lista[[series_fred$nombre[k]]] <- baja_fred(series_fred$id[k])
}
message("Banxico: SF43718")
lista[["tipo_cambio"]] <- baja_banxico("SF43718")
for (k in seq_len(nrow(series_inegi))) {
  message("INEGI: ", series_inegi$nombre[k])
  lista[[series_inegi$nombre[k]]] <- baja_inegi(series_inegi$id_bie[k], series_inegi$nombre[k])
}
message("Taiwan y SIA (archivos locales opcionales)")
for (n in c("taiwan_pedidos_ict", "taiwan_pedidos_eua", "sia_ventas_semiconductores")) lista[[n]] <- lee_local(n)
message("Census: centros de datos y construccion electrica")
lista[["construccion_centros_datos"]] <- lee_census("Data center") %||% lee_local("census_centros_datos")
lista[["construccion_electrica"]] <- lee_census("Electric") %||% lee_local("census_electrica")

catalogo <- bind_rows(
  series_fred %>% transmute(nombre, fuente = "FRED", clave = id, frecuencia, descripcion),
  tibble(nombre = "tipo_cambio", fuente = "Banxico", clave = "SF43718", frecuencia = "mensual",
         descripcion = "Mexico: tipo de cambio FIX, pesos por dolar (promedio mensual)"),
  series_inegi %>% transmute(nombre, fuente = "INEGI", clave = id_bie, frecuencia = "mensual", descripcion),
  tibble(nombre = "construccion_centros_datos", fuente = "Census C30", clave = "Data center", frecuencia = "mensual",
         descripcion = "EUA: gasto privado en construccion de centros de datos (no incluye el equipo), tasa anual desestacionalizada"),
  tibble(nombre = c("taiwan_pedidos_ict", "taiwan_pedidos_eua", "sia_ventas_semiconductores"),
         fuente = c("MOEA Taiwan", "MOEA Taiwan", "SIA/WSTS"), clave = "archivo local", frecuencia = "mensual",
         descripcion = c("Taiwan: pedidos de exportacion de productos de informacion y comunicacion",
                         "Taiwan: pedidos de exportacion con origen en EUA",
                         "Mundo: ventas de semiconductores (promedio movil de 3 meses)")),
  tibble(nombre = "construccion_electrica", fuente = "Census C30", clave = "Electric", frecuencia = "mensual",
         descripcion = "EUA: gasto privado en construccion electrica (generacion y red), tasa anual desestacionalizada"))

# =============================================================================
# 3. A mensual y trimestral
# =============================================================================
largo <- imap_dfr(lista, function(d, n) if (is.null(d) || nrow(d) == 0) NULL else mutate(d, nombre = n))
if (nrow(largo) == 0) stop("No se obtuvo ninguna serie: revisar conexion o archivos en input/raw/")
frec <- setNames(catalogo$frecuencia, catalogo$nombre)

mensual <- largo %>% filter(frec[nombre] == "mensual") %>%
  mutate(mes = floor_date(fecha, "month")) %>%
  group_by(nombre, mes) %>% summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop")
trimestral <- bind_rows(
  mensual %>% mutate(trimestre = floor_date(mes, "quarter")) %>%
    group_by(nombre, trimestre) %>% summarise(valor = mean(valor), n = n(), .groups = "drop") %>%
    filter(n == 3) %>% select(-n),
  largo %>% filter(frec[nombre] == "trimestral") %>%
    transmute(nombre, trimestre = floor_date(fecha, "quarter"), valor))

write_excel_csv(mensual %>% pivot_wider(names_from = nombre, values_from = valor) %>% arrange(mes),
                file.path(dir_out, "variables_mensuales.csv"))
write_excel_csv(trimestral %>% pivot_wider(names_from = nombre, values_from = valor) %>% arrange(trimestre),
                file.path(dir_out, "variables_trimestrales.csv"))

fuentes <- catalogo %>%
  left_join(largo %>% group_by(nombre) %>%
              summarise(origen = first(origen), desde = min(fecha), hasta = max(fecha), n = n(), .groups = "drop"),
            by = "nombre") %>%
  mutate(estado = if_else(is.na(n), "FALTA: ver instrucciones en el encabezado", "ok"))
write_excel_csv(fuentes, file.path(dir_out, "00_fuentes.csv"))

cat("\nVariables obtenidas:\n")
print(as.data.frame(fuentes %>% select(nombre, fuente, clave, origen, desde, hasta, n, estado)), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
