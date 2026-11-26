# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/14_sectores_cercanos_ia.R
# Objetivo:       ROBUSTEZ EXPLORATORIA: efectos del choque de valuacion en bolsa de IA sobre sectores
#                 "cercanos" a la IA (energia, servicios de datos, construccion, turismo,
#                 BDCs en EUA), con proyecciones locales (Jorda 2005) de 0 a 8 trimestres.
#                 La identificacion es debil (la valuacion en bolsa responde a muchas cosas que tambien
#                 mueven a estos sectores): presentar como evidencia descriptiva, no causal.
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          17/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Prueba exploratoria y opcional: como responden otros sectores a la
#     valuacion en bolsa de la IA.
#   - Es descriptivo, no causal; no entra en los resultados principales.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Requiere: salidas del script 01 (mcap_indices_grupo.csv, macro_trimestral.csv)
#             input/sectores_cercanos.csv (lista de series; editar ahi)
#             opcional: input/sectores/etef_subsector_entidad.csv
#   Fuentes por serie (columna fuente):
#     yahoo  : ticker en la columna clave (precio ajustado)
#     inegi  : clave del indicador BIE en la columna clave; token en la variable
#              de entorno INEGI_TOKEN. Si no hay clave o token, lee el archivo local.
#     banxico: clave SIE en la columna clave; token en BANXICO_TOKEN
#     local  : input/sectores/<archivo>, dos columnas: fecha (AAAA-MM-DD o
#              AAAA-MM o AAAA/TT) y valor
#   Salidas (output/sectores_ia/):
#     00_series_usadas.csv          series que si se pudieron leer y su rango
#     01_lp_por_serie.csv           respuesta acumulada de cada serie, h = 0..8
#     02_lp_cercanos_vs_resto.csv   respuesta de sectores cercanos menos la del resto
#     03_exposicion_entidades.csv   peso del subsector 334 en las exportaciones de cada estado
#     04_lp_entidades.csv           respuesta de exportaciones 334 por estado expuesto
# _____________________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, timeout = 120)
# Paquetes: instala los que falten y los carga
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate",
               "fixest", "sandwich", "httr", "jsonlite", "quantmod", "zoo")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

dir_base <- getwd()
dir_emp  <- file.path(dir_base, "datos", "empresas")
dir_sec  <- file.path(dir_base, "input", "sectores")
dir_out  <- file.path(dir_base, "output", "sectores_ia")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Tokens (BANXICO_TOKEN, INEGI_TOKEN) desde el .env del proyecto, formato CLAVE=valor
archivo_env <- file.path(dir_base, ".env")
if (file.exists(archivo_env)) readRenviron(archivo_env)

# ---- Parametros -------------------------------------------------------------
horizontes   <- 0:8                  # trimestres despues del choque
rezagos_y    <- 4                    # rezagos de la variable dependiente
rezagos_choq <- 2                    # rezagos del choque
indice_choque <- "es_hyperscaler"    # o "es_semiconductores" (rotacion hacia semiconductores)
# Controles de macro_trimestral.csv. El S&P 500 separa el choque propio de IA
# del movimiento de todo el mercado (sin el, cualquier activo de riesgo "responde")
controles    <- c("tasa10", "sp500")  # character() para quitar
desde        <- as.Date("2015-01-01")
fuente_bie   <- "BIE-BISE"           # VERIFICAR en la documentacion de la API de INEGI

# =============================================================================
# 1. Choque: cambio trimestral del log valuacion en bolsa
# =============================================================================
indices <- read_csv(file.path(dir_emp, "mcap_indices_grupo.csv"), show_col_types = FALSE)
choque <- indices %>%
  filter(indicadora == indice_choque) %>%
  mutate(trimestre = floor_date(as.Date(mes), "quarter")) %>%
  group_by(trimestre) %>%
  summarise(idx = mean(coalesce(idx_ponderado, idx_equiponderado)), .groups = "drop") %>%
  arrange(trimestre) %>%
  transmute(trimestre, choque = log(idx) - lag(log(idx)))

archivo_macro <- file.path(dir_emp, "macro_trimestral.csv")
macro <- if (length(controles) && file.exists(archivo_macro)) {
  read_csv(archivo_macro, show_col_types = FALSE) %>%
    mutate(trimestre = as.Date(trimestre)) %>% arrange(trimestre) %>%
    select(trimestre, any_of(controles)) %>%
    mutate(across(-trimestre, ~ if (cur_column() %in% c("sp500", "vix")) log(.x) - lag(log(.x)) else .x - lag(.x))) %>%
    rename_with(~ paste0("c_", .x), -trimestre)
} else { controles <- character(); tibble(trimestre = as.Date(character())) }

# =============================================================================
# 2. Lectura de series
# =============================================================================
lista <- read_csv(file.path(dir_base, "input", "sectores_cercanos.csv"),
                  col_types = cols(.default = col_character()),
                  locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  mutate(cercano_ia = as.logical(cercano_ia))

parsea_fecha <- function(x) {   # AAAA-MM-DD, AAAA-MM o AAAA/MM
  x <- as.character(x); out <- as.Date(rep(NA_character_, length(x)))
  i1 <- !is.na(x) & str_detect(x, "^\\d{4}-\\d{2}-\\d{2}")
  out[i1] <- as.Date(substr(x[i1], 1, 10))
  i2 <- !is.na(x) & !i1 & str_detect(x, "^\\d{4}[-/]\\d{2}$")
  out[i2] <- as.Date(paste0(substr(x[i2], 1, 4), "-", substr(x[i2], 6, 7), "-01"))
  out
}
parsea_trim <- function(x) {   # "2024/01".."2024/04" = trimestres
  as.Date(sprintf("%s-%02d-01", substr(x, 1, 4), (as.integer(substr(x, 6, 7)) - 1) * 3 + 1))
}

lee_local <- function(archivo, frecuencia) {
  f <- file.path(dir_sec, archivo)
  if (is.na(archivo) || !file.exists(f)) return(NULL)
  d <- read_csv(f, col_types = cols(.default = col_character()),
                locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
  names(d)[1:2] <- c("fecha", "valor")
  d %>% transmute(fecha = if (frecuencia == "trimestral" & any(str_detect(fecha, "/0[1-4]$")))
                            parsea_trim(fecha) else parsea_fecha(fecha),
                  valor = as.numeric(gsub(",", "", valor)))
}
lee_yahoo <- function(tk) {
  if (!requireNamespace("quantmod", quietly = TRUE)) return(NULL)
  x <- tryCatch(quantmod::getSymbols(tk, src = "yahoo", from = desde - years(2), auto.assign = FALSE),
                error = function(e) NULL)
  if (is.null(x)) return(NULL)
  tibble(fecha = as.Date(zoo::index(x)), valor = as.numeric(quantmod::Ad(x)))
}
lee_inegi <- function(clave, frecuencia) {
  token <- Sys.getenv("INEGI_TOKEN")
  if (is.na(clave) || clave == "" || token == "") return(NULL)
  url <- sprintf("https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/%s/es/0700/false/%s/2.0/%s?type=json",
                 clave, fuente_bie, token)
  r <- tryCatch(GET(url, timeout(60)), error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) return(NULL)
  obs <- fromJSON(content(r, as = "text", encoding = "UTF-8"))$Series$OBSERVATIONS[[1]]
  tibble(fecha = if (frecuencia == "trimestral") parsea_trim(obs$TIME_PERIOD) else
                   as.Date(paste0(gsub("/", "-", obs$TIME_PERIOD), "-01")),
         valor = as.numeric(obs$OBS_VALUE))
}
lee_banxico <- function(clave) {
  token <- Sys.getenv("BANXICO_TOKEN")
  if (is.na(clave) || clave == "" || token == "") return(NULL)
  url <- sprintf("https://www.banxico.org.mx/SieAPIRest/service/v1/series/%s/datos/%s/%s",
                 clave, desde - years(2), Sys.Date())
  r <- tryCatch(GET(url, add_headers(`Bmx-Token` = token), timeout(60)), error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) return(NULL)
  x <- fromJSON(content(r, as = "text", encoding = "UTF-8"))$bmx$series$datos[[1]]
  tibble(fecha = dmy(x$fecha), valor = suppressWarnings(as.numeric(gsub(",", "", x$dato))))
}

lee_serie <- function(fuente, clave, archivo, frecuencia) {
  d <- switch(fuente,
              yahoo   = lee_yahoo(clave),
              inegi   = lee_inegi(clave, frecuencia),
              banxico = lee_banxico(clave),
              NULL)
  if (is.null(d) || nrow(d) == 0) d <- lee_local(archivo, frecuencia)   # respaldo local
  d
}

series <- lista %>%
  mutate(datos = pmap(list(fuente, clave, archivo, frecuencia), lee_serie)) %>%
  filter(map_lgl(datos, ~ !is.null(.x) && nrow(.x) > 0))
sin_datos <- setdiff(lista$serie, series$serie)
if (length(sin_datos)) message("Series sin datos (revisar clave, token o archivo local): ",
                               paste(sin_datos, collapse = ", "))
if (nrow(series) == 0) stop("Ninguna serie disponible")

# Trimestral: promedio del trimestre, en logaritmo
panel <- series %>%
  select(serie, nombre, pais, cercano_ia, datos) %>%
  unnest(datos) %>%
  filter(!is.na(valor), valor > 0, !is.na(fecha)) %>%
  mutate(trimestre = floor_date(fecha, "quarter")) %>%
  group_by(serie, nombre, pais, cercano_ia, trimestre) %>%
  summarise(y = log(mean(valor)), .groups = "drop") %>%
  filter(trimestre >= desde - years(2))

usadas <- panel %>% group_by(serie, nombre, pais, cercano_ia) %>%
  summarise(desde = min(trimestre), hasta = max(trimestre), n = n(), .groups = "drop")
print(usadas, n = Inf)
write_excel_csv(usadas, file.path(dir_out, "00_series_usadas.csv"))

# =============================================================================
# 3. Proyecciones locales
#    y_{t+h} - y_{t-1} = a + b_h choque_t + sum_l c_l dy_{t-l} + sum_l d_l choque_{t-l}
#                        + controles + trimestre del anio + e
#    b_h = cambio acumulado (log) de la serie h trimestres despues de un alza
#    de 1 log punto en la valuacion en bolsa. Errores Newey-West con h + 1 rezagos.
# =============================================================================
arma <- function(d, h) {
  d %>% arrange(trimestre) %>%
    left_join(choque, by = "trimestre") %>%
    left_join(macro, by = "trimestre") %>%
    mutate(dep = lead(y, h) - lag(y, 1), dy = y - lag(y),
           tda = factor(quarter(trimestre))) %>%
    { reduce(1:rezagos_y, function(z, l) mutate(z, !!paste0("dy_l", l) := lag(dy, l)), .init = .) } %>%
    { reduce(seq_len(rezagos_choq), function(z, l) mutate(z, !!paste0("ch_l", l) := lag(choque, l)), .init = .) } %>%
    filter(trimestre >= desde)
}
rhs <- paste(c("choque", paste0("dy_l", 1:rezagos_y), paste0("ch_l", seq_len(rezagos_choq)),
               if (length(controles)) paste0("c_", controles), "tda"), collapse = " + ")

lp_serie <- function(d, h) {
  dd <- arma(d, h) %>% filter(if_all(c(dep, choque, starts_with("dy_l"), starts_with("ch_l")), ~ !is.na(.x)))
  if (nrow(dd) < 20) return(NULL)
  m <- lm(as.formula(paste("dep ~", rhs)), data = dd)
  V <- NeweyWest(m, lag = h + 1, prewhite = FALSE)
  tibble(h = h, b = unname(coef(m)["choque"]), ee = sqrt(V["choque", "choque"]), n = nrow(dd))
}

lp <- panel %>% group_by(serie, nombre, pais, cercano_ia) %>%
  group_modify(~ map_dfr(horizontes, function(h) lp_serie(.x, h))) %>%
  ungroup() %>%
  mutate(li90 = b - 1.645 * ee, ls90 = b + 1.645 * ee)
write_excel_csv(lp, file.path(dir_out, "01_lp_por_serie.csv"))
cat("\nRespuesta acumulada a 4 y 8 trimestres:\n")
print(lp %>% filter(h %in% c(4, 8)) %>% select(serie, cercano_ia, h, b, ee) %>%
        mutate(across(c(b, ee), ~ round(.x, 3))), n = Inf)

# ---- Panel: cercanos contra el resto ------------------------------------------
# Efectos fijos de serie y de trimestre: el efecto comun de la valuacion en bolsa lo absorbe
# el FE de trimestre; b_h mide cuanto MAS responden los sectores cercanos.
# Errores Driscoll-Kraay (pocas series, choques comunes).
if (n_distinct(panel$cercano_ia) == 2) {
  lp_panel <- map_dfr(horizontes, function(h) {
    dd <- panel %>% group_by(serie) %>% group_modify(~ arma(.x, h)) %>% ungroup() %>%
      filter(!is.na(dep), !is.na(choque)) %>%
      mutate(cercano_x_choque = as.numeric(cercano_ia) * choque,
             t_id = as.integer(factor(trimestre)))
    m <- feols(as.formula(paste("dep ~ cercano_x_choque +", paste0("dy_l", 1:rezagos_y, collapse = " + "),
                                "| serie + trimestre")),
               data = dd, panel.id = ~ serie + t_id, vcov = "DK")
    tibble(h = h, b = coef(m)["cercano_x_choque"], ee = se(m)["cercano_x_choque"], n = nobs(m))
  })
  write_excel_csv(lp_panel, file.path(dir_out, "02_lp_cercanos_vs_resto.csv"))
  cat("\nCercanos menos resto:\n"); print(lp_panel)
} else message("Para el panel hacen falta series cercanas y de control (cercano_ia TRUE y FALSE)")

# =============================================================================
# 4. Exposicion por entidad: exportaciones trimestrales por entidad y subsector
#    (INEGI, ETEF). Archivo: input/sectores/etef_subsector_entidad.csv con
#    columnas entidad, subsector (SCIAN 3 digitos), trimestre (AAAA/TT o fecha), valor
# =============================================================================
archivo_etef <- file.path(dir_sec, "etef_subsector_entidad.csv")
if (file.exists(archivo_etef)) {
  etef <- read_csv(archivo_etef, col_types = cols(.default = col_character()),
                   locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
    mutate(trimestre = if_else(str_detect(trimestre, "/0[1-4]$"), parsea_trim(trimestre),
                               floor_date(parsea_fecha(trimestre), "quarter")),
           valor = as.numeric(gsub(",", "", valor)))

  # Peso del subsector 334 (computo, comunicacion, componentes electronicos) en
  # las exportaciones de cada estado, promedio 2023
  exposicion <- etef %>% filter(year(trimestre) == 2023) %>%
    group_by(entidad) %>%
    summarise(export_total = sum(valor, na.rm = TRUE),
              export_334 = sum(valor[subsector == "334"], na.rm = TRUE), .groups = "drop") %>%
    mutate(peso_334 = export_334 / export_total,
           participacion_nacional_334 = export_334 / sum(export_334)) %>%
    arrange(desc(export_334))
  write_excel_csv(exposicion, file.path(dir_out, "03_exposicion_entidades.csv"))
  print(exposicion, n = 10)

  # LP de las exportaciones 334 de los estados: respuesta promedio, ponderada por tamanio
  e334 <- etef %>% filter(subsector == "334", valor > 0) %>%
    group_by(entidad, trimestre) %>% summarise(y = log(sum(valor)), .groups = "drop")
  lp_ent <- map_dfr(horizontes, function(h) {
    dd <- e334 %>% group_by(entidad) %>% group_modify(~ arma(.x, h)) %>% ungroup() %>%
      filter(!is.na(dep), !is.na(choque)) %>%
      left_join(exposicion %>% select(entidad, peso_334, export_334), by = "entidad") %>%
      mutate(t_id = as.integer(factor(trimestre)))
    m <- feols(as.formula(paste("dep ~ choque +", paste0("dy_l", 1:rezagos_y, collapse = " + "),
                                "+ tda | entidad")),
               data = dd, weights = ~export_334, panel.id = ~ entidad + t_id, vcov = "DK")
    tibble(h = h, b = coef(m)["choque"], ee = se(m)["choque"], n = nobs(m))
  })
  write_excel_csv(lp_ent, file.path(dir_out, "04_lp_entidades.csv"))
  cat("\nExportaciones 334 de los estados:\n"); print(lp_ent)
} else message("Sin ", archivo_etef, ": se omite la exposicion por entidad")

message("Listo. Resultados en: ", dir_out)
