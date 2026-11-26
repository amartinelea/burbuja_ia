# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/08_robustez_calibracion.R
# Objetivo:       (1) Robustez: corre el script 07 con varias especificaciones y junta betas
#                 e impactos en una sola tabla.
#                 (2) Calibracion de escenarios por fecha: "la valuacion en bolsa regresa al nivel de
#                 tal mes", usando los indices del script 01.
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
#   - Pruebas de robustez de la etapa 1 y calibracion de escenarios por fecha.
#     Es lento.
#   - Opcional: 00_correr_todo lo corre solo con correr_complementarias <-
#     TRUE.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Requiere: scripts/07_analisis_choque_ia_anam.R (version 2026-11-17 o
#             posterior), las salidas del script 01 (incluida la seccion 7,
#             macro) y las del script 02 (output/panel/).
#   Salidas : output/robustez/
#     robustez_betas.csv            beta por especificacion y grupo
#     robustez_impactos.csv         impacto total por especificacion y escenario
#     calibracion_niveles.csv       caida de valuacion en bolsa si regresa a cada fecha
#     escenarios_calibrados.csv     escenarios B, C y D definidos por fecha; E y F fijos
#     robustez_forma_reducida.csv   regresion directa comercio vs valuacion en bolsa
#     robustez_theta_gamma.csv      theta y gamma (tasa) por especificacion
# _____________________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
# Paquetes: instala los que falten y los carga
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate",
               "fixest", "sandwich", "readxl")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

dir_base   <- getwd()
script_02  <- file.path(dir_base, "scripts", "07_analisis_choque_ia_anam.R")
dir_rob    <- file.path(dir_base, "output", "robustez")
dir.create(dir_rob, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. Calibracion de escenarios por fecha
# =============================================================================
# Para cada grupo: cuanto tendria que caer la valuacion en bolsa desde el ultimo mes
# para regresar al nivel promedio de un mes de referencia.
indices <- read_csv(file.path(dir_base, "datos", "empresas", "mcap_indices_grupo.csv"),
                    show_col_types = FALSE) %>%
  mutate(idx = coalesce(idx_ponderado, idx_equiponderado))

fechas_ref <- as.Date(c("2023-01-01", "2024-01-01", "2025-01-01", "2025-07-01"))
grupos_mc  <- c(hyper = "es_hyperscaler", dc = "es_datacenter",
                semiconductores = "es_semiconductores", intel = "es_inteligencia")

ultimo_mes <- max(indices$mes)
calibracion <- map_dfr(names(grupos_mc), function(g) {
  d <- indices %>% filter(indicadora == grupos_mc[[g]])
  hoy <- d$idx[d$mes == ultimo_mes]
  tibble(grupo = g, fecha_ref = fechas_ref,
         idx_ref = map_dbl(fechas_ref, ~ { v <- d$idx[d$mes == .x]; if (length(v)) v else NA_real_ }),
         idx_hoy = hoy) %>%
    mutate(caida_para_regresar = idx_ref / idx_hoy - 1)
})
print(calibracion, n = Inf)
write_excel_csv(calibracion, file.path(dir_rob, "calibracion_niveles.csv"))

# Definicion por fecha (AJUSTAR aqui la historia de cada escenario):
#   B. Ajuste de expectativas: hyperscalers regresan a jul 2025;
#      neoclouds/data centers regresan a ene 2025 (caen mas, estan apalancadas)
#   C. Contraccion de la industria: todo regresa a ene 2024
#   D. Auge: la valuacion en bolsa regresa al maximo de los ultimos 12 meses
toma <- function(g, f) {
  v <- calibracion$caida_para_regresar[calibracion$grupo == g & calibracion$fecha_ref == as.Date(f)]
  if (length(v) == 0 || is.na(v)) NA_real_ else v
}
# Auge: la valuacion en bolsa regresa al maximo de los ultimos 12 meses (y nunca menos
# que el nivel actual). Antes era "repetir el crecimiento de los ultimos 12
# meses", que despues de una caida da un auge negativo.
max_12m <- function(g) {
  d <- indices %>% filter(indicadora == grupos_mc[[g]], mes > ultimo_mes %m-% months(12))
  v1 <- d$idx[d$mes == ultimo_mes]
  if (length(v1) == 0 || nrow(d) == 0) NA_real_ else max(d$idx, na.rm = TRUE) / v1 - 1
}

escenarios_cal <- tribble(
  ~escenario, ~mcap_hyper, ~mcap_dc, ~mcap_semiconductores, ~mcap_intel, ~theta_dc, ~arrastre, ~mult_caida,
  "A. Ceteris paribus", 0, 0, 0, 0, 1.0, 0, 1.0,
  "B. Ajuste de expectativas (neoclouds)",
     toma("hyper", "2025-07-01"), toma("dc", "2025-01-01"),
     toma("semiconductores", "2025-07-01"), toma("intel", "2025-07-01"), 1.0, 0, 1.0,
  "C. Contraccion de la industria",
     toma("hyper", "2024-01-01"), toma("dc", "2024-01-01"),
     toma("semiconductores", "2024-01-01"), toma("intel", "2024-01-01"), 1.0, -0.10, 1.3,
  "D. Auge (aceleracion)",
     max_12m("hyper"), max_12m("dc"), max_12m("semiconductores"), max_12m("intel"), 1.0, 0, 0.8,
  "E. Alza de tasas (+100 pb)",       0, 0, 0, 0, 1.0, 0, 1.0,
  "F. Transicion a monetizacion",     0, 0, 0, 0, 1.0, 0, 1.0) %>%
  mutate(delta_tasa    = if_else(str_starts(escenario, "E\\."), 1.0, 0),
         capex_directo = if_else(str_starts(escenario, "F\\."), -0.30, 0)) %>%
  # si un grupo no tiene historia (ej. neoclouds antes de 2025) usar la del escenario original
  mutate(across(starts_with("mcap"), ~ coalesce(.x, 0)))
print(escenarios_cal)
write_excel_csv(escenarios_cal %>% mutate(ultimo_mes = ultimo_mes),
                file.path(dir_rob, "escenarios_calibrados.csv"))

# =============================================================================
# 2. Robustez: especificaciones
# =============================================================================
# Modo trimestral (panel del script 02, USD, PPML). Base: origen EUA, todos
# los regimenes, paso 1 con ratio y controles de tasa y diferencial.
# Sin placebo y con menos simulaciones en cada especificacion (para que no tarde
# horas); la corrida base del script 07 hace las pruebas completas.
base <- list(n_placebo = 0, n_sim = 500)
con  <- function(...) c(base, list(...))
especificaciones <- list(
  base                 = base,
  ols_log              = con(estimador = "ols"),
  tendencia_grupo      = con(tendencia_grupo = TRUE),
  tendencia_grupo_ols  = con(tendencia_grupo = TRUE, estimador = "ols"),
  cambios_4t           = con(forma_paso2 = "d4"),
  unidad_fraccion      = con(unidad = "fraccion"),
  continuos_ppml       = con(solo_continuos = TRUE),
  continuos_ols        = con(solo_continuos = TRUE, estimador = "ols"),
  continuos_tend_ols   = con(solo_continuos = TRUE, estimador = "ols", tendencia_grupo = TRUE),
  continuos_4t         = con(solo_continuos = TRUE, forma_paso2 = "d4"),
  todos_origenes       = con(origen = "todos"),
  solo_definitivas     = con(regimen = "definitiva"),
  solo_temporales      = con(regimen = "temporal"),
  temporales_todos_origenes = con(regimen = "temporal", origen = "todos"),
  sin_8473_30_04       = con(excluir_fracciones = "84733004"),
  sin_top3_nucleo      = con(excluir_fracciones = c("84733004", "85423103", "84715001")),
  sin_contam_alta      = con(excluir_contaminacion_alta = TRUE),
  rezago_1t            = con(rezago_trim = 1),
  con_neoclouds        = con(incluir_dc_capex = TRUE),
  por_eslabon          = con(agrupacion_beta = "eslabon"),
  theta_sin_controles  = con(controles_theta = character()),
  theta_con_sp500      = con(controles_theta = c("tasa10", "spread_baa", "sp500")),
  tasa_fedfunds        = con(controles_theta = c("fedfunds", "spread_baa"), tasa_sim = "fedfunds"),
  ratio_deuda_ingresos = con(ratio_theta = "deuda_ingresos"),
  theta_agregado       = con(theta_spec = "agregado"),
  theta_desde_2018     = con(theta_desde = "2018-01-01"),
  ratio_sin_acotar     = con(ratio_acotar = FALSE),
  theta_desde_2022     = con(theta_desde = "2022-01-01"),
  directo_mc_semiconductores     = con(grupo_mcap_directo = "es_semiconductores"),
  escenarios_por_fecha = con(escenarios = escenarios_cal)
)

corre <- function(nombre, pars) {
  message("\n==== Especificacion: ", nombre, " ====")
  env <- new.env()
  env$params_override <- c(pars, list(dir_out = file.path(dir_rob, nombre)))
  ok <- tryCatch({ sys.source(script_02, envir = env); TRUE },
                 error = function(e) { message("ERROR en ", nombre, ": ", conditionMessage(e)); FALSE })
  if (!ok) return(NULL)
  list(
    betas = env$betas %>% mutate(especificacion = nombre, theta = env$theta,
                                 n_fracciones = map_int(grupo, ~ sum(env$valor_base$grupo == .x))),
    impactos = env$resultado_total %>% mutate(especificacion = nombre),
    directo  = if (!is.null(env$forma_reducida)) env$forma_reducida %>% mutate(especificacion = nombre),
    gamma    = tibble(especificacion = nombre, theta = env$theta, theta_se = env$theta_se,
                      gamma = env$gamma, gamma_se = env$gamma_se,
                      n_obs_theta = if (!is.null(env$theta_panel)) env$theta_panel$n_obs else nrow(env$d1))
  )
}

res <- imap(especificaciones, ~ corre(.y, .x)) %>% compact()

rob_betas <- map_dfr(res, "betas") %>%
  select(especificacion, grupo, n_fracciones, beta_relativo, ee_relativo,
         beta_absoluto, ee_absoluto, theta)
rob_imp <- map_dfr(res, "impactos") %>%
  select(especificacion, version_beta, escenario, var_pct, delta_valor_mdp, delta_recaud_mdp)

write_excel_csv(rob_betas, file.path(dir_rob, "robustez_betas.csv"))
write_excel_csv(rob_imp,   file.path(dir_rob, "robustez_impactos.csv"))
rob_dir <- map_dfr(res, "directo")
write_excel_csv(rob_dir, file.path(dir_rob, "robustez_forma_reducida.csv"))
rob_gamma <- map_dfr(res, "gamma")
write_excel_csv(rob_gamma, file.path(dir_rob, "robustez_theta_gamma.csv"))
cat("\ntheta y gamma (tasa) por especificacion:\n"); print(rob_gamma, n = Inf)

# ---- Tablas para leer rapido -------------------------------------------------
cat("\nBeta del nucleo por especificacion:\n")
print(rob_betas %>% filter(grupo %in% c("1_nucleo", "servidores", "semiconductores")) %>%
        select(especificacion, grupo, n_fracciones, beta_relativo, ee_relativo, beta_absoluto),
      n = Inf)

cat("\nImpacto total (%), version relativa:\n")
print(rob_imp %>% filter(version_beta == "relativo") %>%
        select(especificacion, escenario, var_pct) %>%
        mutate(var_pct = round(100 * var_pct, 1)) %>%
        pivot_wider(names_from = escenario, values_from = var_pct), n = Inf)

message("\nListo. Resultados en: ", dir_rob)
