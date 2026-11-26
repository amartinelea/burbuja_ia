# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/07_analisis_choque_ia_anam.R
# Objetivo:       Choque por estallido de la burbuja IA sobre importaciones de electronicos.
#                 Cadena: valuacion en bolsa (por grupo) -> capex hyperscalers/datacenters -> valor
#                 importado por fraccion segun exposicion a la canasta IA.
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          13/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Etapa 1 del modelo: cuanto recortan su gasto de capital los
#     hiperescaladores cuando cae su valuacion en bolsa (theta).
#   - Aqui se definen los escenarios A a F (tamano del choque, lambda y efecto
#     de arrastre); si se cambia un escenario, es aqui.
#   - Tambien trae la cadena agregada de la version anterior del modelo, que ya
#     no es la principal.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Requiere: salidas de 01_construye_datos_empresas.R en datos/empresas/
#             salidas de 02_prepara_panel_anam.R en output/panel/ (panel mensual
#             de ANAM, en USD, que aqui se agrega a fraccion x trimestre)
#
#   Nota: recaudacion = pagos en efectivo de todas las contribuciones (IVA, IGI,
#         DTA, ...), en pesos
# _____________________________________________________________________________

# Limpiar entorno de trabajo. Si el script lo llama 08_robustez_calibracion.R
# (con params_override), NO se limpia: borraria los parametros de la corrida.
if (!exists("params_override", inherits = FALSE)) {
  rm(list = ls())       # Limpiar entorno de trabajo
  # source("~/.Rprofile") # Cargar configuraciones globales
  cat("\014")           # Limpiar consola
}

options(encoding = "UTF-8", scipen = 999)
# Paquetes: instala los que falten y los carga
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate",
               "fixest", "sandwich", "readxl")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
# Cualquier parametro se puede sobrescribir desde otro script definiendo antes
# una lista `params_override` (lo usa 08_robustez_calibracion.R).
.p <- if (exists("params_override")) params_override else list()
get_p <- function(nombre, defecto) if (!is.null(.p[[nombre]])) .p[[nombre]] else defecto

dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_out   <- get_p("dir_out", file.path(dir_base, "output", "choque_ia"))
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# ---- Panel de ANAM (salidas del script 02) ----
dir_panel   <- get_p("dir_panel", file.path(dir_base, "output", "panel"))
origen      <- get_p("origen", "USA")        # "USA" o "todos" (todos los paises de origen)
regimen_sel <- get_p("regimen", "todos")     # "todos", "definitiva" (A1, C1) o "temporal" (IN, AF)
estimador   <- get_p("estimador", "ppml")    # "ppml" (recomendado) u "ols" (log(1 + valor))
rezago_trim <- get_p("rezago_trim", 0)       # capex adelantado k trimestres (sensibilidad: 1)
# Regresion directa del comercio contra la valuacion en bolsa (robustez): indice usado
grupo_mcap_directo <- get_p("grupo_mcap_directo", "es_hyperscaler")   # o "es_semiconductores"

incluir_dc_capex <- get_p("incluir_dc_capex", FALSE)  # sumar capex de neoclouds/datacenters al de hyperscalers

# Especificacion del eslabon 1:
#  "agregado":    capex agregado contra valuacion en bolsa agregado (version original)
#  "panel_ratio": panel por empresa con un ratio financiero y su interaccion
#                 con la valuacion en bolsa (si las empresas mas endeudadas recortan mas)
theta_spec   <- get_p("theta_spec", "panel_ratio")
ratio_theta  <- get_p("ratio_theta", "capex_ocf")  # principal; robustez: deuda_ingresos, deuda_ebitda, capex_ingresos
theta_manual   <- get_p("theta_manual", NA_real_)  # elasticidad capex a valuacion en bolsa; NA = usar estimada
# Controles macro del paso 1 (salen de datos/empresas/macro_trimestral.csv,
# script 01). Entran como cambio de 4 trimestres, rezagado un trimestre, igual
# que la valuacion en bolsa. Tasas y diferencial en puntos porcentuales; sp500 y vix en log.
#   tasa10, fedfunds, spread_baa: mueven el costo de capital (y tambien la valuacion en bolsa)
#   sp500: mercado en general; con el, theta mide solo el componente propio del
#          valuacion en bolsa de las empresas (cambia la interpretacion; usar como robustez)
controles_theta <- get_p("controles_theta", c("tasa10", "spread_baa"))
# Tasa cuyo coeficiente (gamma) pasa a la simulacion: escenarios con delta_tasa
tasa_sim <- get_p("tasa_sim", "tasa10")
# Inicio de la muestra del paso 1. Es independiente del panel de aduanas (2022+):
# el paso 1 solo usa datos de empresas, que existen desde 2012. Robustez: 2018 o
# 2022 (si el ciclo de IA cambio la relacion; con 2022 quedan ~14 trimestres).
theta_desde <- as.Date(get_p("theta_desde", "2012-01-01"))
# Acotar el ratio a estos percentiles de la muestra del paso 1 antes de estimar,
# para que valores extremos (ej. Applied Digital, capex/flujo ~20) no dominen la
# interaccion. FALSE para no acotar.
ratio_acotar <- get_p("ratio_acotar", c(0.05, 0.95))

# ---- Confianza estadistica ----
# Robustez del paso 2: tendencia lineal propia de cada grupo. Con ella, eta se
# identifica solo con las desviaciones del capex respecto a su tendencia (el
# capex 2022 a 2026 es casi una recta; sin esto, eta puede captar cualquier
# crecimiento diferencial del nucleo).
tendencia_grupo <- get_p("tendencia_grupo", FALSE)
# Robustez del paso 2: "d4" estima en cambios de 4 trimestres (D4 ln comercio
# contra D4 ln capex, OLS ponderado), que tambien quita la tendencia.
forma_paso2 <- get_p("forma_paso2", "niveles")
# Robustez del paso 2: solo NICO con comercio positivo en TODOS los trimestres
# (margen intensivo). En logaritmos (OLS, cambios de 4 trimestres) las entradas y
# salidas de NICO chicos (de 0 a positivo) pueden dominar el coeficiente.
solo_continuos <- get_p("solo_continuos", FALSE)
nivel_ic   <- get_p("nivel_ic", 0.90)    # intervalos reportados (0.90 o 0.95)
n_sim      <- get_p("n_sim", 2000)       # simulaciones Monte Carlo de los impactos (0 = no)
n_placebo  <- get_p("n_placebo", 200)    # permutaciones de la prueba placebo del paso 2 (0 = no)
semilla    <- get_p("semilla", 20261117)
z_ic <- qnorm(1 - (1 - nivel_ic) / 2)
tabla_coef <- function(est, ee, termino, n = NA, modelo = "") {
  tibble(modelo, termino, estimado = est, ee,
         ic_inf = est - z_ic * ee, ic_sup = est + z_ic * ee,
         valor_p = 2 * pnorm(-abs(est / ee)), n_obs = n)
}

# Agrupacion con la que se estima beta y se simula:
#  "canasta": nivel de exposicion (nucleo, infraestructura, mixta)  [principal]
#  "eslabon": tipo de producto, mismo vocabulario que las empresas
#             (semiconductores, servidores, redes, energia_enfriamiento, ...)
# Los resultados se reportan por AMBAS agrupaciones en cualquier caso.
agrupacion_beta <- get_p("agrupacion_beta", "canasta")

# Fracciones a quitar de la muestra (robustez; ej. "84733004")
excluir_fracciones <- get_p("excluir_fracciones", character())   # 8 digitos (fraccion) o 10 (NICO)
# Unidad del paso 2: "nico" (10 digitos, fraccion + subdivision) o "fraccion" (8)
unidad <- get_p("unidad", "nico")

# =============================================================================
# 1. Canasta IA por subpartida (6 digitos). REVISAR A MANO.
# =============================================================================
# La clasificacion vive en input/canasta_ia_subpartidas.csv (subpartida a
# 6 digitos -> nivel). Editar ahi, no aqui. Cualquier fraccion cuya subpartida
# no este en el archivo queda como resto (control).
archivo_canasta <- file.path(dir_base, "input", "canasta_ia_subpartidas.csv")
# Robustez: TRUE quita de la muestra las fracciones con contaminacion_no_ia
# "alta" (mucho uso fuera de data centers). No pasan a resto: se eliminan,
# para no contaminar tampoco al grupo control.
excluir_contaminacion_alta <- get_p("excluir_contaminacion_alta", FALSE)
canasta_def <- read_csv(archivo_canasta, locale = locale(encoding = "UTF-8"),
                        col_types = cols(subpartida = col_character()),
                        show_col_types = FALSE) %>%
  mutate(subpartida = str_pad(subpartida, 6, pad = "0"))
subp_excluidas <- if (excluir_contaminacion_alta)
  canasta_def$subpartida[canasta_def$contaminacion_no_ia == "alta"] else character()

ref_resto <- "4_resto_electronicos"     # grupo de control en ambas agrupaciones

# Ajustes por fraccion (8 digitos) o por NICO (10 digitos): GANAN sobre la regla
# por subpartida; un ajuste a NICO gana sobre uno a la fraccion.
# Para sacar algo de la canasta: nivel y eslabon = 4_resto_electronicos.
archivo_ajustes <- file.path(dir_base, "input", "canasta_ia_ajustes_fraccion.csv")
ajustes <- if (file.exists(archivo_ajustes))
  read_csv(archivo_ajustes, locale = locale(encoding = "UTF-8"),
           col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
  mutate(fraccion = gsub("[^0-9]", "", fraccion),
         fraccion = if_else(nchar(fraccion) > 8, str_pad(fraccion, 10, pad = "0"), str_pad(fraccion, 8, pad = "0"))) else
  tibble(fraccion = character(), nivel = character(), eslabon = character())
message("Ajustes por fraccion leidos: ", nrow(ajustes),
        if (!file.exists(archivo_ajustes)) " (NO se encontro el archivo de ajustes)" else "")

# fr puede venir a 8 o 10 digitos; orden: ajuste a NICO, ajuste a fraccion, subpartida
clasifica <- function(fr, campo) {
  f8 <- str_sub(fr, 1, 8); sp <- str_sub(fr, 1, 6)
  coalesce(if (any(nchar(fr) == 10)) ajustes[[campo]][match(fr, ajustes$fraccion)] else NA_character_,
           ajustes[[campo]][match(f8, ajustes$fraccion)],
           canasta_def[[campo]][match(sp, canasta_def$subpartida)], ref_resto)
}
clasifica_ia      <- function(fr) clasifica(fr, "nivel")
clasifica_eslabon <- function(fr) clasifica(fr, "eslabon")
# Asigna la columna "grupo" segun la agrupacion elegida para beta
agrega_grupos <- function(d) {
  d %>% mutate(canasta = clasifica_ia(fraccion),
               eslabon = clasifica_eslabon(fraccion),
               grupo   = if (agrupacion_beta == "eslabon") eslabon else canasta)
}
lee <- function(f) read_csv(f, locale = locale(encoding = "UTF-8"),
                            col_types = cols(fraccion = col_character()),
                            show_col_types = FALSE)

# =============================================================================
# 2. Datos de empresas (script 01)
# =============================================================================
empresas    <- read_csv(file.path(dir_emp, "empresas_ia.csv"),
                                   locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
capex_emp   <- read_csv(file.path(dir_emp, "capex_trimestral_empresa.csv"), show_col_types = FALSE)
indices     <- read_csv(file.path(dir_emp, "mcap_indices_grupo.csv"), show_col_types = FALSE)

# Capex agregado como indice ENCADENADO: el crecimiento de cada trimestre usa
# solo las empresas presentes en ese trimestre y en el anterior, asi que la
# entrada de una empresa (ej. CoreWeave en 2025) no se lee como un salto del
# capex. El nivel se escala para que el ultimo trimestre sea el capex real.
# Empieza cuando ya estan todos los hiperescaladores.
grupo_capex <- empresas %>%
  filter(es_hyperscaler | (incluir_dc_capex & es_datacenter)) %>% pull(empresa)
capex_bal <- capex_emp %>% filter(empresa %in% grupo_capex)
t0 <- capex_emp %>% filter(empresa %in% empresas$empresa[empresas$es_hyperscaler]) %>%
  group_by(empresa) %>% summarise(t = min(trimestre)) %>% pull(t) %>% max()
capex_tot <- capex_bal %>%
  filter(trimestre >= t0) %>%
  arrange(empresa, trimestre) %>%
  group_by(empresa) %>%
  mutate(previo = lag(capex_musd),
         continua = !is.na(previo) & lag(trimestre) == trimestre %m-% months(3)) %>%
  group_by(trimestre) %>%
  summarise(total_musd = sum(capex_musd), n = n(),
            crec = if (any(continua)) sum(capex_musd[continua]) / sum(previo[continua]) else NA_real_,
            .groups = "drop") %>%
  arrange(trimestre) %>%
  mutate(crec = if_else(row_number() == 1 | is.na(crec), 1, crec),
         idx_capex = cumprod(crec),
         capex_musd = idx_capex / last(idx_capex) * last(total_musd)) %>%
  select(trimestre, capex_musd, total_musd, n)
message("Capex agregado (encadenado) desde ", t0, "; empresas en el ultimo trimestre: ", last(capex_tot$n))

# =============================================================================
# 3. Eslabon 1: valuacion en bolsa -> capex (theta)
#    D4 ln capex_t = a + theta1 D4 ln mcap_{t-1} + theta2 D4 ln mcap_{t-2}
#    theta = theta1 + theta2. Errores Newey-West.
# =============================================================================
# Controles macro: cambio de 4 trimestres, rezagado un trimestre
archivo_macro <- file.path(dir_emp, "macro_trimestral.csv")
if (length(controles_theta) && !file.exists(archivo_macro)) {
  message("Aviso: no esta ", archivo_macro, " (seccion 7 del script 01); paso 1 sin controles")
  controles_theta <- character()
}
macro_ctrl <- if (length(controles_theta)) {
  read_csv(archivo_macro, show_col_types = FALSE) %>%
    mutate(trimestre = as.Date(trimestre)) %>% arrange(trimestre) %>%
    select(trimestre, any_of(controles_theta)) %>%
    mutate(across(-trimestre, ~ if (cur_column() %in% c("sp500", "vix"))
                    log(.x) - lag(log(.x), 4) else .x - lag(.x, 4))) %>%
    mutate(across(-trimestre, ~ lag(.x, 1))) %>%
    rename_with(~ paste0("c_", .x), -trimestre)
} else tibble(trimestre = as.Date(character()))
faltan_ctrl <- setdiff(paste0("c_", controles_theta), names(macro_ctrl))
if (length(faltan_ctrl)) {
  message("Aviso: controles no encontrados en macro_trimestral: ", paste(faltan_ctrl, collapse = ", "))
  controles_theta <- setdiff(controles_theta, sub("^c_", "", faltan_ctrl))
}
terminos_ctrl <- if (length(controles_theta)) paste("+", paste0("c_", controles_theta, collapse = " + ")) else ""
gamma_de <- function(modelo) {
  nm <- paste0("c_", tasa_sim)
  v <- if (nm %in% names(coef(modelo))) unname(coef(modelo)[nm]) else NA_real_
  if (is.na(v)) 0 else v
}

mcap_q <- indices %>%
  filter(indicadora == "es_hyperscaler") %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>%
  group_by(trimestre) %>%
  summarise(idx = mean(coalesce(idx_ponderado, idx_equiponderado)), .groups = "drop")

d1 <- capex_tot %>% inner_join(mcap_q, by = "trimestre") %>% arrange(trimestre) %>%
  mutate(d4_capex = log(capex_musd) - lag(log(capex_musd), 4),
         d4_mcap  = log(idx) - lag(log(idx), 4),
         d4_mcap_l1 = lag(d4_mcap, 1), d4_mcap_l2 = lag(d4_mcap, 2)) %>%
  filter(!is.na(d4_capex), !is.na(d4_mcap_l2)) %>%
  left_join(macro_ctrl, by = "trimestre") %>%
  filter(if_all(starts_with("c_"), ~ !is.na(.x)), trimestre >= theta_desde)
if (nrow(d1) < 20) message("Aviso: el paso 1 agregado solo tiene ", nrow(d1), " trimestres")

m1 <- lm(as.formula(paste("d4_capex ~ d4_mcap_l1 + d4_mcap_l2", terminos_ctrl)), data = d1)
V1 <- NeweyWest(m1, lag = 4, prewhite = FALSE)
theta_est <- sum(coef(m1)[c("d4_mcap_l1", "d4_mcap_l2")])
theta_se  <- sqrt(sum(V1[c("d4_mcap_l1", "d4_mcap_l2"), c("d4_mcap_l1", "d4_mcap_l2")]))
message(sprintf("theta estimada = %.3f (ee NW %.3f, n = %d)", theta_est, theta_se, nrow(d1)))
theta_agregado <- theta_est
coef_paso1 <- tabla_coef(coef(m1)[rownames(V1)], sqrt(diag(V1)), rownames(V1), nrow(d1), "paso1_agregado_NW")
gamma_est <- gamma_de(m1)
gamma_se  <- if (paste0("c_", tasa_sim) %in% rownames(V1))
  sqrt(V1[paste0("c_", tasa_sim), paste0("c_", tasa_sim)]) else NA_real_
cf1 <- coef(m1)[!is.na(coef(m1))]
print(data.frame(coef = cf1, ee_nw = sqrt(diag(V1))[names(cf1)]))

# ---- Version con ratio financiero (panel por empresa) -------------------------
#  D4 ln K_it = a_i + th1 D4 ln MC_i,t-1 + th2 D4 ln MC_i,t-2 + phi R_i,t-1
#               + psi1 (D4 ln MC_i,t-1 x R_i,t-1) + psi2 (D4 ln MC_i,t-2 x R_i,t-1) + u_it
#  R centrado en su promedio: th1 + th2 = respuesta de una empresa con
#  apalancamiento promedio; psi = cuanto cambia esa respuesta por cada punto
#  adicional del ratio. Errores Driscoll-Kraay (pocas empresas, choques comunes).
#  theta efectiva = th1 + th2 + (psi1 + psi2) x (ratio actual - promedio),
#  con el ratio actual ponderado por capex de las empresas.
theta_panel <- NULL
if (theta_spec == "panel_ratio") {
  archivo_ratios <- file.path(dir_emp, "ratios_financieros_trimestral.csv")
  if (!file.exists(archivo_ratios)) stop("Falta ", archivo_ratios, ": corre la version nueva del script 01")
  ratios <- read_csv(archivo_ratios, show_col_types = FALSE)
  precios_q <- read_csv(file.path(dir_emp, "mcap_mensual_empresa.csv"), show_col_types = FALSE) %>%
    mutate(trimestre = floor_date(mes, "quarter")) %>%
    group_by(empresa, trimestre) %>%
    summarise(precio = mean(precio_usd, na.rm = TRUE), .groups = "drop")   # D4 ln precio = D4 ln valuacion en bolsa

  dp <- capex_emp %>%
    filter(empresa %in% grupo_capex) %>%
    select(empresa, trimestre, capex_musd) %>%
    inner_join(precios_q, by = c("empresa", "trimestre")) %>%
    left_join(ratios %>% select(empresa, trimestre, ratio = all_of(ratio_theta)),
              by = c("empresa", "trimestre")) %>%
    group_by(empresa) %>%
    complete(trimestre = seq(min(trimestre), max(trimestre), by = "quarter")) %>%
    arrange(trimestre) %>%
    mutate(d4_capex = log(capex_musd) - lag(log(capex_musd), 4),
           d4_mcap  = log(precio) - lag(log(precio), 4),
           d4_mcap_l1 = lag(d4_mcap, 1), d4_mcap_l2 = lag(d4_mcap, 2),
           ratio_l1 = lag(ratio, 1)) %>%
    ungroup() %>%
    filter(is.finite(d4_capex), is.finite(d4_mcap_l2), is.finite(ratio_l1)) %>%
    left_join(macro_ctrl, by = "trimestre") %>%
    filter(if_all(starts_with("c_"), ~ !is.na(.x)), trimestre >= theta_desde) %>%
    mutate(t_id = as.integer(factor(trimestre)))

  acota <- function(x) x
  if (!isFALSE(ratio_acotar) && !is.null(ratio_acotar)) {
    lim <- quantile(dp$ratio_l1, ratio_acotar, na.rm = TRUE)
    acota <- function(x) pmin(pmax(x, lim[1]), lim[2])
    message(sprintf("Ratio %s acotado a [%.2f, %.2f] (percentiles %s)", ratio_theta, lim[1], lim[2],
                    paste(100 * ratio_acotar, collapse = "-")))
  }
  dp <- dp %>% mutate(ratio_l1 = acota(ratio_l1))
  ratio_media <- mean(dp$ratio_l1)
  dp <- dp %>% mutate(r_c = ratio_l1 - ratio_media)

  m1p <- feols(as.formula(paste("d4_capex ~ d4_mcap_l1 + d4_mcap_l2 + r_c + d4_mcap_l1:r_c + d4_mcap_l2:r_c",
                                terminos_ctrl, "| empresa")),
               data = dp, panel.id = ~ empresa + t_id, vcov = "DK")
  print(summary(m1p))
  coef_paso1 <- bind_rows(coef_paso1,
    tabla_coef(coef(m1p), se(m1p), names(coef(m1p)), nobs(m1p), paste0("paso1_panel_", ratio_theta, "_DK")))

  # Ratio actual: ultimo trimestre de cada empresa, ponderado por su capex
  ratio_hoy <- ratios %>%
    filter(empresa %in% grupo_capex, !is.na(.data[[ratio_theta]])) %>%
    group_by(empresa) %>% slice_max(trimestre, n = 1) %>% ungroup() %>%
    left_join(capex_emp %>% group_by(empresa) %>% slice_max(trimestre, n = 4) %>%
                summarise(peso = sum(capex_musd), .groups = "drop"), by = "empresa") %>%
    summarise(r = weighted.mean(acota(.data[[ratio_theta]]), peso, na.rm = TRUE)) %>% pull(r)

  b <- coef(m1p); Vp <- vcov(m1p)
  g <- setNames(rep(0, length(b)), names(b))
  g[c("d4_mcap_l1", "d4_mcap_l2")] <- 1
  ints <- grep(":r_c|r_c:", names(b), value = TRUE)
  g[ints] <- ratio_hoy - ratio_media
  theta_panel_est <- sum(g * b)
  theta_panel_se  <- sqrt(as.numeric(t(g) %*% Vp %*% g))
  psi <- sum(b[ints])

  theta_panel <- tibble(ratio = ratio_theta, ratio_media, ratio_hoy,
                        theta_promedio = sum(b[c("d4_mcap_l1", "d4_mcap_l2")]),
                        psi_interaccion = psi,
                        theta_efectiva = theta_panel_est, ee_theta_efectiva = theta_panel_se,
                        n_obs = nobs(m1p), n_empresas = n_distinct(dp$empresa))
  print(theta_panel)
  message(sprintf("theta efectiva con %s actual (%.2f vs promedio %.2f) = %.3f (ee DK %.3f)",
                  ratio_theta, ratio_hoy, ratio_media, theta_panel_est, theta_panel_se))
  theta_est <- theta_panel_est
  theta_se  <- theta_panel_se
  coef_paso1 <- bind_rows(coef_paso1,
    tabla_coef(theta_panel_est, theta_panel_se, "theta_efectiva (delta)", nobs(m1p), "paso1_panel_combinacion"))
  gamma_est <- gamma_de(m1p)
  gamma_se  <- if (paste0("c_", tasa_sim) %in% names(b)) sqrt(Vp[paste0("c_", tasa_sim), paste0("c_", tasa_sim)]) else NA_real_
}

theta <- if (is.na(theta_manual)) theta_est else theta_manual
# gamma: cambio en D4 ln capex por cada punto porcentual de alza en la tasa
# (manteniendo fijo la valuacion en bolsa). Negativa si el costo de capital frena el capex.
gamma <- get_p("gamma_manual", gamma_est)
message(sprintf("gamma (%s) = %.3f (ee %.3f); controles del paso 1: %s", tasa_sim, gamma, gamma_se,
                if (length(controles_theta)) paste(controles_theta, collapse = ", ") else "ninguno"))

# =============================================================================
# 4. Datos ANAM y eslabon 2: capex -> importaciones por nivel de canasta (beta)
# =============================================================================
forma_reducida <- NULL
{

  lee_panel <- function(f) read_csv(file.path(dir_panel, f), locale = locale(encoding = "UTF-8"),
                                    col_types = cols(fraccion = col_character(), pais = col_character(),
                                                     .default = col_guess()), show_col_types = FALSE)
  filtra <- function(d) d %>%
    filter(tipo_operacion == "importacion",
           origen == "todos" | pais == origen,
           regimen_sel == "todos" | regimen == regimen_sel)

  # "fraccion" es el identificador de la unidad: NICO (10 digitos) o fraccion (8)
  id_unidad <- function(d) d %>%
    mutate(fraccion = str_pad(fraccion, 8, pad = "0"),
           fraccion = if (unidad == "nico" && "nico" %in% names(d))
             paste0(fraccion, str_pad(nico, 2, pad = "0")) else fraccion)
  pfm <- lee_panel("panel_fraccion_pais_mes.csv") %>% filtra() %>%
    mutate(mes = as.Date(mes), trimestre = floor_date(mes, "quarter")) %>% id_unidad()
  # Solo trimestres completos (3 meses con datos)
  trim_ok <- pfm %>% distinct(trimestre, mes) %>% count(trimestre) %>% filter(n == 3) %>% pull(trimestre)

  # Recaudacion (pesos, pagos en efectivo, todas las contribuciones). El regimen
  # se infiere de la clave: el archivo de recaudacion no lo trae.
  rec <- lee_panel("recaudacion_fraccion_pais_mes.csv") %>%
    filter(tipo_operacion == "importacion", origen == "todos" | pais == origen) %>%
    mutate(trimestre = floor_date(as.Date(mes), "quarter")) %>% id_unidad() %>%
    group_by(fraccion, trimestre) %>% summarise(recaud_mxn = sum(importe_mxn), .groups = "drop")

  capex_q <- capex_tot %>% transmute(trimestre = trimestre %m+% months(3 * rezago_trim),
                                     ln_capex = log(capex_musd)) %>%
    arrange(trimestre) %>%
    mutate(ln_capex_f1 = lead(ln_capex))   # capex del trimestre siguiente (prueba de anticipacion)
  mc_dir <- indices %>% filter(indicadora == grupo_mcap_directo) %>%
    mutate(trimestre = floor_date(mes, "quarter")) %>%
    group_by(trimestre) %>%
    summarise(idx = mean(coalesce(idx_ponderado, idx_equiponderado)), .groups = "drop") %>%
    arrange(trimestre) %>% transmute(trimestre, ln_mc_l1 = lag(log(idx), 1))

  panel <- pfm %>% filter(trimestre %in% trim_ok) %>%
    group_by(fraccion, trimestre) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
    agrega_grupos() %>%
    filter(!str_sub(fraccion, 1, 6) %in% subp_excluidas,
           !fraccion %in% excluir_fracciones, !str_sub(fraccion, 1, 8) %in% excluir_fracciones) %>%
    complete(nesting(fraccion, canasta, eslabon, grupo), trimestre, fill = list(valor_usd = 0)) %>%
    left_join(rec, by = c("fraccion", "trimestre")) %>%
    mutate(recaud_mxn = coalesce(recaud_mxn, 0)) %>%
    inner_join(capex_q, by = "trimestre") %>%
    left_join(mc_dir, by = "trimestre") %>%
    group_by(fraccion) %>% mutate(peso = mean(valor_usd)) %>% ungroup() %>%
    filter(peso > 0) %>%
    mutate(ln_valor = log1p(valor_usd), trim_del_anio = quarter(trimestre),
           t_num = as.numeric(factor(trimestre)))
  if (isTRUE(solo_continuos)) {
    continuos <- panel %>% group_by(fraccion) %>% summarise(ok = all(valor_usd > 0), .groups = "drop") %>%
      filter(ok) %>% pull(fraccion)
    message("Margen intensivo: ", length(continuos), " de ", n_distinct(panel$fraccion),
            " unidades con comercio en todos los trimestres")
    panel <- panel %>% filter(fraccion %in% continuos)
  }
  if (forma_paso2 == "d4") {
    # Cambios de 4 trimestres de cada variable; se pierde el primer anio
    estimador <- "ols"
    panel <- panel %>% arrange(fraccion, trimestre) %>% group_by(fraccion) %>%
      mutate(ln_valor = ln_valor - lag(ln_valor, 4), ln_capex = ln_capex - lag(ln_capex, 4),
             ln_capex_f1 = ln_capex_f1 - lag(ln_capex_f1, 4), ln_mc_l1 = ln_mc_l1 - lag(ln_mc_l1, 4)) %>%
      ungroup() %>% filter(!is.na(ln_valor), !is.na(ln_capex))
    message("Paso 2 en cambios de 4 trimestres (OLS ponderado)")
  }
  message("Unidad del paso 2: ", unidad)
  message(sprintf("Panel trimestral: %d fracciones, %d trimestres (%s a %s), origen %s, regimen %s",
                  n_distinct(panel$fraccion), n_distinct(panel$trimestre),
                  min(panel$trimestre), max(panel$trimestre), origen, regimen_sel))

  # Paso 2. Relativa: FE fraccion + trimestre; eta = respuesta de cada grupo
  # frente al resto de electronicos. Absoluta: FE fraccion + trimestre del anio.
  estima <- function(rhs) {
    if (estimador == "ppml") fepois(as.formula(paste("valor_usd ~", rhs)), data = panel, cluster = ~fraccion)
    else feols(as.formula(paste("ln_valor ~", rhs)), data = panel, weights = ~peso, cluster = ~fraccion)
  }
  tend <- if (isTRUE(tendencia_grupo)) " + i(grupo, t_num, ref = ref_resto)" else ""
  m2  <- estima(paste0("i(grupo, ln_capex, ref = ref_resto)", tend, " | fraccion + trimestre"))
  m2b <- estima(paste0("i(grupo, ln_capex)", tend, " | fraccion + trim_del_anio"))
  solo_capex <- function(ct) ct[grepl(":ln_capex$", rownames(ct)), , drop = FALSE]
  print(summary(m2))

  # ---- Confianza del paso 2 ----
  ct2 <- solo_capex(coeftable(m2))
  coef_paso2 <- tabla_coef(ct2[, 1], ct2[, 2], rownames(ct2), nobs(m2), paste0("paso2_relativo_", estimador))
  ct2b <- solo_capex(coeftable(m2b))
  coef_paso2 <- bind_rows(coef_paso2,
    tabla_coef(ct2b[, 1], ct2b[, 2], rownames(ct2b), nobs(m2b), paste0("paso2_absoluto_", estimador)))

  # Anticipacion (pretendencia): si el comercio responde al capex del trimestre
  # SIGUIENTE con el capex actual en la regresion, hay algo que mueve a ambos
  # (o causalidad inversa). Esperado: coeficientes del adelanto cercanos a 0.
  m2_ade <- estima(paste0("i(grupo, ln_capex, ref = ref_resto) + i(grupo, ln_capex_f1, ref = ref_resto)", tend,
                          " | fraccion + trimestre"))
  ctl <- coeftable(m2_ade)
  coef_paso2 <- bind_rows(coef_paso2,
    tabla_coef(ctl[, 1], ctl[, 2], rownames(ctl), nobs(m2_ade), "paso2_con_adelanto"))

  # Placebo por permutacion: se reparten las etiquetas de la canasta al azar
  # entre fracciones (mismo numero por grupo) y se reestima. valor_p = parte de
  # las permutaciones con un coeficiente al menos tan grande como el real.
  placebo <- NULL
  if (n_placebo > 0) {
    set.seed(semilla)
    gr_fr <- panel %>% distinct(fraccion, grupo)
    reales <- setNames(ct2[, 1], sub("^grupo::(.*):ln_capex$", "\\1", rownames(ct2)))
    perm <- map_dfr(seq_len(n_placebo), function(k) {
      mapa <- gr_fr %>% mutate(grupo_p = sample(grupo))
      dpp <- panel %>% select(-grupo) %>% left_join(mapa %>% select(fraccion, grupo = grupo_p), by = "fraccion")
      f_p <- as.formula(paste0(if (estimador == "ppml") "valor_usd" else "ln_valor",
                               " ~ i(grupo, ln_capex, ref = ref_resto)", tend, " | fraccion + trimestre"))
      m <- tryCatch(if (estimador == "ppml") fepois(f_p, data = dpp, notes = FALSE)
                    else feols(f_p, data = dpp, weights = ~peso, notes = FALSE), error = function(e) NULL)
      if (is.null(m)) return(NULL)
      cf <- coef(m)[grepl(":ln_capex$", names(coef(m)))]
      tibble(k, grupo = sub("^grupo::(.*):ln_capex$", "\\1", names(cf)), coef = unname(cf))
    })
    placebo <- perm %>% group_by(grupo) %>%
      summarise(permutaciones = n(), placebo_media = mean(coef), placebo_p95 = quantile(coef, 0.95),
                .groups = "drop") %>%
      mutate(eta_real = reales[grupo]) %>%
      left_join(perm %>% mutate(eta_real = reales[grupo]) %>% group_by(grupo) %>%
                  summarise(valor_p_permutacion = mean(abs(coef) >= abs(first(eta_real))), .groups = "drop"),
                by = "grupo")
    print(placebo)
  }
  ct  <- solo_capex(coeftable(m2)); ctb <- solo_capex(coeftable(m2b))
  betas <- full_join(
    tibble(grupo = sub("^grupo::(.*):ln_capex$", "\\1", rownames(ct)),
           beta_relativo = ct[, 1], ee_relativo = ct[, 2]),
    tibble(grupo = sub("^grupo::(.*):ln_capex$", "\\1", rownames(ctb)),
           beta_absoluto = ctb[, 1], ee_absoluto = ctb[, 2]) %>% filter(grupo != ref_resto),
    by = "grupo")

  # Robustez: regresion directa (forma reducida) del comercio contra el market
  # cap, sin pasar por el capex. Si el canal es solo el capex, el efecto directo
  # deberia parecerse a theta x eta; si es mucho mayor, hay otros canales
  # (expectativas, financiamiento, inventarios).
  m2r <- estima(paste0("i(grupo, ln_mc_l1, ref = ref_resto)", tend, " | fraccion + trimestre"))
  ctr <- coeftable(m2r); ctr <- ctr[grepl(":ln_mc_l1$", rownames(ctr)), , drop = FALSE]
  forma_reducida <- tibble(grupo = sub("^grupo::(.*):ln_mc_l1$", "\\1", rownames(ctr)),
                           efecto_directo_mc = ctr[, 1], ee_directo = ctr[, 2]) %>%
    left_join(betas %>% select(grupo, beta_relativo), by = "grupo") %>%
    mutate(indice_mc = grupo_mcap_directo,
           theta_x_eta = theta * beta_relativo,
           cociente_directo_vs_cadena = efecto_directo_mc / theta_x_eta)
  print(forma_reducida)
  coef_paso2 <- bind_rows(coef_paso2,
    tabla_coef(ctr[, 1], ctr[, 2], rownames(ctr), nobs(m2r), paste0("forma_reducida_", grupo_mcap_directo)))

  # Varianzas de eta para la simulacion Monte Carlo
  nm_rel <- grep("^grupo::.*:ln_capex$", names(coef(m2)), value = TRUE)
  nm_abs <- grep("^grupo::.*:ln_capex$", names(coef(m2b)), value = TRUE)
  nm_abs <- nm_abs[!grepl(paste0("::", ref_resto, ":"), nm_abs)]
  V_eta <- list(relativo = vcov(m2)[nm_rel, nm_rel, drop = FALSE],
                absoluto = vcov(m2b)[nm_abs, nm_abs, drop = FALSE])
  eta_hat <- list(relativo = coef(m2)[nm_rel], absoluto = coef(m2b)[nm_abs])

  # Valor base: ultimos 4 trimestres completos. OJO unidades en este modo:
  # valor_mdp = millones de DOLARES; recaudacion_mdp = millones de PESOS.
  ult4 <- sort(unique(panel$trimestre), decreasing = TRUE)[1:4]
  valor_base <- panel %>% filter(trimestre %in% ult4) %>%
    group_by(fraccion, canasta, eslabon, grupo) %>%
    summarise(valor_mdp = sum(valor_usd) / 1e6, recaudacion_mdp = sum(recaud_mxn) / 1e6,
              .groups = "drop")

}

# Control: cuantas fracciones quedaron en cada grupo
print(count(valor_base, grupo))

betas <- bind_rows(betas, tibble(grupo = ref_resto,
                                 beta_relativo = 0, ee_relativo = 0,
                                 beta_absoluto = 0, ee_absoluto = 0))
# Una elasticidad negativa no tiene sentido economico aqui (infraestructura mal
# cubierta): se acota a 0 y se avisa.
if (any(betas$beta_absoluto < 0 | betas$beta_relativo < 0, na.rm = TRUE))
  message("Aviso: betas negativos acotados a 0: ",
          paste(betas$grupo[betas$beta_absoluto < 0 | betas$beta_relativo < 0], collapse = ", "))
print(betas)

# Formato largo para simular con ambas versiones (el rango del reporte)
betas_largo <- betas %>%
  transmute(grupo,
            relativo = pmax(beta_relativo, 0),
            absoluto = pmax(beta_absoluto, 0)) %>%
  pivot_longer(c(relativo, absoluto), names_to = "version_beta", values_to = "beta")

# Descriptivo por canasta y por eslabon
resume <- function(d, ...) d %>%
  group_by(...) %>%
  summarise(n_fracciones = n(), valor_mdp = sum(valor_mdp, na.rm = TRUE),
            recaudacion_mdp = sum(recaudacion_mdp, na.rm = TRUE), .groups = "drop") %>%
  mutate(share_valor = valor_mdp / sum(valor_mdp),
         carga_contrib = recaudacion_mdp / valor_mdp)
resumen_canasta <- resume(valor_base, canasta)
resumen_eslabon <- resume(valor_base, eslabon)
print(resumen_canasta); print(resumen_eslabon)

# =============================================================================
# 5. Escenarios (SUPUESTOS ILUSTRATIVOS: calibrar antes de reportar)
#    caidas de valuacion en bolsa por grupo (fraccion, -0.30 = -30%)
#    theta_dc: respuesta del capex de neoclouds (apalancadas, recortan mas)
#    arrastre: choque comun a todos los electronicos (recesion EUA)
#    mult_caida: asimetria; >1 en caidas (inventario), <1 en subidas (topes de oferta)
#    A. Ceteris paribus: todo se queda en el nivel actual (base de comparacion)
#    B. Ajuste de expectativas (neoclouds): correccion de valuaciones, golpe
#       concentrado en neoclouds apalancadas; hyperscalers solo posponen
#    C. Contraccion de la industria: estallido tipo punto com, recortes reales
#       de capex, sobreoferta de GPUs y recesion en EUA (arrastre)
#    D. Auge (aceleracion): el auge sigue y se intensifica (choque positivo)
#    E. Alza de tasas: la tasa sube delta_tasa puntos con la valuacion en bolsa fijo;
#       el capex cae por el costo de capital (gamma del paso 1)
#    F. Transicion a monetizacion: el capex cae de forma directa (capex_directo)
#       con el flujo y la valuacion en bolsa sin cambio; ej. capex/flujo operativo de
#       ~95% a ~65% con el flujo constante = capex -30%
#    delta_tasa: cambio en la tasa (pp). Si el escenario ya fija una caida del
#       valuacion en bolsa por las tasas, gamma suma solo el canal directo del costo de
#       capital. Calibrar con los futuros de fondos federales al momento de correr.
#    capex_directo: cambio directo del capex (fraccion), fuera de la cadena
# =============================================================================
escenarios <- get_p("escenarios", tribble(
  ~escenario,                              ~mcap_hyper, ~mcap_dc, ~mcap_semiconductores, ~mcap_intel, ~theta_dc, ~arrastre, ~mult_caida,
  "A. Ceteris paribus",                     0.00,        0.00,     0.00,        0.00,        1.0,       0.00,      1.0,
  "B. Ajuste de expectativas (neoclouds)", -0.20,       -0.60,    -0.35,       -0.40,        1.0,       0.00,      1.0,
  "C. Contraccion de la industria",        -0.40,       -0.80,    -0.55,       -0.60,        1.0,      -0.10,      1.3,
  # Auge continua: mult_caida < 1 porque al alza hay topes de oferta
  # (semiconductores, energia, capacidad de plantas en MX) que frenan la respuesta
  "D. Auge (aceleracion)",                 0.20,        0.40,     0.30,        0.30,        1.0,       0.00,      0.8,
  "E. Alza de tasas (+100 pb)",            0.00,        0.00,     0.00,        0.00,        1.0,       0.00,      1.0,
  "F. Transicion a monetizacion",          0.00,        0.00,     0.00,        0.00,        1.0,       0.00,      1.0))
# Columnas nuevas; si el tribble viene de otro script sin ellas, valen 0
if (!"delta_tasa" %in% names(escenarios))
  escenarios$delta_tasa <- if_else(str_starts(escenarios$escenario, "E\\."), 1.0, 0)
if (!"capex_directo" %in% names(escenarios))
  escenarios$capex_directo <- if_else(str_starts(escenarios$escenario, "F\\."), -0.30, 0)

# Pesos de capex (ultimos 4 trimestres) hyperscalers vs datacenters
pesos_capex <- capex_emp %>%
  filter(trimestre > max(trimestre) %m-% months(12)) %>%
  left_join(empresas %>% select(empresa, es_hyperscaler, es_datacenter), by = "empresa") %>%
  summarise(h = sum(capex_musd[es_hyperscaler]), dc = sum(capex_musd[es_datacenter]))
s_dc <- if (incluir_dc_capex) pesos_capex$dc / (pesos_capex$h + pesos_capex$dc) else 0

escenarios <- escenarios %>%
  mutate(dln_capex = (1 - s_dc) * theta * log1p(mcap_hyper) +
                     s_dc * theta * theta_dc * log1p(mcap_dc) +
                     gamma * delta_tasa + log1p(capex_directo),
         var_capex = exp(dln_capex) - 1)
print(escenarios %>% select(escenario, starts_with("mcap"), delta_tasa, capex_directo, var_capex))

simulacion <- crossing(valor_base, escenarios) %>%
  inner_join(betas_largo, by = "grupo", relationship = "many-to-many") %>%
  mutate(dln_sim        = beta * dln_capex * mult_caida + arrastre,
         valor_sim_mdp  = valor_mdp * exp(dln_sim),
         recaud_sim_mdp = recaudacion_mdp * exp(dln_sim),
         delta_valor_mdp  = valor_sim_mdp - valor_mdp,
         delta_recaud_mdp = recaud_sim_mdp - recaudacion_mdp)

# =============================================================================
# 5b. Intervalos de los impactos (Monte Carlo)
#    Se sacan n_sim valores de theta, gamma y eta de sus distribuciones
#    estimadas (normal; eta con su matriz de covarianzas completa) y se recalcula
#    el impacto de cada escenario. theta/gamma y eta vienen de muestras distintas
#    (empresas vs aduanas): se sortean de forma independiente. Los supuestos del
#    escenario (caida de la valuacion en bolsa, lambda, arrastre) NO tienen incertidumbre:
#    el intervalo mide solo la incertidumbre de la estimacion.
# =============================================================================
intervalos <- NULL
if (n_sim > 0) {
  set.seed(semilla + 1)
  sorteo_mvn <- function(mu, V, n) {
    L <- tryCatch(chol(V), error = function(e) chol(V + diag(1e-10, nrow(V))))
    sweep(matrix(rnorm(n * length(mu)), n) %*% L, 2, mu, "+")
  }
  th_s <- if (is.na(theta_manual)) rnorm(n_sim, theta, theta_se) else rep(theta, n_sim)
  ga_s <- if (!is.na(gamma_se)) rnorm(n_sim, gamma, gamma_se) else rep(gamma, n_sim)
  base_g <- valor_base %>% group_by(grupo) %>%
    summarise(valor = sum(valor_mdp), recaud = sum(recaudacion_mdp), .groups = "drop")
  intervalos <- map_dfr(names(eta_hat), function(v) {
    eta_s <- sorteo_mvn(eta_hat[[v]], V_eta[[v]], n_sim)
    colnames(eta_s) <- sub("^grupo::(.*):ln_capex$", "\\1", names(eta_hat[[v]]))
    eta_s <- pmax(eta_s, 0)                       # misma cota que la estimacion puntual
    map_dfr(seq_len(nrow(escenarios)), function(e) {
      sc <- escenarios[e, ]
      dln_k <- (1 - s_dc) * th_s * log1p(sc$mcap_hyper) + s_dc * th_s * sc$theta_dc * log1p(sc$mcap_dc) +
               ga_s * sc$delta_tasa + log1p(sc$capex_directo)
      # cambio por grupo en cada simulacion (n_sim x grupos)
      por_g <- sapply(base_g$grupo, function(g) {
        b <- if (g %in% colnames(eta_s)) eta_s[, g] else 0
        base_g$valor[base_g$grupo == g] * (exp(b * dln_k * sc$mult_caida + sc$arrastre) - 1)
      })
      por_g <- matrix(por_g, nrow = n_sim)
      tot <- rowSums(por_g)
      resume_ic <- function(x, base, nombre) tibble(
        version_beta = v, escenario = sc$escenario, nivel = nombre,
        delta_valor_p50 = median(x),
        delta_valor_inf = quantile(x, (1 - nivel_ic) / 2), delta_valor_sup = quantile(x, 1 - (1 - nivel_ic) / 2),
        var_pct_p50 = median(x) / base, var_pct_inf = quantile(x, (1 - nivel_ic) / 2) / base,
        var_pct_sup = quantile(x, 1 - (1 - nivel_ic) / 2) / base,
        prob_caida = mean(x < 0))
      bind_rows(resume_ic(tot, sum(base_g$valor), "total"),
                map_dfr(seq_along(base_g$grupo), ~ resume_ic(por_g[, .x], base_g$valor[.x], base_g$grupo[.x])))
    })
  }) %>% mutate(nivel_ic = nivel_ic, n_sim = n_sim)
  cat("\nIntervalos (", 100 * nivel_ic, "%) del impacto total, version relativa:\n", sep = "")
  print(intervalos %>% filter(nivel == "total", version_beta == "relativo") %>%
          select(escenario, var_pct_inf, var_pct_p50, var_pct_sup, prob_caida) %>%
          mutate(across(starts_with("var"), ~ round(100 * .x, 1))))
}

agrega_sim <- function(d, ...) d %>%
  group_by(version_beta, escenario, ...) %>%
  summarise(valor_base_mdp = sum(valor_mdp), valor_sim_mdp = sum(valor_sim_mdp),
            delta_valor_mdp = sum(delta_valor_mdp),
            delta_recaud_mdp = sum(delta_recaud_mdp), .groups = "drop") %>%
  mutate(var_pct = valor_sim_mdp / valor_base_mdp - 1)
resultado         <- agrega_sim(simulacion, canasta)
resultado_eslabon <- agrega_sim(simulacion, eslabon)

resultado_total <- resultado %>%
  group_by(version_beta, escenario) %>%
  summarise(across(c(valor_base_mdp, valor_sim_mdp, delta_valor_mdp, delta_recaud_mdp), sum),
            .groups = "drop") %>%
  mutate(var_pct = valor_sim_mdp / valor_base_mdp - 1)
print(resultado_total)

# Empresas por canal hacia Mexico. Para insumo_importado se cruza
# fraccion_canal (partida 4 dig o subpartida 6 dig) con las fracciones ANAM
# por prefijo, y se suma el cambio simulado en valor importado.
# Se lee directo de input/ para que un cambio en fraccion_canal no obligue a
# volver a correr el script 01 (que baja todo de la SEC y Yahoo)
canales <- read_csv(file.path(dir_base, "input", "empresas_ia_clasificacion.csv"),
                    col_types = cols(.default = col_character()),
                    locale = locale(encoding = "UTF-8"), show_col_types = FALSE) %>%
  transmute(empresa, ticker, grupo, canal_mexico,
            fraccion_canal = na_if(str_trim(fraccion_canal), ""),
            canal_verificado = as.logical(canal_verificado))

impacto_insumos <- canales %>%
  filter(canal_mexico == "insumo_importado", !is.na(fraccion_canal)) %>%
  distinct(empresa, fraccion_canal) %>%
  crossing(simulacion %>% select(version_beta, escenario, fraccion, valor_mdp, delta_valor_mdp)) %>%
  filter(str_starts(fraccion, fraccion_canal)) %>%
  group_by(empresa, fraccion_canal, version_beta, escenario) %>%
  summarise(n_fracciones_anam = n_distinct(fraccion),
            valor_base_mdp = sum(valor_mdp), delta_valor_mdp = sum(delta_valor_mdp),
            .groups = "drop") %>%
  mutate(var_pct = delta_valor_mdp / valor_base_mdp)

empresas_mx <- canales %>%
  left_join(impacto_insumos, by = c("empresa", "fraccion_canal"),
            relationship = "many-to-many") %>%
  arrange(canal_mexico, empresa, fraccion_canal, version_beta, escenario)

# =============================================================================
# 6. Salidas
# =============================================================================
parametros <- tibble(
  theta_spec, ratio_theta, theta_agregado,
  agrupacion_beta, excluir_contaminacion_alta,
  excluir_fracciones = paste(excluir_fracciones, collapse = ";"),
  incluir_dc_capex, theta_est, theta_se,
  theta_usada = theta, n_obs_theta = nrow(d1), capex_desde = t0,
  controles_theta = paste(controles_theta, collapse = ";"), tasa_sim, gamma, gamma_se,
  theta_desde, theta_hasta = max(d1$trimestre),
  origen, regimen = regimen_sel, estimador, rezago_trim, tendencia_grupo, forma_paso2, unidad, solo_continuos,
  unidad_valor = "millones USD",
  unidad_recaudacion = "millones MXN")

write_excel_csv(parametros,      file.path(dir_out, "00_parametros.csv"))
if (!is.null(theta_panel)) write_excel_csv(theta_panel, file.path(dir_out, "00b_theta_panel_ratio.csv"))
write_excel_csv(bind_rows(coef_paso1, coef_paso2) %>% mutate(nivel_ic = nivel_ic),
                file.path(dir_out, "00c_coeficientes_ic.csv"))
if (!is.null(placebo))    write_excel_csv(placebo, file.path(dir_out, "01d_placebo_permutacion.csv"))
if (!is.null(intervalos)) write_excel_csv(intervalos, file.path(dir_out, "04b_intervalos_impacto.csv"))
write_excel_csv(betas,           file.path(dir_out, paste0("01_betas_", agrupacion_beta, ".csv")))
if (!is.null(forma_reducida)) write_excel_csv(forma_reducida, file.path(dir_out, "01c_forma_reducida_mcap.csv"))
write_excel_csv(resumen_canasta, file.path(dir_out, "02_resumen_canasta.csv"))
write_excel_csv(resumen_eslabon, file.path(dir_out, "02b_resumen_eslabon.csv"))
write_excel_csv(escenarios,      file.path(dir_out, "03_escenarios_supuestos.csv"))
write_excel_csv(resultado_total, file.path(dir_out, "04_resultado_total.csv"))
write_excel_csv(resultado,       file.path(dir_out, "05_resultado_por_canasta.csv"))
write_excel_csv(resultado_eslabon, file.path(dir_out, "05b_resultado_por_eslabon.csv"))
write_excel_csv(simulacion,      file.path(dir_out, "06_resultado_por_fraccion.csv"))
write_excel_csv(empresas_mx,     file.path(dir_out, "07_empresas_canal_mx_impacto.csv"))

# Clasificacion por unidad (NICO o fraccion) con su subpartida y descripcion
desc_unidad <- pfm %>%
  transmute(fraccion, descripcion = coalesce(if ("desc_nico" %in% names(pfm)) desc_nico else NA_character_,
                                             if ("desc_fraccion" %in% names(pfm)) desc_fraccion else NA_character_)) %>%
  distinct(fraccion, .keep_all = TRUE)
clasif_fracciones <- panel %>% distinct(fraccion) %>%
  left_join(desc_unidad, by = "fraccion") %>%
  mutate(subpartida = str_sub(fraccion, 1, 6),
         canasta = clasifica_ia(fraccion), eslabon = clasifica_eslabon(fraccion)) %>%
  left_join(canasta_def %>% select(subpartida, uso_en_data_center, contaminacion_no_ia, nota),
            by = "subpartida") %>%
  arrange(canasta, fraccion)
write_excel_csv(clasif_fracciones, file.path(dir_out, "08_clasificacion_fracciones.csv"))
message("Listo. Resultados en: ", dir_out)
