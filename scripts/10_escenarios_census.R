# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/10_escenarios_census.R
# Objetivo:       Simulacion de los escenarios A a F con la cadena nueva, en la que el paso 2
#                 sale del diseno por producto (scripts 06 y 09):
#                 valuacion en bolsa -> capex de hiperescaladores        (paso 1, theta; script 07)
#                 capex -> lo que EUA importa de cada nivel           (paso 2a, epsilon; aqui)
#                 lo que EUA importa de un HS6 -> lo que Mexico
#                 importa de ese HS6                                  (paso 2b, eta; script 09)
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
#   - Resultado principal del canal de importaciones: impacto de cada escenario
#     en valor (dolares y pesos) y en IVA e IGI.
#   - Tambien estima la etapa 2a (epsilon por nivel).
#   - Los pesos salen del valor de la ANAM con el tipo de cambio de cada
#     operacion; la recaudacion es solo lo pagado en efectivo.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Simulacion por HS6 (todos los origenes, ultimos 4 trimestres completos):
#     dln K_s   = theta * ln(1 + mcap_hyper) + gamma * delta_tasa + ln(1 + capex_directo)
#     dln EUA_g = epsilon_g * dln K_s
#     dln MX_p  = eta * dln EUA_g(p) * lambda_s + arrastre_s
#   eta principal = con tendencia propia de cada HS6 (09: 1_tendencia); se reporta
#   tambien con eta sin tendencia (09: 1_base). epsilon_g: cambio anual de lo que
#   EUA importa de los HS6 del nivel g (sin Mexico) contra el cambio anual del
#   capex (contemporaneo y un trimestre antes, suma), Newey-West, 2014 en adelante.
#   Robustez: paso directo valuacion en bolsa -> EUA (sin pasar por el capex), B a D.
#   epsilon se estima solo con subpartidas que existen en todo el periodo (la
#   nomenclatura de 2022 creo subpartidas nuevas y rompe la serie por nivel).
#   Intervalos: Monte Carlo con la incertidumbre de theta, gamma, epsilon y eta.
#   Salidas con el mismo formato del 07 (output/choque_ia_census/), para que el 23
#   las grafique con fuente <- "census". version_beta: "principal" (eta con
#   tendencia) y "eta_base" (eta sin tendencia).
#   Requiere: 07 (output/choque_ia: escenarios y parametros), 06 (datos/census),
#             09 (output/census_robustez), 02 (output/panel), 01 (datos/empresas)
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "sandwich")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_02    <- file.path(dir_base, "output", "choque_ia")
dir_18    <- file.path(dir_base, "output", "census_robustez")
dir_cen   <- file.path(dir_base, "datos", "census")
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "choque_ia_census")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
n_sim     <- 2000
nivel_ic  <- 0.90
ref_resto <- "4_resto_electronicos"
set.seed(20261118)
lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Insumos: escenarios y paso 1 (02), eta (18), canasta y panel
# =============================================================================
esc <- lee_csv(file.path(dir_02, "03_escenarios_supuestos.csv"))
par <- lee_csv(file.path(dir_02, "00_parametros.csv"))
num <- function(x) as.numeric(par[[x]][1])
theta <- num("theta_usada"); theta_se <- num("theta_se"); gamma <- num("gamma"); gamma_se <- num("gamma_se")
if (isTRUE(as.logical(par$incluir_dc_capex[1]))) message("Aviso: el 02 incluye capex de centros de datos; aqui solo se usa el de hiperescaladores.")
for (v in c("delta_tasa", "capex_directo")) if (!v %in% names(esc)) esc[[v]] <- 0
esc <- esc %>% mutate(dln_capex = theta * log1p(mcap_hyper) + gamma * delta_tasa + log1p(capex_directo))

r18 <- lee_csv(file.path(dir_18, "01_pruebas.csv"))
eta_de <- function(prueba_) r18 %>% filter(prueba == prueba_, termino == "x") %>% slice(1)
etas <- list(unica = eta_de("1_tendencia"), eta_base = eta_de("1_base"))
# Principal: eta por nivel con tendencia (script 06), igual que en exportaciones.
# Las negativas se ponen en cero.
r06 <- lee_csv(file.path(dir_base, "output", "census", "01_eta_por_producto.csv"))
eta_nv <- r06 %>% filter(especificacion == "4_por_nivel_tendencia") %>%
  transmute(grupo = str_extract(termino, "(?<=grupo::)[^:]+"), eta_est = eta, eta = pmax(eta, 0), ee, p)
if (nrow(eta_nv) < 4) stop("Falta 4_por_nivel_tendencia en output/census/01_eta_por_producto.csv: corre el 06")
eta_nv_hat <- setNames(eta_nv$eta, eta_nv$grupo)
cat(sprintf("\neta unica (con tendencia): %.3f (%.3f); eta sin tendencia: %.3f (%.3f)\n",
            etas$unica$eta, etas$unica$ee, etas$eta_base$eta, etas$eta_base$ee))
cat("eta por nivel (principal):\n"); print(as.data.frame(eta_nv), row.names = FALSE)

canasta_def <- lee_csv(file.path(dir_base, "input", "canasta_ia_subpartidas.csv"),
                       col_types = cols(subpartida = col_character())) %>%
  mutate(subpartida = str_pad(subpartida, 6, pad = "0"))
archivo_ajustes <- file.path(dir_base, "input", "canasta_ia_ajustes_fraccion.csv")
ajustes <- if (file.exists(archivo_ajustes))
  lee_csv(archivo_ajustes, col_types = cols(.default = col_character())) %>%
  mutate(fraccion = gsub("[^0-9]", "", fraccion),
         fraccion = if_else(nchar(fraccion) > 8, str_pad(fraccion, 10, pad = "0"), str_pad(fraccion, 8, pad = "0"))) else
  tibble(fraccion = character(), nivel = character())
clasifica_ia <- function(fr) {
  f8 <- str_sub(fr, 1, 8); sp <- str_sub(fr, 1, 6)
  coalesce(if (any(nchar(fr) == 10)) ajustes$nivel[match(fr, ajustes$fraccion)] else NA_character_,
           ajustes$nivel[match(f8, ajustes$fraccion)], canasta_def$nivel[match(sp, canasta_def$subpartida)], ref_resto)
}

pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(), pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion") %>%
  mutate(mes = as.Date(mes), unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")),
         hs6 = str_sub(unidad, 1, 6), trimestre = floor_date(mes, "quarter"))
grupo_hs6 <- pfm %>% group_by(hs6, unidad) %>% summarise(v = sum(valor_usd), .groups = "drop") %>%
  mutate(grupo = clasifica_ia(unidad)) %>% group_by(hs6, grupo) %>% summarise(v = sum(v), .groups = "drop") %>%
  group_by(hs6) %>% slice_max(v, n = 1, with_ties = FALSE) %>% ungroup() %>% select(hs6, grupo) %>%
  mutate(eslabon = if ("eslabon" %in% names(canasta_def)) canasta_def$eslabon[match(hs6, canasta_def$subpartida)] else NA_character_,
         eslabon = if_else(grupo == ref_resto | is.na(eslabon), ref_resto, eslabon))

# Base: ultimos 4 trimestres completos, todos los origenes, por HS6 (valor en
# millones de USD; recaudacion en millones de pesos, pagos de todas las contribuciones)
trim_ok <- pfm %>% distinct(trimestre, mes) %>% count(trimestre) %>% filter(n == 3) %>% pull(trimestre)
ult4 <- sort(trim_ok, decreasing = TRUE)[1:4]
rec <- tryCatch(lee_csv(file.path(dir_panel, "recaudacion_fraccion_pais_mes.csv"),
                        col_types = cols(fraccion = col_character(), .default = col_guess())) %>%
                  filter(tipo_operacion == "importacion") %>%
                  mutate(trimestre = floor_date(as.Date(mes), "quarter"), hs6 = str_sub(str_pad(fraccion, 8, pad = "0"), 1, 6)) %>%
                  filter(trimestre %in% ult4) %>% group_by(hs6) %>%
                  summarise(recaudacion_mdp = sum(importe_mxn) / 1e6,
                            iva_mxn = sum(importe_mxn[clave_contribucion == 3]) / 1e6,
                            igi_mxn = sum(importe_mxn[clave_contribucion == 6]) / 1e6, .groups = "drop"),
                error = function(e) { message("Sin archivo de recaudacion: ", conditionMessage(e)); tibble(hs6 = character(), recaudacion_mdp = numeric(), iva_mxn = numeric(), igi_mxn = numeric()) })
cred <- tryCatch(lee_csv(file.path(dir_panel, "iva_credito_fraccion_pais_mes.csv"),
                         col_types = cols(fraccion = col_character(), .default = col_guess())) %>%
                   filter(tipo_operacion == "importacion") %>%
                   mutate(trimestre = floor_date(as.Date(mes), "quarter"), hs6 = str_sub(str_pad(fraccion, 8, pad = "0"), 1, 6)) %>%
                   filter(trimestre %in% ult4) %>% group_by(hs6) %>% summarise(iva_credito_mxn = sum(importe_mxn) / 1e6, .groups = "drop"),
                 error = function(e) { message("Sin archivo de IVA con credito: ", conditionMessage(e)); tibble(hs6 = character(), iva_credito_mxn = numeric()) })
# valor_mdp = millones de USD (nombre historico); valor_mxn = millones de pesos
# (valor_aduana); recaudacion, iva, igi y credito en millones de pesos
if (!"valor_mxn" %in% names(pfm)) stop("El panel no trae valor_mxn: corre la version nueva del 02")
base <- pfm %>% filter(trimestre %in% ult4) %>% group_by(hs6) %>%
  summarise(valor_mdp = sum(valor_usd) / 1e6, valor_mxn = sum(valor_mxn) / 1e6, .groups = "drop") %>%
  left_join(grupo_hs6, by = "hs6") %>% left_join(rec, by = "hs6") %>% left_join(cred, by = "hs6") %>%
  mutate(grupo = coalesce(grupo, ref_resto), eslabon = coalesce(eslabon, ref_resto),
         across(c(recaudacion_mdp, iva_mxn, igi_mxn, iva_credito_mxn), ~ coalesce(.x, 0))) %>% filter(valor_mdp > 0)
cat(sprintf("Base: %s millones de USD en %d HS6 (%s a %s)\n", format(round(sum(base$valor_mdp)), big.mark = ","),
            nrow(base), min(ult4), max(ult4)))

# =============================================================================
# 2. Paso 2a: capex (y valuacion en bolsa) -> lo que EUA importa de cada nivel
# =============================================================================
eua <- lee_csv(file.path(dir_cen, "eua_importaciones_hs6_mes.csv"),
               col_types = cols(hs6 = col_character(), mes = col_date(), .default = col_guess())) %>%
  left_join(grupo_hs6 %>% select(hs6, grupo), by = "hs6") %>% filter(!is.na(grupo))
# Antes de 2021 solo se bajaron los capitulos del nucleo: para que la serie de
# cada nivel sea comparable en el tiempo, se usan los capitulos con historia completa
caps_hist <- eua %>% filter(year(mes) < 2021) %>% distinct(cap = str_sub(hs6, 1, 2)) %>% pull(cap)
# Solo subpartidas con datos de principio a fin: la nomenclatura de 2022 creo
# subpartidas nuevas (p. ej. 8517.13 y 8517.14) que antes no existian; si se
# dejan, la serie de cada nivel salta en 2022 por composicion y no por demanda.
hs6_bal <- eua %>% filter(str_sub(hs6, 1, 2) %in% caps_hist) %>% group_by(hs6) %>%
  summarise(a = min(mes), b = max(mes), .groups = "drop") %>%
  filter(year(a) == year(min(eua$mes)), b == max(eua$mes)) %>% pull(hs6)
cat(sprintf("Etapa 2a: %d subpartidas con serie completa de %d\n", length(hs6_bal),
            n_distinct(eua$hs6[str_sub(eua$hs6, 1, 2) %in% caps_hist])))
eua_g <- eua %>% filter(hs6 %in% hs6_bal) %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>% group_by(grupo, trimestre) %>%
  summarise(n_meses = n_distinct(mes), dem = sum(dem_ex_mx, na.rm = TRUE),
            dem_mx = sum(exp_mx_eua, na.rm = TRUE), .groups = "drop") %>%
  filter(n_meses == 3, dem > 0) %>% mutate(ln_dem = log(dem), ln_dem_mx = log(if_else(dem_mx > 0, dem_mx, NA_real_)))

empresas  <- lee_csv(file.path(dir_emp, "empresas_ia.csv"))
capex_emp <- lee_csv(file.path(dir_emp, "capex_trimestral_empresa.csv")) %>% mutate(trimestre = as.Date(trimestre))
hyper <- empresas$empresa[empresas$es_hyperscaler]
t0 <- capex_emp %>% filter(empresa %in% hyper) %>% group_by(empresa) %>% summarise(t = min(trimestre)) %>% pull(t) %>% max()
capex <- capex_emp %>% filter(empresa %in% hyper, trimestre >= t0, capex_musd > 0) %>%
  arrange(empresa, trimestre) %>% group_by(empresa) %>%
  mutate(previo = lag(capex_musd), continua = !is.na(previo) & lag(trimestre) == trimestre %m-% months(3)) %>%
  group_by(trimestre) %>%
  summarise(total = sum(capex_musd),
            crec = if (any(continua)) sum(capex_musd[continua]) / sum(previo[continua]) else NA_real_, .groups = "drop") %>%
  arrange(trimestre) %>%
  mutate(crec = if_else(row_number() == 1 | is.na(crec), 1, crec), ln_capex = log(cumprod(crec))) %>%
  select(trimestre, ln_capex)
indices <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
mc_q <- indices %>% filter(indicadora == "es_hyperscaler") %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>% group_by(trimestre) %>%
  summarise(ln_mc = log(mean(coalesce(idx_ponderado, idx_equiponderado))), .groups = "drop")

elasticidad <- function(g, x, rezagos, y = "ln_dem") {
  d <- eua_g %>% filter(grupo == g) %>% select(trimestre, ln_dem = all_of(y)) %>%
    inner_join(x, by = "trimestre") %>% arrange(trimestre) %>%
    mutate(dy = ln_dem - lag(ln_dem, 4), dx = z - lag(z, 4))
  for (l in rezagos) d[[paste0("dx_", l)]] <- dplyr::lag(d$dx, l)
  regs <- paste0("dx_", rezagos)
  d <- d %>% filter(!is.na(dy), if_all(all_of(regs), ~ !is.na(.x)))
  if (nrow(d) < 16) return(tibble(grupo = g, est = NA_real_, ee = NA_real_, p = NA_real_, n = nrow(d)))
  m <- lm(as.formula(paste("dy ~", paste(regs, collapse = " + "))), data = d)
  V <- NeweyWest(m, lag = 4, prewhite = FALSE)
  b <- sum(coef(m)[regs]); s <- sqrt(sum(V[regs, regs]))
  tibble(grupo = g, est = b, ee = s, p = 2 * (1 - pnorm(abs(b / s))), n = nrow(d),
         desde = min(d$trimestre), hasta = max(d$trimestre))
}
grupos <- sort(unique(base$grupo))
eps <- map_dfr(grupos, ~ elasticidad(.x, capex %>% rename(z = ln_capex), 0:1)) %>% mutate(paso = "capex_a_eua")
tmc <- map_dfr(grupos, ~ elasticidad(.x, mc_q %>% rename(z = ln_mc), 1:2)) %>% mutate(paso = "valor_en_bolsa_a_eua")
# Prueba directa para el canal de exportaciones: lo que EUA le compra a Mexico
# de cada nivel contra el capex (sin pasar por eta). Lo usa el script 12.
dmx <- map_dfr(grupos, ~ elasticidad(.x, capex %>% rename(z = ln_capex), 0:1, y = "ln_dem_mx")) %>%
  mutate(paso = "capex_a_eua_desde_mexico")
elas <- bind_rows(eps, tmc, dmx)
faltan_g <- eps$grupo[is.na(eps$est)]
if (length(faltan_g)) message("Sin elasticidad para: ", paste(faltan_g, collapse = ", "), " (se usa 0)")
eps <- eps %>% mutate(est = coalesce(est, 0), ee = coalesce(ee, 0))
tmc <- tmc %>% mutate(est = coalesce(est, 0), ee = coalesce(ee, 0))

# =============================================================================
# 3. Simulacion
# =============================================================================
simula <- function(eta, eps_g, dln_k_s, tmc_g = NULL, mc_s = NULL) {
  # dln de lo que EUA importa de cada nivel, por escenario: via capex o directo
  base %>% crossing(esc %>% select(escenario, dln_capex, mcap_hyper, mult_caida, arrastre)) %>%
    left_join(tibble(grupo = names(eps_g), e = eps_g), by = "grupo") %>%
    mutate(e = coalesce(e, 0),
           dln_eua = if (is.null(tmc_g)) e * dln_capex else
             coalesce(tmc_g[grupo], 0) * log1p(mcap_hyper),
           h = if (length(eta) > 1) coalesce(eta[grupo], 0) else eta,
           dln_sim = h * dln_eua * mult_caida + arrastre, f = exp(dln_sim) - 1,
           valor_sim_mdp = valor_mdp * exp(dln_sim), recaud_sim_mdp = recaudacion_mdp * exp(dln_sim),
           delta_valor_mdp = valor_sim_mdp - valor_mdp, delta_recaud_mdp = recaud_sim_mdp - recaudacion_mdp,
           delta_valor_mxn = valor_mxn * f, delta_iva_mxn = iva_mxn * f, delta_igi_mxn = igi_mxn * f,
           delta_iva_credito_mxn = iva_credito_mxn * f)
}
eps_hat <- setNames(eps$est, eps$grupo); tmc_hat <- setNames(tmc$est, tmc$grupo)
sim <- bind_rows(
  simula(eta_nv_hat, eps_hat) %>% mutate(version_beta = "principal"),
  simula(etas$unica$eta, eps_hat) %>% mutate(version_beta = "eta_unica"),
  simula(etas$eta_base$eta, eps_hat) %>% mutate(version_beta = "eta_base"),
  simula(eta_nv_hat, eps_hat, tmc_g = tmc_hat) %>% mutate(version_beta = "directo_valor_en_bolsa"))

agrega <- function(d, ...) d %>% group_by(version_beta, escenario, ...) %>%
  summarise(valor_base_mdp = sum(valor_mdp), valor_sim_mdp = sum(valor_sim_mdp),
            delta_valor_mdp = sum(delta_valor_mdp), delta_recaud_mdp = sum(delta_recaud_mdp),
            valor_base_mxn = sum(valor_mxn), delta_valor_mxn = sum(delta_valor_mxn),
            iva_base_mxn = sum(iva_mxn), delta_iva_mxn = sum(delta_iva_mxn),
            igi_base_mxn = sum(igi_mxn), delta_igi_mxn = sum(delta_igi_mxn),
            iva_credito_base_mxn = sum(iva_credito_mxn), delta_iva_credito_mxn = sum(delta_iva_credito_mxn),
            .groups = "drop") %>%
  mutate(var_pct = valor_sim_mdp / valor_base_mdp - 1, var_pct_mxn = delta_valor_mxn / valor_base_mxn)
resultado_total   <- agrega(sim)
resultado         <- agrega(sim, canasta = grupo)
resultado_eslabon <- agrega(sim, eslabon)

# Monte Carlo (version principal): theta y gamma (paso 1), epsilon por nivel y eta
th_s  <- rnorm(n_sim, theta, theta_se); ga_s <- rnorm(n_sim, gamma, ifelse(is.na(gamma_se), 0, gamma_se))
eta_s <- sapply(eta_nv$grupo, function(g) pmax(rnorm(n_sim, eta_nv$eta_est[eta_nv$grupo == g], eta_nv$ee[eta_nv$grupo == g]), 0))
eps_s <- sapply(eps$grupo, function(g) rnorm(n_sim, eps$est[eps$grupo == g], eps$ee[eps$grupo == g]))
base_g <- base %>% group_by(grupo) %>% summarise(valor = sum(valor_mdp), valor_mxn = sum(valor_mxn), .groups = "drop")
intervalos <- map_dfr(seq_len(nrow(esc)), function(i) {
  sc <- esc[i, ]
  dk <- th_s * log1p(sc$mcap_hyper) + ga_s * sc$delta_tasa + log1p(sc$capex_directo)
  por_g <- sapply(base_g$grupo, function(g) {
    e <- if (g %in% colnames(eps_s)) eps_s[, g] else 0
    h <- if (g %in% colnames(eta_s)) eta_s[, g] else 0
    base_g$valor[base_g$grupo == g] * (exp(h * e * dk * sc$mult_caida + sc$arrastre) - 1)
  })
  por_g <- matrix(por_g, nrow = n_sim)
  # mismo cambio % por nivel, aplicado a la base en pesos
  por_g_mxn <- sweep(por_g, 2, base_g$valor_mxn / base_g$valor, `*`)
  q <- function(x, b, nombre) tibble(version_beta = "principal", escenario = sc$escenario, nivel = nombre,
    delta_valor_p50 = median(x), delta_valor_inf = quantile(x, (1 - nivel_ic) / 2), delta_valor_sup = quantile(x, 1 - (1 - nivel_ic) / 2),
    var_pct_p50 = median(x) / b, var_pct_inf = quantile(x, (1 - nivel_ic) / 2) / b, var_pct_sup = quantile(x, 1 - (1 - nivel_ic) / 2) / b,
    prob_caida = mean(x < 0))
  qm <- function(x) tibble(delta_valor_mxn_p50 = median(x), delta_valor_mxn_inf = quantile(x, (1 - nivel_ic) / 2),
                           delta_valor_mxn_sup = quantile(x, 1 - (1 - nivel_ic) / 2))
  bind_rows(bind_cols(q(rowSums(por_g), sum(base_g$valor), "total"), qm(rowSums(por_g_mxn))),
            map_dfr(seq_along(base_g$grupo), ~ bind_cols(q(por_g[, .x], base_g$valor[.x], base_g$grupo[.x]), qm(por_g_mxn[, .x]))))
}) %>% mutate(nivel_ic = nivel_ic, n_sim = n_sim)

# =============================================================================
# 4. Guardar (mismo formato que el 07) e imprimir
# =============================================================================
parametros <- tibble(theta_usada = theta, theta_se = theta_se, gamma = gamma, gamma_se = gamma_se,
                     eta_principal = "por nivel (01b_eta_por_nivel_usada.csv)",
                     eta_unica = etas$unica$eta, eta_unica_ee = etas$unica$ee,
                     eta_base = etas$eta_base$eta, eta_base_ee = etas$eta_base$ee,
                     origen = "todos", base_desde = min(ult4), base_hasta = max(ult4),
                     valor_base_musd = sum(base$valor_mdp), valor_base_mmxn = sum(base$valor_mxn),
                     tc_implicito = sum(base$valor_mxn) / sum(base$valor_mdp),
                     unidad_valor = "millones USD (columnas _mdp) y millones MXN (columnas _mxn)", unidad_recaudacion = "millones MXN")
write_excel_csv(parametros, file.path(dir_out, "00_parametros.csv"))
write_excel_csv(elas, file.path(dir_out, "01_elasticidades_eua.csv"))
write_excel_csv(eta_nv, file.path(dir_out, "01b_eta_por_nivel_usada.csv"))
write_excel_csv(esc, file.path(dir_out, "03_escenarios_supuestos.csv"))
write_excel_csv(resultado_total, file.path(dir_out, "04_resultado_total.csv"))
write_excel_csv(intervalos, file.path(dir_out, "04b_intervalos_impacto.csv"))
write_excel_csv(resultado, file.path(dir_out, "05_resultado_por_canasta.csv"))
write_excel_csv(resultado_eslabon, file.path(dir_out, "05b_resultado_por_eslabon.csv"))
write_excel_csv(sim %>% filter(version_beta == "principal"), file.path(dir_out, "06_resultado_por_hs6.csv"))
# Para las graficas del 23 (tiempos del paso 1)
for (f in c("00b_theta_panel_ratio.csv", "00c_coeficientes_ic.csv"))
  if (file.exists(file.path(dir_02, f))) file.copy(file.path(dir_02, f), file.path(dir_out, f), overwrite = TRUE)

estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
cat("\nPaso 2a: lo que EUA importa de cada nivel (sin Mexico) contra el capex y contra la valuacion en bolsa (cambios anuales):\n")
print(as.data.frame(elas %>% transmute(paso, grupo, celda = sprintf("%.2f%s (%.2f)", est, estrellas(p), ee), n, desde, hasta)), row.names = FALSE)
cat("\nImpacto total por escenario (% del valor importado de todos los origenes; recaudacion en millones de pesos):\n")
print(as.data.frame(resultado_total %>% mutate(var_pct = round(100 * var_pct, 2), across(c(valor_base_mdp, valor_sim_mdp, delta_valor_mdp, delta_recaud_mdp), ~ round(.x, 1))) %>%
                      arrange(version_beta, escenario)), row.names = FALSE)
cat("\nPor nivel, version principal (%):\n")
print(as.data.frame(resultado %>% filter(version_beta == "principal") %>% transmute(canasta, escenario, var = round(100 * var_pct, 2)) %>%
                      pivot_wider(names_from = escenario, values_from = var)), row.names = FALSE)
cat("\nPor eslabon, version principal (%):\n")
print(as.data.frame(resultado_eslabon %>% filter(version_beta == "principal") %>% transmute(eslabon, escenario, var = round(100 * var_pct, 2)) %>%
                      pivot_wider(names_from = escenario, values_from = var)), row.names = FALSE)
cat("\nIntervalos de 90% (version principal, %):\n")
print(as.data.frame(intervalos %>% transmute(escenario, nivel, inf = round(100 * var_pct_inf, 2), p50 = round(100 * var_pct_p50, 2),
                                             sup = round(100 * var_pct_sup, 2), prob_caida = round(prob_caida, 2))), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
