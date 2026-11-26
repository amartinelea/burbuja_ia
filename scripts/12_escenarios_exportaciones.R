# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/12_escenarios_exportaciones.R
# Objetivo:       Canal 2, etapa 3: simulacion de los escenarios A a F sobre lo que Mexico le
#                 exporta a EUA, con la misma cadena del canal de importaciones (10):
#                 valuacion en bolsa -> capex de hiperescaladores      (etapa 1, theta; 07)
#                 capex -> lo que EUA importa de cada nivel        (etapa 2a, epsilon; 10)
#                 lo que EUA importa de un HS6 (sin Mexico) ->
#                 lo que Mexico exporta a EUA de ese HS6           (etapa 2b, eta; 11)
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          23/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Resultado principal del canal de exportaciones: impacto de cada escenario
#     sobre lo que Mexico le vende a EUA.
#   - Usa los mismos escenarios y supuestos que el canal de importaciones (10).
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#     dln EXP_p = eta * epsilon_g(p) * dln K_s * lambda_s + arrastre_s
#   Versiones (version_beta). eta se estima por partida (HS4) porque a 6 digitos
#   Mexico y EUA clasifican distinto parte de los mismos productos y ese ruido
#   lleva eta hacia cero:
#     principal     eta de cada nivel de la canasta (11: 7_hs4_por_nivel_tendencia;
#                   negativos en cero). La respuesta de las exportaciones se
#                   concentra en el nucleo (eta cerca de 1) y es casi nula en el
#                   resto; una eta unica subestima el nucleo y sobreestima el resto
#     eta_unica     una sola eta para todos los niveles (11: 7_hs4_tendencia);
#                   cota inferior del impacto
#     directo_capex sin eta: elasticidad de lo que EUA le compra a Mexico de cada
#                   nivel respecto del capex, 50 trimestres de Census (10:
#                   capex_a_eua_desde_mexico; negativos en cero). Comprobacion
#                   independiente de la version principal
#     census_espejo eta unica con la serie espejo de Census por partida
#     eta_hs6       eta unica por subpartida (11: 1_tendencia), la que tiene el ruido
#   Base: exportaciones de Mexico con destino EUA (ANAM, A1, C1 y RT), ultimos 4
#     trimestres completos, los mismos del canal de importaciones.
#   Efecto de arrastre y lambda: los mismos supuestos de cada escenario del canal de
#     importaciones (03_escenarios_supuestos del 10).
#   Las exportaciones no pagan contribuciones: no hay columna de recaudacion.
#   Intervalos: Monte Carlo con la incertidumbre de theta, gamma, epsilon y eta.
#   Requiere: 10 (output/choque_ia_census), 11 (output/eta_exportaciones),
#             05 (output/panel_exportaciones)
#   Salidas : output/choque_ia_exportaciones/ (mismo formato que el 10)
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("data.table", "dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base <- getwd()
dir_19   <- file.path(dir_base, "output", "choque_ia_census")
dir_24   <- file.path(dir_base, "output", "eta_exportaciones")
dir_exp  <- file.path(dir_base, "output", "panel_exportaciones")
dir_out  <- file.path(dir_base, "output", "choque_ia_exportaciones")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
n_sim     <- 2000
nivel_ic  <- 0.90
ref_resto <- "4_resto_electronicos"
set.seed(20261124)
lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Insumos
# =============================================================================
par19 <- lee_csv(file.path(dir_19, "00_parametros.csv"))
esc   <- lee_csv(file.path(dir_19, "03_escenarios_supuestos.csv"))
elas19 <- lee_csv(file.path(dir_19, "01_elasticidades_eua.csv"))
eps   <- elas19 %>% filter(paso == "capex_a_eua") %>% mutate(est = coalesce(est, 0), ee = coalesce(ee, 0))
dmx   <- elas19 %>% filter(paso == "capex_a_eua_desde_mexico") %>% mutate(est = coalesce(est, 0), ee = coalesce(ee, 0))
if (!nrow(dmx)) stop("Falta capex_a_eua_desde_mexico: corre la version nueva del 19")
theta <- par19$theta_usada[1]; theta_se <- par19$theta_se[1]; gamma <- par19$gamma[1]
gamma_se <- ifelse(is.na(par19$gamma_se[1]), 0, par19$gamma_se[1])
base_desde <- as.Date(par19$base_desde[1]); base_hasta <- as.Date(par19$base_hasta[1])

r24 <- lee_csv(file.path(dir_24, "01_pruebas.csv"))
eta_de <- function(prueba_, termino_ = "x") r24 %>% filter(prueba == prueba_, termino == termino_) %>% slice(1)
eta_pr <- eta_de("7_hs4_tendencia"); eta_ce <- eta_de("7_hs4_census_espejo_tendencia")
eta_h6 <- eta_de("1_tendencia")
eta_nv <- r24 %>% filter(prueba == "7_hs4_por_nivel_tendencia") %>%
  transmute(grupo = str_remove(termino, "^x:"), eta = pmax(eta, 0), ee = ee)

grupo_hs6 <- lee_csv(file.path(dir_24, "00_grupo_hs6.csv"), col_types = cols(.default = "c"))
pex <- fread(file.path(dir_exp, "panel_exp_fraccion_pais_mes.csv"), colClasses = c(fraccion = "character")) %>%
  as_tibble() %>% mutate(mes = as.Date(mes), hs6 = str_sub(str_pad(fraccion, 8, pad = "0"), 1, 6),
                         trimestre = floor_date(mes, "quarter"))
trim_ok <- pex %>% distinct(trimestre, mes) %>% count(trimestre) %>% filter(n == 3) %>% pull(trimestre)
ult4 <- trim_ok[trim_ok >= base_desde & trim_ok <= base_hasta]
if (length(ult4) != 4) stop("La base de exportaciones no tiene los 4 trimestres del canal de importaciones")
# valor_mdp = millones de USD (nombre historico); valor_mxn = millones de pesos (valor_comercial)
if (!"valor_mxn" %in% names(pex)) stop("El panel de exportaciones no trae valor_mxn: corre la version nueva del 05")
base <- pex %>% filter(pais == "USA", trimestre %in% ult4) %>% group_by(hs6) %>%
  summarise(valor_mdp = sum(valor_usd) / 1e6, valor_mxn = sum(valor_mxn) / 1e6, .groups = "drop") %>%
  left_join(grupo_hs6, by = "hs6") %>% mutate(grupo = coalesce(grupo, ref_resto)) %>% filter(valor_mdp > 0)
cat(sprintf("Base: %s millones de USD exportados a EUA en %d HS6 (%s a %s)\n",
            format(round(sum(base$valor_mdp)), big.mark = ","), nrow(base), min(ult4), max(ult4)))

# =============================================================================
# 2. Simulacion
# =============================================================================
eps_hat <- setNames(eps$est, eps$grupo)
simula <- function(eta_g, e_g = eps_hat) {
  base %>% crossing(esc %>% select(escenario, dln_capex, mult_caida, arrastre)) %>%
    mutate(e = coalesce(e_g[grupo], 0), h = coalesce(eta_g[grupo], 0),
           dln_sim = h * e * dln_capex * mult_caida + arrastre,
           valor_sim_mdp = valor_mdp * exp(dln_sim), delta_valor_mdp = valor_sim_mdp - valor_mdp,
           delta_valor_mxn = valor_mxn * (exp(dln_sim) - 1))
}
todos <- function(v) setNames(rep(v, 4), c("1_nucleo", "2_infraestructura", "3_mixta", ref_resto))
dmx_hat <- setNames(pmax(dmx$est, 0), dmx$grupo)
sim <- bind_rows(
  simula(setNames(eta_nv$eta, eta_nv$grupo)) %>% mutate(version_beta = "principal"),
  simula(todos(eta_pr$eta)) %>% mutate(version_beta = "eta_unica"),
  simula(todos(1), e_g = dmx_hat) %>% mutate(version_beta = "directo_capex"),
  simula(todos(eta_ce$eta)) %>% mutate(version_beta = "census_espejo"),
  simula(todos(eta_h6$eta)) %>% mutate(version_beta = "eta_hs6"))

agrega <- function(d, ...) d %>% group_by(version_beta, escenario, ...) %>%
  summarise(valor_base_mdp = sum(valor_mdp), valor_sim_mdp = sum(valor_sim_mdp),
            delta_valor_mdp = sum(delta_valor_mdp),
            valor_base_mxn = sum(valor_mxn), delta_valor_mxn = sum(delta_valor_mxn), .groups = "drop") %>%
  mutate(var_pct = valor_sim_mdp / valor_base_mdp - 1, var_pct_mxn = delta_valor_mxn / valor_base_mxn)
resultado_total <- agrega(sim)
resultado       <- agrega(sim, canasta = grupo)

# Monte Carlo por version (theta, gamma, epsilon por nivel y eta)
th_s <- rnorm(n_sim, theta, theta_se); ga_s <- rnorm(n_sim, gamma, gamma_se)
eps_s <- sapply(eps$grupo, function(g) rnorm(n_sim, eps$est[eps$grupo == g], eps$ee[eps$grupo == g]))
base_g <- base %>% group_by(grupo) %>% summarise(valor = sum(valor_mdp), valor_mxn = sum(valor_mxn), .groups = "drop")
eta_draws <- list(
  principal     = sapply(base_g$grupo, function(g) {
    r <- eta_nv[eta_nv$grupo == g, ]; if (!nrow(r)) return(rep(0, n_sim)); pmax(rnorm(n_sim, r$eta, r$ee), 0) }),
  eta_unica     = sapply(base_g$grupo, function(g) rnorm(n_sim, eta_pr$eta, eta_pr$ee)),
  directo_capex = sapply(base_g$grupo, function(g) rep(1, n_sim)),
  census_espejo = sapply(base_g$grupo, function(g) rnorm(n_sim, eta_ce$eta, eta_ce$ee)),
  eta_hs6       = sapply(base_g$grupo, function(g) rnorm(n_sim, eta_h6$eta, eta_h6$ee)))
# Con una sola eta, el mismo sorteo vale para todos los niveles
for (v in c("eta_unica", "census_espejo", "eta_hs6")) eta_draws[[v]][] <- eta_draws[[v]][, 1]
# En directo_capex la incertidumbre es la de la elasticidad directa
dmx_s <- sapply(dmx$grupo, function(g) pmax(rnorm(n_sim, dmx$est[dmx$grupo == g], dmx$ee[dmx$grupo == g]), 0))

intervalos <- map_dfr(names(eta_draws), function(v) map_dfr(seq_len(nrow(esc)), function(i) {
  sc <- esc[i, ]
  dk <- th_s * log1p(sc$mcap_hyper) + ga_s * sc$delta_tasa + log1p(sc$capex_directo)
  por_g <- sapply(seq_along(base_g$grupo), function(j) {
    g <- base_g$grupo[j]; ee_s <- if (v == "directo_capex") dmx_s else eps_s
    e <- if (g %in% colnames(ee_s)) ee_s[, g] else 0
    base_g$valor[j] * (exp(eta_draws[[v]][, j] * e * dk * sc$mult_caida + sc$arrastre) - 1)
  })
  por_g <- matrix(por_g, nrow = n_sim)
  por_g_mxn <- sweep(por_g, 2, base_g$valor_mxn / base_g$valor, `*`)
  q <- function(x, b, nombre) tibble(version_beta = v, escenario = sc$escenario, nivel = nombre,
    delta_valor_p50 = median(x), delta_valor_inf = quantile(x, (1 - nivel_ic) / 2), delta_valor_sup = quantile(x, 1 - (1 - nivel_ic) / 2),
    var_pct_p50 = median(x) / b, var_pct_inf = quantile(x, (1 - nivel_ic) / 2) / b, var_pct_sup = quantile(x, 1 - (1 - nivel_ic) / 2) / b,
    prob_caida = mean(x < 0))
  qm <- function(x) tibble(delta_valor_mxn_p50 = median(x), delta_valor_mxn_inf = quantile(x, (1 - nivel_ic) / 2),
                           delta_valor_mxn_sup = quantile(x, 1 - (1 - nivel_ic) / 2))
  bind_rows(bind_cols(q(rowSums(por_g), sum(base_g$valor), "total"), qm(rowSums(por_g_mxn))),
            map_dfr(seq_along(base_g$grupo), ~ bind_cols(q(por_g[, .x], base_g$valor[.x], base_g$grupo[.x]), qm(por_g_mxn[, .x]))))
})) %>% mutate(nivel_ic = nivel_ic, n_sim = n_sim)

# =============================================================================
# 3. Guardar e imprimir
# =============================================================================
parametros <- tibble(theta_usada = theta, theta_se = theta_se, gamma = gamma, gamma_se = gamma_se,
                     eta_unica = eta_pr$eta, eta_unica_ee = eta_pr$ee,
                     eta_census = eta_ce$eta, eta_census_ee = eta_ce$ee,
                     eta_hs6 = eta_h6$eta, eta_hs6_ee = eta_h6$ee,
                     destino = "USA", base_desde = min(ult4), base_hasta = max(ult4),
                     valor_base_musd = sum(base$valor_mdp), valor_base_mmxn = sum(base$valor_mxn),
                     tc_implicito = sum(base$valor_mxn) / sum(base$valor_mdp),
                     unidad_valor = "millones USD (columnas _mdp) y millones MXN (columnas _mxn)")
write_excel_csv(parametros, file.path(dir_out, "00_parametros.csv"))
write_excel_csv(eta_nv, file.path(dir_out, "01_eta_por_nivel_usada.csv"))
write_excel_csv(dmx, file.path(dir_out, "01b_elasticidad_directa_capex.csv"))
write_excel_csv(esc, file.path(dir_out, "03_escenarios_supuestos.csv"))
write_excel_csv(resultado_total, file.path(dir_out, "04_resultado_total.csv"))
write_excel_csv(intervalos, file.path(dir_out, "04b_intervalos_impacto.csv"))
write_excel_csv(resultado, file.path(dir_out, "05_resultado_por_canasta.csv"))
write_excel_csv(sim %>% filter(version_beta == "principal"), file.path(dir_out, "06_resultado_por_hs6.csv"))

cat(sprintf("\neta unica (ANAM, HS4): %.3f (%.3f) | Census espejo HS4: %.3f (%.3f) | ANAM HS6: %.3f (%.3f)\n",
            eta_pr$eta, eta_pr$ee, eta_ce$eta, eta_ce$ee, eta_h6$eta, eta_h6$ee))
cat("eta por nivel, version principal (negativos en cero):\n"); print(as.data.frame(eta_nv), row.names = FALSE)
cat("\nImpacto total por escenario (% de lo exportado a EUA; millones de USD):\n")
print(as.data.frame(resultado_total %>% mutate(var_pct = round(100 * var_pct, 2),
                                               across(c(valor_base_mdp, valor_sim_mdp, delta_valor_mdp), ~ round(.x, 1))) %>%
                      arrange(version_beta, escenario)), row.names = FALSE)
cat("\nPor nivel (%):\n")
print(as.data.frame(resultado %>% transmute(version_beta, canasta, escenario, var = round(100 * var_pct, 2)) %>%
                      pivot_wider(names_from = escenario, values_from = var)), row.names = FALSE)
cat("\nIntervalos de 90% del total (%):\n")
print(as.data.frame(intervalos %>% filter(nivel == "total") %>%
                      transmute(version_beta, escenario, inf = round(100 * var_pct_inf, 2), p50 = round(100 * var_pct_p50, 2),
                                sup = round(100 * var_pct_sup, 2), prob_caida = round(prob_caida, 2))), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
