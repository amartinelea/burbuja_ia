# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/11_eta_exportaciones.R
# Objetivo:       Canal 2, etapa 2b: lo que EUA importa de un producto (HS6) desde el resto
#                 del mundo -> lo que Mexico le exporta a EUA de ese producto.
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
#   - Etapa 2b del canal de exportaciones: cuanto cambia lo que Mexico le vende
#     a EUA cuando EUA compra mas o menos a otros paises.
#   - Se estima por partida (4 digitos) porque a 6 digitos Mexico y EUA
#     clasifican distinto algunos bienes.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Mismo diseno que la etapa 2b de importaciones (06 y 09): PPML por HS6 y
#   trimestre, efectos fijos de HS6 y de trimestre, tendencia propia de cada HS6
#   en la version principal, errores agrupados por HS6. El regresor excluye a
#   Mexico (dem_ex_mx) para que la relacion no sea mecanica: lo que Mexico vende
#   a EUA forma parte de lo que EUA importa del mundo.
#
#   Variable dependiente:
#     principal  : exportaciones de Mexico con destino EUA (ANAM, A1, C1 y RT)
#     comparacion: importaciones de EUA desde Mexico (Census, espejo)
#     alternativa: exportaciones de Mexico a todos los destinos (ANAM)
#   Pruebas: sin y con tendencia, por nivel de la canasta, sin 2026, sin los 5
#     HS6 de mas valor, errores agrupados por HS6 y trimestre, y todo a nivel de
#     partida (HS4) para quitar el ruido de clasificacion entre Mexico y EUA.
#
#   Eslabon 3 (exploratorio): exportaciones a EUA -> importaciones temporales de
#     insumos (IN y AF), en cambios anuales del agregado, Newey-West. Ojo: buena
#     parte de los insumos de IMMEX entra por la Regla Octava (capitulo 98), que
#     no viene en la consulta; el resultado es solo una referencia.
#
#   Requiere: 05 (output/panel_exportaciones), 06 (datos/census), 02 (output/panel)
#   Salidas : output/eta_exportaciones/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("data.table", "dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "fixest", "sandwich")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()
dir_exp   <- file.path(dir_base, "output", "panel_exportaciones")
dir_panel <- file.path(dir_base, "output", "panel")
dir_cen   <- file.path(dir_base, "datos", "census")
dir_out   <- file.path(dir_base, "output", "eta_exportaciones")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
ref_resto <- "4_resto_electronicos"
lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Datos
# =============================================================================
pex <- fread(file.path(dir_exp, "panel_exp_fraccion_pais_mes.csv"), colClasses = c(fraccion = "character", nico = "character")) %>%
  as_tibble() %>% mutate(mes = as.Date(mes), fraccion = str_pad(fraccion, 8, pad = "0"), nico = str_pad(nico, 2, pad = "0"),
                         hs6 = str_sub(fraccion, 1, 6), unidad = paste0(fraccion, nico))
canasta <- lee_csv(file.path(dir_base, "input", "canasta_ia_fracciones.csv"), col_types = cols(.default = "c")) %>%
  transmute(unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")), nivel, eslabon)
grupo_hs6 <- pex %>% group_by(hs6, unidad) %>% summarise(v = sum(valor_usd), .groups = "drop") %>%
  left_join(canasta, by = "unidad") %>% mutate(nivel = coalesce(nivel, ref_resto)) %>%
  group_by(hs6, nivel) %>% summarise(v = sum(v), .groups = "drop") %>%
  group_by(hs6) %>% slice_max(v, n = 1, with_ties = FALSE) %>% ungroup() %>% select(hs6, grupo = nivel)
write_excel_csv(grupo_hs6, file.path(dir_out, "00_grupo_hs6.csv"))

meses_anam <- sort(unique(pex$mes))
mx_mes <- pex %>% group_by(hs6, mes) %>%
  summarise(exp_eua = sum(valor_usd[pais == "USA"]), exp_todos = sum(valor_usd), .groups = "drop") %>%
  complete(hs6, mes = meses_anam, fill = list(exp_eua = 0, exp_todos = 0))
eua_hs6 <- lee_csv(file.path(dir_cen, "eua_importaciones_hs6_mes.csv"),
                   col_types = cols(hs6 = col_character(), mes = col_date(), .default = col_guess()))

a_trimestre <- function(d, cols) {
  d %>% mutate(periodo = floor_date(mes, "quarter")) %>% group_by(hs6, periodo) %>%
    summarise(n_meses = n_distinct(mes), across(all_of(cols), sum), .groups = "drop") %>%
    filter(n_meses == 3) %>% select(-n_meses)
}
panel <- a_trimestre(mx_mes, c("exp_eua", "exp_todos")) %>%
  inner_join(a_trimestre(eua_hs6, c("dem_ex_mx", "exp_mx_eua")), by = c("hs6", "periodo")) %>%
  left_join(grupo_hs6, by = "hs6") %>% mutate(grupo = coalesce(grupo, ref_resto)) %>%
  group_by(hs6) %>% filter(sum(exp_eua) > 0) %>% arrange(periodo, .by_group = TRUE) %>%
  mutate(x = log(if_else(dem_ex_mx > 0, dem_ex_mx, NA_real_))) %>% ungroup() %>%
  mutate(t_num = as.numeric(factor(periodo)), nucleo = as.numeric(grupo == "1_nucleo"))
cat("Panel: ", n_distinct(panel$hs6), " HS6, ", n_distinct(panel$periodo), " trimestres (",
    format(min(panel$periodo)), " a ", format(max(panel$periodo)), ")\n", sep = "")

# =============================================================================
# 2. Etapa 2b de exportaciones
# =============================================================================
estima <- function(d, y = "exp_eua", rhs = "x", tend = TRUE, cl = ~hs6, etiqueta) {
  f <- as.formula(paste0(y, " ~ ", rhs, " | ", if (tend) "hs6[t_num]" else "hs6", " + periodo"))
  d <- d %>% filter(!is.na(x))
  m <- tryCatch(fepois(f, data = d, cluster = cl, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) return(tibble(prueba = etiqueta, termino = NA_character_, eta = NA_real_, ee = NA_real_, p = NA_real_))
  ct <- coeftable(m)
  tibble(prueba = etiqueta, termino = rownames(ct), eta = ct[, 1], ee = ct[, 2], p = ct[, 4],
         n_hs6 = n_distinct(d$hs6), n_obs = nobs(m))
}
top5 <- panel %>% group_by(hs6) %>% summarise(v = sum(exp_eua)) %>% slice_max(v, n = 5) %>% pull(hs6)

# Version por partida (HS4): Mexico y EUA a veces clasifican el mismo
# producto en subpartidas distintas (p. ej. computadoras en 8471.41/49 contra
# 8471.50); al sumar a 4 digitos ese ruido se cancela en buena parte.
grupo_hs4 <- pex %>% mutate(hs4 = str_sub(hs6, 1, 4)) %>% left_join(grupo_hs6, by = "hs6") %>%
  group_by(hs4, grupo) %>% summarise(v = sum(valor_usd), .groups = "drop") %>%
  group_by(hs4) %>% slice_max(v, n = 1, with_ties = FALSE) %>% ungroup() %>% select(hs4, grupo)
panel4 <- panel %>% mutate(hs4 = str_sub(hs6, 1, 4)) %>% group_by(hs4, periodo) %>%
  summarise(across(c(exp_eua, exp_todos, dem_ex_mx, exp_mx_eua), sum), .groups = "drop") %>%
  left_join(grupo_hs4, by = "hs4") %>% mutate(grupo = coalesce(grupo, ref_resto)) %>%
  group_by(hs4) %>% filter(sum(exp_eua) > 0) %>% ungroup() %>%
  mutate(x = log(if_else(dem_ex_mx > 0, dem_ex_mx, NA_real_)), t_num = as.numeric(factor(periodo)),
         nucleo = as.numeric(grupo == "1_nucleo"), hs6 = hs4)   # hs6 = clave del producto para estima()

top5_hs4 <- panel4 %>% group_by(hs4) %>% summarise(v = sum(exp_eua)) %>% slice_max(v, n = 5) %>% pull(hs4)

res <- bind_rows(
  estima(panel, tend = FALSE, etiqueta = "1_base"),
  estima(panel, etiqueta = "1_tendencia"),                                   # principal
  estima(panel, cl = ~hs6 + periodo, etiqueta = "1_tendencia_cluster_hs6_y_trimestre"),
  estima(panel, rhs = "i(grupo, x)", etiqueta = "2_por_nivel_tendencia"),
  estima(panel, rhs = "x + x:nucleo", etiqueta = "2_nucleo_menos_resto"),
  estima(panel %>% filter(year(periodo) < 2026), etiqueta = "3_sin_2026"),
  estima(panel %>% filter(!hs6 %in% top5), etiqueta = "4_sin_top5_hs6"),
  estima(panel, y = "exp_mx_eua", tend = FALSE, etiqueta = "5_census_espejo_base"),
  estima(panel, y = "exp_mx_eua", etiqueta = "5_census_espejo_tendencia"),
  estima(panel, y = "exp_todos", etiqueta = "6_anam_todos_destinos_tendencia"),
  estima(panel4, tend = FALSE, etiqueta = "7_hs4_base"),
  estima(panel4, etiqueta = "7_hs4_tendencia"),
  estima(panel4, rhs = "i(grupo, x)", etiqueta = "7_hs4_por_nivel_tendencia"),
  estima(panel4, y = "exp_mx_eua", etiqueta = "7_hs4_census_espejo_tendencia"),
  estima(panel4 %>% filter(year(periodo) < 2026), etiqueta = "7_hs4_sin_2026"),
  estima(panel4 %>% filter(!hs4 %in% top5_hs4), etiqueta = "7_hs4_sin_top5"),
  estima(panel4, cl = ~hs6 + periodo, etiqueta = "7_hs4_tendencia_cluster_hs4_y_trimestre")
) %>% mutate(termino = str_replace(termino, "^grupo::(.*):x$", "x:\\1"))
write_excel_csv(res, file.path(dir_out, "01_pruebas.csv"))

# =============================================================================
# 3. Eslabon 3 (exploratorio): exportaciones -> importaciones temporales
# =============================================================================
pfm <- fread(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
             select = c("tipo_operacion", "regimen", "mes", "valor_usd")) %>% as_tibble() %>%
  filter(tipo_operacion == "importacion", regimen == "temporal") %>% mutate(mes = as.Date(mes))
temp_q <- pfm %>% mutate(trimestre = floor_date(mes, "quarter")) %>%
  group_by(trimestre) %>% summarise(n = n_distinct(mes), temp = sum(valor_usd), .groups = "drop") %>% filter(n == 3)
exp_q <- pex %>% filter(pais == "USA") %>% mutate(trimestre = floor_date(mes, "quarter")) %>%
  group_by(trimestre) %>% summarise(n = n_distinct(mes), expo = sum(valor_usd), .groups = "drop") %>% filter(n == 3)
d3 <- inner_join(temp_q %>% select(-n), exp_q %>% select(-n), by = "trimestre") %>% arrange(trimestre) %>%
  mutate(dy = log(temp) - lag(log(temp), 4), dx = log(expo) - lag(log(expo), 4)) %>% filter(!is.na(dy), !is.na(dx))
eslabon3 <- if (nrow(d3) >= 8) {
  m3 <- lm(dy ~ dx, data = d3); V3 <- NeweyWest(m3, lag = 4, prewhite = FALSE)
  tibble(eslabon = "exportaciones_a_eua -> importaciones_temporales", est = coef(m3)["dx"], ee = sqrt(V3["dx", "dx"]),
         n = nrow(d3), desde = min(d3$trimestre), hasta = max(d3$trimestre),
         nota = "Exploratorio: la consulta no trae el capitulo 98 (Regla Octava), por donde entra parte de los insumos IMMEX")
} else tibble(eslabon = "exportaciones_a_eua -> importaciones_temporales", est = NA_real_, ee = NA_real_, n = nrow(d3))
eslabon3 <- eslabon3 %>% mutate(p = 2 * (1 - pnorm(abs(est / ee))))
write_excel_csv(eslabon3, file.path(dir_out, "02_eslabon3_temporales.csv"))
write_excel_csv(d3, file.path(dir_out, "02b_series_eslabon3.csv"))

# =============================================================================
# 4. Resumen
# =============================================================================
estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
cat("\nEtapa 2b de exportaciones (PPML; efectos fijos de HS6 y trimestre):\n")
print(as.data.frame(res %>% transmute(prueba, termino, celda = sprintf("%.3f%s (%.3f)", eta, estrellas(p), ee), n_hs6, n_obs)),
      row.names = FALSE)
cat("\nEslabon 3 (exploratorio):\n"); print(as.data.frame(eslabon3 %>% select(eslabon, est, ee, p, n)), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
