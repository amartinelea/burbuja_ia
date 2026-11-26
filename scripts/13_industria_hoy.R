# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/13_industria_hoy.R
# Objetivo:       Indicadores de "como esta la industria hoy" (punto de partida del escenario A)
#                 con datos propios: panel de ANAM, Census (EUA) y estados financieros (SEC).
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          19/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Cifras de como esta la industria hoy; alimentan la lamina de panorama y
#     la seccion de contexto de la nota.
#   - No es obligatorio para el modelo.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Salidas en output/industria_hoy/:
#     00_resumen.txt                 resumen legible de todos los indicadores
#     01_eua_desde_mexico.csv        EUA: importaciones totales y desde Mexico
#                                    (= exportaciones de Mexico a EUA) por grupo,
#                                    ultimos 12 meses contra los 12 previos
#     02_eua_trimestral.csv          participacion de Mexico por trimestre
#     03_mx_importaciones_nivel.csv  Mexico: importaciones por nivel, 12m vs 12m previos
#     04_mx_regimen.csv              Mexico: importaciones por regimen (definitiva / temporal)
#     05_mx_origen.csv               Mexico: principales origenes (total y nucleo)
#     06_mx_subpartidas_clave.csv    Mexico: subpartidas clave por origen
#     07_mx_recaudacion.csv          recaudacion por nivel, ultimos 12 meses
#     08_capex_valuacion.csv         capex, capex/flujo y valuacion de hiperescaladores
#     09_diagnostico_origen_mex.csv  importaciones con origen Mexico, por regimen y subpartida
#
#   Requiere: 01 (datos/empresas), 02 (output/panel), 06 (datos/census)
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "stringr", "readr", "lubridate")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # raiz de boom_ia
dir_cen   <- file.path(dir_base, "datos", "census")
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "industria_hoy")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
ref_resto <- "4_resto_electronicos"
hyper     <- c("Microsoft", "Alphabet", "Amazon", "Meta", "Oracle")
claves    <- c("847150", "847180", "847330", "847170", "854231", "854232", "851762", "850440")
paises_asia <- c("TWN", "CHN", "KOR", "VNM", "JPN", "MYS", "THA", "SGP", "PHL", "IND")

lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)
pct     <- function(a, b) 100 * (a / b - 1)

# Ventanas de 12 meses: termina en el ultimo mes completo de cada fuente
ventanas <- function(ult) {
  list(act = c(ult %m-% months(11), ult), prev = c(ult %m-% months(23), ult %m-% months(12)))
}
en <- function(m, v) m >= v[1] & m <= v[2]

# ---- Canasta: nivel y eslabon por subpartida (y ajustes por fraccion) --------
canasta <- lee_csv(file.path(dir_base, "input", "canasta_ia_subpartidas.csv"),
                   col_types = cols(subpartida = col_character())) %>%
  mutate(subpartida = str_pad(subpartida, 6, pad = "0"))
archivo_aj <- file.path(dir_base, "input", "canasta_ia_ajustes_fraccion.csv")
ajustes <- if (file.exists(archivo_aj))
  lee_csv(archivo_aj, col_types = cols(.default = col_character())) %>%
  mutate(fraccion = gsub("[^0-9]", "", fraccion)) else tibble(fraccion = character(), nivel = character())
nivel_de <- function(unidad) {
  f8 <- str_sub(unidad, 1, 8); sp <- str_sub(unidad, 1, 6)
  coalesce(ajustes$nivel[match(unidad, ajustes$fraccion)], ajustes$nivel[match(f8, ajustes$fraccion)],
           canasta$nivel[match(sp, canasta$subpartida)], ref_resto)
}

# =============================================================================
# 1. EUA (Census): lo que importa en total y desde Mexico
# =============================================================================
cen <- lee_csv(file.path(dir_cen, "eua_importaciones_hs6_mes.csv"),
               col_types = cols(hs6 = col_character(), mes = col_date(), .default = col_guess())) %>%
  mutate(hs6 = str_pad(hs6, 6, pad = "0"),
         nivel = coalesce(canasta$nivel[match(hs6, canasta$subpartida)], ref_resto))
ult_cen <- max(cen$mes); vc <- ventanas(ult_cen)

grupos_eua <- bind_rows(
  cen %>% mutate(grupo = paste0("nivel_", nivel)),
  cen %>% filter(str_sub(hs6, 1, 4) == "8471") %>% mutate(grupo = "partida_8471"),
  cen %>% filter(str_sub(hs6, 1, 4) == "8473") %>% mutate(grupo = "partida_8473"),
  cen %>% filter(hs6 %in% claves) %>% mutate(grupo = paste0("hs6_", hs6)),
  cen %>% mutate(grupo = "total_cap_84_85_90")
)
eua <- grupos_eua %>%
  mutate(periodo = case_when(en(mes, vc$act) ~ "act", en(mes, vc$prev) ~ "prev")) %>%
  filter(!is.na(periodo)) %>%
  group_by(grupo, periodo) %>% summarise(total = sum(total, na.rm = TRUE) / 1e6, mexico = sum(mexico, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  pivot_wider(names_from = periodo, values_from = c(total, mexico)) %>%
  mutate(sh_mexico_act = 100 * mexico_act / total_act, sh_mexico_prev = 100 * mexico_prev / total_prev,
         crec_total = pct(total_act, total_prev), crec_desde_mexico = pct(mexico_act, mexico_prev),
         ventana = paste(format(vc$act[1], "%Y-%m"), "a", format(vc$act[2], "%Y-%m"))) %>%
  arrange(grupo)
write_excel_csv(eua, file.path(dir_out, "01_eua_desde_mexico.csv"))

eua_q <- grupos_eua %>% filter(grupo %in% c("hs6_847150", "partida_8471", "nivel_1_nucleo")) %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>%
  group_by(grupo, trimestre) %>% summarise(meses = n_distinct(mes), total = sum(total, na.rm = TRUE) / 1e6,
                                            mexico = sum(mexico, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  filter(meses == 3, trimestre >= ult_cen %m-% months(35)) %>%
  mutate(sh_mexico = 100 * mexico / total)
write_excel_csv(eua_q, file.path(dir_out, "02_eua_trimestral.csv"))

# =============================================================================
# 2. Mexico (ANAM): importaciones por nivel, regimen y origen
# =============================================================================
pan <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(), pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion") %>%
  mutate(mes = as.Date(mes), unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")),
         hs6 = str_sub(unidad, 1, 6))
pan$nivel <- nivel_de(pan$unidad)
ult_mx <- max(pan$mes); vm <- ventanas(ult_mx)
pan <- pan %>% mutate(periodo = case_when(en(mes, vm$act) ~ "act", en(mes, vm$prev) ~ "prev"))
txt_vm <- paste(format(vm$act[1], "%Y-%m"), "a", format(vm$act[2], "%Y-%m"))

doce <- function(d, ...) {
  d %>% filter(!is.na(periodo)) %>% group_by(..., periodo) %>% summarise(v = sum(valor_usd, na.rm = TRUE) / 1e6, .groups = "drop") %>%
    pivot_wider(names_from = periodo, values_from = v, values_fill = 0, names_prefix = "musd_") %>%
    mutate(crec = pct(musd_act, musd_prev), sh_act = 100 * musd_act / sum(musd_act), ventana = txt_vm)
}
mx_nivel   <- bind_rows(doce(pan, nivel), doce(pan %>% mutate(nivel = "total"), nivel))
mx_regimen <- doce(pan, regimen)
write_excel_csv(mx_nivel,   file.path(dir_out, "03_mx_importaciones_nivel.csv"))
write_excel_csv(mx_regimen, file.path(dir_out, "04_mx_regimen.csv"))

origen <- bind_rows(
  doce(pan %>% mutate(grupo = "total"), grupo, pais),
  doce(pan %>% filter(nivel == "1_nucleo") %>% mutate(grupo = "nucleo"), grupo, pais)
) %>% group_by(grupo) %>% mutate(sh_act = 100 * musd_act / sum(musd_act)) %>%
  arrange(grupo, desc(musd_act)) %>% slice_head(n = 12) %>% ungroup()
asia <- bind_rows(
  doce(pan %>% mutate(grupo = "total", pais = if_else(pais %in% paises_asia, "ASIA", "RESTO")), grupo, pais),
  doce(pan %>% filter(nivel == "1_nucleo") %>% mutate(grupo = "nucleo", pais = if_else(pais %in% paises_asia, "ASIA", "RESTO")), grupo, pais)
) %>% group_by(grupo) %>% mutate(sh_act = 100 * musd_act / sum(musd_act)) %>% ungroup()
write_excel_csv(bind_rows(origen, asia), file.path(dir_out, "05_mx_origen.csv"))

mx_claves <- doce(pan %>% filter(hs6 %in% claves), hs6, pais) %>%
  group_by(hs6) %>% mutate(sh_act = 100 * musd_act / sum(musd_act)) %>%
  arrange(hs6, desc(musd_act)) %>% slice_head(n = 5) %>% ungroup()
write_excel_csv(mx_claves, file.path(dir_out, "06_mx_subpartidas_clave.csv"))

# Diagnostico: importaciones con origen Mexico (reimportaciones u otros)
diag_mex <- pan %>% filter(periodo == "act", pais == "MEX") %>%
  group_by(regimen, hs6, nivel) %>% summarise(musd = sum(valor_usd, na.rm = TRUE) / 1e6, .groups = "drop") %>%
  arrange(desc(musd)) %>% slice_head(n = 25)
write_excel_csv(diag_mex, file.path(dir_out, "09_diagnostico_origen_mex.csv"))

# Recaudacion (todas las contribuciones), ultimos 12 meses
rec <- tryCatch({
  r <- lee_csv(file.path(dir_panel, "recaudacion_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(), .default = col_guess())) %>%
    filter(tipo_operacion == "importacion") %>% mutate(mes = as.Date(mes))
  nico_col <- if ("nico" %in% names(r)) str_pad(r$nico, 2, pad = "0") else "00"
  r$nivel <- nivel_de(paste0(str_pad(r$fraccion, 8, pad = "0"), nico_col))
  r %>% filter(en(mes, vm$act)) %>% group_by(nivel) %>%
    summarise(mdp = sum(importe_mxn, na.rm = TRUE) / 1e6, .groups = "drop") %>%
    mutate(sh = 100 * mdp / sum(mdp), ventana = txt_vm)
}, error = function(e) { message("Sin recaudacion: ", conditionMessage(e)); tibble() })
write_excel_csv(rec, file.path(dir_out, "07_mx_recaudacion.csv"))

# =============================================================================
# 3. Capex y valuacion de los hiperescaladores
# =============================================================================
capex <- lee_csv(file.path(dir_emp, "capex_trimestral_empresa.csv")) %>%
  mutate(trimestre = as.Date(trimestre)) %>% filter(empresa %in% hyper) %>%
  group_by(trimestre) %>% summarise(n = n_distinct(empresa), capex = sum(capex_musd), .groups = "drop") %>%
  filter(n == length(hyper)) %>% arrange(trimestre)
ult_q <- max(capex$trimestre)
capex_ttm  <- sum(capex$capex[capex$trimestre > ult_q %m-% months(12)])
capex_prev <- sum(capex$capex[capex$trimestre > ult_q %m-% months(24) & capex$trimestre <= ult_q %m-% months(12)])

rat <- lee_csv(file.path(dir_emp, "ratios_financieros_trimestral.csv")) %>%
  mutate(trimestre = as.Date(trimestre)) %>% filter(empresa %in% hyper)
rat_hoy <- rat %>% filter(trimestre == max(trimestre)) %>% select(empresa, capex_ttm, ocf_ttm, capex_ocf)
rat_hist <- rat %>% filter(!is.na(capex_ttm), !is.na(ocf_ttm)) %>% group_by(trimestre) %>%
  summarise(capex_ocf = sum(capex_ttm) / sum(ocf_ttm), .groups = "drop")

idx <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
val <- idx %>% group_by(indicadora) %>% arrange(mes) %>%
  summarise(mes_hoy = max(mes), idx_hoy = last(idx_ponderado), mcap_musd_hoy = last(mcap_musd),
            idx_hace_12m = idx_ponderado[mes == max(mes) %m-% months(12)][1],
            max_12m = max(idx_ponderado[mes > max(mes) %m-% months(12)]),
            mes_max_12m = mes[mes > max(mes) %m-% months(12)][which.max(idx_ponderado[mes > max(mes) %m-% months(12)])],
            idx_ene2023 = idx_ponderado[mes == as.Date("2023-01-01")][1],
            idx_ene2024 = idx_ponderado[mes == as.Date("2024-01-01")][1],
            idx_ene2025 = idx_ponderado[mes == as.Date("2025-01-01")][1], .groups = "drop") %>%
  mutate(cambio_12m = pct(idx_hoy, idx_hace_12m), vs_max_12m = pct(idx_hoy, max_12m),
         caida_para_ene2023 = pct(idx_ene2023, idx_hoy), caida_para_ene2024 = pct(idx_ene2024, idx_hoy),
         caida_para_ene2025 = pct(idx_ene2025, idx_hoy))

cv <- bind_rows(
  tibble(indicador = c("capex_ultimo_trimestre_musd", "capex_12m_musd", "capex_12m_previos_musd", "crec_capex_12m",
                       "capex_ocf_hoy_agregado", "capex_ocf_promedio_historico"),
         valor = c(last(capex$capex), capex_ttm, capex_prev, pct(capex_ttm, capex_prev),
                   sum(rat_hoy$capex_ttm) / sum(rat_hoy$ocf_ttm), mean(rat_hist$capex_ocf, na.rm = TRUE)),
         referencia = as.character(ult_q)),
  rat_hoy %>% transmute(indicador = paste0("capex_ocf_", empresa), valor = capex_ocf, referencia = as.character(ult_q))
)
write_excel_csv(cv, file.path(dir_out, "08_capex_valuacion.csv"))
write_excel_csv(val, file.path(dir_out, "08b_valuacion_indices.csv"))

# =============================================================================
# 4. Resumen legible
# =============================================================================
sink(file.path(dir_out, "00_resumen.txt"), split = TRUE)
cat("INDUSTRIA HOY: punto de partida del escenario A\n")
cat("Generado:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")
cat("== EUA (Census), ventana", eua$ventana[1], "(millones de USD) ==\n")
print(eua %>% select(grupo, total_act, crec_total, mexico_act, crec_desde_mexico, sh_mexico_act, sh_mexico_prev) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
cat("\nParticipacion trimestral de Mexico (%):\n")
print(eua_q %>% select(grupo, trimestre, sh_mexico) %>% mutate(sh_mexico = round(sh_mexico, 1)) %>%
        pivot_wider(names_from = grupo, values_from = sh_mexico), n = Inf)
cat("\n== Mexico (ANAM), ventana", txt_vm, "(millones de USD) ==\n")
print(mx_nivel %>% mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
cat("\nPor regimen:\n"); print(mx_regimen %>% mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
cat("\nPrincipales origenes:\n"); print(bind_rows(origen, asia) %>% mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
cat("\nSubpartidas clave por origen:\n"); print(mx_claves %>% mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
cat("\nOrigen Mexico (diagnostico):\n"); print(diag_mex %>% mutate(musd = round(musd, 1)), n = Inf)
if (nrow(rec)) { cat("\nRecaudacion 12m (millones de pesos):\n"); print(rec %>% mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf) }
cat("\n== Hiperescaladores ==\n")
print(cv %>% mutate(valor = round(valor, 3)), n = Inf)
cat("\nIndices de valuacion en bolsa:\n")
print(val %>% select(indicadora, mes_hoy, mcap_musd_hoy, cambio_12m, vs_max_12m, mes_max_12m,
                     caida_para_ene2025, caida_para_ene2024, caida_para_ene2023) %>%
        mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
sink()
cat("\nListo. Comparte output/industria_hoy/00_resumen.txt\n")
