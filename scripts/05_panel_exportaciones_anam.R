# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/05_panel_exportaciones_anam.R
# Objetivo:       Panel de exportaciones de electronicos de Mexico (ANAM) para el canal 2 del
#                 modelo: lo que Mexico vende a EUA.
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
#   - Deja limpia la consulta de exportaciones; es la base del canal 2 (lo que
#     Mexico le vende a EUA).
#   - Correr despues del 06 si se quiere la comparacion con Census.
#   - Para servidores y partes (8471 y 8473) ANAM y Census difieren menos de
#     4%; si sale mucho mas, revisar la consulta.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Convencion de claves (la misma de textiles y slides_contexto):
#     definitivas = A1 y C1 ; temporales = RT (retorno de IMMEX)
#     el resto (V1, V5, F4, H1, etc.) no entra: transferencias virtuales,
#     cambios de regimen o regularizaciones, sin cruce fisico.
#   Valor: valor_comercial (pesos) / tipo_cambio de cada registro = dolares.
#     Si falta el tipo de cambio se usa valor_dolares.
#   La consulta de exportaciones ya viene con una linea por celda (no trae
#   contribuciones), asi que no hay que deduplicar.
#
#   Requiere: input/raw/consulta_electro_exp*.csv,
#             datos/census/eua_importaciones_hs6_mes.csv (06, para comparar)
#   Salidas : output/panel_exportaciones/
#     panel_exp_fraccion_pais_mes.csv   fraccion, nico, pais, mes, regimen, valor_usd
#     00_diagnostico.csv                valor por clave y anio
#     01_destinos.csv                   participacion de EUA por anio
#     02_anam_vs_census.csv             ANAM (destino EUA) contra Census
#                                       (importaciones de EUA desde Mexico), por
#                                       nivel y anio, con los mismos meses
#     02b_anam_vs_census_hs6.csv        lo mismo por subpartida (que explica la brecha)
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("data.table", "dplyr", "tidyr", "stringr", "readr", "lubridate")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base <- getwd()
ruta_exp <- sort(list.files(file.path(dir_base, "input", "raw"), pattern = "^consulta_electro_exp(_[0-9].*)?\\.csv$",
                            full.names = TRUE), decreasing = TRUE)[1]
dir_cen  <- file.path(dir_base, "datos", "census")
dir_out  <- file.path(dir_base, "output", "panel_exportaciones")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
claves_def  <- c("A1", "C1")
claves_temp <- c("RT")
ref_resto   <- "4_resto_electronicos"
lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)
stopifnot(!is.na(ruta_exp))

# =============================================================================
# 1. Lectura
# =============================================================================
d <- fread(ruta_exp, colClasses = "character", encoding = "UTF-8", na.strings = c("", "NA", "NULL"))
cat("Consulta:", basename(ruta_exp), "|", format(nrow(d), big.mark = ","), "lineas\n")
d <- d[clave_tipo_operacion == "2"]
d[, `:=`(valor_comercial = as.numeric(valor_comercial), tipo_cambio = as.numeric(tipo_cambio),
         valor_dolares = as.numeric(valor_dolares))]
d[, valor_usd := fifelse(!is.na(tipo_cambio) & tipo_cambio > 0, valor_comercial / tipo_cambio, valor_dolares)]
d[, `:=`(mes = as.Date(paste0(substr(fecha, 1, 7), "-01")),
         fraccion = str_pad(fraccion_arancelaria, 8, pad = "0"),
         nico = str_pad(subdivision_fraccion_arancelaria, 2, pad = "0"),
         pais = trimws(clave_pais_origen_destino), clave = trimws(clave_documento))]

diag <- d[, .(valor_musd = sum(valor_usd, na.rm = TRUE) / 1e6, lineas = .N), by = .(clave, anio = year(mes))]
diag[, entra := clave %in% c(claves_def, claves_temp)]
write_excel_csv(diag[order(anio, -valor_musd)], file.path(dir_out, "00_diagnostico.csv"))

d <- d[clave %in% c(claves_def, claves_temp)]
d[, regimen := fifelse(clave %in% claves_temp, "temporal", "definitiva")]
# valor_mxn: valor comercial en pesos, para reportar el efecto en pesos
panel <- d[, .(valor_usd = sum(valor_usd, na.rm = TRUE), valor_mxn = sum(valor_comercial, na.rm = TRUE)),
           by = .(fraccion, nico, pais, mes, regimen)]
fwrite(panel, file.path(dir_out, "panel_exp_fraccion_pais_mes.csv"))

destinos <- panel[, .(total = sum(valor_usd), eua = sum(valor_usd[pais == "USA"])), by = .(anio = year(mes))][
  , sh_eua := round(100 * eua / total, 1)][order(anio)]
write_excel_csv(destinos, file.path(dir_out, "01_destinos.csv"))

# =============================================================================
# 2. ANAM contra Census (importaciones de EUA desde Mexico), mismos meses
# =============================================================================
canasta <- lee_csv(file.path(dir_base, "input", "canasta_ia_fracciones.csv"), col_types = cols(.default = "c")) %>%
  transmute(unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")), nivel)
grupo_hs6 <- as_tibble(panel) %>% mutate(unidad = paste0(fraccion, nico), hs6 = str_sub(fraccion, 1, 6)) %>%
  group_by(hs6, unidad) %>% summarise(v = sum(valor_usd), .groups = "drop") %>%
  left_join(canasta, by = "unidad") %>% mutate(nivel = coalesce(nivel, ref_resto)) %>%
  group_by(hs6, nivel) %>% summarise(v = sum(v), .groups = "drop") %>%
  group_by(hs6) %>% slice_max(v, n = 1, with_ties = FALSE) %>% ungroup() %>% select(hs6, nivel)

eua <- lee_csv(file.path(dir_cen, "eua_importaciones_hs6_mes.csv"),
               col_types = cols(hs6 = col_character(), mes = col_date(), .default = col_guess()))
meses <- sort(unique(panel$mes[panel$mes %in% unique(eua$mes)]))
anam_eua <- as_tibble(panel) %>% filter(pais == "USA", mes %in% meses) %>%
  mutate(hs6 = str_sub(fraccion, 1, 6)) %>% group_by(hs6, anio = year(mes)) %>%
  summarise(anam = sum(valor_usd), .groups = "drop")
census <- eua %>% filter(mes %in% meses, hs6 %in% grupo_hs6$hs6) %>%
  group_by(hs6, anio = year(mes)) %>% summarise(census = sum(exp_mx_eua, na.rm = TRUE), .groups = "drop")
comp <- full_join(anam_eua, census, by = c("hs6", "anio")) %>%
  left_join(grupo_hs6, by = "hs6") %>% mutate(across(c(anam, census), ~ coalesce(.x, 0)))
comp_n <- bind_rows(comp, comp %>% mutate(nivel = "total")) %>%
  group_by(nivel, anio) %>% summarise(anam_musd = sum(anam) / 1e6, census_musd = sum(census) / 1e6, .groups = "drop") %>%
  mutate(dif_pct = round(100 * (anam_musd / census_musd - 1), 1),
         meses = paste(format(min(meses), "%Y-%m"), "a", format(max(meses), "%Y-%m")))
write_excel_csv(comp_n, file.path(dir_out, "02_anam_vs_census.csv"))
comp <- comp %>% mutate(dif = anam - census)
write_excel_csv(comp %>% arrange(anio, desc(abs(dif))), file.path(dir_out, "02b_anam_vs_census_hs6.csv"))
# Que subpartidas explican la diferencia en el ultimo anio completo
anio_ref <- max(comp$anio[comp$anio < max(comp$anio)])
brecha <- comp %>% filter(anio == anio_ref) %>% mutate(sh_brecha = 100 * dif / sum(dif)) %>%
  arrange(desc(abs(dif))) %>% slice_head(n = 12) %>%
  transmute(hs6, nivel, anam_musd = round(anam / 1e6), census_musd = round(census / 1e6),
            dif_musd = round(dif / 1e6), sh_brecha = round(sh_brecha, 1))

cat("\nValor por clave (millones de USD), claves que entran:\n")
print(as.data.frame(diag[entra == TRUE] %>% as_tibble() %>% select(clave, anio, valor_musd) %>%
                      mutate(valor_musd = round(valor_musd)) %>% pivot_wider(names_from = anio, values_from = valor_musd)))
cat("\nParticipacion de EUA como destino (%):\n"); print(as.data.frame(destinos))
cat("\nANAM (destino EUA) contra Census (EUA importa de Mexico), millones de USD, mismos meses:\n")
print(as.data.frame(comp_n %>% mutate(across(c(anam_musd, census_musd), round))), row.names = FALSE)
cat("\nSubpartidas que mas explican la diferencia en", anio_ref, "(millones de USD):\n")
print(as.data.frame(brecha), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
