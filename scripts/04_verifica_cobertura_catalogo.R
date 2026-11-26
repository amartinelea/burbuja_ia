# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/04_verifica_cobertura_catalogo.R
# Objetivo:       Verifica si la consulta de ANAM trae todas las fracciones de los capitulos
#                 84, 85 y 90 del catalogo LIGIE/NICO, o solo un subconjunto.
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
#   - Solo diagnostico: dice si la ANAM nos mando todas las fracciones de los
#     capitulos 84, 85 y 90.
#   - No cambia nada del modelo; se puede saltar si la consulta no cambio.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Compara tres conjuntos:
#     catalogo : fracciones y NICO vigentes de la LIGIE (input/Catalogo LIGIE ... .xlsx)
#     consulta : lo que entrego ANAM (input/raw/consulta_electro_imp*.csv)
#     panel    : lo que usa el analisis (output/panel/panel_fraccion_pais_mes.csv)
#
#   Salidas en output/cobertura_catalogo/:
#     00_resumen.txt                  resumen legible
#     01_cobertura_capitulo.csv       fracciones, NICO y subpartidas por capitulo
#     02_cobertura_partida.csv        cobertura por partida (4 digitos)
#     03_subpartidas_parciales.csv    subpartidas con solo una parte de sus fracciones
#     04_fracciones_faltantes.csv     fracciones del catalogo que no vienen en la consulta
#     05_canasta_ia.csv               cobertura de las subpartidas de la canasta de IA
#     06_capitulos_fuera.csv          capitulos que trae la consulta fuera de 84, 85 y 90
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "stringr", "readr", "readxl", "purrr")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base   <- getwd()                                   # raiz de boom_ia
dir_input  <- file.path(dir_base, "input")
dir_out    <- file.path(dir_base, "output", "cobertura_catalogo")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
capitulos  <- c("84", "85", "90")

# El nombre del catalogo trae acento; se busca por patron para no escribirlo
archivo_cat <- list.files(dir_input, pattern = "LIGIE.*\\.xlsx$", full.names = TRUE)[1]
archivo_raw <- sort(list.files(file.path(dir_input, "raw"), pattern = "^consulta_electro_imp.*\\.csv$", full.names = TRUE), decreasing = TRUE)[1]
archivo_pan <- file.path(dir_base, "output", "panel", "panel_fraccion_pais_mes.csv")
stopifnot(!is.na(archivo_cat), !is.na(archivo_raw))

lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)
pct <- function(a, b) round(100 * a / b, 1)

# =============================================================================
# 1. Catalogo, consulta y panel
# =============================================================================
# Copia temporal con nombre sin acentos: readxl falla con rutas acentuadas en algunos sistemas
cat_tmp <- tempfile(fileext = ".xlsx"); file.copy(archivo_cat, cat_tmp, overwrite = TRUE)
cat_ <- read_excel(cat_tmp, col_types = "text") %>%
  mutate(fraccion = str_pad(fraccion, 8, pad = "0"), nico = str_pad(nico, 2, pad = "0"),
         capitulo = str_sub(fraccion, 1, 2), partida = str_sub(fraccion, 1, 4), subpartida = str_sub(fraccion, 1, 6),
         unidad = paste0(fraccion, nico))
cat_cap <- cat_ %>% filter(capitulo %in% capitulos)

raw <- lee_csv(archivo_raw, col_select = c(fecha, clave_documento, fraccion_arancelaria,
                                           subdivision_fraccion_arancelaria, valor_dolares),
               col_types = cols(.default = col_character())) %>%
  mutate(fraccion = str_pad(fraccion_arancelaria, 8, pad = "0"),
         nico = str_pad(subdivision_fraccion_arancelaria, 2, pad = "0"),
         capitulo = str_sub(fraccion, 1, 2), unidad = paste0(fraccion, nico),
         valor_dolares = suppressWarnings(as.numeric(valor_dolares)))

# Valor por fraccion en la consulta (sin deduplicar lineas de contribucion: solo para ordenar)
val_raw <- raw %>% group_by(fraccion) %>% summarise(registros = n(), .groups = "drop")

pan_fr <- if (file.exists(archivo_pan)) {
  lee_csv(archivo_pan, col_select = c(fraccion), col_types = cols(fraccion = col_character())) %>%
    distinct() %>% mutate(fraccion = str_pad(fraccion, 8, pad = "0")) %>% pull(fraccion)
} else character(0)

fr_cat <- unique(cat_cap$fraccion); fr_raw <- unique(raw$fraccion)
un_cat <- unique(cat_cap$unidad);   un_raw <- unique(raw$unidad)
sp_cat <- unique(cat_cap$subpartida); sp_raw <- unique(str_sub(fr_raw, 1, 6))

# =============================================================================
# 2. Cobertura por capitulo y partida
# =============================================================================
por_cap <- map_dfr(capitulos, function(k) {
  a <- fr_cat[str_sub(fr_cat, 1, 2) == k]; b <- fr_raw[str_sub(fr_raw, 1, 2) == k]
  u <- un_cat[str_sub(un_cat, 1, 2) == k]; v <- un_raw[str_sub(un_raw, 1, 2) == k]
  s <- sp_cat[str_sub(sp_cat, 1, 2) == k]
  tibble(capitulo = k,
         fracciones_catalogo = length(a), fracciones_consulta = length(intersect(a, b)),
         pct_fracciones = pct(length(intersect(a, b)), length(a)),
         nico_catalogo = length(u), nico_consulta = length(intersect(u, v)),
         pct_nico = pct(length(intersect(u, v)), length(u)),
         subpartidas_catalogo = length(s), subpartidas_con_algo = length(intersect(s, sp_raw)),
         fracciones_consulta_fuera_catalogo = length(setdiff(b, a)),
         fracciones_en_panel = sum(str_sub(pan_fr, 1, 2) == k))
})
por_cap <- bind_rows(por_cap, por_cap %>% summarise(capitulo = "total", across(where(is.numeric), sum)) %>%
                       mutate(pct_fracciones = pct(fracciones_consulta, fracciones_catalogo), pct_nico = pct(nico_consulta, nico_catalogo)))
write_excel_csv(por_cap, file.path(dir_out, "01_cobertura_capitulo.csv"))

por_partida <- cat_cap %>% distinct(capitulo, partida, fraccion) %>%
  mutate(en_consulta = fraccion %in% fr_raw) %>%
  group_by(capitulo, partida) %>%
  summarise(fracciones_catalogo = n(), fracciones_consulta = sum(en_consulta), .groups = "drop") %>%
  mutate(pct = pct(fracciones_consulta, fracciones_catalogo),
         estado = case_when(fracciones_consulta == 0 ~ "ausente", pct < 100 ~ "parcial", TRUE ~ "completa"))
write_excel_csv(por_partida, file.path(dir_out, "02_cobertura_partida.csv"))

parciales <- cat_cap %>% distinct(subpartida, fraccion) %>% mutate(en_consulta = fraccion %in% fr_raw) %>%
  group_by(subpartida) %>% summarise(fracciones_catalogo = n(), fracciones_consulta = sum(en_consulta), .groups = "drop") %>%
  filter(fracciones_consulta > 0, fracciones_consulta < fracciones_catalogo)
write_excel_csv(parciales, file.path(dir_out, "03_subpartidas_parciales.csv"))

col_desc <- intersect(c("desc_fraccion", "descripcion_completa"), names(cat_cap))[1]
faltantes <- cat_cap %>% filter(!fraccion %in% fr_raw) %>% distinct(capitulo, partida, subpartida, fraccion, .keep_all = TRUE) %>%
  select(capitulo, partida, subpartida, fraccion, descripcion = all_of(col_desc))
write_excel_csv(faltantes, file.path(dir_out, "04_fracciones_faltantes.csv"))

# =============================================================================
# 3. Canasta de IA y capitulos fuera de 84, 85 y 90
# =============================================================================
canasta <- lee_csv(file.path(dir_input, "canasta_ia_subpartidas.csv"), col_types = cols(subpartida = col_character())) %>%
  mutate(subpartida = str_pad(subpartida, 6, pad = "0"))
can_cob <- canasta %>% select(subpartida, nivel, any_of("eslabon")) %>%
  left_join(cat_ %>% distinct(subpartida, fraccion) %>% mutate(en_consulta = fraccion %in% fr_raw) %>%
              group_by(subpartida) %>% summarise(fracciones_catalogo = n(), fracciones_consulta = sum(en_consulta),
                                                 faltan = paste(fraccion[!en_consulta], collapse = " "), .groups = "drop"),
            by = "subpartida") %>%
  mutate(estado = case_when(fracciones_consulta == 0 ~ "ausente", fracciones_consulta < fracciones_catalogo ~ "parcial", TRUE ~ "completa")) %>%
  arrange(nivel, subpartida)
write_excel_csv(can_cob, file.path(dir_out, "05_canasta_ia.csv"))

fuera <- raw %>% filter(!capitulo %in% capitulos) %>% group_by(capitulo) %>%
  summarise(fracciones = n_distinct(fraccion), registros = n(), .groups = "drop") %>% arrange(desc(registros))
write_excel_csv(fuera, file.path(dir_out, "06_capitulos_fuera.csv"))

# =============================================================================
# 4. Resumen
# =============================================================================
sink(file.path(dir_out, "00_resumen.txt"), split = TRUE)
cat("COBERTURA DE LA CONSULTA DE ANAM FRENTE AL CATALOGO LIGIE/NICO\n")
cat("Catalogo:", iconv(basename(archivo_cat), to = "ASCII//TRANSLIT"), "\nConsulta:", basename(archivo_raw), "\n")
cat("Periodo de la consulta:", min(raw$fecha), "a", max(raw$fecha), "\n\n")
cat("== Por capitulo ==\n"); print(por_cap, n = Inf, width = Inf)
cat("\n== Partidas por estado ==\n"); print(count(por_partida, capitulo, estado) %>% pivot_wider(names_from = estado, values_from = n, values_fill = 0))
cat("\n== Canasta de IA ==\n"); print(can_cob %>% select(subpartida, nivel, fracciones_catalogo, fracciones_consulta, estado, faltan), n = Inf, width = Inf)
cat("\n== Capitulos en la consulta fuera de 84, 85 y 90 ==\n"); print(fuera, n = Inf)
cat("\nConclusion: si pct_fracciones es menor a 100, la consulta es un subconjunto del catalogo.\n")
sink()
cat("\nListo. Revisa output/cobertura_catalogo/00_resumen.txt\n")
