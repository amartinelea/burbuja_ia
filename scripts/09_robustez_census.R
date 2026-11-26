# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/09_robustez_census.R
# Objetivo:       Pruebas de robustez del diseno por producto del 06: importaciones de Mexico
#                 por HS6 contra la demanda de EUA de ese mismo HS6 (sin Mexico).
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
#   - Etapa 2b del canal de importaciones: cuanto cambia lo que importa Mexico
#     cuando cambia la demanda de EUA (eta), con todas sus pruebas.
#   - Si eta cambia mucho entre corridas, revisar primero el placebo y la
#     prueba sin cada anio.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Pruebas (PPML, efectos fijos de HS6 y trimestre, con tendencia propia de cada
#   HS6 salvo que se diga):
#     1. Reproducir base y tendencia, con errores agrupados por HS6 y por HS6 y
#        trimestre a la vez
#     2. Nucleo contra resto: la diferencia y su significancia (no solo cada uno)
#     3. Placebo: la demanda de EUA de cada HS6 se asigna a otro HS6 al azar
#        (misma forma en el tiempo, producto equivocado). Si el efecto es real, los
#        placebos deben dar cerca de cero
#     4. Quitar un anio a la vez (en especial 2026, el anio del salto)
#     5. Quitar los 5 HS6 con mas valor (que no lo cargue un solo producto)
#     6. Cambios anuales (MCO en dlog a 4 trimestres, pesado por valor) en vez de niveles
#     7. Rezagos distribuidos 0 a 3 juntos (efecto acumulado)
#     8. Todo a nivel de partida (HS4), contra el ruido de clasificacion
#   Requiere: 06 (datos/census/eua_importaciones_hs6_mes.csv) y 02 (output/panel)
#   Salidas : output/census_robustez/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "fixest")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_panel <- file.path(dir_base, "output", "panel")
dir_cen   <- file.path(dir_base, "datos", "census")
dir_out   <- file.path(dir_base, "output", "census_robustez")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
n_placebo <- 200
ref_resto <- "4_resto_electronicos"
set.seed(20261118)
lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Panel (igual que el 06)
# =============================================================================
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
         hs6 = str_sub(unidad, 1, 6))
grupo_hs6 <- pfm %>% group_by(hs6, unidad) %>% summarise(v = sum(valor_usd), .groups = "drop") %>%
  mutate(grupo = clasifica_ia(unidad)) %>% group_by(hs6, grupo) %>% summarise(v = sum(v), .groups = "drop") %>%
  group_by(hs6) %>% slice_max(v, n = 1, with_ties = FALSE) %>% ungroup() %>% select(hs6, grupo)
mx_mes <- pfm %>% group_by(hs6, mes) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
  complete(hs6, mes = sort(unique(pfm$mes)), fill = list(valor_usd = 0))
eua_hs6 <- lee_csv(file.path(dir_cen, "eua_importaciones_hs6_mes.csv"),
                   col_types = cols(hs6 = col_character(), mes = col_date(), .default = col_guess()))

a_trimestre <- function(d, cols) {
  d %>% mutate(periodo = floor_date(mes, "quarter")) %>% group_by(hs6, periodo) %>%
    summarise(n_meses = n_distinct(mes), across(all_of(cols), sum), .groups = "drop") %>%
    filter(n_meses == 3) %>% select(-n_meses)
}
panel <- a_trimestre(mx_mes, "valor_usd") %>%
  inner_join(a_trimestre(eua_hs6, "dem_ex_mx"), by = c("hs6", "periodo")) %>%
  left_join(grupo_hs6, by = "hs6") %>% mutate(grupo = coalesce(grupo, ref_resto)) %>%
  group_by(hs6) %>% filter(sum(valor_usd) > 0) %>% arrange(periodo, .by_group = TRUE) %>%
  mutate(x = log(if_else(dem_ex_mx > 0, dem_ex_mx, NA_real_)),
         x_r1 = lag(x), x_r2 = lag(x, 2), x_r3 = lag(x, 3)) %>% ungroup() %>%
  mutate(t_num = as.numeric(factor(periodo)), nucleo = as.numeric(grupo == "1_nucleo"))
cat("\nPanel: ", n_distinct(panel$hs6), " HS6, ", n_distinct(panel$periodo), " trimestres\n", sep = "")

# =============================================================================
# 2. Pruebas
# =============================================================================
estima <- function(d, rhs = "x", tend = TRUE, cl = ~hs6, terminos = rhs, etiqueta) {
  f <- as.formula(paste0("valor_usd ~ ", paste(rhs, collapse = " + "), " | ",
                         if (tend) "hs6[t_num]" else "hs6", " + periodo"))
  usadas <- intersect(c("x", "x_r1", "x_r2", "x_r3"), all.vars(f))
  d <- d %>% filter(if_all(all_of(usadas), ~ !is.na(.x)))
  m <- tryCatch(fepois(f, data = d, cluster = cl, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) return(tibble(prueba = etiqueta, termino = terminos, eta = NA_real_, ee = NA_real_, p = NA_real_))
  ct <- coeftable(m); terminos <- intersect(terminos, rownames(ct))
  tibble(prueba = etiqueta, termino = terminos, eta = ct[terminos, 1], ee = ct[terminos, 2], p = ct[terminos, 4],
         n_hs6 = n_distinct(d$hs6), n_obs = nobs(m))
}

res <- list()
# 1. Reproducir y errores en dos dimensiones
res$r1 <- bind_rows(
  estima(panel, tend = FALSE, etiqueta = "1_base"),
  estima(panel, etiqueta = "1_tendencia"),
  estima(panel, tend = FALSE, cl = ~hs6 + periodo, etiqueta = "1_base_cluster_hs6_y_trimestre"),
  estima(panel, cl = ~hs6 + periodo, etiqueta = "1_tendencia_cluster_hs6_y_trimestre"))
# 2. Nucleo contra resto: x es el efecto del resto, x:nucleo la diferencia
res$r2 <- estima(panel, rhs = c("x", "x:nucleo"), terminos = c("x", "x:nucleo"), etiqueta = "2_nucleo_menos_resto")

# 3. Placebo: cada HS6 recibe la serie de demanda de otro HS6
real <- res$r1 %>% filter(prueba == "1_tendencia") %>% pull(eta)
real_dif <- res$r2 %>% filter(termino == "x:nucleo") %>% pull(eta)
series_x <- panel %>% select(hs6, periodo, x)
ids <- unique(panel$hs6)
plac <- map_dfr(seq_len(n_placebo), function(r) {
  mapa <- tibble(hs6 = ids, hs6_origen = sample(ids))
  dp <- panel %>% select(-x) %>% left_join(mapa, by = "hs6") %>%
    left_join(series_x %>% rename(hs6_origen = hs6), by = c("hs6_origen", "periodo"))
  a <- estima(dp, etiqueta = "p")$eta
  b <- estima(dp, rhs = c("x", "x:nucleo"), terminos = "x:nucleo", etiqueta = "p")$eta
  tibble(rep = r, eta = a[1], dif = b[1])
})
res$r3 <- tibble(prueba = c("3_placebo_productos", "3_placebo_nucleo_menos_resto"), termino = c("x", "x:nucleo"),
                 eta = c(real, real_dif),
                 ee = c(sd(plac$eta, na.rm = TRUE), sd(plac$dif, na.rm = TRUE)),
                 p = c(mean(abs(plac$eta) >= abs(real), na.rm = TRUE), mean(abs(plac$dif) >= abs(real_dif), na.rm = TRUE)))

# 4. Quitar un anio a la vez
res$r4 <- map_dfr(sort(unique(year(panel$periodo))), function(a)
  estima(panel %>% filter(year(periodo) != a), etiqueta = paste0("4_sin_", a)))
# 5. Sin los 5 HS6 de mayor valor
top5 <- panel %>% group_by(hs6) %>% summarise(v = sum(valor_usd)) %>% slice_max(v, n = 5) %>% pull(hs6)
res$r5 <- estima(panel %>% filter(!hs6 %in% top5), etiqueta = "5_sin_top5_hs6")
# 6. Cambios anuales (MCO, dlog a 4 trimestres, pesado por el valor promedio del HS6)
dd <- panel %>% group_by(hs6) %>% arrange(periodo, .by_group = TRUE) %>%
  mutate(peso = mean(valor_usd), v4 = lag(valor_usd, 4), x4 = lag(x, 4)) %>% ungroup() %>%
  filter(valor_usd > 0, v4 > 0, !is.na(x), !is.na(x4)) %>%
  mutate(dy = log(valor_usd) - log(v4), dx = x - x4)
m6 <- feols(dy ~ dx | periodo, data = dd, weights = ~peso, cluster = ~hs6, notes = FALSE)
m6b <- feols(dy ~ dx + dx:nucleo | periodo, data = dd, weights = ~peso, cluster = ~hs6, notes = FALSE)
c6 <- coeftable(m6); c6b <- coeftable(m6b)
res$r6 <- tibble(prueba = "6_cambios_anuales", termino = c("dx", "dx:nucleo"),
                 eta = c(c6["dx", 1], c6b["dx:nucleo", 1]), ee = c(c6["dx", 2], c6b["dx:nucleo", 2]),
                 p = c(c6["dx", 4], c6b["dx:nucleo", 4]), n_hs6 = n_distinct(dd$hs6), n_obs = nobs(m6))
# 7. Rezagos distribuidos 0 a 3 y su suma
m7 <- tryCatch(fepois(valor_usd ~ x + x_r1 + x_r2 + x_r3 | hs6[t_num] + periodo,
                      data = panel %>% filter(!is.na(x), !is.na(x_r3)), cluster = ~hs6, notes = FALSE, warn = FALSE),
               error = function(e) NULL)
res$r7 <- if (!is.null(m7)) {
  b <- coef(m7); V <- vcov(m7); s <- sum(b); se <- sqrt(sum(V))
  bind_rows(tibble(prueba = "7_rezagos_distribuidos", termino = names(b), eta = b, ee = sqrt(diag(V)),
                   p = 2 * (1 - pnorm(abs(b / sqrt(diag(V)))))),
            tibble(prueba = "7_rezagos_distribuidos", termino = "suma_0_a_3", eta = s, ee = se, p = 2 * (1 - pnorm(abs(s / se)))))
} else NULL

# 8. Todo a nivel de partida (HS4): Mexico y EUA a veces clasifican el mismo
#    producto en subpartidas distintas; al sumar a 4 digitos ese ruido se cancela
panel4 <- panel %>% mutate(hs4 = str_sub(hs6, 1, 4)) %>% group_by(hs4, periodo) %>%
  summarise(valor_usd = sum(valor_usd), dem_ex_mx = sum(dem_ex_mx), .groups = "drop") %>%
  group_by(hs4) %>% filter(sum(valor_usd) > 0) %>% ungroup() %>%
  mutate(x = log(if_else(dem_ex_mx > 0, dem_ex_mx, NA_real_)), t_num = as.numeric(factor(periodo)), hs6 = hs4)
res$r8 <- bind_rows(estima(panel4, tend = FALSE, etiqueta = "8_hs4_base"),
                    estima(panel4, etiqueta = "8_hs4_tendencia"))

res <- bind_rows(res)
estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen <- res %>% mutate(celda = sprintf("%.2f%s (%.2f)", eta, estrellas(p), ee)) %>%
  select(prueba, termino, celda, any_of(c("n_hs6", "n_obs")))

write_excel_csv(res, file.path(dir_out, "01_pruebas.csv"))
write_excel_csv(resumen, file.path(dir_out, "02_resumen.csv"))
write_excel_csv(plac, file.path(dir_out, "03_placebos.csv"))

cat("\nRobustez del diseno por producto (*** 99%, ** 95%, * 90% de confianza).\n")
cat("x = demanda de EUA del mismo HS6; x:nucleo = cuanto mas responde el nucleo que el resto.\n")
cat("Placebo: p = parte de los placebos con un efecto igual o mayor que el real.\n\n")
print(as.data.frame(resumen), right = FALSE, row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
