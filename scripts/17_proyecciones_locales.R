# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/17_proyecciones_locales.R
# Objetivo:       Proyecciones locales (Jorda, 2005) mensuales: respuesta de las importaciones
#                 de la canasta de IA, relativas al resto de electronicos, a choques en el
#                 valuacion en bolsa de hiperescaladores y de empresas de semiconductores.
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
#   - Prueba complementaria mensual (proyecciones locales). Opcional; la usa la
#     grafica mensual del 23.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Por que asi:
#     - Choque = rendimiento mensual de la valuacion en bolsa (cambio log del indice).
#       Los rendimientos son casi impredecibles, asi que son lo mas parecido a
#       una sorpresa que tenemos; el capex trimestral no sirve (19 trimestres).
#     - Resultado = r_g = ln(importaciones del nivel g) - ln(importaciones del
#       resto de electronicos), desde EUA, sumando los NICO: no hay ceros y se
#       quitan choques comunes (tipo de cambio, aranceles a todo, demanda en Mexico).
#     - Se estima el cambio acumulado r_{t+h} - r_{t-1}: una tendencia lineal
#       propia de cada nivel queda en la constante.
#     r_{t+h} - r_{t-1} = a_h + b_h choque_t + sum_l c_l dr_{t-l} + sum_l d_l choque_{t-l}
#                         + mes del anio + e
#     b_h = cambio acumulado (log) del nivel g frente al resto, h meses despues
#     de un alza permanente de 1 punto log en la valuacion en bolsa.
#     Errores: robustos a heterocedasticidad (HC1). Con rezagos de las variables
#     como controles ("lag augmentation"), Montiel Olea y Plagborg-Moller
#     (Econometrica, 2021) muestran que no hace falta corregir por correlacion
#     serial; Newey-West se guarda solo para comparar.
#   Especificaciones fijas: base, con_sp500 (controla el rendimiento del S&P 500:
#   queda la parte propia de la IA) y sin_aranceles (sin marzo a junio de 2025).
#   Prueba previa: el cambio de r en los 3 y 6 meses ANTES del choque no deberia
#   responder (pre_3, pre_6).
#   Inferencia en muestra chica (especificacion base), dos pruebas mas:
#     bootstrap: wild bootstrap percentil-t (pesos +-1), 999 repeticiones; da
#                intervalo de 90% y valor p
#     permutacion: se reordena al azar la serie de rendimientos 200 veces y se
#                compara b_h real con los b_h que salen sin relacion verdadera
#   Para las estrellas de la base se usa el valor p mas alto de los dos.
#   Requiere: salidas de 01 (datos/empresas) y 02 (output/panel)
#   Salidas : output/proyecciones_locales/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "sandwich", "ggplot2")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "proyecciones_locales")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

origen      <- "USA"
horizontes  <- 0:12          # meses despues del choque
rezagos     <- 3             # rezagos de dr y del choque
meses_aranceles <- seq(as.Date("2025-03-01"), as.Date("2025-06-01"), by = "month")
ref_resto   <- "4_resto_electronicos"
n_perm      <- 200           # permutaciones del choque (0 para omitir)
n_boot      <- 999           # repeticiones del wild bootstrap (0 para omitir)
set.seed(20261117)

lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Canasta (misma regla que el script 07)
# =============================================================================
canasta_def <- lee_csv(file.path(dir_base, "input", "canasta_ia_subpartidas.csv"),
                       col_types = cols(subpartida = col_character())) %>%
  mutate(subpartida = str_pad(subpartida, 6, pad = "0"))
archivo_ajustes <- file.path(dir_base, "input", "canasta_ia_ajustes_fraccion.csv")
ajustes <- if (file.exists(archivo_ajustes))
  lee_csv(archivo_ajustes, col_types = cols(.default = col_character())) %>%
  mutate(fraccion = gsub("[^0-9]", "", fraccion),
         fraccion = if_else(nchar(fraccion) > 8, str_pad(fraccion, 10, pad = "0"),
                            str_pad(fraccion, 8, pad = "0"))) else
  tibble(fraccion = character(), nivel = character(), eslabon = character())
clasifica_ia <- function(fr) {
  f8 <- str_sub(fr, 1, 8); sp <- str_sub(fr, 1, 6)
  coalesce(if (any(nchar(fr) == 10)) ajustes$nivel[match(fr, ajustes$fraccion)] else NA_character_,
           ajustes$nivel[match(f8, ajustes$fraccion)],
           canasta_def$nivel[match(sp, canasta_def$subpartida)], ref_resto)
}

# =============================================================================
# 2. Importaciones mensuales por nivel, relativas al resto
# =============================================================================
pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(),
                                pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion", pais == origen) %>%
  mutate(mes = as.Date(mes),
         unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")),
         grupo = clasifica_ia(unidad))
por_grupo <- pfm %>% group_by(grupo, mes) %>% summarise(valor = sum(valor_usd), .groups = "drop")
resto <- por_grupo %>% filter(grupo == ref_resto) %>% select(mes, valor_resto = valor)
rel <- por_grupo %>% filter(grupo != ref_resto) %>%
  inner_join(resto, by = "mes") %>%
  filter(valor > 0, valor_resto > 0) %>%
  mutate(r = log(valor) - log(valor_resto)) %>%
  select(grupo, mes, r)
message("Meses por nivel: ", paste(rel %>% count(grupo) %>% transmute(paste0(grupo, "=", n)) %>% pull(), collapse = ", "))

# =============================================================================
# 3. Choques: rendimiento mensual de la valuacion en bolsa; control S&P 500
# =============================================================================
indices <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
rend <- function(ind) indices %>% filter(indicadora == ind) %>% arrange(mes) %>%
  transmute(mes, choque = log(coalesce(idx_ponderado, idx_equiponderado)) -
                          lag(log(coalesce(idx_ponderado, idx_equiponderado))))
choques <- list(hiperescaladores = rend("es_hyperscaler"), semiconductores = rend("es_semiconductores"))

# S&P 500 mensual: Yahoo (quantmod); si falla, input/raw/GSPC_mensual.csv (fecha, valor)
sp <- tryCatch({
  if (!"quantmod" %in% rownames(installed.packages())) install.packages("quantmod", repos = "https://cloud.r-project.org")
  x <- quantmod::getSymbols("^GSPC", src = "yahoo", from = "2015-01-01", periodicity = "monthly", auto.assign = FALSE)
  tibble(mes = floor_date(as.Date(zoo::index(x)), "month"), valor = as.numeric(quantmod::Ad(x)))
}, error = function(e) NULL)
if (is.null(sp) && file.exists(file.path(dir_base, "input", "raw", "GSPC_mensual.csv")))
  sp <- lee_csv(file.path(dir_base, "input", "raw", "GSPC_mensual.csv")) %>%
    transmute(mes = floor_date(as.Date(fecha), "month"), valor = as.numeric(valor))
if (!is.null(sp)) {
  sp <- sp %>% arrange(mes) %>% transmute(mes, sp500 = log(valor) - lag(log(valor)))
} else message("Sin S&P 500 mensual: se omite la especificacion con_sp500")

# =============================================================================
# 4. Proyecciones locales
# =============================================================================
arma <- function(d, ch, h) {
  d %>% arrange(mes) %>% left_join(ch, by = "mes") %>%
    { if (!is.null(sp)) left_join(., sp, by = "mes") else . } %>%
    mutate(dep = if (h >= 0) lead(r, h) - lag(r, 1) else lag(r, 1) - lag(r, 1 - h),
           dr = r - lag(r), mda = factor(month(mes))) %>%
    { reduce(1:rezagos, function(z, l) mutate(z, !!paste0("dr_l", l) := lag(dr, l),
                                               !!paste0("ch_l", l) := lag(choque, l)), .init = .) }
}

lp_una <- function(d, ch, h, espec) {
  dd <- arma(d, ch, h)
  if (espec == "sin_aranceles") dd <- dd %>% filter(!mes %in% meses_aranceles)
  # Prueba previa (h < 0): cambio de r ANTES del choque, sin rezagos de dr (serian la misma variable)
  ctrl <- if (h >= 0) c(paste0("dr_l", 1:rezagos), paste0("ch_l", 1:rezagos)) else paste0("ch_l", 1:rezagos)
  if (espec == "con_sp500") ctrl <- c(ctrl, "sp500")
  dd <- dd %>% filter(if_all(all_of(c("dep", "choque", ctrl)), ~ !is.na(.x)))
  if (nrow(dd) < 24) return(NULL)
  m <- lm(as.formula(paste("dep ~ choque +", paste(c(ctrl, "mda"), collapse = " + "))), data = dd)
  b  <- unname(coef(m)["choque"])
  ee <- sqrt(vcovHC(m, type = "HC1")["choque", "choque"])
  ee_nw <- sqrt(NeweyWest(m, lag = abs(h) + 1, prewhite = FALSE)["choque", "choque"])
  bt <- if (espec == "base" && n_boot > 0) wild_boot(m, b, ee) else c(p_boot = NA_real_, li_boot = NA_real_, ls_boot = NA_real_)
  tibble(h, b, ee, ee_nw, n = nrow(dd), p_boot = bt[["p_boot"]], li_boot = bt[["li_boot"]], ls_boot = bt[["ls_boot"]])
}

# Wild bootstrap percentil-t: y* = ajuste + residuo x (+-1); en cada repeticion
# t* = (b* - b) / ee*(HC1). Valor p = proporcion de |t*| >= |t|; intervalo de 90%
# = [b - t*_95 ee, b - t*_05 ee]
wild_boot <- function(m, b, ee) {
  X <- model.matrix(m); X <- X[, !is.na(coef(m)), drop = FALSE]
  j <- which(colnames(X) == "choque"); n <- nrow(X); k <- ncol(X)
  XtXi <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
  if (is.null(XtXi)) return(c(p_boot = NA_real_, li_boot = NA_real_, ls_boot = NA_real_))
  yhat <- fitted(m); e <- resid(m)
  tstar <- vapply(seq_len(n_boot), function(r) {
    ys <- yhat + e * sample(c(-1, 1), n, replace = TRUE)
    bs <- XtXi %*% crossprod(X, ys); es <- as.vector(ys - X %*% bs)
    V <- XtXi %*% crossprod(X * es) %*% XtXi * n / (n - k)
    (bs[j] - b) / sqrt(V[j, j])
  }, numeric(1))
  q <- quantile(tstar, c(0.05, 0.95), na.rm = TRUE)
  c(p_boot = mean(abs(tstar) >= abs(b / ee), na.rm = TRUE), li_boot = b - q[[2]] * ee, ls_boot = b - q[[1]] * ee)
}
# Solo el coeficiente (para las permutaciones)
b_solo <- function(d, ch, h) {
  dd <- arma(d, ch, h)
  ctrl <- if (h >= 0) c(paste0("dr_l", 1:rezagos), paste0("ch_l", 1:rezagos)) else paste0("ch_l", 1:rezagos)
  dd <- dd %>% filter(if_all(all_of(c("dep", "choque", ctrl)), ~ !is.na(.x)))
  if (nrow(dd) < 24) return(NA_real_)
  unname(coef(lm(as.formula(paste("dep ~ choque +", paste(c(ctrl, "mda"), collapse = " + "))), data = dd))["choque"])
}

especs <- c("base", if (!is.null(sp)) "con_sp500", "sin_aranceles")
lp <- imap_dfr(choques, function(ch, nombre_ch) {
  rel %>% group_by(grupo) %>% group_modify(function(d, k) {
    map_dfr(especs, function(e) map_dfr(c(-6, -3, horizontes), function(h) lp_una(d, ch, h, e) %>%
                                          mutate(especificacion = e)))
  }) %>% ungroup() %>% mutate(choque = nombre_ch)
}) %>%
  mutate(li90 = b - 1.645 * ee, ls90 = b + 1.645 * ee,
         horizonte = if_else(h < 0, paste0("pre_", -h), as.character(h))) %>%
  select(choque, especificacion, grupo, horizonte, h, b, ee, ee_nw, li90, ls90, li_boot, ls_boot, p_boot, n)

# Permutaciones (especificacion base)
hs <- c(-6, -3, horizontes)
perm <- if (n_perm > 0) imap_dfr(choques, function(ch, nombre_ch) {
  message("Permutaciones: ", nombre_ch)
  map_dfr(seq_len(n_perm), function(k) {
    chp <- ch %>% mutate(choque = sample(choque))
    rel %>% group_by(grupo) %>%
      group_modify(function(d, g) tibble(h = hs, b_perm = map_dbl(hs, ~ b_solo(d, chp, .x)))) %>%
      ungroup() %>% mutate(choque = nombre_ch)
  })
}) else tibble(choque = character(), grupo = character(), h = numeric(), b_perm = numeric())
lp <- lp %>%
  left_join(lp %>% filter(especificacion == "base") %>% select(choque, grupo, h, b_base = b) %>%
              inner_join(perm, by = c("choque", "grupo", "h")) %>%
              group_by(choque, grupo, h) %>%
              summarise(p_perm = mean(abs(b_perm) >= abs(first(b_base)), na.rm = TRUE), .groups = "drop") %>%
              mutate(especificacion = "base"),
            by = c("choque", "grupo", "h", "especificacion")) %>%
  mutate(p_hc = 2 * (1 - pnorm(abs(b / ee))), p_nw = 2 * (1 - pnorm(abs(b / ee_nw))),
         p = if_else(especificacion == "base", pmax(p_boot, p_perm, na.rm = TRUE), p_hc))

estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen <- lp %>% filter(h %in% c(-6, -3, 0, 3, 6, 9, 12)) %>%
  mutate(celda = sprintf("%.2f%s (%.2f)", b, estrellas(p), ee)) %>%
  select(choque, especificacion, grupo, horizonte, celda) %>%
  pivot_wider(names_from = horizonte, values_from = celda) %>%
  arrange(choque, grupo, especificacion)

write_excel_csv(rel, file.path(dir_out, "00_importaciones_relativas_mensuales.csv"))
write_excel_csv(lp, file.path(dir_out, "01_lp_relativo.csv"))
write_excel_csv(resumen, file.path(dir_out, "02_resumen.csv"))

# Grafica: respuesta del nucleo, especificacion base, con intervalo de 90%
# Solo la especificacion base: con_sp500 y sin_aranceles son pruebas de
# robustez (sin_aranceles quita marzo a junio de 2025 porque la caida del valor
# en bolsa de abril coincidio con los aranceles de EUA), no escenarios
g <- lp %>% filter(grupo == "1_nucleo", h >= 0, especificacion == "base") %>%
  ggplot(aes(h, b, color = especificacion, fill = especificacion)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_ribbon(aes(ymin = coalesce(li_boot, li90), ymax = coalesce(ls_boot, ls90)), alpha = 0.12, color = NA) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.4) +
  facet_wrap(~ choque, labeller = labeller(choque = c(hiperescaladores = "Valuaci\u00f3n en bolsa de hiperescaladores",
                                                      semiconductores = "Valuaci\u00f3n en bolsa de semiconductores"))) +
  scale_x_continuous(breaks = seq(0, 12, 3)) +
  labs(x = "Meses despues del choque", y = "Cambio acumulado (log), nucleo frente al resto",
       color = NULL, fill = NULL) +
  scale_color_manual(values = c(base = "#992246"), guide = "none") +
  scale_fill_manual(values = c(base = "#992246"), guide = "none") +
  theme_minimal(base_size = 11)
ggsave(file.path(dir_out, "03_lp_nucleo.png"), g, width = 9, height = 4.5, dpi = 200, bg = "white")

cat("\nRespuesta acumulada (log) por horizonte en meses; pre_k = cambio en los k meses previos (deberia ser ~0)\n")
cat("*** p<0.01, ** p<0.05, * p<0.10; ee HC1; en base, p = el mayor entre wild bootstrap y permutacion\n")
print(as.data.frame(resumen), right = FALSE, row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
