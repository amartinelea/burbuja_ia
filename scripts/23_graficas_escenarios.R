# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/23_graficas_escenarios.R
# Objetivo:       Graficas por escenario (A a F): importaciones observadas, el choque, la
#                 trayectoria del escenario y el contrafactual sin choque (punteado).
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
#   - Graficas de trayectoria por escenario (ggplot). La lamina de trayectoria
#     esta oculta en la presentacion.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Dos versiones:
#     1. Cadena (scripts 01 y 07, trimestral): total de electronicos y nucleo.
#        Escenario = cota alta (eta de la base) con su intervalo Monte Carlo de
#        90%; cota baja = eta = 0 (solo queda el efecto de arrastre supuesto).
#        Tiempos: los choques a la valuacion en bolsa (A a D) llegan al capex con los
#        rezagos del paso 1 (1 y 2 trimestres); el de tasas (E) con 1 trimestre;
#        la caida directa del capex (F) en el mismo trimestre.
#     2. Proyecciones locales (script 17, mensual): nucleo, solo para choques al
#        valuacion en bolsa de hiperescaladores (B, C y D). E y F no cambian el valor
#        en bolsa, asi que las proyecciones locales no dicen nada de ellos.
#        La proyeccion local mide el nucleo frente al resto: la trayectoria
#        supone que el resto de electronicos no cambia (sin efecto de arrastre).
#   Contrafactual en las dos: el ultimo nivel observado se mantiene (escenario A).
#   En la version mensual el observado es promedio movil de 3 meses (los meses
#   sueltos son muy ruidosos). Los cambios en % son los mismos que en el script
#   07, que los calcula sobre los ultimos 4 trimestres.
#   Requiere: salidas de 07 (output/choque_ia), 02 (output/panel) y 17
#             (output/proyecciones_locales)
#   Salidas : output/graficas_escenarios/ (PNG 300 dpi fondo transparente y SVG)
#   Cadena "census" (scripts 06, 09 y 10, por defecto): paso 2 del diseno por
#     producto; importaciones de todos los origenes; linea = eta con tendencia,
#     punteada larga = eta sin tendencia; banda Monte Carlo de 90%.
#   Cadena "capex" (script 07, version anterior): importaciones desde EUA; linea =
#     cota alta, punteada larga = cota baja (eta = 0).
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "ggplot2")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# Poppins (estilo ATDT) si showtext esta disponible; si no, fuente por defecto
fuente <- tryCatch({
  if (!"showtext" %in% rownames(installed.packages())) install.packages("showtext", repos = "https://cloud.r-project.org")
  showtext::font_add_google("Poppins", "Poppins"); showtext::showtext_auto(); showtext::showtext_opts(dpi = 300)
  "Poppins"
}, error = function(e) "")

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
cadena    <- "census"   # "census" (scripts 06, 09 y 10) o "capex" (script 07)
dir_02    <- file.path(dir_base, "output", if (cadena == "census") "choque_ia_census" else "choque_ia")
dir_10    <- file.path(dir_base, "output", "proyecciones_locales")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "graficas_escenarios")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
origen    <- if (cadena == "census") "todos" else "USA"
version_alta <- if (cadena == "census") "principal" else "relativo"
lab_alta  <- if (cadena == "census") "Escenario (\u03b7 con tendencia)" else "Escenario (cota alta)"
lab_baja  <- if (cadena == "census") "\u03b7 sin tendencia" else "Cota baja (\u03b7 = 0)"
de_donde  <- if (cadena == "census") "de todos los or\u00edgenes" else "desde EUA"
trim_proy <- 5          # trimestres proyectados
ref_resto <- "4_resto_electronicos"

# Paleta ATDT
col_obs <- "#16191C"; col_cf <- "#98979A"; col_esc <- "#992246"; col_baja <- "#A37D2E"

lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Canasta e historia observada
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
  tibble(fraccion = character(), nivel = character())
clasifica_ia <- function(fr) {
  f8 <- str_sub(fr, 1, 8); sp <- str_sub(fr, 1, 6)
  coalesce(if (any(nchar(fr) == 10)) ajustes$nivel[match(fr, ajustes$fraccion)] else NA_character_,
           ajustes$nivel[match(f8, ajustes$fraccion)],
           canasta_def$nivel[match(sp, canasta_def$subpartida)], ref_resto)
}

pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(),
                                pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion", origen == "todos" | pais == origen) %>%
  mutate(mes = as.Date(mes),
         grupo = clasifica_ia(paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0"))))
mensual <- pfm %>% group_by(mes) %>%
  summarise(total = sum(valor_usd) / 1e6, nucleo = sum(valor_usd[grupo == "1_nucleo"]) / 1e6, .groups = "drop")
trim_ok <- mensual %>% count(trimestre = floor_date(mes, "quarter")) %>% filter(n == 3) %>% pull(trimestre)
trimestral <- mensual %>% mutate(periodo = floor_date(mes, "quarter")) %>% filter(periodo %in% trim_ok) %>%
  group_by(periodo) %>% summarise(total = sum(total), nucleo = sum(nucleo), .groups = "drop")

# =============================================================================
# 2. Resultados del script 07
# =============================================================================
esc   <- lee_csv(file.path(dir_02, "03_escenarios_supuestos.csv"))
canas_todas <- lee_csv(file.path(dir_02, "05_resultado_por_canasta.csv"))
total_todas <- lee_csv(file.path(dir_02, "04_resultado_total.csv"))
canas <- canas_todas %>% filter(version_beta == version_alta)
total <- total_todas %>% filter(version_beta == version_alta)
ic    <- lee_csv(file.path(dir_02, "04b_intervalos_impacto.csv")) %>% filter(version_beta == version_alta)
coefs <- tryCatch(lee_csv(file.path(dir_02, "00c_coeficientes_ic.csv")), error = function(e) NULL)
tpan  <- tryCatch(lee_csv(file.path(dir_02, "00b_theta_panel_ratio.csv")), error = function(e) NULL)

# Parte del efecto de la valuacion en bolsa que llega al capex al trimestre 1 (el resto
# al trimestre 2), segun el paso 1: (theta_l1 + psi_l1 (R hoy - R medio)) / theta efectiva
parte_t1 <- tryCatch({
  g <- function(t) coefs$estimado[coefs$modelo == "paso1_panel_capex_ocf_DK" & coefs$termino == t]
  dR <- tpan$ratio_hoy - tpan$ratio_media
  min(max((g("d4_mcap_l1") + g("d4_mcap_l1:r_c") * dR) / tpan$theta_efectiva, 0), 1)
}, error = function(e) 0.5)
if (length(parte_t1) != 1 || is.na(parte_t1)) parte_t1 <- 0.5
message(sprintf("Parte del efecto en el trimestre 1: %.2f (el resto en el trimestre 2)", parte_t1))

# Ponderador del efecto por trimestre despues del choque (q = 0 es el trimestre del choque)
pondera <- function(fila, q) {
  if (fila$capex_directo != 0 && fila$mcap_hyper == 0 && fila$delta_tasa == 0) return(1)   # F
  if (fila$delta_tasa != 0 && fila$mcap_hyper == 0) return(as.numeric(q >= 1))             # E
  c(0, parte_t1, 1)[pmin(q, 2) + 1]                                                         # A a D
}

# Variacion (%) por escenario: cota alta, intervalo y cota baja (solo efecto de arrastre).
# Los archivos del 07 traen var_pct como fraccion.
var_esc <- function(nivel_) {
  alta <- if (nivel_ == "total") total %>% transmute(escenario, alta = 100 * var_pct) else
    canas %>% filter(canasta == nivel_) %>% transmute(escenario, alta = 100 * var_pct)
  baja <- if (cadena == "census") {
    if (nivel_ == "total") total_todas %>% filter(version_beta == "eta_base") %>% transmute(escenario, baja = 100 * var_pct) else
      canas_todas %>% filter(version_beta == "eta_base", canasta == nivel_) %>% transmute(escenario, baja = 100 * var_pct)
  } else canas %>% filter(canasta == ref_resto) %>% transmute(escenario, baja = 100 * var_pct)
  band <- ic %>% filter(nivel == nivel_) %>% transmute(escenario, inf = 100 * var_pct_inf, sup = 100 * var_pct_sup)
  esc %>% select(escenario, mcap_hyper, delta_tasa, capex_directo) %>%
    left_join(alta, by = "escenario") %>% left_join(baja, by = "escenario") %>% left_join(band, by = "escenario")
}

# Trayectorias trimestrales por escenario
trayectoria <- function(hist, var, nivel_) {
  base_q <- last(hist[[var]])
  ult <- max(hist$periodo)
  ve <- var_esc(nivel_)
  map_dfr(seq_len(nrow(ve)), function(i) {
    f <- ve[i, ]
    tibble(q = 0:(trim_proy - 1), periodo = ult %m+% months(3 * (1:trim_proy))) %>%
      mutate(w = map_dbl(q, ~ pondera(f, .x)),
             contrafactual = base_q,
             escenario_alta = base_q * (1 + w * f$alta / 100),
             escenario_baja = base_q * (1 + w * f$baja / 100),
             inf = base_q * (1 + w * f$inf / 100), sup = base_q * (1 + w * f$sup / 100),
             escenario = f$escenario)
  })
}

# =============================================================================
# 3. Graficas: A a D (principal) y E y F (anexo)
# =============================================================================
etq <- c("A. Ceteris paribus" = "A. Escenario base",
         "B. Ajuste de expectativas (neoclouds)" = "B. Ajuste de expectativas",
         "C. Contraccion de la industria" = "C. Contracci\u00f3n de la industria",
         "D. Auge (aceleracion)" = "D. Auge (aceleraci\u00f3n)",
         "E. Alza de tasas (+100 pb)" = "E. Alza de tasas (+100 pb)",
         "F. Transicion a monetizacion" = "F. Transici\u00f3n a monetizaci\u00f3n")
tema <- theme_minimal(base_size = 12, base_family = fuente) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", hjust = 0), plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, color = "#555555", size = 9))

guardar <- function(g, nombre, ancho = 30.8, alto = 17) {
  ggsave(file.path(dir_out, paste0(nombre, ".png")), g, width = ancho, height = alto, units = "cm", dpi = 300, bg = "transparent")
  if ("svglite" %in% rownames(installed.packages()))
    ggsave(file.path(dir_out, paste0(nombre, ".svg")), g, width = ancho, height = alto, units = "cm", bg = "transparent")
}

grafica <- function(hist, proy, var, titulo, eje_y, nota, x_choque, texto_vacio = NULL, ncol = 3, subtitulo = NULL) {
  hist_l <- hist %>% filter(periodo >= max(periodo) %m-% months(36)) %>% transmute(periodo, valor = .data[[var]])
  ult <- tail(hist_l, 1)
  # Une la ultima observacion con las proyecciones para que las lineas no tengan hueco
  une <- function(col) bind_rows(proy %>% distinct(escenario) %>% mutate(periodo = ult$periodo, v = ult$valor),
                                 proy %>% transmute(escenario, periodo, v = .data[[col]]))
  esc_alta <- une("escenario_alta"); cf <- une("contrafactual")
  # La linea secundaria solo se dibuja donde se separa del contrafactual
  con_baja <- proy %>% group_by(escenario) %>%
    summarise(dif = any(abs(escenario_baja - contrafactual) > 1e-9, na.rm = TRUE), .groups = "drop") %>%
    filter(dif) %>% pull(escenario)
  esc_baja <- une("escenario_baja") %>% filter(escenario %in% con_baja)
  banda <- bind_rows(proy %>% distinct(escenario) %>% mutate(periodo = ult$periodo, inf = ult$valor, sup = ult$valor),
                     proy %>% select(escenario, periodo, inf, sup))
  col_lin <- function(e) rep(col_esc, length(e))
  # En A el escenario es el contrafactual: no se dibuja linea ni banda propias
  esc_alta <- esc_alta %>% filter(!str_detect(escenario, "^A\\."))
  banda <- banda %>% filter(!str_detect(escenario, "^A\\."))
  g <- ggplot() +
    geom_vline(xintercept = x_choque, color = "#CCCCCC", linewidth = 0.4) +
    geom_ribbon(data = banda, aes(periodo, ymin = inf, ymax = sup, fill = col_lin(escenario)), alpha = 0.18) +
    geom_line(data = tidyr::crossing(hist_l, escenario = unique(proy$escenario)),
              aes(periodo, valor, color = "Observado", linetype = "Observado"), linewidth = 0.7) +
    geom_line(data = cf, aes(periodo, v, color = "Contrafactual sin choque", linetype = "Contrafactual sin choque"), linewidth = 0.7) +
    geom_line(data = esc_baja, aes(periodo, v, color = lab_baja, linetype = lab_baja), linewidth = 0.6) +
    geom_line(data = esc_alta, aes(periodo, v, color = lab_alta, linetype = lab_alta), linewidth = 0.9) +
    facet_wrap(~ escenario, ncol = ncol, labeller = as_labeller(function(x) unname(coalesce(etq[x], x)))) +
    scale_fill_identity() +
    scale_color_manual(NULL, values = setNames(c(col_obs, col_cf, col_baja, col_esc),
                                               c("Observado", "Contrafactual sin choque", lab_baja, lab_alta))) +
    scale_linetype_manual(NULL, values = setNames(c("solid", "dotted", "dashed", "solid"),
                                                  c("Observado", "Contrafactual sin choque", lab_baja, lab_alta))) +
    scale_x_date(expand = expansion(mult = c(0.02, 0.07))) +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = eje_y, caption = nota) + tema
  if (!is.null(texto_vacio))
    g <- g + geom_text(data = texto_vacio, aes(x, y, label = etiqueta), size = 3, color = "#555555",
                       family = fuente, lineheight = 0.95, hjust = 0, vjust = 1)
  g
}

hist_q <- trimestral
x_ch <- max(hist_q$periodo) + 45
nota_cadena <- paste0("Contrafactual: el \u00faltimo trimestre observado se mantiene. Banda: intervalo Monte Carlo de 90% (incertidumbre de ",
                      if (cadena == "census") "\u03b8, \u03b3, \u03b5 y \u03b7" else "\u03b8, \u03b3 y \u03b7", ").\n",
                      "Tiempos: la valuaci\u00f3n en bolsa llega al gasto de capital en 1 y 2 trimestres (paso 1); la tasa en 1; la ca\u00edda directa del gasto de capital (F) en el mismo trimestre.")
# Principales: A a D (2 x 2). Anexo: E (tasas) y F (monetizacion), en otra grafica
grupos_esc <- list(principales = "^[A-D]\\.", anexo_tasas_monetizacion = "^[EF]\\.")
subt <- "Distancia entre la l\u00ednea del escenario y el punteado = efecto del escenario sobre el valor importado (millones de USD por trimestre)"
for (v in c("total", "nucleo")) {
  nivel_ <- if (v == "total") "total" else "1_nucleo"
  proy_todo <- trayectoria(hist_q, v, nivel_)
  for (gr in names(grupos_esc)) {
    proy <- proy_todo %>% filter(str_detect(escenario, grupos_esc[[gr]]))
    if (!nrow(proy)) next
    nombre <- if (gr == "principales") paste0("01_cadena_", v) else paste0("01b_cadena_", v, "_tasas_monetizacion")
    g <- grafica(hist_q, proy, v,
                 titulo = if (v == "total") paste("Importaciones de electr\u00f3nicos", de_donde, "por escenario") else
                   paste("Importaciones del n\u00facleo de IA", de_donde, "por escenario"),
                 eje_y = "Millones de USD por trimestre", nota = nota_cadena, x_choque = x_ch, ncol = 2, subtitulo = subt)
    if (gr == "principales") guardar(g, nombre, ancho = 26, alto = 19) else guardar(g, nombre, ancho = 26, alto = 11.5)
  }
  write_excel_csv(proy_todo, file.path(dir_out, paste0("01_cadena_", v, ".csv")))
}

# =============================================================================
# 4. Proyecciones locales (mensual, nucleo; A a D, version anterior del paso 2)
# =============================================================================
archivo_lp <- file.path(dir_10, "01_lp_relativo.csv")
if (file.exists(archivo_lp)) {
  lp <- lee_csv(archivo_lp) %>% filter(choque == "hiperescaladores", especificacion == "base",
                                       grupo == "1_nucleo", h >= 0) %>% arrange(h)
  hist_m <- mensual %>% arrange(mes) %>%
    transmute(periodo = mes, nucleo = (nucleo + lag(nucleo) + lag(nucleo, 2)) / 3) %>% filter(!is.na(nucleo))
  base_m <- last(hist_m$nucleo); ult <- max(hist_m$periodo)
  esc_lp <- esc %>% filter(str_detect(escenario, "^[A-D]\\."))   # E y F no mueven la valuacion en bolsa
  proy_lp <- map_dfr(seq_len(nrow(esc_lp)), function(i) {
    s <- log(1 + esc_lp$mcap_hyper[i])
    lp %>% transmute(periodo = ult %m+% months(h + 1),
                     contrafactual = base_m,
                     escenario_alta = base_m * exp(b * s),
                     escenario_baja = base_m,
                     inf = base_m * exp(pmin(coalesce(li_boot, li90) * s, coalesce(ls_boot, ls90) * s)),
                     sup = base_m * exp(pmax(coalesce(li_boot, li90) * s, coalesce(ls_boot, ls90) * s)),
                     escenario = esc_lp$escenario[i]) %>%
      { if (s == 0) mutate(., escenario_alta = NA_real_, inf = NA_real_, sup = NA_real_) else . }
  })
  y_tope <- 2 * max(hist_m$nucleo[hist_m$periodo >= max(hist_m$periodo) %m-% months(36)])
  g <- grafica(hist_m, proy_lp, "nucleo",
               titulo = "Importaciones del n\u00facleo de IA desde EUA por escenario: proyecciones locales",
               eje_y = "Millones de USD por mes",
               nota = paste0("Respuesta mensual del n\u00facleo frente al resto a un choque a la valuaci\u00f3n en bolsa de hiperescaladores (script 10, especificaci\u00f3n base).\n",
                             "Observado: promedio m\u00f3vil de 3 meses. Supone que el resto de electr\u00f3nicos no cambia (sin efecto de arrastre). Contrafactual: el \u00faltimo nivel se mantiene.\n",
                             "Banda: wild bootstrap de 90%, recortada en el doble del m\u00e1ximo observado."),
               x_choque = ult + 15, ncol = 2) +
    scale_linetype_manual(NULL, values = setNames(c("solid", "dotted", "blank", "solid"),
                                                  c("Observado", "Contrafactual sin choque", lab_baja, lab_alta)),
                          breaks = c("Observado", "Contrafactual sin choque", lab_alta),
                          labels = c("Observado", "Contrafactual sin choque", "Escenario")) +
    scale_color_manual(NULL, values = setNames(c(col_obs, col_cf, NA, col_esc),
                                               c("Observado", "Contrafactual sin choque", lab_baja, lab_alta)),
                       breaks = c("Observado", "Contrafactual sin choque", lab_alta),
                       labels = c("Observado", "Contrafactual sin choque", "Escenario")) +
    coord_cartesian(ylim = c(0, y_tope))
  guardar(g, "02_proyecciones_locales_nucleo", ancho = 26, alto = 19)
  write_excel_csv(proy_lp, file.path(dir_out, "02_proyecciones_locales_nucleo.csv"))
} else message("No esta ", archivo_lp, ": correr el script 10 antes")

message("Listo. Graficas en: ", dir_out)
