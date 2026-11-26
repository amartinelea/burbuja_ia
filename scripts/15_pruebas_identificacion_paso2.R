# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/15_pruebas_identificacion_paso2.R
# Objetivo:       Pruebas de identificacion del paso 2 con datos de ANAM: otras variables de
#                 demanda de IA en lugar del capex, y frecuencia mensual.
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
#   - Pruebas de la version anterior del modelo (antes de usar la demanda de
#     EUA por producto). Opcional.
#   - Sus resultados van al anexo 4 de la presentacion.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Problema: con 19 trimestres y un capex casi lineal, eta del paso 2 no separa
#   la respuesta al capex del crecimiento propio de cada nivel (con tendencia por
#   grupo, eta del nucleo ~ 0). Aqui se prueban, con una lista FIJA de antemano:
#     A. Trimestral, otra variable en lugar de ln capex:
#        capex          : capex trimestral encadenado (la base del script 07)
#        capex_ttm      : capex de 12 meses
#        capex_ocf      : capex / flujo operativo, 12 meses (intensidad de inversion)
#        ocf            : flujo operativo de 12 meses
#        capex_y_ocf    : capex y flujo operativo juntos (si lo que mueve las
#                         importaciones es el capex, el escenario F es valido;
#                         si es el flujo, F sobreestima)
#        mc_hyper       : valuacion en bolsa de hiperescaladores, rezago 1
#        mc_semiconductores       : valuacion en bolsa de empresas de semiconductores, rezago 1
#        ingresos_semiconductores : ingresos de empresas de semiconductores (Nvidia, AMD, Broadcom...),
#                         si existe datos/empresas/ingresos_trimestral_semiconductores.csv
#     B. Mensual (todos los meses del panel): valuacion en bolsa de hiperescaladores
#        y de semiconductores, promedio de los 3 meses previos.
#   Cada variable se corre en 4 especificaciones. Regla, fijada antes de ver los
#   resultados: una variable identifica eta si el nucleo es positivo y
#   significativo al 17% en niveles Y con tendencia por grupo (valor p mas alto
#   entre errores agrupados por unidad y Driscoll-Kraay). Se reportan TODAS.
#
#   Requiere: salidas de 01 (datos/empresas) y 02 (output/panel)
#   Salidas : output/identificacion/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "fixest")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "identificacion")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

origen    <- "USA"          # origen de las importaciones
unidad    <- "nico"         # "nico" (10 digitos) o "fraccion" (8)
nivel_sig <- 0.10
# Meses con el choque arancelario de 2025 (robustez mensual: se quitan)
meses_aranceles <- seq(as.Date("2025-03-01"), as.Date("2025-06-01"), by = "month")
ref_resto <- "4_resto_electronicos"

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
# 2. Variables de demanda de IA
# =============================================================================
empresas  <- lee_csv(file.path(dir_emp, "empresas_ia.csv"))
capex_emp <- lee_csv(file.path(dir_emp, "capex_trimestral_empresa.csv")) %>%
  mutate(trimestre = as.Date(trimestre))
indices   <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
ratios    <- lee_csv(file.path(dir_emp, "ratios_financieros_trimestral.csv")) %>%
  mutate(trimestre = as.Date(trimestre))
hyper <- empresas$empresa[empresas$es_hyperscaler]

# Indice encadenado: el crecimiento de cada trimestre usa solo las empresas
# presentes en ese trimestre y en el anterior (igual que el capex del script 07)
encadena <- function(d, var) {
  d %>% filter(!is.na(.data[[var]]), .data[[var]] > 0) %>%
    arrange(empresa, trimestre) %>% group_by(empresa) %>%
    mutate(previo = lag(.data[[var]]),
           continua = !is.na(previo) & lag(trimestre) == trimestre %m-% months(3)) %>%
    group_by(trimestre) %>%
    summarise(total = sum(.data[[var]]),
              crec = if (any(continua)) sum(.data[[var]][continua]) / sum(previo[continua]) else NA_real_,
              .groups = "drop") %>%
    arrange(trimestre) %>%
    mutate(crec = if_else(row_number() == 1 | is.na(crec), 1, crec),
           idx = cumprod(crec), nivel = idx / last(idx) * last(total)) %>%
    select(trimestre, nivel)
}

# Capex trimestral y de 12 meses por empresa (hiperescaladores)
t0 <- capex_emp %>% filter(empresa %in% hyper) %>% group_by(empresa) %>%
  summarise(t = min(trimestre)) %>% pull(t) %>% max()
capex_h <- capex_emp %>% filter(empresa %in% hyper, trimestre >= t0) %>%
  group_by(empresa) %>%
  complete(trimestre = seq(min(trimestre), max(trimestre), by = "quarter")) %>%
  arrange(trimestre) %>%
  mutate(capex_ttm = capex_musd + lag(capex_musd) + lag(capex_musd, 2) + lag(capex_musd, 3)) %>%
  ungroup()

# Flujo operativo de 12 meses: directo del script 01 si viene (ocf_ttm, en USD);
# si no, se deduce de capex_ocf = capex_ttm / ocf_ttm
if ("ocf_ttm" %in% names(ratios)) {
  ocf_h <- ratios %>% filter(empresa %in% hyper) %>% transmute(empresa, trimestre, ocf_ttm = ocf_ttm / 1e6)
} else {
  ocf_h <- ratios %>% filter(empresa %in% hyper) %>%
    inner_join(capex_h %>% select(empresa, trimestre, capex_ttm), by = c("empresa", "trimestre")) %>%
    transmute(empresa, trimestre, ocf_ttm = capex_ttm / capex_ocf)
  message("ocf_ttm no viene en ratios_financieros_trimestral.csv: se deduce de capex_ocf")
}
fin_h <- capex_h %>% select(empresa, trimestre, capex_ttm) %>%
  inner_join(ocf_h, by = c("empresa", "trimestre")) %>%
  filter(!is.na(capex_ttm), !is.na(ocf_ttm), ocf_ttm > 0)

ratio_agr <- fin_h %>% group_by(trimestre) %>%
  summarise(capex_ocf = log(sum(capex_ttm) / sum(ocf_ttm)), .groups = "drop")

mc_trim <- function(ind) indices %>% filter(indicadora == ind) %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>% group_by(trimestre) %>%
  summarise(v = log(mean(coalesce(idx_ponderado, idx_equiponderado))), .groups = "drop") %>%
  arrange(trimestre) %>% transmute(trimestre, v = lag(v))   # rezago 1

vars_q <- encadena(capex_h, "capex_musd") %>% transmute(trimestre, capex = log(nivel)) %>%
  full_join(encadena(fin_h, "capex_ttm") %>% transmute(trimestre, capex_ttm = log(nivel)), by = "trimestre") %>%
  full_join(encadena(fin_h, "ocf_ttm") %>% transmute(trimestre, ocf = log(nivel)), by = "trimestre") %>%
  full_join(ratio_agr, by = "trimestre") %>%
  full_join(mc_trim("es_hyperscaler") %>% rename(mc_hyper = v), by = "trimestre") %>%
  full_join(mc_trim("es_semiconductores") %>% rename(mc_semiconductores = v), by = "trimestre")

archivo_ing <- file.path(dir_emp, "ingresos_trimestral_semiconductores.csv")
if (file.exists(archivo_ing)) {
  ing <- lee_csv(archivo_ing) %>% mutate(trimestre = as.Date(trimestre))
  vars_q <- vars_q %>% full_join(encadena(ing, "ingresos_musd") %>%
                                   transmute(trimestre, ingresos_semiconductores = log(nivel)), by = "trimestre")
} else {
  message("No esta ", archivo_ing, ": se omite ingresos_semiconductores (correr el script 01 actualizado)")
}
vars_q <- vars_q %>% arrange(trimestre)

# Mensuales: valuacion en bolsa, promedio de los 3 meses previos
mc_mes <- function(ind) indices %>% filter(indicadora == ind) %>% arrange(mes) %>%
  transmute(mes, v = log(coalesce(idx_ponderado, idx_equiponderado))) %>%
  mutate(v = (lag(v) + lag(v, 2) + lag(v, 3)) / 3)
vars_m <- mc_mes("es_hyperscaler") %>% rename(mc_hyper = v) %>%
  full_join(mc_mes("es_semiconductores") %>% rename(mc_semiconductores = v), by = "mes") %>% arrange(mes)

# =============================================================================
# 3. Panel de ANAM: unidad x mes y unidad x trimestre, con ceros
# =============================================================================
pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(),
                                pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion", pais == origen) %>%
  mutate(mes = as.Date(mes),
         fraccion = str_pad(fraccion, 8, pad = "0"),
         fraccion = if (unidad == "nico") paste0(fraccion, str_pad(nico, 2, pad = "0")) else fraccion)
meses <- sort(unique(pfm$mes))

prepara <- function(d) d %>%
  mutate(grupo = clasifica_ia(fraccion)) %>%
  group_by(fraccion) %>% mutate(peso = mean(valor_usd), continuo = all(valor_usd > 0)) %>%
  ungroup() %>% filter(peso > 0) %>%
  mutate(ln_valor = log1p(valor_usd), t_num = as.numeric(factor(periodo)))

panel_m <- pfm %>% group_by(fraccion, periodo = mes) %>%
  summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
  complete(fraccion, periodo = meses, fill = list(valor_usd = 0)) %>% prepara()

trim_ok <- tibble(mes = meses) %>% count(trimestre = floor_date(mes, "quarter")) %>%
  filter(n == 3) %>% pull(trimestre)
panel_q <- panel_m %>% select(fraccion, periodo, valor_usd) %>%
  mutate(periodo = floor_date(periodo, "quarter")) %>% filter(periodo %in% trim_ok) %>%
  group_by(fraccion, periodo) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
  prepara()
message(sprintf("Panel: %d unidades; %d meses y %d trimestres completos",
                n_distinct(panel_m$fraccion), n_distinct(panel_m$periodo), n_distinct(panel_q$periodo)))

# =============================================================================
# 4. Estimacion
# =============================================================================
# variables: una o dos columnas de demanda; especificacion:
#   niveles            PPML, efectos fijos de unidad y periodo
#   tendencia          + tendencia lineal propia de cada nivel
#   tend_sin_aranceles igual, sin marzo a junio de 2025 (solo mensual)
#   continuos_tend_ols OLS en logs, solo unidades con comercio en todos los periodos, con tendencia
#   continuos_cambios  OLS en cambios de 4 trimestres, solo unidades continuas (solo trimestral)
corre <- function(panel, vars_df, frecuencia, nombre, vars, espec) {
  d <- panel %>% inner_join(vars_df %>% rename(periodo = 1) %>% select(periodo, all_of(vars)),
                            by = "periodo") %>%
    filter(if_all(all_of(vars), ~ !is.na(.x)))
  if (espec == "tend_sin_aranceles") d <- d %>% filter(!periodo %in% meses_aranceles)
  if (espec %in% c("continuos_tend_ols", "continuos_cambios")) d <- d %>% filter(continuo)
  regs <- vars
  if (espec == "continuos_cambios") {
    d <- d %>% arrange(fraccion, periodo) %>% group_by(fraccion) %>%
      mutate(across(all_of(c("ln_valor", vars)), ~ .x - lag(.x, 4))) %>% ungroup() %>%
      filter(if_all(all_of(c("ln_valor", vars)), ~ !is.na(.x)))
  }
  if (nrow(d) == 0 || n_distinct(d$periodo) < 6) return(NULL)
  d <- d %>% mutate(t_num = as.numeric(factor(periodo)))
  rhs  <- paste0("i(grupo, ", regs, ", ref = ref_resto)", collapse = " + ")
  tend <- if (espec %in% c("tendencia", "tend_sin_aranceles", "continuos_tend_ols"))
    " + i(grupo, t_num, ref = ref_resto)" else ""
  fml <- if (espec %in% c("niveles", "tendencia", "tend_sin_aranceles"))
    as.formula(paste0("valor_usd ~ ", rhs, tend, " | fraccion + periodo")) else
    as.formula(paste0("ln_valor ~ ", rhs, tend, " | fraccion + periodo"))
  m <- tryCatch({
    if (espec %in% c("niveles", "tendencia", "tend_sin_aranceles"))
      fepois(fml, data = d, panel.id = ~fraccion + t_num) else
      feols(fml, data = d, weights = ~peso, panel.id = ~fraccion + t_num)
  }, error = function(e) { message("  falla ", nombre, " / ", espec, ": ", conditionMessage(e)); NULL })
  if (is.null(m)) return(NULL)
  saca <- function(ct, sufijo) {
    filas <- rownames(ct)[grepl(paste0(":(", paste(regs, collapse = "|"), ")$"), rownames(ct))]
    tibble(termino = filas, eta = ct[filas, 1], ee = ct[filas, 2], p = ct[filas, 4]) %>%
      rename_with(~ paste0(.x, sufijo), c(ee, p))
  }
  cl <- saca(coeftable(summary(m, cluster = ~fraccion)), "_unidad")
  dk <- tryCatch(saca(coeftable(summary(m, vcov = "DK")), "_dk") %>% select(-eta),
                 error = function(e) tibble(termino = cl$termino, ee_dk = NA_real_, p_dk = NA_real_))
  cl %>% left_join(dk, by = "termino") %>%
    mutate(frecuencia, variable = nombre, especificacion = espec,
           regresor = str_extract(termino, "[^:]+$"),
           grupo = str_match(termino, "grupo::(.*):")[, 2],
           n_obs = nobs(m), n_unidades = n_distinct(d$fraccion), n_periodos = n_distinct(d$periodo)) %>%
    select(frecuencia, variable, especificacion, regresor, grupo, eta, ee_unidad, p_unidad,
           ee_dk, p_dk, n_obs, n_unidades, n_periodos)
}

lista_q <- list(capex = "capex", capex_ttm = "capex_ttm", capex_ocf = "capex_ocf", ocf = "ocf",
                capex_y_ocf = c("capex_ttm", "ocf"), mc_hyper = "mc_hyper", mc_semiconductores = "mc_semiconductores")
if ("ingresos_semiconductores" %in% names(vars_q)) lista_q$ingresos_semiconductores <- "ingresos_semiconductores"
lista_m <- list(mc_hyper = "mc_hyper", mc_semiconductores = "mc_semiconductores")
espec_q <- c("niveles", "tendencia", "continuos_tend_ols", "continuos_cambios")
espec_m <- c("niveles", "tendencia", "tend_sin_aranceles", "continuos_tend_ols")

res <- bind_rows(
  imap_dfr(lista_q, function(v, n) map_dfr(espec_q, function(e) {
    message("Trimestral: ", n, " / ", e); corre(panel_q, vars_q, "trimestral", n, v, e) })),
  imap_dfr(lista_m, function(v, n) map_dfr(espec_m, function(e) {
    message("Mensual: ", n, " / ", e); corre(panel_m, vars_m, "mensual", n, v, e) }))) %>%
  mutate(p_max = pmax(p_unidad, p_dk, na.rm = TRUE))

# =============================================================================
# 5. Diagnostico de las variables: cuanta variacion tienen fuera de la tendencia
# =============================================================================
diag_var <- function(vdf, periodos, frecuencia) {
  vdf <- vdf %>% rename(periodo = 1) %>% filter(periodo %in% periodos)
  map_dfr(setdiff(names(vdf), "periodo"), function(v) {
    x <- vdf[[v]]; ok <- !is.na(x); t <- seq_along(x)
    if (sum(ok) < 6) return(NULL)
    f <- lm(x[ok] ~ t[ok])
    tibble(frecuencia, variable = v, n_periodos = sum(ok),
           r2_tendencia = summary(f)$r.squared,
           de_fuera_tendencia = sd(resid(f)),
           correlacion_con_capex = if ("capex" %in% names(vdf)) cor(x, vdf$capex, use = "pairwise") else NA_real_)
  })
}
diagnostico <- bind_rows(diag_var(vars_q, unique(panel_q$periodo), "trimestral"),
                         diag_var(vars_m, unique(panel_m$periodo), "mensual"))

# Resumen: eta del nucleo por variable y especificacion, y la regla de arriba
estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen <- res %>% filter(grupo == "1_nucleo") %>%
  mutate(etiqueta = if_else(variable == regresor, variable, paste0(variable, ": ", regresor)),
         celda = sprintf("%.2f%s (%.2f)", eta, estrellas(p_max), pmax(ee_unidad, ee_dk, na.rm = TRUE)))
regla <- resumen %>% filter(especificacion %in% c("niveles", "tendencia")) %>%
  group_by(frecuencia, etiqueta) %>%
  summarise(identifica = n() == 2 && all(eta > 0 & p_max < nivel_sig), .groups = "drop")
resumen <- resumen %>% select(frecuencia, etiqueta, especificacion, celda) %>%
  pivot_wider(names_from = especificacion, values_from = celda) %>%
  left_join(regla, by = c("frecuencia", "etiqueta")) %>% arrange(desc(frecuencia), etiqueta)

write_excel_csv(vars_q, file.path(dir_out, "00_variables_trimestrales.csv"))
write_excel_csv(vars_m, file.path(dir_out, "00_variables_mensuales.csv"))
write_excel_csv(diagnostico, file.path(dir_out, "01_diagnostico_variables.csv"))
write_excel_csv(res, file.path(dir_out, "02_eta_por_variable.csv"))
write_excel_csv(resumen, file.path(dir_out, "03_resumen_nucleo.csv"))

cat("\nVariacion fuera de la tendencia (r2 alto = casi una linea recta):\n")
print(diagnostico %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf)
cat("\neta del nucleo (*** p<0.01, ** p<0.05, * p<0.10; ee y p: el mayor entre agrupado por unidad y Driscoll-Kraay)\n")
print(as.data.frame(resumen), right = FALSE)
message("Listo. Archivos en: ", dir_out)
