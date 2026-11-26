# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/18_bootstrap_paso2.R
# Objetivo:       Inferencia en muestra chica para el paso 2 (eta por nivel, PPML trimestral):
#                 bootstrap por bloques de trimestres.
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
#   - Prueba complementaria: bootstrap por bloques de trimestres para la
#     version anterior del paso 2. Opcional.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Por que por bloques de tiempo: la variable que explica (ln capex) solo cambia
#   en el tiempo, asi que la informacion viene de 19 trimestres, no de 1,034 NICO.
#   Los errores agrupados por NICO tratan a cada NICO como independiente y no
#   capturan choques comunes de un trimestre. Aqui se remuestrean bloques de 4
#   trimestres seguidos (con todos los NICO de esos trimestres), se vuelve a
#   estimar el PPML y se ve cuanto varia eta.
#     Un trimestre que sale dos veces se trata como dos periodos distintos (su
#     propio efecto fijo); la tendencia por nivel usa la fecha real.
#   Especificaciones: base (efectos fijos de NICO y trimestre) y con tendencia por nivel.
#   Con 19 trimestres hay pocos bloques: el bootstrap tambien es impreciso. Leerlo
#   como "cuanto mas ancho es el intervalo honesto", no como un valor p exacto.
#   Requiere: salidas de 01 (datos/empresas) y 02 (output/panel)
#   Salidas : output/bootstrap_paso2/
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
dir_out   <- file.path(dir_base, "output", "bootstrap_paso2")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

origen    <- "USA"
unidad    <- "nico"
n_boot    <- 499         # repeticiones (cada una es un PPML: tarda unos minutos)
largo_blq <- 4           # trimestres por bloque
ref_resto <- "4_resto_electronicos"
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
# 2. Variables de demanda (capex encadenado y valuacion en bolsa)
# =============================================================================
empresas  <- lee_csv(file.path(dir_emp, "empresas_ia.csv"))
capex_emp <- lee_csv(file.path(dir_emp, "capex_trimestral_empresa.csv")) %>% mutate(trimestre = as.Date(trimestre))
indices   <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
hyper <- empresas$empresa[empresas$es_hyperscaler]

t0 <- capex_emp %>% filter(empresa %in% hyper) %>% group_by(empresa) %>%
  summarise(t = min(trimestre)) %>% pull(t) %>% max()
capex_q <- capex_emp %>% filter(empresa %in% hyper, trimestre >= t0, capex_musd > 0) %>%
  arrange(empresa, trimestre) %>% group_by(empresa) %>%
  mutate(previo = lag(capex_musd),
         continua = !is.na(previo) & lag(trimestre) == trimestre %m-% months(3)) %>%
  group_by(trimestre) %>%
  summarise(total = sum(capex_musd),
            crec = if (any(continua)) sum(capex_musd[continua]) / sum(previo[continua]) else NA_real_,
            .groups = "drop") %>%
  arrange(trimestre) %>%
  mutate(crec = if_else(row_number() == 1 | is.na(crec), 1, crec),
         capex_q = log(cumprod(crec) / last(cumprod(crec)) * last(total))) %>%
  transmute(periodo = trimestre, capex_q)

mc_h <- indices %>% filter(indicadora == "es_hyperscaler") %>% arrange(mes) %>%
  transmute(mes, v = log(coalesce(idx_ponderado, idx_equiponderado)))
mc_q <- mc_h %>% mutate(periodo = floor_date(mes, "quarter")) %>% group_by(periodo) %>%
  summarise(v = log(mean(exp(v))), .groups = "drop") %>% arrange(periodo) %>%
  transmute(periodo, mc_hyper_q = lag(v))
mc_m <- mc_h %>% transmute(periodo = mes, mc_hyper_m = (lag(v) + lag(v, 2) + lag(v, 3)) / 3)

# =============================================================================
# 3. Panel de ANAM con ceros: unidad x mes y unidad x trimestre
# =============================================================================
pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(),
                                pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion", pais == origen) %>%
  mutate(mes = as.Date(mes), fraccion = str_pad(fraccion, 8, pad = "0"),
         fraccion = if (unidad == "nico") paste0(fraccion, str_pad(nico, 2, pad = "0")) else fraccion)
meses <- sort(unique(pfm$mes))

prepara <- function(d) d %>%
  mutate(grupo = clasifica_ia(fraccion)) %>%
  group_by(fraccion) %>%
  mutate(peso = mean(valor_usd), media_pos = mean(valor_usd[valor_usd > 0])) %>%
  ungroup() %>% filter(peso > 0) %>%
  mutate(t_num = as.numeric(factor(periodo)),
         positivo = as.numeric(valor_usd > 0),
         log1p_usd = log1p(valor_usd), log1p_miles = log1p(valor_usd / 1e3),
         log1p_millones = log1p(valor_usd / 1e6),
         ln_pos = if_else(valor_usd > 0, log(valor_usd), NA_real_),
         rel = if_else(valor_usd > 0, log(valor_usd / media_pos), NA_real_))

panel_m <- pfm %>% group_by(fraccion, periodo = mes) %>%
  summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
  complete(fraccion, periodo = meses, fill = list(valor_usd = 0)) %>% prepara()
trim_ok <- tibble(mes = meses) %>% count(trimestre = floor_date(mes, "quarter")) %>%
  filter(n == 3) %>% pull(trimestre)
panel_q <- panel_m %>% select(fraccion, periodo, valor_usd) %>%
  mutate(periodo = floor_date(periodo, "quarter")) %>% filter(periodo %in% trim_ok) %>%
  group_by(fraccion, periodo) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
  prepara()


# =============================================================================
# 4. Bootstrap por bloques de trimestres
# =============================================================================
d0 <- panel_q %>% inner_join(capex_q, by = "periodo") %>%
  mutate(t_real = as.numeric(factor(periodo)))
periodos <- sort(unique(d0$periodo)); T_ <- length(periodos)
message(sprintf("Panel: %d NICO, %d trimestres; bloques de %d", n_distinct(d0$fraccion), T_, largo_blq))

formula_esp <- function(espec) as.formula(paste0(
  "valor_usd ~ i(grupo, capex_q, ref = ref_resto)",
  if (espec == "tendencia") " + i(grupo, t_real, ref = ref_resto)" else "", " | fraccion + periodo_b"))
saca <- function(m) {
  ct <- coef(m); f <- names(ct)[grepl(":capex_q$", names(ct))]
  tibble(grupo = str_match(f, "grupo::(.*):")[, 2], eta = unname(ct[f]))
}

estima <- function(d, espec) tryCatch(saca(fepois(formula_esp(espec), data = d, notes = FALSE, warn = FALSE)),
                                      error = function(e) NULL)

# Muestra de bloques: inicios al azar entre 1 y T - largo + 1, hasta juntar T trimestres
muestra_bloques <- function() {
  n_blq <- ceiling(T_ / largo_blq)
  ini <- sample(seq_len(T_ - largo_blq + 1), n_blq, replace = TRUE)
  idx <- unlist(lapply(ini, function(i) i:(i + largo_blq - 1)))[seq_len(T_)]
  tibble(periodo = periodos[idx], periodo_b = seq_along(idx))
}

res <- map_dfr(c("niveles", "tendencia"), function(espec) {
  real <- estima(d0 %>% mutate(periodo_b = as.numeric(factor(periodo))), espec)
  message("Bootstrap: ", espec)
  boot <- map_dfr(seq_len(n_boot), function(r) {
    s <- muestra_bloques()
    db <- s %>% inner_join(d0, by = "periodo", relationship = "many-to-many")
    b <- estima(db, espec); if (is.null(b)) NULL else mutate(b, rep = r)
  })
  boot %>% group_by(grupo) %>%
    summarise(ee_boot = sd(eta, na.rm = TRUE), repeticiones = sum(!is.na(eta)), .groups = "drop") %>%
    right_join(real, by = "grupo") %>%
    # Intervalo y valor p con el error del bootstrap, centrados en eta: con pocos
    # bloques, los percentiles del bootstrap pueden quedar corridos
    mutate(li90 = eta - 1.645 * ee_boot, ls90 = eta + 1.645 * ee_boot,
           p_boot = 2 * (1 - pnorm(abs(eta / ee_boot))), especificacion = espec)
})

# Errores agrupados por NICO de la misma regresion, para comparar
ee_nico <- map_dfr(c("niveles", "tendencia"), function(espec) {
  m <- fepois(formula_esp(espec), data = d0 %>% mutate(periodo_b = as.numeric(factor(periodo))), cluster = ~fraccion)
  ct <- coeftable(m); f <- rownames(ct)[grepl(":capex_q$", rownames(ct))]
  tibble(grupo = str_match(f, "grupo::(.*):")[, 2], ee_nico = ct[f, 2], p_nico = ct[f, 4], especificacion = espec)
})
res <- res %>% left_join(ee_nico, by = c("grupo", "especificacion")) %>%
  select(especificacion, grupo, eta, ee_nico, p_nico, ee_boot, li90, ls90, p_boot, repeticiones)

write_excel_csv(res, file.path(dir_out, "01_eta_bootstrap_bloques.csv"))
cat("\neta por nivel: errores agrupados por NICO vs bootstrap por bloques de trimestres\n")
print(as.data.frame(res %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
