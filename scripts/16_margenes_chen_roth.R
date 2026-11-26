# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/16_margenes_chen_roth.R
# Objetivo:       Paso 2 con ceros: forma funcional (Chen y Roth, 2024) y margenes extensivo e
#                 intensivo, en frecuencia trimestral y mensual.
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
#   - Prueba de la version anterior: si el resultado depende de como se tratan
#     los ceros. Opcional.
#   - Sus resultados van al anexo 5 de la presentacion.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Chen y Roth (QJE 2024, "Logs with zeros?"): con ceros, el coeficiente de
#   log(1 + y) depende de las unidades de y (USD, miles, millones), porque las
#   unidades fijan cuanto pesa pasar de 0 a positivo. Proponen: (i) efecto en
#   porcentaje de la media (Poisson / PPML, la base del script 07), (ii) fijar
#   explicitamente cuanto vale el margen extensivo, (iii) estimar los dos
#   margenes por separado. Aqui se corre todo eso para tres variables fijas:
#     capex_q    : ln capex de hiperescaladores, trimestral (la base)
#     mc_hyper_q : ln valuacion en bolsa de hiperescaladores, rezago 1, trimestral
#     mc_hyper_m : ln valuacion en bolsa de hiperescaladores, promedio de 3 meses
#                  previos, mensual (misma variable que la anterior, otra frecuencia)
#   Resultados (cada uno en niveles y con tendencia propia por nivel):
#     ppml               efecto en % de la media (referencia)
#     log1p_usd / _miles / _millones  log(1 + y) con y en tres unidades: si el
#                        coeficiente cambia mucho, el resultado es un artefacto
#     extensivo          probabilidad de importar (y > 0), sin ponderar
#     valor_extensivo_x  m(y) = log(y / media positiva del NICO) si y > 0 y -x si
#                        y = 0: pasar de 0 a un valor tipico vale x puntos log
#                        (x = 0.1, 1 y 3); version propia en el espiritu de (ii)
#     intensivo          log(y) solo con y > 0 (descriptivo: hay seleccion)
#   Requiere: salidas de 01 (datos/empresas) y 02 (output/panel)
#   Salidas : output/margenes/
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
dir_out   <- file.path(dir_base, "output", "margenes")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

origen    <- "USA"
unidad    <- "nico"
valores_x <- c(0.1, 1, 3)        # valor del margen extensivo, en puntos log
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

# Diagnostico de ceros: cuantos hay y cuantas entradas y salidas por nivel
ceros <- function(d, frecuencia) d %>% arrange(fraccion, periodo) %>% group_by(fraccion) %>%
  mutate(entra = positivo == 1 & lag(positivo) == 0, sale = positivo == 0 & lag(positivo) == 1) %>%
  group_by(grupo) %>%
  summarise(frecuencia, unidades = n_distinct(fraccion), pct_ceros = 100 * mean(positivo == 0),
            entradas_por_100 = 100 * mean(entra, na.rm = TRUE), salidas_por_100 = 100 * mean(sale, na.rm = TRUE),
            pct_valor_en_unidades_con_ceros = 100 * sum(valor_usd[fraccion %in% fraccion[positivo == 0]]) / sum(valor_usd),
            .groups = "drop")
diag_ceros <- bind_rows(ceros(panel_q, "trimestral"), ceros(panel_m, "mensual"))

# =============================================================================
# 4. Estimacion
# =============================================================================
resultados <- c("ppml", "log1p_usd", "log1p_miles", "log1p_millones", "extensivo",
                paste0("valor_extensivo_", valores_x), "intensivo")

corre <- function(panel, vars_df, var, frecuencia, resultado, espec) {
  d <- panel %>% inner_join(vars_df, by = "periodo") %>% filter(!is.na(.data[[var]]))
  if (startsWith(resultado, "valor_extensivo_")) {
    x <- as.numeric(sub("valor_extensivo_", "", resultado))
    d <- d %>% mutate(y = if_else(valor_usd > 0, rel, -x))
  } else if (resultado == "extensivo") {
    d <- d %>% mutate(y = positivo)
  } else if (resultado == "intensivo") {
    d <- d %>% filter(valor_usd > 0) %>% mutate(y = ln_pos)
  } else if (resultado != "ppml") {
    d <- d %>% mutate(y = .data[[resultado]])
  }
  d <- d %>% mutate(t_num = as.numeric(factor(periodo)))
  tend <- if (espec == "tendencia") " + i(grupo, t_num, ref = ref_resto)" else ""
  rhs <- paste0("i(grupo, ", var, ", ref = ref_resto)", tend, " | fraccion + periodo")
  m <- tryCatch({
    if (resultado == "ppml") fepois(as.formula(paste("valor_usd ~", rhs)), data = d, panel.id = ~fraccion + t_num)
    else if (resultado == "extensivo") feols(as.formula(paste("y ~", rhs)), data = d, panel.id = ~fraccion + t_num)
    else feols(as.formula(paste("y ~", rhs)), data = d, weights = ~peso, panel.id = ~fraccion + t_num)
  }, error = function(e) { message("  falla ", var, " / ", resultado, " / ", espec, ": ", conditionMessage(e)); NULL })
  if (is.null(m)) return(NULL)
  saca <- function(ct) {
    f <- rownames(ct)[grepl(paste0(":", var, "$"), rownames(ct))]
    tibble(termino = f, eta = ct[f, 1], ee = ct[f, 2], p = ct[f, 4])
  }
  cl <- saca(coeftable(summary(m, cluster = ~fraccion)))
  dk <- tryCatch(saca(coeftable(summary(m, vcov = "DK"))), error = function(e) NULL)
  if (!is.null(dk)) cl <- cl %>% left_join(dk %>% select(termino, ee_dk = ee, p_dk = p), by = "termino") else
    cl <- cl %>% mutate(ee_dk = NA_real_, p_dk = NA_real_)
  base_pos <- d %>% group_by(grupo) %>% summarise(prob_positivo = mean(positivo), .groups = "drop")
  cl %>% mutate(variable = var, frecuencia, resultado, especificacion = espec,
                grupo = str_match(termino, "grupo::(.*):")[, 2], n_obs = nobs(m)) %>%
    left_join(base_pos, by = "grupo") %>%
    transmute(variable, frecuencia, resultado, especificacion, grupo, eta,
              ee = pmax(ee, ee_dk, na.rm = TRUE), p = pmax(p, p_dk, na.rm = TRUE),
              ee_unidad = ee, ee_dk, prob_positivo, n_obs)
}

casos <- list(list(panel_q, capex_q, "capex_q", "trimestral"),
              list(panel_q, mc_q, "mc_hyper_q", "trimestral"),
              list(panel_m, mc_m, "mc_hyper_m", "mensual"))
res <- map_dfr(casos, function(cs) map_dfr(resultados, function(rr) map_dfr(c("niveles", "tendencia"), function(e) {
  message(cs[[3]], " / ", rr, " / ", e); corre(cs[[1]], cs[[2]], cs[[3]], cs[[4]], rr, e) })))

estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen <- res %>% filter(grupo == "1_nucleo") %>%
  mutate(celda = sprintf("%.3f%s (%.3f)", eta, estrellas(p), ee)) %>%
  select(variable, resultado, especificacion, celda) %>%
  pivot_wider(names_from = c(variable, especificacion), values_from = celda) %>%
  arrange(match(resultado, resultados))

write_excel_csv(diag_ceros, file.path(dir_out, "01_ceros_por_nivel.csv"))
write_excel_csv(res, file.path(dir_out, "02_eta_por_resultado.csv"))
write_excel_csv(resumen, file.path(dir_out, "03_resumen_nucleo.csv"))

cat("\nCeros por nivel y frecuencia:\n")
print(diag_ceros %>% mutate(across(where(is.numeric), ~ round(.x, 1))), n = Inf)
cat("\neta del nucleo por resultado (ee y p: el mayor entre agrupado por NICO y Driscoll-Kraay)\n",
    "extensivo: cambio en la probabilidad de importar por cada punto log de la variable\n")
print(as.data.frame(resumen), right = FALSE, row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
