# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/20_pruebas_variables_alternativas.R
# Objetivo:       Paso 2 con variables distintas al capex (script 19), contra el capex como
#                 referencia. Importaciones desde EUA por NICO (ANAM).
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
#   - Prueba complementaria: el paso 2 con otras variables en lugar del gasto
#     de capital. Opcional.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Tres disenos, con una lista y una regla FIJAS de antemano:
#     A. Uniforme: la misma variable para los tres niveles (como el capex hoy).
#     B. Emparejado: cada nivel con su propia variable. El efecto fijo de periodo
#        se lleva lo comun, y eta sale de que cada nivel se mueve con su propia
#        demanda. Emparejamientos:
#          eua_trimestral : nucleo <- inv_computadoras_real; infraestructura <-
#                           construccion_centros_datos; mixta <- pedidos_electronicos
#          eua_mensual    : nucleo <- pedidos_computadoras; infraestructura <-
#                           construccion_centros_datos; mixta <- pedidos_electronicos
#          mexico         : nucleo <- fbcf_maq_importada; infraestructura <-
#                           construccion_no_residencial; mixta <- consumo_bienes_importados
#     C. Muestras: todas las importaciones; solo definitivas (A1 y C1: lo que se
#        consume o invierte en Mexico); solo definitivas de bienes finales (sin
#        partes ni componentes, aproximacion a la clasificacion por uso final).
#        Cada una desde EUA y desde todos los origenes (China, Taiwan, Corea...).
#   Especificaciones: niveles (PPML, efectos fijos de NICO y periodo) y con
#   tendencia propia por nivel. En B tambien con el tipo de cambio como control.
#   Regla: una variable sirve si eta es positiva y significativa al 17% en niveles
#   Y con tendencia (valor p mas alto entre agrupado por NICO y Driscoll-Kraay).
#   Diagnostico previo: cuanta variacion tiene cada variable fuera de la tendencia
#   en los anios del panel (si casi toda es tendencia, no puede identificar).
#   Nota sobre Mexico: la inversion en maquinaria importada y el consumo de bienes
#   importados contienen a estas mismas importaciones; un eta alto ahi puede ser
#   en parte mecanico.
#   Requiere: 19 (datos/alternativas), 01 (datos/empresas) y 02 (output/panel)
#   Salidas : output/alternativas/
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
dir_alt   <- file.path(dir_base, "datos", "alternativas")
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "alternativas")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Origenes: "USA" (base) y "todos" (todas las importaciones, incluidas China,
# Taiwan y Corea: los precios de semiconductores y memoria son globales). Se puede agregar
# un pais por clave ANAM, por ejemplo "CHN".
origenes  <- c("USA", "todos")
nivel_sig <- 0.10
correr_mensual <- TRUE      # FALSE para una corrida rapida solo trimestral
ref_resto <- "4_resto_electronicos"

# Bienes intermedios (se quitan en la muestra de bienes finales): partes y
# componentes, por partida (4 digitos) o subpartida (6), mas toda fraccion
# cuya descripcion empiece con "Partes". Aproximacion a la clasificacion BEC.
hs4_intermedios <- c("8473", "8503", "8505", "8522", "8529", "8532", "8533", "8534", "8536",
                     "8538", "8540", "8541", "8542", "8547", "8548")
hs6_intermedios <- c("850490", "851771", "851779", "841590", "841891", "841899", "841990")

emparejamientos <- list(
  eua_trimestral = c("1_nucleo" = "inv_computadoras_real", "2_infraestructura" = "construccion_centros_datos",
                     "3_mixta" = "pedidos_electronicos"),
  eua_mensual    = c("1_nucleo" = "pedidos_computadoras", "2_infraestructura" = "construccion_centros_datos",
                     "3_mixta" = "pedidos_electronicos"),
  mexico         = c("1_nucleo" = "fbcf_maq_importada", "2_infraestructura" = "construccion_no_residencial",
                     "3_mixta" = "consumo_bienes_importados"))

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
  tibble(fraccion = character(), nivel = character())
clasifica_ia <- function(fr) {
  f8 <- str_sub(fr, 1, 8); sp <- str_sub(fr, 1, 6)
  coalesce(if (any(nchar(fr) == 10)) ajustes$nivel[match(fr, ajustes$fraccion)] else NA_character_,
           ajustes$nivel[match(f8, ajustes$fraccion)],
           canasta_def$nivel[match(sp, canasta_def$subpartida)], ref_resto)
}

# =============================================================================
# 2. Variables: las del script 19 y el capex como referencia
# =============================================================================
vars_m <- lee_csv(file.path(dir_alt, "variables_mensuales.csv")) %>% mutate(mes = as.Date(mes)) %>% rename(periodo = mes)
vars_q <- lee_csv(file.path(dir_alt, "variables_trimestrales.csv")) %>% mutate(trimestre = as.Date(trimestre)) %>%
  rename(periodo = trimestre)

# Capex de hiperescaladores encadenado (igual que el 07)
empresas  <- lee_csv(file.path(dir_emp, "empresas_ia.csv"))
capex_emp <- lee_csv(file.path(dir_emp, "capex_trimestral_empresa.csv")) %>% mutate(trimestre = as.Date(trimestre))
hyper <- empresas$empresa[empresas$es_hyperscaler]
t0 <- capex_emp %>% filter(empresa %in% hyper) %>% group_by(empresa) %>% summarise(t = min(trimestre)) %>% pull(t) %>% max()
capex <- capex_emp %>% filter(empresa %in% hyper, trimestre >= t0, capex_musd > 0) %>%
  arrange(empresa, trimestre) %>% group_by(empresa) %>%
  mutate(previo = lag(capex_musd), continua = !is.na(previo) & lag(trimestre) == trimestre %m-% months(3)) %>%
  group_by(trimestre) %>%
  summarise(total = sum(capex_musd),
            crec = if (any(continua)) sum(capex_musd[continua]) / sum(previo[continua]) else NA_real_, .groups = "drop") %>%
  arrange(trimestre) %>%
  mutate(crec = if_else(row_number() == 1 | is.na(crec), 1, crec),
         capex = cumprod(crec) / last(cumprod(crec)) * last(total)) %>%
  transmute(periodo = trimestre, capex)
vars_q <- vars_q %>% full_join(capex, by = "periodo") %>% arrange(periodo)

nombres_q <- setdiff(names(vars_q), "periodo")
nombres_m <- setdiff(names(vars_m), "periodo")
# En logaritmos (elasticidades)
vars_q <- vars_q %>% mutate(across(all_of(nombres_q), ~ if_else(.x > 0, log(.x), NA_real_)))
vars_m <- vars_m %>% mutate(across(all_of(nombres_m), ~ if_else(.x > 0, log(.x), NA_real_)))

# =============================================================================
# 3. Panel de ANAM por muestra
# =============================================================================
pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(), pais = col_character(),
                                .default = col_guess())) %>%
  filter(tipo_operacion == "importacion")
for (cc in c("desc_fraccion", "desc_nico")) if (!cc %in% names(pfm)) pfm[[cc]] <- NA_character_
pfm <- pfm %>%
  mutate(mes = as.Date(mes), fraccion = str_pad(fraccion, 8, pad = "0"),
         unidad = paste0(fraccion, str_pad(nico, 2, pad = "0")),
         desc = str_trim(tolower(paste(coalesce(as.character(desc_fraccion), ""), coalesce(as.character(desc_nico), "")))),
         intermedio = str_sub(fraccion, 1, 4) %in% hs4_intermedios | str_sub(fraccion, 1, 6) %in% hs6_intermedios |
           str_detect(desc, "^partes"))
meses <- sort(unique(pfm$mes))
trim_ok <- tibble(mes = meses) %>% count(trimestre = floor_date(mes, "quarter")) %>% filter(n == 3) %>% pull(trimestre)

arma_panel <- function(d, frecuencia) {
  d <- d %>% mutate(periodo = if (frecuencia == "trimestral") floor_date(mes, "quarter") else mes)
  if (frecuencia == "trimestral") d <- d %>% filter(periodo %in% trim_ok)
  per <- sort(unique(if (frecuencia == "trimestral") trim_ok else meses))
  d %>% group_by(fraccion = unidad, periodo) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
    complete(fraccion, periodo = per, fill = list(valor_usd = 0)) %>%
    mutate(grupo = clasifica_ia(fraccion)) %>%
    group_by(fraccion) %>% mutate(peso = mean(valor_usd)) %>% ungroup() %>% filter(peso > 0) %>%
    mutate(t_num = as.numeric(factor(periodo)))
}
muestras <- list()
for (o in origenes) {
  base_o <- if (o == "todos") pfm else pfm %>% filter(pais == o)
  etq_o <- if (o == "USA") "eua" else tolower(o)
  muestras[[paste0(etq_o, "_todas")]] <- base_o
  muestras[[paste0(etq_o, "_definitivas")]] <- base_o %>% filter(regimen == "definitiva")
  muestras[[paste0(etq_o, "_definitivas_finales")]] <- base_o %>% filter(regimen == "definitiva", !intermedio)
}
rm(base_o)
cat("\nValor 12 meses por muestra (millones de USD):\n")
print(map_dfr(muestras, ~ tibble(valor = sum(.x$valor_usd[.x$mes > max(meses) %m-% months(12)]) / 1e6), .id = "muestra"))

# =============================================================================
# 4. Diagnostico: variacion fuera de la tendencia en los anios del panel
# =============================================================================
diag_var <- function(vdf, periodos, frecuencia) {
  v <- vdf %>% filter(periodo %in% periodos)
  map_dfr(setdiff(names(v), "periodo"), function(n) {
    x <- v[[n]]; ok <- !is.na(x); t <- seq_along(x)
    if (sum(ok) < 6) return(tibble(frecuencia, variable = n, n_periodos = sum(ok)))
    f <- lm(x[ok] ~ t[ok])
    tibble(frecuencia, variable = n, n_periodos = sum(ok),
           fuera_de_tendencia_pct = 100 * (1 - summary(f)$r.squared),
           correlacion_con_capex = if ("capex" %in% names(v)) suppressWarnings(cor(x, v$capex, use = "pairwise")) else NA_real_)
  })
}
diagnostico <- bind_rows(diag_var(vars_q, trim_ok, "trimestral"),
                         if (correr_mensual) diag_var(vars_m, meses, "mensual"))

# =============================================================================
# 5. Estimacion
# =============================================================================
# d trae la columna x (regresor). espec: niveles, tendencia, niveles_tc, tendencia_tc
estima <- function(d, espec, ctrl_tc = NULL) {
  d <- d %>% filter(!is.na(x))
  if (!is.null(ctrl_tc)) d <- d %>% left_join(ctrl_tc, by = "periodo") %>% filter(!is.na(tc))
  if (nrow(d) == 0 || n_distinct(d$periodo) < 8) return(NULL)
  d <- d %>% mutate(t_num = as.numeric(factor(periodo)))
  rhs <- "i(grupo, x, ref = ref_resto)"
  if (str_detect(espec, "tendencia")) rhs <- paste(rhs, "+ i(grupo, t_num, ref = ref_resto)")
  if (str_detect(espec, "_tc")) rhs <- paste(rhs, "+ i(grupo, tc, ref = ref_resto)")
  m <- tryCatch(fepois(as.formula(paste("valor_usd ~", rhs, "| fraccion + periodo")), data = d,
                       panel.id = ~fraccion + t_num, notes = FALSE),
                error = function(e) { message("  falla: ", conditionMessage(e)); NULL })
  if (is.null(m)) return(NULL)
  saca <- function(ct) { f <- rownames(ct)[grepl(":x$", rownames(ct))]
    tibble(grupo = str_match(f, "grupo::(.*):")[, 2], eta = ct[f, 1], ee = ct[f, 2], p = ct[f, 4]) }
  cl <- saca(coeftable(summary(m, cluster = ~fraccion)))
  dk <- tryCatch(saca(coeftable(summary(m, vcov = "DK"))), error = function(e) NULL)
  if (!is.null(dk)) cl <- cl %>% left_join(dk %>% select(grupo, ee_dk = ee, p_dk = p), by = "grupo") else
    cl <- cl %>% mutate(ee_dk = NA_real_, p_dk = NA_real_)
  cl %>% transmute(grupo, eta, ee = pmax(ee, ee_dk, na.rm = TRUE), p = pmax(p, p_dk, na.rm = TRUE),
                   n_obs = nobs(m), n_periodos = n_distinct(d$periodo))
}

tc_q <- if ("tipo_cambio" %in% names(vars_q)) vars_q %>% transmute(periodo, tc = tipo_cambio) else NULL
tc_m <- if ("tipo_cambio" %in% names(vars_m)) vars_m %>% transmute(periodo, tc = tipo_cambio) else NULL

frecuencias <- c("trimestral", if (correr_mensual) "mensual")
paneles <- list()
for (fq in frecuencias) for (mu in names(muestras)) paneles[[paste(fq, mu)]] <- arma_panel(muestras[[mu]], fq)

# A. Uniforme
res_a <- map_dfr(frecuencias, function(fq) {
  vdf <- if (fq == "trimestral") vars_q else vars_m
  noms <- setdiff(names(vdf), "periodo")
  if (fq == "mensual") noms <- setdiff(noms, "capex")
  map_dfr(names(muestras), function(mu) map_dfr(noms, function(v) map_dfr(c("niveles", "tendencia"), function(e) {
    message("A ", fq, " / ", mu, " / ", v, " / ", e)
    d <- paneles[[paste(fq, mu)]] %>% left_join(vdf %>% select(periodo, x = all_of(v)), by = "periodo")
    estima(d, e) %>% mutate(diseno = "A_uniforme", frecuencia = fq, muestra = mu, variable = v, especificacion = e)
  })))
})

# B. Emparejado
res_b <- map_dfr(names(emparejamientos), function(nm) {
  emp <- emparejamientos[[nm]]
  fq <- if (nm == "eua_mensual") "mensual" else "trimestral"
  if (fq == "mensual" && !correr_mensual) return(NULL)
  vdf <- if (fq == "trimestral") vars_q else vars_m
  if (!all(emp %in% names(vdf))) { message("B ", nm, ": faltan variables (", paste(setdiff(emp, names(vdf)), collapse = ", "), ")"); return(NULL) }
  largo_x <- map_dfr(names(emp), function(g) vdf %>% transmute(periodo, grupo = g, x = .data[[emp[[g]]]]))
  tc <- if (fq == "trimestral") tc_q else tc_m
  map_dfr(names(muestras), function(mu) map_dfr(c("niveles", "tendencia", if (!is.null(tc)) c("niveles_tc", "tendencia_tc")), function(e) {
    message("B ", nm, " / ", mu, " / ", e)
    d <- paneles[[paste(fq, mu)]] %>% left_join(largo_x, by = c("periodo", "grupo")) %>%
      mutate(x = if_else(grupo == ref_resto, 0, x))
    estima(d, e, if (str_detect(e, "_tc")) tc else NULL) %>%
      mutate(diseno = "B_emparejado", frecuencia = fq, muestra = mu, variable = nm, especificacion = e,
             regresor = unname(emp[grupo]))
  }))
})

res <- bind_rows(res_a, res_b) %>% select(diseno, frecuencia, muestra, variable, especificacion, everything())

# =============================================================================
# 6. Resumen y regla
# =============================================================================
estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < nivel_sig ~ "*", TRUE ~ "")
regla <- res %>% filter(especificacion %in% c("niveles", "tendencia")) %>%
  group_by(diseno, frecuencia, muestra, variable, grupo) %>%
  summarise(sirve = n() == 2 && all(eta > 0 & p < nivel_sig), .groups = "drop")
resumen <- res %>% filter(grupo == "1_nucleo" | diseno == "B_emparejado") %>%
  mutate(celda = sprintf("%.2f%s (%.2f)", eta, estrellas(p), ee)) %>%
  select(diseno, frecuencia, muestra, variable, grupo, especificacion, celda) %>%
  pivot_wider(names_from = especificacion, values_from = celda) %>%
  left_join(regla, by = c("diseno", "frecuencia", "muestra", "variable", "grupo")) %>%
  arrange(diseno, desc(frecuencia), muestra, variable, grupo)

write_excel_csv(diagnostico, file.path(dir_out, "01_diagnostico_variables.csv"))
write_excel_csv(res, file.path(dir_out, "02_resultados.csv"))
write_excel_csv(resumen, file.path(dir_out, "03_resumen.csv"))

cat("\nVariacion fuera de la tendencia (% de la varianza en los anios del panel):\n")
print(as.data.frame(diagnostico %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE)
cat("\neta (*** 99%, ** 95%, * 90% de confianza; ee: el mayor entre agrupado por NICO y Driscoll-Kraay)\n")
cat("A: nucleo con la misma variable para los tres niveles. B: cada nivel con su variable.\n")
print(as.data.frame(resumen), right = FALSE, row.names = FALSE)
cat("\nPasan la regla:\n")
print(as.data.frame(regla %>% filter(sirve)), row.names = FALSE)
message("Listo. Archivos en: ", dir_out)
