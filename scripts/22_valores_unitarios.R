# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/22_valores_unitarios.R
# Objetivo:       Separa el valor importado en precio (valor unitario) y cantidad, por nivel de
#                 la canasta y por origen (EUA y todos), para saber si lo que se mueve con los
#                 indices de precios de FRED es precio o cantidad.
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
#   - Prueba complementaria: separa el valor importado en precio y cantidad.
#     Opcional.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Que hace:
#     1. Valor unitario por NICO y trimestre (valor / cantidad en unidad de tarifa;
#        la unidad de tarifa es fija por fraccion, asi que se puede sumar entre
#        paises). Solo NICO que reportan cantidad en al menos 95% de sus celdas
#        con valor. Celdas con valor unitario 10 veces arriba o abajo de la mediana
#        de su NICO (o sin cantidad) se corrigen con cantidad = valor / mediana.
#     2. Indices encadenados de Tornqvist por nivel: precio y cantidad, con
#        valor = precio x cantidad (base primer trimestre = 100).
#     3. Descomposicion del cambio del valor: que parte de su varianza es precio
#        y que parte cantidad (cov(precio, valor) / var(valor); las dos suman 1),
#        por nivel y para el nucleo frente al resto.
#     4. Correlacion del precio, la cantidad y el valor del nucleo con los indices
#        de precios de FRED y con el capex, en cambios anuales.
#     5. eta del nucleo con dos resultados: valor y volumen a precios constantes
#        (cantidad x valor unitario promedio de la NICO). Si la variable mueve el
#        valor pero no el volumen, el efecto es precio.
#   Requiere: 02 (output/panel), 19 (datos/alternativas), 01 (datos/empresas)
#   Salidas : output/valores_unitarios/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "fixest", "ggplot2")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_alt   <- file.path(dir_base, "datos", "alternativas")
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "valores_unitarios")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

origenes      <- c("USA", "todos")
variables_eta <- c("capex", "precio_export_computadoras", "precio_export_computo")
variables_fred <- c("precio_export_computadoras", "precio_export_computo", "precio_import_computo",
                    "ppi_semiconductores")
min_reporte   <- 0.95   # NICO que reportan cantidad en al menos esta parte de sus celdas con valor
umbral_corr   <- 10     # valor unitario mas de 10 veces arriba o abajo de la mediana de la NICO
ref_resto     <- "4_resto_electronicos"

lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Canasta, variables y panel
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

# Variables (logaritmos) y capex de hiperescaladores encadenado (igual que el 21)
vars_q <- lee_csv(file.path(dir_alt, "variables_trimestrales.csv")) %>% mutate(trimestre = as.Date(trimestre))
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
  mutate(crec = if_else(row_number() == 1 | is.na(crec), 1, crec), capex = cumprod(crec) / last(cumprod(crec)) * last(total)) %>%
  select(trimestre, capex)
usadas <- unique(c(variables_eta, variables_fred))
vars_q <- vars_q %>% select(-any_of("capex")) %>% full_join(capex, by = "trimestre") %>% arrange(trimestre) %>%
  mutate(across(any_of(usadas), ~ if_else(.x > 0, log(.x), NA_real_)))
faltan_v <- setdiff(usadas, names(vars_q))
if (length(faltan_v)) message("No estan en variables_trimestrales.csv (se omiten): ", paste(faltan_v, collapse = ", "))
variables_eta  <- intersect(variables_eta, names(vars_q))
variables_fred <- intersect(variables_fred, names(vars_q))

pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(), pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion") %>%
  mutate(mes = as.Date(mes), unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")),
         cantidad = coalesce(as.numeric(cantidad), 0))
meses   <- sort(unique(pfm$mes))
trim_ok <- tibble(mes = meses) %>% count(trimestre = floor_date(mes, "quarter")) %>% filter(n == 3) %>% pull(trimestre)

arma_uv <- function(o) {
  pfm %>% filter(o == "todos" | pais == o) %>%
    mutate(periodo = floor_date(mes, "quarter")) %>% filter(periodo %in% trim_ok) %>%
    group_by(fraccion = unidad, periodo) %>%
    summarise(valor_usd = sum(valor_usd), cantidad = sum(cantidad), .groups = "drop") %>%
    complete(fraccion, periodo = trim_ok, fill = list(valor_usd = 0, cantidad = 0)) %>%
    group_by(fraccion) %>%
    mutate(reporta = mean(cantidad[valor_usd > 0] > 0)) %>%
    filter(sum(valor_usd) > 0) %>%
    mutate(uv = if_else(valor_usd > 0 & cantidad > 0, valor_usd / cantidad, NA_real_),
           uv_med = median(uv, na.rm = TRUE)) %>% ungroup() %>%
    mutate(grupo = clasifica_ia(fraccion),
           atipico  = !is.na(uv) & abs(log(uv / uv_med)) > log(umbral_corr),
           sin_cant = valor_usd > 0 & cantidad <= 0,
           cantidad_c = if_else(atipico | sin_cant, valor_usd / uv_med, cantidad),
           uv_c = if_else(valor_usd > 0 & cantidad_c > 0, valor_usd / cantidad_c, NA_real_))
}
paneles <- setNames(lapply(origenes, arma_uv), origenes)

# Cobertura: que parte del valor queda en NICO que reportan cantidad
cobertura <- imap_dfr(paneles, function(d, o) d %>%
  mutate(entra = reporta >= min_reporte & !is.na(uv_med)) %>%
  group_by(origen = o, grupo) %>%
  summarise(nico = n_distinct(fraccion), nico_entran = n_distinct(fraccion[entra]),
            pct_valor_entra = 100 * sum(valor_usd[entra]) / sum(valor_usd),
            pct_celdas_corregidas = 100 * sum((atipico | sin_cant)[entra]) / sum((valor_usd > 0)[entra]),
            .groups = "drop"))
paneles <- lapply(paneles, function(d) d %>% filter(reporta >= min_reporte, !is.na(uv_med)) %>%
  group_by(fraccion) %>% mutate(uv_base = sum(valor_usd) / sum(cantidad_c)) %>% ungroup() %>%
  mutate(volumen = cantidad_c * uv_base,
         t_real = as.numeric(factor(periodo)), periodo_b = t_real))

# =============================================================================
# 2. Indices de Tornqvist encadenados por nivel
# =============================================================================
tornqvist <- function(d) {
  per <- sort(unique(d$periodo))
  map_dfr(seq_along(per)[-1], function(k) {
    a <- d %>% filter(periodo == per[k - 1]); b <- d %>% filter(periodo == per[k])
    par <- inner_join(a %>% filter(!is.na(uv_c)) %>% select(fraccion, v0 = valor_usd, u0 = uv_c),
                      b %>% filter(!is.na(uv_c)) %>% select(fraccion, v1 = valor_usd, u1 = uv_c), by = "fraccion")
    dlp <- if (nrow(par)) with(par, sum(0.5 * (v0 / sum(v0) + v1 / sum(v1)) * log(u1 / u0))) else NA_real_
    tibble(periodo = per[k], dlv = log(sum(b$valor_usd)) - log(sum(a$valor_usd)), dlp = dlp,
           pct_valor_emparejado = 100 * sum(par$v1) / sum(b$valor_usd))
  }) %>%
    mutate(dlq = dlv - dlp) %>%
    bind_rows(tibble(periodo = per[1], dlv = 0, dlp = 0, dlq = 0), .) %>%
    mutate(ln_valor = cumsum(dlv), ln_precio = cumsum(coalesce(dlp, 0)), ln_cantidad = ln_valor - ln_precio)
}
indices <- imap_dfr(paneles, function(d, o) d %>% group_by(grupo) %>% group_modify(~ tornqvist(.x)) %>%
  ungroup() %>% mutate(origen = o))

# =============================================================================
# 3. Descomposicion del cambio del valor (precio contra cantidad)
# =============================================================================
rel <- indices %>% filter(grupo %in% c("1_nucleo", ref_resto)) %>%
  select(origen, grupo, periodo, ln_valor, ln_precio, ln_cantidad) %>%
  pivot_wider(names_from = grupo, values_from = c(ln_valor, ln_precio, ln_cantidad)) %>%
  transmute(origen, periodo, grupo = "nucleo_frente_al_resto",
            ln_valor = .data[["ln_valor_1_nucleo"]] - .data[[paste0("ln_valor_", ref_resto)]],
            ln_precio = .data[["ln_precio_1_nucleo"]] - .data[[paste0("ln_precio_", ref_resto)]],
            ln_cantidad = .data[["ln_cantidad_1_nucleo"]] - .data[[paste0("ln_cantidad_", ref_resto)]])
series <- bind_rows(indices %>% select(origen, grupo, periodo, ln_valor, ln_precio, ln_cantidad), rel)
descomp <- series %>% group_by(origen, grupo) %>% arrange(periodo, .by_group = TRUE) %>%
  summarise(across(c(ln_valor, ln_precio, ln_cantidad), list(d1 = ~ list(.x - lag(.x)), d4 = ~ list(.x - lag(.x, 4)))),
            .groups = "drop") %>%
  pivot_longer(-c(origen, grupo), names_to = c("serie", "cambio"), names_pattern = "ln_(.*)_(d[14])") %>%
  pivot_wider(names_from = serie, values_from = value) %>%
  mutate(res = pmap(list(valor, precio, cantidad), function(v, p, q) {
    ok <- !is.na(v) & !is.na(p)
    tibble(n = sum(ok), parte_precio = cov(p[ok], v[ok]) / var(v[ok]), parte_cantidad = cov(q[ok], v[ok]) / var(v[ok]),
           de_valor = sd(v[ok]), de_precio = sd(p[ok]), de_cantidad = sd(q[ok]))
  })) %>% select(origen, grupo, cambio, res) %>% unnest(res) %>%
  mutate(cambio = recode(cambio, d1 = "trimestral", d4 = "anual")) %>% arrange(origen, cambio, grupo)

# =============================================================================
# 4. Correlacion con los indices de FRED y el capex (cambios anuales)
# =============================================================================
d4 <- function(x) x - lag(x, 4)
fred_d4 <- vars_q %>% arrange(trimestre) %>% transmute(periodo = trimestre, across(all_of(c(variables_fred, "capex")), d4))
correl <- series %>% filter(grupo %in% c("1_nucleo", "nucleo_frente_al_resto")) %>%
  group_by(origen, grupo) %>% arrange(periodo, .by_group = TRUE) %>%
  mutate(across(c(ln_valor, ln_precio, ln_cantidad), d4)) %>% ungroup() %>%
  left_join(fred_d4, by = "periodo") %>%
  pivot_longer(all_of(c(variables_fred, "capex")), names_to = "variable", values_to = "dx") %>%
  group_by(origen, grupo, variable) %>%
  summarise(n = sum(!is.na(dx) & !is.na(ln_valor)),
            cor_valor = cor(dx, ln_valor, use = "complete.obs"),
            cor_precio = cor(dx, ln_precio, use = "complete.obs"),
            cor_cantidad = cor(dx, ln_cantidad, use = "complete.obs"), .groups = "drop")

# =============================================================================
# 5. eta del nucleo: valor contra volumen a precios constantes
# =============================================================================
fml <- function(y, tend) as.formula(paste0(y, " ~ i(grupo, x, ref = ref_resto)",
                                           if (tend) " + i(grupo, t_real, ref = ref_resto)" else "", " | fraccion + periodo_b"))
eta <- function(d, y, tend) {
  m <- tryCatch(fepois(fml(y, tend), data = d, cluster = ~fraccion, notes = FALSE, warn = FALSE), error = function(e) NULL)
  f <- "grupo::1_nucleo:x"
  if (is.null(m) || !f %in% rownames(coeftable(m))) return(tibble(eta = NA_real_, ee = NA_real_, p = NA_real_))
  ct <- coeftable(m); tibble(eta = ct[f, 1], ee = ct[f, 2], p = ct[f, 4])
}
grilla <- expand_grid(origen = origenes, variable = variables_eta, resultado = c("valor_usd", "volumen"),
                      especificacion = c("niveles", "tendencia"))
res_eta <- bind_cols(grilla, pmap_dfr(grilla, function(origen, variable, resultado, especificacion) {
  xv <- vars_q %>% select(periodo = trimestre, x = all_of(variable))
  d <- paneles[[origen]] %>% left_join(xv, by = "periodo") %>% filter(!is.na(x))
  eta(d, resultado, especificacion == "tendencia")
}))

estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen_eta <- res_eta %>%
  mutate(celda = sprintf("%.2f%s (%.2f)", eta, estrellas(p), ee),
         columna = paste(if_else(resultado == "valor_usd", "valor", "volumen"), especificacion, sep = "_")) %>%
  select(origen, variable, columna, celda) %>% pivot_wider(names_from = columna, values_from = celda) %>%
  left_join(res_eta %>% select(origen, variable, resultado, especificacion, eta) %>%
              pivot_wider(names_from = resultado, values_from = eta) %>%
              transmute(origen, variable, especificacion, parte_precio = valor_usd - volumen) %>%
              pivot_wider(names_from = especificacion, values_from = parte_precio, names_prefix = "parte_precio_"),
            by = c("origen", "variable"))

# ---- Guardar e imprimir ------------------------------------------------------
write_excel_csv(cobertura, file.path(dir_out, "01_cobertura.csv"))
write_excel_csv(indices, file.path(dir_out, "02_indices_tornqvist.csv"))
write_excel_csv(descomp, file.path(dir_out, "03_descomposicion.csv"))
write_excel_csv(correl, file.path(dir_out, "04_correlacion_fred.csv"))
write_excel_csv(res_eta, file.path(dir_out, "05_eta_valor_volumen.csv"))
write_excel_csv(resumen_eta, file.path(dir_out, "06_resumen_eta.csv"))

redondea <- function(x) mutate(x, across(where(is.numeric), ~ round(.x, 3)))
cat("\nCobertura (NICO que reportan cantidad):\n"); print(as.data.frame(redondea(cobertura)), row.names = FALSE)
cat("\nQue parte de la varianza del cambio del valor es precio y que parte cantidad:\n")
print(as.data.frame(redondea(descomp)), row.names = FALSE)
cat("\nCorrelacion con FRED y capex (cambios anuales):\n"); print(as.data.frame(redondea(correl)), row.names = FALSE)
cat("\neta del nucleo con valor y con volumen (*** 99%, ** 95%, * 90% de confianza):\n")
print(as.data.frame(resumen_eta %>% mutate(across(where(is.numeric), ~ round(.x, 2)))), row.names = FALSE, right = FALSE)

# =============================================================================
# 6. Grafica: valor, precio y cantidad del nucleo y precio del resto
# =============================================================================
grafica <- tryCatch({
  etq <- c(valor = "Valor del n\u00facleo", precio = "Precio del n\u00facleo",
           cantidad = "Cantidad del n\u00facleo", precio_resto = "Precio del resto")
  gd <- indices %>% filter(grupo %in% c("1_nucleo", ref_resto)) %>%
    transmute(origen = if_else(origen == "USA", "Desde EUA", "Todos los or\u00edgenes"), periodo, grupo,
              valor = 100 * exp(ln_valor), precio = 100 * exp(ln_precio), cantidad = 100 * exp(ln_cantidad)) %>%
    pivot_longer(c(valor, precio, cantidad), names_to = "serie", values_to = "indice") %>%
    filter(grupo == "1_nucleo" | serie == "precio") %>%
    mutate(serie = if_else(grupo == ref_resto, "precio_resto", serie),
           serie = factor(etq[serie], levels = etq))
  g <- ggplot(gd, aes(periodo, indice, color = serie, linetype = serie)) +
    geom_hline(yintercept = 100, color = "grey75") + geom_line(linewidth = 0.8) +
    facet_wrap(~ origen, ncol = 2, scales = "free_y") +
    scale_color_manual(values = c("#16191C", "#992246", "#A37D2E", "#98979A")) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "dashed")) +
    labs(x = NULL, y = "\u00cdndice (primer trimestre = 100)", color = NULL, linetype = NULL,
         title = "Importaciones del n\u00facleo: valor, precio y cantidad (Tornqvist encadenado)") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(dir_out, "07_valor_precio_cantidad.png"), g, width = 28, height = 13, units = "cm", dpi = 300, bg = "white")
  TRUE
}, error = function(e) { message("La grafica no se pudo hacer (los CSV ya se guardaron): ", conditionMessage(e)); FALSE })

message("Listo. Archivos en: ", dir_out)
