# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/21_confirma_pedidos_computadoras.R
# Objetivo:       Confirmacion de las variables que pasaron la regla en el script 20, cada una
#                 con el origen donde paso (importaciones desde EUA o desde todos los paises),
#                 contra el capex en esos mismos origenes.
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
#   - Prueba complementaria: confirma las variables que pasaron en el 20.
#     Opcional.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Por que hace falta: pasaron con tendencia por nivel, pero en 2022 a 2026 solo
#   2% de su variacion no es tendencia, el coeficiente con tendencia (2.5 a 3.5)
#   es mucho mayor que en niveles (~1) y la version mensual no lo confirma.
#   Pruebas fijas (nucleo, PPML, con tendencia propia por nivel salvo que diga):
#     1. Reproducir niveles y tendencia
#     2. Bootstrap por bloques de 4 trimestres (la informacion son 19 trimestres)
#     3. Placebo: grupos de NICO al azar del mismo tamano que el nucleo
#     4. Quitar un trimestre a la vez: si un solo trimestre mueve el resultado
#     5. Rezagos y adelanto: la variable deberia ir ANTES que las importaciones
#        (efecto en t y t-1, t-2; nada en t+1)
#     5b. Carrera: adelanto y rezago en la misma regresion. Si la variable se
#        mueve lento, uno por uno salen iguales; juntos se ve cual gana
#     7. Cambios anuales (panel en cambios de 4 trimestres por NICO, MCO pesado
#        por el valor promedio de la NICO, efectos de periodo y de grupo): quita
#        el ciclo lento y prueba si los giros de la variable coinciden con los
#        giros de las importaciones. Contemporaneo y con un trimestre de rezago
#     6. Mensual con los pedidos de los 3 meses previos
#     7. Paso 1 con esta variable: valuacion en bolsa -> pedidos (cambios anuales,
#        Newey-West), para saber si la cadena funcionaria en los escenarios
#   Grafica: nucleo frente al resto y pedidos, ambos sin tendencia, 2022 a 2026.
#   Requiere: 19 (datos/alternativas), 01 (datos/empresas) y 02 (output/panel)
#   Salidas : output/confirmacion/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "fixest", "sandwich", "ggplot2")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_alt   <- file.path(dir_base, "datos", "alternativas")
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_out   <- file.path(dir_base, "output", "confirmacion")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# Variables a confirmar: las que pasaron la regla en el 20 (output/alternativas/
# 03_resumen.csv, diseno uniforme, nucleo), cada una con el origen donde paso
# (muestras eua_* = "USA", todos_* = "todos"), mas el capex en cada origen como
# referencia. Si no existe ese archivo, la lista fija de abajo.
pares <- tibble(variable = c("pedidos_computadoras", "pedidos_comp_electronicas"), origen = "USA")
archivo_14 <- file.path(dir_base, "output", "alternativas", "03_resumen.csv")
if (file.exists(archivo_14)) {
  pares <- read_csv(archivo_14, show_col_types = FALSE, locale = locale(encoding = "UTF-8")) %>%
    filter(diseno == "A_uniforme", grupo == "1_nucleo", sirve) %>%
    transmute(variable, origen = if_else(str_starts(muestra, "eua_"), "USA", "todos")) %>% distinct()
}
pares <- pares %>% bind_rows(tibble(variable = "capex", origen = unique(pares$origen))) %>% distinct() %>%
  arrange(origen, variable)
message("A confirmar (variable / origen): ", paste(pares$variable, pares$origen, sep = " / ", collapse = "; "))
variables <- unique(pares$variable)
origenes  <- unique(pares$origen)
n_boot     <- 299
n_placebo  <- 200
largo_blq  <- 4
ref_resto  <- "4_resto_electronicos"
set.seed(20261118)

lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Canasta, variables y panel (igual que el 20, muestra con todas las claves)
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

vars_m <- lee_csv(file.path(dir_alt, "variables_mensuales.csv")) %>% mutate(mes = as.Date(mes))
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
vars_q <- vars_q %>% full_join(capex, by = "trimestre") %>% arrange(trimestre) %>%
  mutate(across(any_of(variables), ~ if_else(.x > 0, log(.x), NA_real_)))
vars_m <- vars_m %>% arrange(mes) %>%
  mutate(across(any_of(variables), ~ if_else(.x > 0, log(.x), NA_real_)))
variables <- intersect(variables, names(vars_q))
pares     <- pares %>% filter(variable %in% variables)

pfm <- lee_csv(file.path(dir_panel, "panel_fraccion_pais_mes.csv"),
               col_types = cols(fraccion = col_character(), nico = col_character(), pais = col_character(), .default = col_guess())) %>%
  filter(tipo_operacion == "importacion") %>%
  mutate(mes = as.Date(mes), unidad = paste0(str_pad(fraccion, 8, pad = "0"), str_pad(nico, 2, pad = "0")))
meses <- sort(unique(pfm$mes))
trim_ok <- tibble(mes = meses) %>% count(trimestre = floor_date(mes, "quarter")) %>% filter(n == 3) %>% pull(trimestre)

arma <- function(frecuencia, o) {
  per <- if (frecuencia == "trimestral") trim_ok else meses
  pfm %>% filter(o == "todos" | pais == o) %>% mutate(periodo = if (frecuencia == "trimestral") floor_date(mes, "quarter") else mes) %>%
    filter(periodo %in% per) %>%
    group_by(fraccion = unidad, periodo) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
    complete(fraccion, periodo = per, fill = list(valor_usd = 0)) %>%
    mutate(grupo = clasifica_ia(fraccion)) %>%
    group_by(fraccion) %>% filter(mean(valor_usd) > 0) %>% ungroup() %>%
    mutate(t_real = as.numeric(factor(periodo)), periodo_b = as.numeric(factor(periodo)))
}
paneles_q <- setNames(lapply(origenes, function(o) arma("trimestral", o)), origenes)
paneles_m <- setNames(lapply(origenes, function(o) arma("mensual", o)), origenes)

# =============================================================================
# 2. Estimacion
# =============================================================================
fml <- function(regs, tend) as.formula(paste0(
  "valor_usd ~ ", paste0("i(grupo, ", regs, ", ref = ref_resto)", collapse = " + "),
  if (tend) " + i(grupo, t_real, ref = ref_resto)" else "", " | fraccion + periodo_b"))
eta_nucleo <- function(d, regs = "x", tend = TRUE, con_ee = TRUE) {
  m <- tryCatch(fepois(fml(regs, tend), data = d, notes = FALSE, warn = FALSE,
                       cluster = if (con_ee) ~fraccion else NULL), error = function(e) NULL)
  if (is.null(m)) return(tibble(regresor = regs, eta = NA_real_, ee = NA_real_, p = NA_real_))
  ct <- coeftable(m); f <- paste0("grupo::1_nucleo:", regs)
  if (!all(f %in% rownames(ct))) return(tibble(regresor = regs, eta = NA_real_, ee = NA_real_, p = NA_real_))
  tibble(regresor = regs, eta = ct[f, 1], ee = if (con_ee) ct[f, 2] else NA_real_, p = if (con_ee) ct[f, 4] else NA_real_)
}
# Cambios anuales: dlog del valor por NICO (t contra t-4, solo con valor > 0 en
# ambos) contra el cambio anual de la variable (contemporaneo o rezagado)
eta_cambios <- function(d, v, desfase = 0) {
  dx <- vars_q %>% arrange(trimestre) %>%
    transmute(periodo = trimestre, dx = .data[[v]] - lag(.data[[v]], 4)) %>%
    mutate(dx = lag(dx, desfase))
  dd <- d %>% arrange(fraccion, periodo) %>% group_by(fraccion) %>%
    mutate(peso = mean(valor_usd), v4 = lag(valor_usd, 4), p4 = lag(periodo, 4)) %>% ungroup() %>%
    filter(valor_usd > 0, v4 > 0, !is.na(p4)) %>%
    mutate(dlv = log(valor_usd) - log(v4)) %>%
    inner_join(dx, by = "periodo") %>% filter(!is.na(dx))
  m <- tryCatch(feols(dlv ~ i(grupo, dx, ref = ref_resto) | periodo + grupo, data = dd,
                      weights = ~peso, cluster = ~fraccion, notes = FALSE, warn = FALSE), error = function(e) NULL)
  f <- "grupo::1_nucleo:dx"
  if (is.null(m) || !f %in% rownames(coeftable(m))) return(tibble(regresor = "dx", eta = NA_real_, ee = NA_real_, p = NA_real_))
  ct <- coeftable(m)
  tibble(regresor = "dx", eta = ct[f, 1], ee = ct[f, 2], p = ct[f, 4])
}
con_x <- function(d, v, desfase = 0, frecuencia = "trimestral") {
  vdf <- if (frecuencia == "trimestral") vars_q %>% transmute(periodo = trimestre, x0 = .data[[v]]) else
    vars_m %>% transmute(periodo = mes, x0 = .data[[v]])
  vdf <- vdf %>% arrange(periodo) %>% mutate(x = if (desfase >= 0) lag(x0, desfase) else lead(x0, -desfase))
  d %>% left_join(vdf %>% select(periodo, x), by = "periodo") %>% filter(!is.na(x))
}

res <- list()
for (i in seq_len(nrow(pares))) {
  v <- pares$variable[i]; o <- pares$origen[i]
  panel_q <- paneles_q[[o]]; panel_m <- paneles_m[[o]]
  message("Variable: ", v, " | origen: ", o)
  dq <- con_x(panel_q, v)
  # 1. Reproducir
  r1 <- bind_rows(eta_nucleo(dq, tend = FALSE) %>% mutate(prueba = "1_niveles"),
                  eta_nucleo(dq, tend = TRUE) %>% mutate(prueba = "1_tendencia"))
  # 2. Bootstrap por bloques de trimestres
  per <- sort(unique(dq$periodo)); T_ <- length(per)
  boot <- map_dbl(seq_len(n_boot), function(r) {
    ini <- sample(seq_len(T_ - largo_blq + 1), ceiling(T_ / largo_blq), replace = TRUE)
    idx <- unlist(lapply(ini, function(i) i:(i + largo_blq - 1)))[seq_len(T_)]
    db <- tibble(periodo = per[idx], periodo_b = seq_along(idx)) %>%
      inner_join(dq %>% select(-periodo_b), by = "periodo", relationship = "many-to-many")
    eta_nucleo(db, tend = TRUE, con_ee = FALSE)$eta
  })
  ee_b <- sd(boot, na.rm = TRUE); e1 <- r1$eta[r1$prueba == "1_tendencia"]
  r2 <- tibble(prueba = "2_bootstrap_bloques", regresor = "x", eta = e1, ee = ee_b, p = 2 * (1 - pnorm(abs(e1 / ee_b))))
  # 3. Placebo: etiquetas de grupo al azar entre NICO (mismos tamanos)
  unidades <- dq %>% distinct(fraccion, grupo)
  plac <- map_dbl(seq_len(n_placebo), function(r) {
    u <- unidades %>% mutate(grupo = sample(grupo))
    eta_nucleo(dq %>% select(-grupo) %>% left_join(u, by = "fraccion"), tend = TRUE, con_ee = FALSE)$eta
  })
  r3 <- tibble(prueba = "3_placebo", regresor = "x", eta = e1, ee = sd(plac, na.rm = TRUE),
               p = mean(abs(plac) >= abs(e1), na.rm = TRUE))
  # 4. Quitar un trimestre a la vez
  r4 <- map_dfr(per, function(q) eta_nucleo(dq %>% filter(periodo != q), tend = TRUE) %>% mutate(sin_trimestre = q)) %>%
    mutate(prueba = "4_sin_un_trimestre")
  # 5. Rezagos y adelanto (uno por regresion)
  r5 <- map_dfr(c(-1, 0, 1, 2), function(k) eta_nucleo(con_x(panel_q, v, k), tend = TRUE) %>%
                  mutate(prueba = paste0("5_", if (k < 0) "adelanto_1" else paste0("rezago_", k))))
  # 5b. Carrera: adelanto y rezago juntos
  vlr <- vars_q %>% arrange(trimestre) %>%
    transmute(periodo = trimestre, x_ad = lead(.data[[v]]), x_r1 = lag(.data[[v]]))
  dlr <- panel_q %>% left_join(vlr, by = "periodo") %>% filter(!is.na(x_ad), !is.na(x_r1))
  r5b <- eta_nucleo(dlr, regs = c("x_ad", "x_r1"), tend = TRUE) %>%
    mutate(prueba = if_else(regresor == "x_ad", "5b_carrera_adelanto_1", "5b_carrera_rezago_1"))
  # 7. Cambios anuales
  r7 <- bind_rows(eta_cambios(panel_q, v, 0) %>% mutate(prueba = "7_cambios_anuales"),
                  eta_cambios(panel_q, v, 1) %>% mutate(prueba = "7_cambios_anuales_rezago_1"))
  # 6. Mensual, promedio de los 3 meses previos (solo para variables mensuales)
  r6 <- if (v %in% names(vars_m)) {
    vm <- vars_m %>% transmute(periodo = mes, x = (lag(.data[[v]]) + lag(.data[[v]], 2) + lag(.data[[v]], 3)) / 3)
    dm <- panel_m %>% left_join(vm, by = "periodo") %>% filter(!is.na(x))
    bind_rows(eta_nucleo(dm, tend = FALSE) %>% mutate(prueba = "6_mensual_niveles"),
              eta_nucleo(dm, tend = TRUE) %>% mutate(prueba = "6_mensual_tendencia"))
  } else NULL
  res[[paste(v, o)]] <- bind_rows(r1, r2, r3, r4, r5, r5b, r6, r7) %>% mutate(variable = v, origen = o)
}
res <- bind_rows(res) %>%
  mutate(serie = paste0(variable, if_else(origen == "USA", " [EUA]", " [todos]")))

# 7. Paso 1 con la variable: cambio anual de la variable contra cambio anual del
#    valuacion en bolsa de hiperescaladores (rezagos 1 y 2), Newey-West
indices <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
mc_q <- indices %>% filter(indicadora == "es_hyperscaler") %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>% group_by(trimestre) %>%
  summarise(mc = log(mean(coalesce(idx_ponderado, idx_equiponderado))), .groups = "drop")
paso1 <- map_dfr(variables, function(v) {
  d <- vars_q %>% select(trimestre, y = all_of(v)) %>% inner_join(mc_q, by = "trimestre") %>% arrange(trimestre) %>%
    mutate(dy = y - lag(y, 4), dmc = mc - lag(mc, 4), dmc_l1 = lag(dmc), dmc_l2 = lag(dmc, 2)) %>% filter(!is.na(dy), !is.na(dmc_l2))
  if (nrow(d) < 20) return(NULL)
  m <- lm(dy ~ dmc_l1 + dmc_l2, data = d)
  V <- NeweyWest(m, lag = 4, prewhite = FALSE)
  b <- sum(coef(m)[c("dmc_l1", "dmc_l2")]); s <- sqrt(sum(V[c("dmc_l1", "dmc_l2"), c("dmc_l1", "dmc_l2")]))
  tibble(variable = v, theta_suma_rezagos = b, ee = s, p = 2 * (1 - pnorm(abs(b / s))),
         desde = min(d$trimestre), hasta = max(d$trimestre), n = nrow(d))
})

# =============================================================================
# 3. Resultados (se guardan antes de la grafica para no perderlos si algo falla)
# =============================================================================
estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen <- res %>% filter(prueba != "4_sin_un_trimestre") %>%
  mutate(celda = sprintf("%.2f%s (%.2f)", eta, estrellas(p), ee)) %>%
  select(serie, prueba, celda) %>% pivot_wider(names_from = serie, values_from = celda)
sin_uno <- res %>% filter(prueba == "4_sin_un_trimestre") %>% group_by(variable, origen) %>%
  summarise(eta_min = min(eta, na.rm = TRUE), eta_max = max(eta, na.rm = TRUE),
            trimestre_que_mas_mueve = sin_trimestre[which.max(abs(eta - median(eta, na.rm = TRUE)))],
            veces_significativo_10 = sum(p < 0.10, na.rm = TRUE), trimestres = n(), .groups = "drop")

write_excel_csv(res, file.path(dir_out, "01_pruebas.csv"))
write_excel_csv(resumen, file.path(dir_out, "03_resumen.csv"))
write_excel_csv(sin_uno, file.path(dir_out, "04_sin_un_trimestre.csv"))
write_excel_csv(paso1, file.path(dir_out, "05_paso1_variable.csv"))

cat("\neta del nucleo (*** 99%, ** 95%, * 90% de confianza). Placebo y bootstrap: p de esa prueba\n")
print(as.data.frame(resumen), right = FALSE, row.names = FALSE)
cat("\nQuitando un trimestre a la vez (con tendencia):\n"); print(as.data.frame(sin_uno), row.names = FALSE)
cat("\nPaso 1 con la variable (cambio anual contra cambio anual de la valuacion en bolsa, suma de rezagos 1 y 2):\n")
print(as.data.frame(paso1 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)

# =============================================================================
# 4. Grafica: nucleo frente al resto y cada variable, sin tendencia (un panel por variable)
# =============================================================================
# na.exclude: si a una serie le falta un trimestre (pasa con algunos indices de
# precios de FRED), los residuales conservan el largo y el hueco queda como NA
sin_tend <- function(x) {
  if (sum(!is.na(x)) < 3) return(rep(NA_real_, length(x)))
  t <- seq_along(x)
  as.numeric(scale(resid(lm(x ~ t, na.action = na.exclude))))
}
etq_nucleo <- "Importaciones del n\u00facleo frente al resto"
grafica <- tryCatch({
  graf <- map_dfr(seq_len(nrow(pares)), function(i) {
    v <- pares$variable[i]; o <- pares$origen[i]
    paneles_q[[o]] %>% group_by(periodo, nucleo = grupo == "1_nucleo", resto = grupo == ref_resto) %>%
      summarise(val = sum(valor_usd), .groups = "drop") %>%
      summarise(.by = periodo, r = log(sum(val[nucleo])) - log(sum(val[resto]))) %>%
      left_join(vars_q %>% transmute(periodo = trimestre, x = .data[[v]]), by = "periodo") %>%
      arrange(periodo) %>% mutate(across(c(r, x), sin_tend),
                                  panel = paste0(v, if_else(o == "USA", " [EUA]", " [todos]")))
  }) %>%
    pivot_longer(c(r, x), names_to = "serie", values_to = "valor") %>%
    filter(!is.na(valor)) %>%
    mutate(serie = factor(if_else(serie == "r", etq_nucleo, "Variable"), levels = c(etq_nucleo, "Variable")),
           panel = factor(panel, levels = unique(panel)))
  g <- ggplot(graf, aes(periodo, valor, color = serie)) + geom_hline(yintercept = 0, color = "grey70") +
    geom_line(linewidth = 0.7) + geom_point(size = 1.2) +
    facet_wrap(~ panel, ncol = 3) +
    scale_color_manual(values = c("#16191C", "#992246"), labels = c(etq_nucleo, "Variable del panel")) +
    labs(x = NULL, y = "Desviaci\u00f3n de la tendencia (desviaciones est\u00e1ndar)", color = NULL,
         title = "Lo que queda despu\u00e9s de quitar la tendencia, 2022 a 2026") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  alto <- 7 * ceiling(nrow(pares) / 3) + 4
  ggsave(file.path(dir_out, "02_sin_tendencia.png"), g, width = 28, height = alto, units = "cm", dpi = 300, bg = "white")
  TRUE
}, error = function(e) { message("La grafica no se pudo hacer (los CSV ya se guardaron): ", conditionMessage(e)); FALSE })

message("Listo. Archivos en: ", dir_out)
