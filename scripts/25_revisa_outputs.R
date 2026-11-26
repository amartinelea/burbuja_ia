# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/25_revisa_outputs.R
# Objetivo:       Revision rapida de las salidas de todos los scripts del modelo. Imprime en consola lo
#                 necesario para verificar que los resultados tienen sentido y guarda todo en
#                 output/revision_outputs.txt (para compartirlo).
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
#   - Revision final: imprime y guarda en output/revision_outputs.txt las
#     cifras clave de todos los scripts.
#   - Es lo primero que hay que mandar si alguien quiere revisar una corrida.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
# _____________________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 200, pillar.sigfig = 4, max.print = 100000)
# Paquetes: instala los que falten y los carga
paquetes <- c("dplyr", "tidyr", "readr", "stringr", "lubridate", "zoo", "readxl")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

dir_base <- getwd()
ruta <- function(...) file.path(dir_base, ...)
lee <- function(...) {
  f <- ruta(...)
  if (!file.exists(f)) stop("no existe ", f)
  # fraccion y subpartida como texto (conservan ceros), el resto se adivina
  cc <- intersect(c("fraccion", "subpartida"), names(read_csv(f, n_max = 0, show_col_types = FALSE)))
  tipos <- do.call(cols, c(setNames(rep(list(col_character()), length(cc)), cc), list(.default = col_guess())))
  read_csv(f, show_col_types = FALSE, locale = locale(encoding = "UTF-8"), col_types = tipos)
}
# Cada seccion corre aparte: si falta un archivo, avisa y sigue con la siguiente
seccion <- function(x, codigo) {
  titulo(x)
  tryCatch(codigo, error = function(e) cat("  [omitida] ", conditionMessage(e), "\n"))
}
titulo <- function(x) cat("\n", strrep("=", 90), "\n", x, "\n", strrep("=", 90), "\n", sep = "")
muestra <- function(d, n = 30) if (!is.null(d)) print(as.data.frame(head(d, n)), row.names = FALSE)
redondea <- function(d) if (is.null(d)) NULL else mutate(d, across(where(is.numeric), ~ round(.x, 3)))

archivo_log <- ruta("output", "revision_outputs.txt")
dir.create(dirname(archivo_log), recursive = TRUE, showWarnings = FALSE)
while (sink.number() > 0) sink()   # por si una corrida anterior quedo a medias
sink(archivo_log, split = TRUE)
cat("Revision de outputs:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")

# =============================================================================
# Script 05: panel de ANAM
# =============================================================================
seccion("02 | Diagnostico del panel", {
  muestra(lee("output", "panel", "00_diagnostico_panel.csv"))
})

seccion("02 | Valor por clave de pedimento (flujo_fisico = TRUE entra al modelo)", {
  muestra(redondea(lee("output", "panel", "01_valor_por_clave.csv")))
})

seccion("02 | Valor anual (millones USD) para comparar con la balanza de INEGI", {
  muestra(redondea(lee("output", "panel", "02_valor_anual_para_validar.csv") %>%
                     arrange(regimen, anio)))
})

seccion("02 | Panel fraccion x trimestre con EUA: cobertura y ceros", {
  eua <- lee("output", "panel", "panel_eua_trimestre.csv")
  {
    muestra(eua %>% group_by(regimen) %>%
              summarise(fracciones = n_distinct(fraccion),
                        nico = if ("fraccion_nico" %in% names(eua)) n_distinct(fraccion_nico) else NA_integer_,
                        trimestres = n_distinct(trimestre),
                        pct_ceros = round(100 * mean(valor_usd == 0), 1),
                        valor_total_musd = round(sum(valor_usd) / 1e6, 1), .groups = "drop"))
    muestra(eua %>% group_by(trimestre) %>%
              summarise(valor_musd = round(sum(valor_usd) / 1e6, 1), .groups = "drop"), 40)
  }
})

# =============================================================================
# Script 01: empresas y macro
# =============================================================================
seccion("01 | Capex de hiperescaladores por trimestre (millones USD, ultimos 10)", {
  capex <- lee("datos", "empresas", "capex_trimestral_empresa.csv")
  emp   <- lee("datos", "empresas", "empresas_ia.csv")
  {
    hyp <- emp$empresa[as.logical(emp$es_hyperscaler)]
    muestra(capex %>% filter(empresa %in% hyp) %>%
              select(empresa, trimestre, capex_musd) %>%
              pivot_wider(names_from = empresa, values_from = capex_musd) %>%
              arrange(trimestre) %>% tail(10) %>% redondea())
  }
})

seccion("01 | Saltos sospechosos de capex (trimestre > 1.8x o < 0.55x la mediana de +/- 2 trimestres)", {
  # Un trimestre muy por encima de sus vecinos suele ser un error de
  # desacumulacion (sumar dos trimestres o mezclar etiquetas), no inversion real
  capex <- lee("datos", "empresas", "capex_trimestral_empresa.csv")
  saltos <- capex %>% arrange(empresa, trimestre) %>% group_by(empresa) %>%
    mutate(mediana_vecinos = zoo::rollapply(capex_musd, 5, function(x) median(x[-3]),
                                            fill = NA, align = "center"),
           razon = capex_musd / mediana_vecinos) %>%
    ungroup() %>% filter(razon > 1.8 | razon < 0.55, trimestre >= as.Date("2018-01-01"))
  cat("Trimestres marcados:", nrow(saltos), "\n")
  muestra(saltos %>% count(empresa, name = "trimestres_marcados") %>% arrange(desc(trimestres_marcados)))
  muestra(saltos %>% select(empresa, trimestre, capex_musd, mediana_vecinos, razon) %>% redondea(), 40)
})

seccion("01 | Diagnostico de capex (huecos por empresa)", {
  muestra(lee("datos", "empresas", "capex_diagnostico.csv") %>% filter(huecos > 0))
})

seccion("01 | Ratios del ultimo trimestre por empresa", {
  rat <- lee("datos", "empresas", "ratios_financieros_trimestral.csv")
  muestra(rat %>% group_by(empresa) %>% filter(!is.na(capex_ocf) | !is.na(deuda_ingresos)) %>%
                               slice_max(trimestre, n = 1) %>% ungroup() %>%
                               select(empresa, trimestre, capex_ocf, deuda_ingresos, deuda_ebitda,
                                      capex_ingresos, fcf_ingresos) %>% redondea())
})
seccion("01 | Trimestres con dato (de los ultimos 8) por partida", {
  muestra(lee("datos", "empresas", "ratios_diagnostico.csv"))
})

seccion("01 | Variables macro (primeros y ultimos trimestres)", {
  mac <- lee("datos", "empresas", "macro_trimestral.csv")
  { muestra(redondea(head(mac, 3))); muestra(redondea(tail(mac, 6))) }
})

seccion("01 | Indices de valuacion en bolsa por grupo (ultimos 6 meses, base ene 2023 = 100)", {
  idx <- lee("datos", "empresas", "mcap_indices_grupo.csv")
  muestra(idx %>% mutate(idx = coalesce(idx_ponderado, idx_equiponderado)) %>%
                               select(indicadora, mes, idx) %>%
                               pivot_wider(names_from = indicadora, values_from = idx) %>%
                               arrange(mes) %>% tail(6) %>% redondea())
})

# =============================================================================
# Script 03: clasificacion
# =============================================================================
seccion("03 | Clasificacion de la canasta (input/canasta_ia_fracciones.csv)", {
  cf <- lee("input", "canasta_ia_fracciones.csv")
  {
    muestra(cf %>% count(nivel, origen_clasificacion))
    cat("\nFracciones de la canasta con mayor valor (12 meses, millones USD):\n")
    muestra(cf %>% filter(nivel != "4_resto_electronicos") %>% arrange(desc(valor_12m_eua_musd)) %>%
              mutate(descripcion = str_trunc(coalesce(descripcion_nico, descripcion_fraccion), 60)) %>%
              select(fraccion_nico, nivel, eslabon, valor_12m_eua_musd, valor_12m_musd, descripcion) %>%
              head(15) %>% redondea())
    cat("\nResiduales 'Los demas' con mas valor desde EUA (hoja 3b_los_demas):\n")
    ld <- readxl::read_xlsx(ruta("output", "choque_ia", "equivalencia_clasificacion.xlsx"), sheet = "3b_los_demas")
    cat("Total:", nrow(ld), "NICO\n")
    muestra(ld %>% mutate(subpartida_desc = str_trunc(coalesce(descripcion_subpartida, ""), 45)) %>%
              select(fraccion_nico, residual_en, nivel, valor_12m_eua_musd, subpartida_desc) %>%
              head(25) %>% redondea(), 25)
  }

  # =============================================================================
  # Script 02: pasos 1, 2 y 3 (base)
  # =============================================================================
  d02 <- c("output", "choque_ia")
})
seccion("07 | Parametros de la corrida base", {
  par <- lee(d02[1], d02[2], "00_parametros.csv")
  print(t(redondea(par)), quote = FALSE)
})

seccion("07 | Coeficientes con intervalos de confianza (pasos 1 y 2, adelanto y forma reducida)", {
  muestra(redondea(lee(d02[1], d02[2], "00c_coeficientes_ic.csv") %>%
                     select(modelo, termino, estimado, ee, ic_inf, ic_sup, valor_p, n_obs, nivel_ic)), 60)
})

seccion("07 | Placebo por permutacion (eta real vs etiquetas al azar)", {
  muestra(redondea(lee(d02[1], d02[2], "01d_placebo_permutacion.csv")))
})

seccion("07 | Intervalos Monte Carlo del impacto (%; total y por grupo)", {
  muestra(lee(d02[1], d02[2], "04b_intervalos_impacto.csv") %>%
            mutate(across(starts_with("var_pct"), ~ round(100 * .x, 1)), prob_caida = round(prob_caida, 2)) %>%
            select(version_beta, escenario, nivel, var_pct_inf, var_pct_p50, var_pct_sup, prob_caida),
          80)
})

seccion("07 | Paso 1 con ratio (theta efectiva)", {
  muestra(redondea(lee(d02[1], d02[2], "00b_theta_panel_ratio.csv")))
})

seccion("07 | Paso 2: eta por grupo (relativo al resto de electronicos)", {
  muestra(redondea(lee(d02[1], d02[2], "01_betas_canasta.csv")))
})

seccion("07 | Regresion directa comercio vs valuacion en bolsa (comparar con theta x eta)", {
  muestra(redondea(lee(d02[1], d02[2], "01c_forma_reducida_mcap.csv")))
})

seccion("07 | Escenarios y cambio implicito del capex", {
  muestra(redondea(lee(d02[1], d02[2], "03_escenarios_supuestos.csv") %>%
                     select(escenario, mcap_hyper, mcap_dc, delta_tasa, capex_directo,
                            arrastre, mult_caida, var_capex)))
})

seccion("07 | Impacto total por escenario (valor en millones USD, recaudacion en millones MXN)", {
  muestra(redondea(lee(d02[1], d02[2], "04_resultado_total.csv") %>%
                     mutate(var_pct = 100 * var_pct) %>% arrange(version_beta, escenario)))
})

seccion("07 | Impacto por canasta, version relativa (%)", {
  rc <- lee(d02[1], d02[2], "05_resultado_por_canasta.csv")
  muestra(rc %>% filter(version_beta == "relativo") %>%
                              mutate(var_pct = round(100 * var_pct, 1)) %>%
                              select(canasta, escenario, var_pct) %>%
                              pivot_wider(names_from = escenario, values_from = var_pct))
})

seccion("07 | Impacto por eslabon, version relativa (%)", {
  re <- lee(d02[1], d02[2], "05b_resultado_por_eslabon.csv")
  muestra(re %>% filter(version_beta == "relativo") %>%
                              mutate(var_pct = round(100 * var_pct, 1)) %>%
                              select(eslabon, escenario, var_pct) %>%
                              pivot_wider(names_from = escenario, values_from = var_pct))
})

seccion("07 | 10 fracciones con mayor perdida en C (contraccion), version relativa", {
  rf <- lee(d02[1], d02[2], "06_resultado_por_fraccion.csv")
  muestra(rf %>% filter(version_beta == "relativo", str_starts(escenario, "C\\.")) %>%
                              arrange(delta_valor_mdp) %>%
                              select(fraccion, canasta, eslabon, valor_mdp, delta_valor_mdp) %>%
                              head(10) %>% redondea())

  # =============================================================================
  # Script 04: robustez
  # =============================================================================
  d04 <- c("output", "robustez")
})
seccion("08 | theta y gamma por especificacion", {
  muestra(redondea(lee(d04[1], d04[2], "robustez_theta_gamma.csv")), 40)
})

seccion("08 | eta del nucleo por especificacion", {
  rb <- lee(d04[1], d04[2], "robustez_betas.csv")
  muestra(rb %>% filter(grupo %in% c("1_nucleo", "servidores")) %>% arrange(especificacion != "base") %>%
                              select(especificacion, grupo, n_fracciones, beta_relativo, ee_relativo,
                                     beta_absoluto) %>% redondea(), 40)
})

seccion("08 | eta de los tres niveles en las especificaciones del paso 2", {
  rb <- lee(d04[1], d04[2], "robustez_betas.csv")
  esp <- c("base", "ols_log", "tendencia_grupo", "tendencia_grupo_ols", "cambios_4t",
           "continuos_ppml", "continuos_ols", "continuos_tend_ols", "continuos_4t", "todos_origenes")
  muestra(rb %>% filter(especificacion %in% esp, grupo != "4_resto_electronicos") %>%
            mutate(eta = sprintf("%.2f (%.2f)", beta_relativo, ee_relativo)) %>%
            select(especificacion, grupo, eta) %>%
            pivot_wider(names_from = grupo, values_from = eta) %>%
            arrange(match(especificacion, esp)), 20)
})

seccion("08 | Impacto total (%) por especificacion, version relativa", {
  ri <- lee(d04[1], d04[2], "robustez_impactos.csv")
  muestra(ri %>% filter(version_beta == "relativo") %>%
                              mutate(var_pct = round(100 * var_pct, 1)) %>%
                              select(especificacion, escenario, var_pct) %>%
                              pivot_wider(names_from = escenario, values_from = var_pct), 40)
})

seccion("08 | Regresion directa por especificacion (nucleo)", {
  rd <- lee(d04[1], d04[2], "robustez_forma_reducida.csv")
  muestra(rd %>% filter(grupo == "1_nucleo") %>%
                              select(especificacion, indice_mc, efecto_directo_mc, ee_directo,
                                     theta_x_eta, cociente_directo_vs_cadena) %>% redondea(), 40)
})

seccion("08 | Escenarios calibrados por fecha", {
  muestra(redondea(lee(d04[1], d04[2], "escenarios_calibrados.csv")))

  # =============================================================================
  # Script 06: sectores cercanos (exploratorio)
  # =============================================================================
  d06 <- c("output", "sectores_ia")
})
seccion("14 | Series usadas", {
  muestra(lee(d06[1], d06[2], "00_series_usadas.csv"))
})

seccion("14 | Respuesta acumulada a 4 y 8 trimestres por serie", {
  lp <- lee(d06[1], d06[2], "01_lp_por_serie.csv")
  muestra(lp %>% filter(h %in% c(0, 4, 8)) %>%
                              select(serie, cercano_ia, h, b, ee, n) %>% redondea(), 60)
})

seccion("14 | Cercanos menos resto", {
  muestra(redondea(lee(d06[1], d06[2], "02_lp_cercanos_vs_resto.csv")))
})

seccion("14 | Exposicion por entidad (subsector 334)", {
  muestra(redondea(lee(d06[1], d06[2], "03_exposicion_entidades.csv")), 15)
})

# =============================================================================
# Script 08: pruebas de identificacion del paso 2
# =============================================================================
d08 <- c("output", "identificacion")
seccion("15 | Variacion de cada variable fuera de la tendencia", {
  muestra(redondea(lee(d08[1], d08[2], "01_diagnostico_variables.csv")))
})

seccion("15 | eta del nucleo por variable y especificacion (regla: niveles y tendencia)", {
  r8 <- lee(d08[1], d08[2], "03_resumen_nucleo.csv")
  print(as.data.frame(r8), right = FALSE, row.names = FALSE)
})

seccion("15 | eta de los tres niveles con tendencia por grupo", {
  e8 <- lee(d08[1], d08[2], "02_eta_por_variable.csv")
  muestra(e8 %>% filter(especificacion == "tendencia") %>%
            mutate(eta = sprintf("%.2f (%.2f)", eta, pmax(ee_unidad, ee_dk, na.rm = TRUE))) %>%
            select(frecuencia, variable, regresor, grupo, eta) %>%
            pivot_wider(names_from = grupo, values_from = eta), 20)
})

# =============================================================================
# Script 09: forma funcional (Chen y Roth) y margenes extensivo e intensivo
# =============================================================================
d09 <- c("output", "margenes")
seccion("16 | Ceros, entradas y salidas por nivel y frecuencia", {
  muestra(redondea(lee(d09[1], d09[2], "01_ceros_por_nivel.csv")))
})
seccion("16 | eta del nucleo por forma funcional y margen", {
  r9 <- lee(d09[1], d09[2], "03_resumen_nucleo.csv")
  print(as.data.frame(r9), right = FALSE, row.names = FALSE)
})
seccion("16 | Margen extensivo de los tres niveles (cambio en la probabilidad de importar)", {
  e9 <- lee(d09[1], d09[2], "02_eta_por_resultado.csv")
  muestra(e9 %>% filter(resultado == "extensivo") %>%
            mutate(eta = sprintf("%.3f (%.3f) [p0 = %.2f]", eta, ee, prob_positivo)) %>%
            select(variable, especificacion, grupo, eta) %>%
            pivot_wider(names_from = grupo, values_from = eta), 20)
})

# =============================================================================
# Script 10: proyecciones locales mensuales
# =============================================================================
d10 <- c("output", "proyecciones_locales")
seccion("17 | Proyecciones locales: nivel frente al resto, por horizonte en meses (pre_k = antes del choque)", {
  r10 <- lee(d10[1], d10[2], "02_resumen.csv")
  print(as.data.frame(r10), right = FALSE, row.names = FALSE)
})
seccion("17 | Cuantos horizontes (0 a 12) son significativos al 10% en la base, segun la prueba", {
  l10 <- lee(d10[1], d10[2], "01_lp_relativo.csv")
  muestra(l10 %>% filter(especificacion == "base", h >= 0) %>% group_by(choque, grupo) %>%
            summarise(horizontes = n(), newey_west = sum(p_nw < 0.10), hc1 = sum(p_hc < 0.10),
                      wild_bootstrap = sum(p_boot < 0.10, na.rm = TRUE), permutacion = sum(p_perm < 0.10, na.rm = TRUE),
                      ambas = sum(p < 0.10, na.rm = TRUE), .groups = "drop"))
})

# =============================================================================
# Script 11: bootstrap por bloques de trimestres del paso 2
# =============================================================================
seccion("18 | eta por nivel: errores agrupados por NICO vs bootstrap por bloques de trimestres", {
  muestra(redondea(lee("output", "bootstrap_paso2", "01_eta_bootstrap_bloques.csv")))
})

# =============================================================================
# Scripts 13 y 14: variables alternativas al capex
# =============================================================================
seccion("19 | Variables obtenidas (fuente, periodo y estado)", {
  f13 <- lee("datos", "alternativas", "00_fuentes.csv")
  print(as.data.frame(f13 %>% select(nombre, fuente, clave, origen, desde, hasta, n, estado)), row.names = FALSE)
})
seccion("20 | Variacion fuera de la tendencia en los anios del panel", {
  muestra(redondea(lee("output", "alternativas", "01_diagnostico_variables.csv")), 40)
})
seccion("20 | eta por variable, muestra y especificacion (A: nucleo; B: cada nivel con su variable)", {
  r14 <- lee("output", "alternativas", "03_resumen.csv")
  print(as.data.frame(r14), right = FALSE, row.names = FALSE)
})
seccion("20 | Pasan la regla (positiva y significativa en niveles y con tendencia)", {
  r14 <- lee("output", "alternativas", "03_resumen.csv")
  print(as.data.frame(r14 %>% filter(sirve) %>% select(diseno, frecuencia, muestra, variable, grupo, niveles, tendencia)),
        right = FALSE, row.names = FALSE)
})

# =============================================================================
# Script 15: confirmacion de pedidos de computadoras
# =============================================================================
seccion("21 | Pruebas de confirmacion (eta del nucleo)", {
  r15 <- lee("output", "confirmacion", "03_resumen.csv")
  cols15 <- setdiff(names(r15), "prueba")
  # De 4 en 4 columnas para que no se corte la tabla
  for (k in split(cols15, ceiling(seq_along(cols15) / 4))) {
    print(as.data.frame(r15[, c("prueba", k)]), right = FALSE, row.names = FALSE); cat("\n")
  }
})
seccion("21 | Quitando un trimestre a la vez", {
  muestra(redondea(lee("output", "confirmacion", "04_sin_un_trimestre.csv")))
})
seccion("21 | Paso 1 con la variable (valuacion en bolsa -> variable)", {
  muestra(redondea(lee("output", "confirmacion", "05_paso1_variable.csv")))
})

# =============================================================================
# Script 16: valores unitarios (precio contra cantidad)
# =============================================================================
seccion("22 | Cobertura: NICO que reportan cantidad", {
  muestra(redondea(lee("output", "valores_unitarios", "01_cobertura.csv")))
})
seccion("22 | Que parte de la varianza del cambio del valor es precio y que parte cantidad", {
  muestra(redondea(lee("output", "valores_unitarios", "03_descomposicion.csv")), 40)
})
seccion("22 | Correlacion con los indices de FRED y el capex (cambios anuales)", {
  muestra(redondea(lee("output", "valores_unitarios", "04_correlacion_fred.csv")), 40)
})
seccion("22 | eta del nucleo con valor y con volumen a precios constantes", {
  print(as.data.frame(lee("output", "valores_unitarios", "06_resumen_eta.csv") %>%
                        mutate(across(where(is.numeric), ~ round(.x, 2)))), right = FALSE, row.names = FALSE)
})

# =============================================================================
# Script 17: demanda de EUA por producto (Census)
# =============================================================================
seccion("06 | Importaciones de Mexico por HS6 contra la demanda de EUA", {
  print(as.data.frame(lee("output", "census", "02_resumen.csv")), right = FALSE, row.names = FALSE)
})
seccion("06 | Paso 1: valuacion en bolsa -> importaciones de EUA del nucleo", {
  muestra(redondea(lee("output", "census", "03_paso1_nucleo_eua.csv")))
})
seccion("06 | Peso de Mexico y de Asia en lo que EUA importa del nucleo", {
  muestra(redondea(lee("output", "census", "04_peso_mexico_nucleo.csv")))
})

# =============================================================================
# Script 18: robustez del diseno por producto
# =============================================================================
seccion("09 | Robustez del diseno por producto (x = demanda de EUA; x:nucleo = diferencia del nucleo)", {
  print(as.data.frame(lee("output", "census_robustez", "02_resumen.csv")), right = FALSE, row.names = FALSE)
})

# =============================================================================
# Script 19: escenarios con la cadena nueva (paso 2 por producto)
# =============================================================================
seccion("10 | Parametros y paso 2a (EUA por nivel contra capex y contra valuacion en bolsa)", {
  muestra(redondea(lee("output", "choque_ia_census", "00_parametros.csv")))
  muestra(redondea(lee("output", "choque_ia_census", "01_elasticidades_eua.csv")))
})
seccion("10 | Impacto total por escenario", {
  muestra(redondea(lee("output", "choque_ia_census", "04_resultado_total.csv")), 40)
})
seccion("10 | Intervalos de 90% (version principal)", {
  muestra(redondea(lee("output", "choque_ia_census", "04b_intervalos_impacto.csv")), 60)
})
seccion("10 | Por nivel y por eslabon (version principal)", {
  muestra(redondea(lee("output", "choque_ia_census", "05_resultado_por_canasta.csv") %>% filter(version_beta == "principal")), 40)
  muestra(redondea(lee("output", "choque_ia_census", "05b_resultado_por_eslabon.csv") %>% filter(version_beta == "principal")), 60)
})

sink()
cat("\nRevision guardada en:", archivo_log, "\n")
