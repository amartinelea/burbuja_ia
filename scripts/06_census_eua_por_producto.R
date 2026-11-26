# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/06_census_eua_por_producto.R
# Objetivo:       Demanda de EUA producto por producto (Census, importaciones mensuales por HS6
#                 y pais) como variable para las importaciones de Mexico, en un panel de
#                 productos en vez de una sola serie agregada.
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
#   - Baja de Census lo que EUA importa de cada producto; de ahi sale la
#     demanda de EUA del modelo.
#   - Necesita CENSUS_API_KEY en el .env (ver README). Nunca poner la clave en
#     el script.
#   - Solo vuelve a bajar los ultimos 9 meses (refrescar_meses); lo demas lo
#     toma de datos/census/.
#   - Si la API no responde, correr 06b_diagnostico_census.R para elegir un
#     tamano de consulta mas chico.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Idea: con efectos fijos de producto y de periodo, la identificacion sale de
#   que productos crecen mas en EUA en cada momento (cientos de productos), no de
#   una sola serie de 19 trimestres. Variables por HS6 y mes:
#     dem_ex_mx      importaciones de EUA desde todos los paises menos Mexico
#                    (demanda de EUA por ese producto)
#     dem_fuera_asia lo mismo sin Mexico ni los principales proveedores de Asia
#                    (revisa que no sea un choque de oferta asiatico que mueva
#                    a la vez lo que compran EUA y Mexico)
#     exp_mx_eua     importaciones de EUA desde Mexico = exportaciones de Mexico a
#                    EUA por producto (adelanta parte del lado de exportaciones
#                    que se pediria a ANAM)
#   Regresiones (PPML, importaciones de Mexico de todos los origenes por HS6 y
#   trimestre, efectos fijos de HS6 y de trimestre, errores agrupados por HS6):
#     base, con tendencia propia de cada HS6, por nivel de la canasta, adelanto y
#     rezago (uno por uno y juntos), version mensual.
#     Tambien por eslabon (servidores, redes, semiconductores, almacenamiento, energia...).
#   Paso 1: valuacion en bolsa de hiperescaladores -> importaciones de EUA de los
#     productos del nucleo (cambios anuales, rezagos 1 y 2, Newey-West, 2013+).
#   Descarga: primero una consulta de prueba (30 s). Luego capitulos 84, 85 y 90
#     desde 2021 y, para el paso 1, solo las partidas del nucleo desde 2013. El
#     tamano de cada consulta (mes o trimestre, capitulo o partida, todos los
#     paises o uno por uno) se elige con 06b_diagnostico_census.R. Cada consulta
#     queda guardada en datos/census/raw: si se corta, retoma donde iba; despues
#     solo vuelve a bajar los ultimos 9 meses.
#
#   Clave de la API: el Census ya pide clave en todas las consultas. Pedirla en
#     https://api.census.gov/data/key_signup.html (gratis, llega por correo) y
#     agregar al .env del proyecto una linea: CENSUS_API_KEY=tu_clave
#     Nunca escribir la clave en el script.
#   Requiere: 02 (output/panel), 01 (datos/empresas), .env con CENSUS_API_KEY
#   Salidas : datos/census/ (descargas) y output/census/
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, width = 250)
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate", "fixest", "sandwich",
              "ggplot2", "curl", "jsonlite")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base  <- getwd()                                    # AJUSTAR si hace falta
dir_emp   <- file.path(dir_base, "datos", "empresas")
dir_panel <- file.path(dir_base, "output", "panel")
dir_cen   <- file.path(dir_base, "datos", "census")
dir_out   <- file.path(dir_base, "output", "census")
dir.create(file.path(dir_cen, "raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

anio_desde  <- 2013                 # historia larga (solo productos del nucleo) para el paso 1
anio_panel  <- 2021                 # capitulos completos desde este anio (el panel usa 2022 en adelante)
capitulos   <- c("84", "85", "90")
refrescar_meses <- 9                # se vuelven a bajar los ultimos 9 meses (revisiones y meses nuevos)
timeout_seg <- 150                  # espera maxima por consulta (el Census corta a los ~125 s)
intentos    <- 3                    # vueltas para reintentar las que fallen
conexiones  <- 2                    # consultas al mismo tiempo
# Tamano de cada consulta. Segun 06b_diagnostico_census.R (18 nov 2026): un
# capitulo, un mes y un pais tarda ~7 s; con rangos de meses o varios paises a
# la vez el Census se pasa de 125 s y corta (error 524). Por eso: mes, capitulo,
# un pais por consulta.
unidad_tiempo  <- "mes"             # "mes" o "trimestre"
nivel_filtro   <- "capitulo"        # "capitulo" (84*, 85*, 90*) o "hs4" (una consulta por partida)
por_pais       <- TRUE              # TRUE: una consulta por pais (mas consultas, cada una mas chica)
historia_paso1 <- TRUE              # FALSE: no baja 2013 a 2020 (sin paso 1 con datos del Census)
# FALSE: solo total y Mexico (~800 consultas, ~25 min con 4 conexiones). TRUE
# agrega 11 paises de Asia al panel (~2,300 consultas mas, ~1 h mas) para la
# prueba "demanda fuera de Asia". Se puede correr primero en FALSE y despues en
# TRUE: lo ya bajado no se vuelve a pedir.
incluir_asia   <- FALSE
# Paises (codigos del Census): "-" = total de todos los paises; 2010 = Mexico.
# Asia: China, Taiwan, Corea, Japon, Vietnam, Malasia, Tailandia, Singapur,
# Filipinas, India, Hong Kong. El script imprime los nombres que regresa la API
# para verificar los codigos.
cod_total  <- "-"
cod_mexico <- "2010"
cod_asia   <- if (incluir_asia) c("5700", "5830", "5800", "5880", "5520", "5570", "5490", "5590", "5650", "5330", "5820") else character(0)
ref_resto  <- "4_resto_electronicos"

readRenviron(file.path(dir_base, ".env"))
clave_census <- Sys.getenv("CENSUS_API_KEY")
lee_csv <- function(f, ...) read_csv(f, locale = locale(encoding = "UTF-8"), show_col_types = FALSE, ...)

# =============================================================================
# 1. Panel de Mexico por HS6 (todos los origenes), nivel y eslabon de la canasta
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
# Nivel de cada HS6: el de las NICO con mas valor dentro del HS6. Eslabon: el de
# la subpartida en la canasta (si el HS6 queda en el resto, eslabon = resto)
grupo_hs6 <- pfm %>% group_by(hs6, unidad) %>% summarise(v = sum(valor_usd), .groups = "drop") %>%
  mutate(grupo = clasifica_ia(unidad)) %>% group_by(hs6, grupo) %>% summarise(v = sum(v), .groups = "drop") %>%
  group_by(hs6) %>% slice_max(v, n = 1, with_ties = FALSE) %>% ungroup() %>% select(hs6, grupo) %>%
  mutate(eslabon = if ("eslabon" %in% names(canasta_def)) canasta_def$eslabon[match(hs6, canasta_def$subpartida)] else NA_character_,
         eslabon = if_else(grupo == ref_resto | is.na(eslabon), ref_resto, eslabon))
hs6_nucleo <- grupo_hs6$hs6[grupo_hs6$grupo == "1_nucleo"]
hs4_nucleo <- sort(unique(str_sub(hs6_nucleo, 1, 4)))
hs4_panel  <- sort(unique(str_sub(pfm$hs6, 1, 4)))
hs4_panel  <- hs4_panel[str_sub(hs4_panel, 1, 2) %in% capitulos]

mx_mes <- pfm %>% group_by(hs6, mes) %>% summarise(valor_usd = sum(valor_usd), .groups = "drop") %>%
  complete(hs6, mes = sort(unique(pfm$mes)), fill = list(valor_usd = 0))
meses_mx <- sort(unique(pfm$mes))

# =============================================================================
# 2. Descarga del Census (por trimestre, con cache: si se corta, retoma)
# =============================================================================
url_base <- "https://api.census.gov/data/timeseries/intltrade/imports/hs"
segundos <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
es_json  <- function(txt) startsWith(trimws(txt), "[")
texto    <- function(res) { x <- rawToChar(res$content); Encoding(x) <- "UTF-8"; x }
limpia   <- function(txt) substr(gsub("\\s+", " ", gsub("<[^>]+>", " ", txt)), 1, 150)

arma_url <- function(filtro, ini, fin, paises, campos = "I_COMMODITY,CTY_CODE,CTY_NAME,GEN_VAL_MO") {
  # Un solo mes va como time=AAAA-MM (los rangos from/to son mucho mas lentos en el Census)
  tiempo <- if (ini == fin) format(ini, "%Y-%m") else sprintf("from+%s+to+%s", format(ini, "%Y-%m"), format(fin, "%Y-%m"))
  paste0(url_base, "?get=", campos, "&COMM_LVL=HS6&I_COMMODITY=", filtro, "&time=", tiempo,
         paste0("&CTY_CODE=", paises, collapse = ""), "&key=", clave_census)
}
parsea <- function(txt) {
  m <- fromJSON(txt)
  encabezado <- make.unique(m[1, ])
  d <- as_tibble(as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE), .name_repair = "minimal")
  names(d) <- encabezado
  d %>% transmute(hs6 = I_COMMODITY, cty_code = CTY_CODE, cty_name = CTY_NAME,
                  mes = as.Date(paste0(time, "-01")), valor_usd = as.numeric(GEN_VAL_MO))
}

# Consulta minima para saber si la API responde y la clave funciona
prueba_api <- function() {
  mes <- floor_date(Sys.Date(), "month") %m-% months(4)
  url <- arma_url("847150", mes, mes, cod_total, "I_COMMODITY,CTY_CODE,GEN_VAL_MO")
  t0 <- Sys.time()
  r <- tryCatch(curl_fetch_memory(url, handle = new_handle(timeout = 30, connecttimeout = 15)), error = function(e) e)
  if (inherits(r, "error")) return(list(ok = FALSE, msg = sprintf("sin respuesta en %.0f s (%s)", segundos(t0), conditionMessage(r))))
  if (r$status_code %in% c(200, 204) && (r$status_code == 204 || es_json(texto(r))))
    return(list(ok = TRUE, msg = sprintf("la API respondio en %.1f s", segundos(t0))))
  list(ok = FALSE, msg = paste0("HTTP ", r$status_code, ": ", limpia(texto(r))))
}

# Descarga en paralelo (varias consultas a la vez); cada respuesta se guarda en
# su archivo en cuanto llega. Las que fallan se reintentan en otra vuelta.
descarga <- function(tt) {
  pendientes <- tt; hechas <- 0; n <- nrow(tt)
  for (vuelta in seq_len(intentos)) {
    if (!nrow(pendientes)) break
    if (vuelta > 1) message(sprintf("Vuelta %d: reintentando %d consultas que fallaron.", vuelta, nrow(pendientes)))
    fallas <- integer(0)
    pool <- new_pool(total_con = conexiones, host_con = conexiones)
    for (k in seq_len(nrow(pendientes))) local({
      kk <- k; tarea <- pendientes[kk, ]
      curl_fetch_multi(tarea$url, pool = pool, handle = new_handle(timeout = timeout_seg, connecttimeout = 30),
        done = function(res) {
          seg <- unname(res$times["total"])
          txt <- texto(res)
          if (res$status_code == 200 && es_json(txt)) {
            d <- parsea(txt); write_excel_csv(d, tarea$archivo)
            hechas <<- hechas + 1
            message(sprintf("  [%d/%d] %s: %s filas en %.1f s", hechas, n, tarea$etiqueta, format(nrow(d), big.mark = ","), seg))
          } else if (res$status_code == 204) {
            hechas <<- hechas + 1
            message(sprintf("  [%d/%d] %s: sin datos todavia", hechas, n, tarea$etiqueta))
          } else {
            fallas <<- c(fallas, kk)
            message(sprintf("  %s: HTTP %d en %.0f s (%s)", tarea$etiqueta, res$status_code, seg, limpia(txt)))
          }
        },
        fail = function(msg) { fallas <<- c(fallas, kk); message("  ", tarea$etiqueta, ": ", msg) })
    })
    multi_run(pool = pool)
    pendientes <- pendientes[fallas, ]
  }
  if (nrow(pendientes)) warning(nrow(pendientes), " consultas no se pudieron bajar; vuelve a correr el script para reintentarlas.")
  invisible(pendientes)
}

mes_hoy <- floor_date(Sys.Date(), "month")
periodos <- function(desde, hasta) {
  if (unidad_tiempo == "mes") seq(desde, hasta, by = "month") else seq(floor_date(desde, "quarter"), hasta, by = "quarter")
}
grupos <- function(p) if (por_pais) as.list(p) else list(p)
etq_pais <- function(p) if (length(p) > 1) paste0("grupo", length(p)) else if (p == "-") "total" else p
# Panel: capitulos (o partidas) completos desde 2021, con Asia si se pide.
# Historia del paso 1 (2013 a 2020): solo total y Mexico, y solo los capitulos
# (o partidas) que contienen al nucleo.
filtros_panel <- if (nivel_filtro == "capitulo") capitulos else hs4_panel
filtros_hist  <- if (nivel_filtro == "capitulo") sort(unique(str_sub(hs4_nucleo, 1, 2))) else hs4_nucleo
tareas <- bind_rows(
  expand_grid(filtro = filtros_panel, ini = periodos(as.Date(paste0(anio_panel, "-01-01")), mes_hoy),
              paises = grupos(c(cod_total, cod_mexico, cod_asia))),
  if (historia_paso1) expand_grid(filtro = filtros_hist,
                                  ini = periodos(as.Date(paste0(anio_desde, "-01-01")), as.Date(paste0(anio_panel - 1, "-12-01"))),
                                  paises = grupos(c(cod_total, cod_mexico)))) %>%
  mutate(fin = if (unidad_tiempo == "mes") ini else ini %m+% months(2),
         pais = vapply(paises, etq_pais, character(1)),
         etiqueta = paste(filtro, format(ini, "%Y-%m"), pais),
         archivo = file.path(dir_cen, "raw", sprintf("imp_%s_%s_%s.csv", filtro, format(ini, "%Y-%m"), pais)),
         url = pmap_chr(list(paste0(filtro, "*"), ini, fin, paises), arma_url),
         bajar = !file.exists(archivo) | ini >= mes_hoy %m-% months(refrescar_meses))
message(sprintf("Consultas: %d en total (unidad = %s, filtro = %s, por pais = %s, Asia = %s).",
                nrow(tareas), unidad_tiempo, nivel_filtro, por_pais, incluir_asia))

if (any(tareas$bajar)) {
  if (clave_census == "") {
    message("Falta CENSUS_API_KEY en el .env; se usan solo los archivos ya descargados.")
    tareas$bajar <- FALSE
  } else {
    pr <- prueba_api()
    message("Prueba de la API: ", pr$msg)
    if (!pr$ok) {
      message("No se pudo consultar el Census (revisa la clave, la red o el proxy de la oficina). ",
              "Se usan solo los archivos ya descargados.")
      tareas$bajar <- FALSE
    } else {
      message(sprintf("Por bajar: %d (las demas ya estan guardadas). Con %d conexiones y ~7 s por consulta: unos %.0f minutos.",
                      sum(tareas$bajar), conexiones, sum(tareas$bajar) * 7 / conexiones / 60))
    }
  }
}
if (any(tareas$bajar)) {
  t_total <- Sys.time()
  descarga(tareas %>% filter(bajar))
  message(sprintf("Descarga terminada en %.1f minutos.", segundos(t_total) / 60))
}

ex <- tareas$archivo[file.exists(tareas$archivo)]
if (!length(ex)) stop("No hay datos del Census. Revisa CENSUS_API_KEY en el .env y la conexion.")
eua <- map_dfr(ex, ~ lee_csv(.x, col_types = cols(hs6 = col_character(), cty_code = col_character(),
                                                  cty_name = col_character(), mes = col_date(), valor_usd = col_double()))) %>%
  distinct(hs6, cty_code, mes, .keep_all = TRUE)
cat("\nPaises que regreso la API (verificar codigos):\n")
print(as.data.frame(eua %>% distinct(cty_code, cty_name) %>% arrange(cty_code)), row.names = FALSE)

meses_asia <- eua %>% filter(cty_code %in% cod_asia) %>% distinct(mes) %>% pull(mes)   # meses con Asia descargada
eua_hs6 <- eua %>% filter(!is.na(valor_usd)) %>%
  group_by(hs6, mes) %>%
  summarise(total = sum(valor_usd[cty_code == cod_total]),
            mexico = sum(valor_usd[cty_code == cod_mexico]),
            asia = sum(valor_usd[cty_code %in% cod_asia]), .groups = "drop") %>%
  mutate(hay_asia = mes %in% meses_asia) %>%
  mutate(dem_ex_mx = total - mexico, dem_fuera_asia = if_else(hay_asia, total - mexico - asia, NA_real_),
         exp_mx_eua = mexico)
write_excel_csv(eua_hs6, file.path(dir_cen, "eua_importaciones_hs6_mes.csv"))

# =============================================================================
# 3. Panel conjunto Mexico y EUA por HS6
# =============================================================================
a_trimestre <- function(d, cols) {
  d %>% mutate(periodo = floor_date(mes, "quarter")) %>% group_by(hs6, periodo) %>%
    summarise(n_meses = n_distinct(mes), across(all_of(cols), sum), .groups = "drop") %>%
    filter(n_meses == 3) %>% select(-n_meses)
}
vars_eua <- c("dem_ex_mx", "dem_fuera_asia", "exp_mx_eua")
eua_q <- a_trimestre(eua_hs6, vars_eua)
mx_q  <- a_trimestre(mx_mes, "valor_usd")

prepara <- function(mx, us, per_col = "periodo") {
  mx %>% inner_join(us, by = c("hs6", per_col)) %>%
    left_join(grupo_hs6, by = "hs6") %>% mutate(grupo = coalesce(grupo, ref_resto), eslabon = coalesce(eslabon, ref_resto)) %>%
    group_by(hs6) %>% filter(sum(valor_usd) > 0) %>% arrange(.data[[per_col]], .by_group = TRUE) %>%
    mutate(across(all_of(vars_eua), ~ log(if_else(.x > 0, .x, NA_real_)), .names = "ln_{.col}"),
           ln_dem_ad = lead(ln_dem_ex_mx), ln_dem_r1 = lag(ln_dem_ex_mx)) %>% ungroup() %>%
    mutate(t_num = as.numeric(factor(.data[[per_col]])))
}
panel_q <- prepara(mx_q, eua_q)
panel_m <- prepara(mx_mes %>% rename(periodo = mes), eua_hs6 %>% select(hs6, periodo = mes, all_of(vars_eua)))

cat("\nPanel trimestral: ", n_distinct(panel_q$hs6), " HS6, ", n_distinct(panel_q$periodo), " trimestres\n", sep = "")
print(as.data.frame(panel_q %>% group_by(grupo) %>% summarise(hs6 = n_distinct(hs6), valor_musd = sum(valor_usd) / 1e6)), row.names = FALSE)

# =============================================================================
# 4. Regresiones por producto
# =============================================================================
estima <- function(d, rhs, tend = FALSE, por_grupo = FALSE, etiqueta, interaccion = "grupo") {
  x <- if (por_grupo) paste0("i(", interaccion, ", ", rhs, ")") else paste(rhs, collapse = " + ")
  f <- as.formula(paste0("valor_usd ~ ", x, " | ", if (tend) "hs6[t_num]" else "hs6", " + periodo"))
  d <- d %>% filter(if_all(all_of(rhs), ~ !is.na(.x)))
  m <- tryCatch(fepois(f, data = d, cluster = ~hs6, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) return(tibble(especificacion = etiqueta, termino = NA_character_, eta = NA_real_, ee = NA_real_, p = NA_real_))
  ct <- coeftable(m)
  tibble(especificacion = etiqueta, termino = rownames(ct), eta = ct[, 1], ee = ct[, 2], p = ct[, 4],
         n_hs6 = n_distinct(d$hs6), n_obs = nobs(m))
}
res <- bind_rows(
  estima(panel_q, "ln_dem_ex_mx", etiqueta = "1_base"),
  estima(panel_q, "ln_dem_ex_mx", tend = TRUE, etiqueta = "2_tendencia_hs6"),
  if (incluir_asia) estima(panel_q, "ln_dem_fuera_asia", etiqueta = "3_demanda_fuera_de_asia"),
  if (incluir_asia) estima(panel_q, "ln_dem_fuera_asia", tend = TRUE, etiqueta = "3_demanda_fuera_de_asia_tendencia"),
  estima(panel_q, "ln_dem_ex_mx", por_grupo = TRUE, etiqueta = "4_por_nivel"),
  estima(panel_q, "ln_dem_ex_mx", tend = TRUE, por_grupo = TRUE, etiqueta = "4_por_nivel_tendencia"),
  estima(panel_q, "ln_dem_ex_mx", por_grupo = TRUE, interaccion = "eslabon", etiqueta = "4b_por_eslabon"),
  estima(panel_q, "ln_dem_ex_mx", tend = TRUE, por_grupo = TRUE, interaccion = "eslabon", etiqueta = "4b_por_eslabon_tendencia"),
  estima(panel_q, "ln_dem_ad", tend = TRUE, etiqueta = "5_adelanto_1"),
  estima(panel_q, "ln_dem_r1", tend = TRUE, etiqueta = "5_rezago_1"),
  estima(panel_q, c("ln_dem_ad", "ln_dem_r1"), tend = TRUE, etiqueta = "5b_carrera"),
  # Adelanto, trimestre actual y rezago juntos: separa anticipacion de pedidos
  # (el adelanto absorbe parte del efecto) de causalidad inversa
  estima(panel_q, c("ln_dem_ad", "ln_dem_ex_mx", "ln_dem_r1"), tend = TRUE, etiqueta = "5c_adelanto_actual_rezago"),
  estima(panel_q, "ln_exp_mx_eua", etiqueta = "6_exportaciones_mx_a_eua"),
  estima(panel_q, "ln_exp_mx_eua", tend = TRUE, etiqueta = "6_exportaciones_mx_a_eua_tendencia"),
  estima(panel_m, "ln_dem_ex_mx", etiqueta = "7_mensual"),
  estima(panel_m, "ln_dem_ex_mx", tend = TRUE, etiqueta = "7_mensual_tendencia")
)
estrellas <- function(p) case_when(is.na(p) ~ "", p < 0.01 ~ "***", p < 0.05 ~ "**", p < 0.10 ~ "*", TRUE ~ "")
resumen <- res %>% mutate(termino = str_remove(termino, "^ln_|^grupo::|^eslabon::"),
                          celda = sprintf("%.2f%s (%.2f)", eta, estrellas(p), ee)) %>%
  select(especificacion, termino, celda, n_hs6, n_obs)

# =============================================================================
# 5. Paso 1: valuacion en bolsa -> importaciones de EUA del nucleo
# =============================================================================
indices <- lee_csv(file.path(dir_emp, "mcap_indices_grupo.csv")) %>% mutate(mes = as.Date(mes))
mc_q <- indices %>% filter(indicadora == "es_hyperscaler") %>%
  mutate(trimestre = floor_date(mes, "quarter")) %>% group_by(trimestre) %>%
  summarise(mc = log(mean(coalesce(idx_ponderado, idx_equiponderado))), .groups = "drop")
nucleo_q <- eua_hs6 %>% filter(hs6 %in% hs6_nucleo) %>% mutate(trimestre = floor_date(mes, "quarter")) %>%
  group_by(trimestre) %>% summarise(n_meses = n_distinct(mes), across(all_of(vars_eua), sum), .groups = "drop") %>%
  filter(n_meses == 3) %>% select(-n_meses)
paso1 <- map_dfr(vars_eua, function(v) {
  d <- nucleo_q %>% select(trimestre, y = all_of(v)) %>% filter(y > 0) %>% mutate(y = log(y)) %>%
    inner_join(mc_q, by = "trimestre") %>% arrange(trimestre) %>%
    mutate(dy = y - lag(y, 4), dmc = mc - lag(mc, 4), dmc_l1 = lag(dmc), dmc_l2 = lag(dmc, 2)) %>%
    filter(!is.na(dy), !is.na(dmc_l2))
  if (nrow(d) < 20) return(NULL)
  m <- lm(dy ~ dmc_l1 + dmc_l2, data = d)
  V <- NeweyWest(m, lag = 4, prewhite = FALSE)
  b <- sum(coef(m)[c("dmc_l1", "dmc_l2")]); s <- sqrt(sum(V[c("dmc_l1", "dmc_l2"), c("dmc_l1", "dmc_l2")]))
  tibble(variable = v, theta_suma_rezagos = b, ee = s, p = 2 * (1 - pnorm(abs(b / s))),
         desde = min(d$trimestre), hasta = max(d$trimestre), n = nrow(d))
})

# Contexto: peso de Mexico en lo que EUA importa del nucleo, por anio
peso_mx <- eua_hs6 %>% filter(hs6 %in% hs6_nucleo) %>% group_by(anio = year(mes)) %>%
  summarise(eua_total_musd = sum(total) / 1e6, desde_mexico_musd = sum(mexico) / 1e6,
            desde_asia_musd = sum(asia) / 1e6, .groups = "drop") %>%
  mutate(pct_mexico = 100 * desde_mexico_musd / eua_total_musd,
         pct_asia = if_else(desde_asia_musd > 0, 100 * desde_asia_musd / eua_total_musd, NA_real_))

# ---- Guardar e imprimir ------------------------------------------------------
write_excel_csv(res, file.path(dir_out, "01_eta_por_producto.csv"))
write_excel_csv(resumen, file.path(dir_out, "02_resumen.csv"))
write_excel_csv(paso1, file.path(dir_out, "03_paso1_nucleo_eua.csv"))
write_excel_csv(peso_mx, file.path(dir_out, "04_peso_mexico_nucleo.csv"))
write_excel_csv(nucleo_q, file.path(dir_out, "05_series_nucleo_eua_trimestral.csv"))

cat("\nImportaciones de Mexico por HS6 contra la demanda de EUA (*** 99%, ** 95%, * 90% de confianza):\n")
print(as.data.frame(resumen), right = FALSE, row.names = FALSE)
cat("\nPaso 1 (valuacion en bolsa -> importaciones de EUA del nucleo, suma de rezagos 1 y 2):\n")
print(as.data.frame(paso1 %>% mutate(across(where(is.numeric), ~ round(.x, 3)))), row.names = FALSE)
cat("\nPeso de Mexico y de Asia en lo que EUA importa del nucleo:\n")
print(as.data.frame(peso_mx %>% mutate(across(where(is.numeric), ~ round(.x, 1)))), row.names = FALSE)

# =============================================================================
# 6. Grafica: nucleo en EUA y en Mexico (indice, promedio 2022 = 100)
# =============================================================================
grafica <- tryCatch({
  etq <- c(dem_ex_mx = "EUA importa del n\u00facleo (sin M\u00e9xico)",
           exp_mx_eua = "EUA importa del n\u00facleo desde M\u00e9xico",
           mx = "M\u00e9xico importa del n\u00facleo (todos los or\u00edgenes)")
  gd <- eua_hs6 %>% filter(hs6 %in% hs6_nucleo, mes >= min(meses_mx)) %>% group_by(mes) %>%
    summarise(dem_ex_mx = sum(dem_ex_mx), exp_mx_eua = sum(exp_mx_eua), .groups = "drop") %>%
    full_join(mx_mes %>% filter(hs6 %in% hs6_nucleo) %>% group_by(mes) %>% summarise(mx = sum(valor_usd)), by = "mes") %>%
    pivot_longer(-mes, names_to = "serie", values_to = "valor") %>% filter(!is.na(valor)) %>%
    group_by(serie) %>% mutate(indice = 100 * valor / mean(valor[year(mes) == 2022])) %>% ungroup() %>%
    mutate(serie = factor(etq[serie], levels = etq))
  g <- ggplot(gd, aes(mes, indice, color = serie)) + geom_hline(yintercept = 100, color = "grey75") +
    geom_line(linewidth = 0.8) +
    scale_color_manual(values = c("#16191C", "#992246", "#A37D2E")) +
    labs(x = NULL, y = "\u00cdndice (promedio 2022 = 100)", color = NULL,
         title = "N\u00facleo de la canasta de IA: lo que compra EUA y lo que importa M\u00e9xico") +
    theme_minimal(base_size = 11) + theme(legend.position = "bottom")
  ggsave(file.path(dir_out, "06_nucleo_eua_mexico.png"), g, width = 26, height = 13, units = "cm", dpi = 300, bg = "white")
  TRUE
}, error = function(e) { message("La grafica no se pudo hacer (los CSV ya se guardaron): ", conditionMessage(e)); FALSE })

message("Listo. Archivos en: ", dir_out)
