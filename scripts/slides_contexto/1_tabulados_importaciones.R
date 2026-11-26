# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/slides_contexto/1_tabulados_importaciones.R
# Objetivo:       Arma los tabulados de las graficas y las cifras de las
#                 tarjetas de la presentacion de contexto de electronicos
#                 (importaciones y exportaciones),
#                 con la misma logica que proyecto_textiles_anam
#                 (1_tabulados_importaciones.R) y proyecto_autopartes:
#
#                   Seccion (slides)                   Desglose
#                   Panorama general        (4 a 8)    capitulo
#                   Bienes de IA            (10 a 14)  eslabon
#                   Nucleo de bienes de IA  (16 a 20)  subpartida
#                   Resto de electronicos   (22 a 26)  partida
#
#                 En cada seccion:
#                   +0  Tendencia mensual (total y temporales)
#                   +1  Tendencia anual, promedio diario, definitivas vs temporales
#                   +2  Barras apiladas por desglose (PD) + caidas
#                   +3  Barras apiladas por pais (PD) + caidas
#                   +4  Tabla desglose x pais del ultimo ano (valor, F y C)
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          19/11/2026
#
# INSUMOS:
#   - input/raw/consulta_electro_imp*.csv     importaciones de la ANAM
#   - input/raw/consulta_electro_exp*.csv     exportaciones de la ANAM
#
# UNIDADES (segun MONEDA; por omision millones de dolares corrientes, con el
#   tipo de cambio de cada operacion; con MONEDA = "mxn", millones de pesos reales, base ene. 2025):
#   Importaciones: valor_aduana.
#   Exportaciones: valor_comercial (la consulta de exportaciones no trae valor_aduana).
#   Deflactor: INPC general (SP1) que se descarga en cada corrida de la API de
#   Banxico con BANXICO_TOKEN del .env; se guarda copia en input/macro/.
#   - input/canasta_ia_fracciones.csv        nivel y eslabon por NICO
#   - input/Catalogo LIGIE ... .xlsx         nombres de partida
#   - API de Banxico (SIE), serie SP1        INPC; copia en input/macro/inpc_banxico_sp1.csv
#
# CONVENCION (igual que el modelo): Definitivas = A1 + C1; Temporales = IN + AF en
# importaciones y RT (retorno de IMMEX) en exportaciones.
#
# DIFERENCIAS CON TEXTILES:
#   - La consulta es mensual y trae una linea por contribucion: se deduplica
#     (ver leer_consulta_anam) y se deflacta aqui con el INPC.
#   - Como en textiles, hay dos insumos por flujo: la consulta completa
#     (series mensuales) y la truncada (enero al dia de corte de cada ano,
#     para los promedios diarios).
#
# PROMEDIO DIARIO:
#   PD = valor real del 1 de enero a FECHA_CORTE (mismo dia y mes en cada ano)
#        / dias naturales de ese periodo.
#
# SALIDAS (output/slides_contexto/<ddmmaa>/<flujo>/tabulados/):
#   - slide_XX_<seccion>_<descripcion>.csv
#   - cifras_tarjetas_<ddmmaa>.csv
#   - eventos_graficas.csv
#   - manifiesto_graficas.csv  (lo lee 2_graficas_importaciones.R)
# _____________________________________________________________________________

# PREAMBULO ___________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
paquetes <- c("tidyverse", "data.table", "lubridate", "glue", "crayon", "stringi",
              "readxl", "jsonlite", "curl")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

source("scripts/slides_contexto/funciones_importaciones.R")

# ---- PARAMETROS (lo unico que hay que tocar en cada corte) ----
ultimo_archivo <- function(patron) sort(list.files("input/raw", pattern = patron, full.names = TRUE),
                                         decreasing = TRUE)[1]
# Consultas completas (meses completos): series mensuales (+0)
RUTAS_CONSULTA <- c(importaciones = ultimo_archivo("^consulta_electro_imp(_[0-9].*)?\\.csv$"),
                    exportaciones = ultimo_archivo("^consulta_electro_exp(_[0-9].*)?\\.csv$"))
# Consultas truncadas: enero a noviembre de cada ano, con noviembre cortado al
# dia de FECHA_CORTE (como textiles_deflactado_truncado_comparacion.csv).
# Mismas columnas que la consulta completa. Alimentan los promedios diarios
# (+1 a +4). Si no existen, se usa la consulta completa de enero al mes de
# corte, suponiendo que ese mes registra hasta el dia de FECHA_CORTE.
RUTAS_TRUNCADO <- c(importaciones = ultimo_archivo("^consulta_electro_imp_truncado.*\\.csv$"),
                    exportaciones = ultimo_archivo("^consulta_electro_exp_truncado.*\\.csv$"))
FLUJOS <- c("importaciones", "exportaciones")   # quitar uno para correr solo el otro
RUTA_CANASTA  <- "input/canasta_ia_fracciones.csv"
RUTA_INPC     <- "input/macro/inpc_banxico_sp1.csv"   # copia de lo que se baja de Banxico

FECHA_CORTE    <- as.Date("2026-11-17")  # PD del 1 de enero a este dia y mes, en cada ano
ANIO_INICIO    <- 2022
ANIO_BASE      <- 2025                   # ano contra el que se comparan las caidas
ANIO_COMP      <- year(FECHA_CORTE)
MES_BASE       <- as.Date("2025-01-01")  # base de deflactacion (igual que textiles)
MES_BASE_TEXTO <- "ene. 2025"
MONEDA         <- "usd"   # "usd": millones de dolares corrientes (tipo de cambio de cada operacion)
                          # "mxn": millones de pesos reales, base MES_BASE (INPC)

TOP_N_DESGLOSE     <- 7
TOP_N_PAISES       <- 6
TOP_N_TABLA_FILAS  <- 7
TOP_N_TABLA_PAISES <- 7
N_CAIDAS           <- 5
UMBRAL_PD_MIN      <- if (MONEDA == "usd") 0.03 else 0.5   # millones diarios en ANIO_BASE para entrar al ranking
N_EJEMPLOS_OTROS   <- 3

# Secciones. niveles = niveles de la canasta que entran (NA = todo).
SECCIONES <- tribble(
    ~seccion,  ~etiqueta,                 ~niveles,                                    ~slide_inicial, ~desglose,
    "panorama", "Electr\u00f3nicos",            NA,                                          4,              "capitulo",
    "bienes_ia", "Bienes de IA",           "1_nucleo,2_infraestructura,3_mixta",        10,             "eslabon",
    "nucleo",   "N\u00facleo de bienes de IA",  "1_nucleo",                                  16,             "subpartida",
    "resto",    "Resto de electr\u00f3nicos",   "4_resto_electronicos",                      22,             "partida"
)

# Palabras para los textos segun el desglose
TEXTOS_DESGLOSE <- tribble(
    ~desglose,    ~sing,        ~plur,          ~art_del,           ~otros_lb,
    "capitulo",   "cap\u00edtulo",   "cap\u00edtulos",    "del cap\u00edtulo",     "Otros cap\u00edtulos",
    "partida",    "partida",    "partidas",     "de la partida",    "Otras partidas",
    "subpartida", "subpartida", "subpartidas",  "de la subpartida", "Otras subpartidas",
    "eslabon",    "eslab\u00f3n",    "eslabones",    "del eslab\u00f3n",      "Otros eslabones"
)

EVENTOS <- tribble(
    ~fecha,                ~etiqueta,                                         ~en_mensual, ~en_anual,
    as.Date("2022-11-30"), "Lanzamiento de ChatGPT",                          TRUE,        TRUE,
    as.Date("2026-01-01"), "Entrada en vigor de reforma arancelaria",         TRUE,        FALSE
)

COLOR_OTROS <- "#98989aff"
COLORES_CAPITULO <- c("84" = "#611232ff", "85" = "#1c4587ff", "90" = "#a57f2cff",
                      "91" = "#e6d194ff", "92" = "#1e5b4fff", "73" = "#4f3a5eff")
COLORES_PAIS <- c("China" = "#3e7a5fff", "Estados Unidos" = "#6e5a3eff", "Vietnam" = "#a5802fff",
                  "Taiw\u00e1n" = "#a35a3cff", "Corea del Sur" = "#6b3a5eff", "Malasia" = "#2f6e8cff",
                  "Jap\u00f3n" = "#1c4587ff", "Tailandia" = "#4f3a5eff")
COLORES_POSICION <- c("#1e5b4fff", "#a57f2cff", "#e6d194ff", "#611232ff",
                      "#1c4587ff", "#4f3a5eff", "#38761dff")
COLORES_RESERVA  <- c("#2f6e8cff", "#a35a3cff", "#6b3a5eff", "#6e5a3eff", "#3e7a5fff",
                      "#9b2247ff", "#011738ff", "#e6d194ff", "#1c4587ff")
# ----------------------------------------------------------------

SUFIJO <- format(FECHA_CORTE, "%d%m%y")

ANIOS <- ANIO_INICIO:ANIO_COMP
DIAS  <- dias_ventana(ANIOS, FECHA_CORTE)

# INSUMOS _____________________________________________________________________

message("Cargando insumos comunes:")
inpc <- if (MONEDA == "mxn") leer_inpc(RUTA_INPC) else NULL   # solo se deflacta en pesos

canasta <- read_csv(RUTA_CANASTA, col_types = cols(.default = "c"), locale = locale(encoding = "UTF-8")) |>
    transmute(fraccion = str_pad(fraccion, 8, "left", "0"), nico = str_pad(nico, 2, "left", "0"),
              nivel, eslabon)

ligie <- leer_catalogo_ligie("input")
nombres_partida <- ligie |> distinct(partida, desc_partida) |>
    group_by(partida) |> slice(1) |> ungroup() |>
    transmute(partida, nombre_partida = nombre_corto(desc_partida))

filtrar_seccion <- function(d, niveles) {
    if (is.na(niveles)) return(d)
    d |> filter(nivel %in% str_split_1(niveles, ","))
}

# FUNCIONES DE CALCULO ________________________________________________________

pd_por <- function(d, ...) {
    d |>
        group_by(anio, ...) |>
        summarise(mill = sum(valor, na.rm = TRUE) / 1e6, .groups = "drop") |>
        left_join(DIAS |> select(anio, dias), by = "anio") |>
        mutate(pd = mill / dias)
}

asignar_colores <- function(llaves, fijos = NULL, por_posicion = NULL) {
    reserva <- setdiff(COLORES_RESERVA, fijos)
    out <- character(length(llaves))
    k <- 1
    for (i in seq_along(llaves)) {
        if (llaves[i] == "otros") { out[i] <- COLOR_OTROS; next }
        if (!is.null(por_posicion)) { out[i] <- por_posicion[(i - 1) %% length(por_posicion) + 1]; next }
        if (!is.null(fijos) && llaves[i] %in% names(fijos)) { out[i] <- fijos[[llaves[i]]]; next }
        out[i] <- reserva[(k - 1) %% length(reserva) + 1]; k <- k + 1
    }
    out
}

armar_top <- function(pd_df, n_top) {
    ranking <- pd_df |>
        filter(anio == ANIO_COMP) |>
        group_by(clave, nombre) |>
        summarise(v = sum(pd), .groups = "drop") |>
        arrange(desc(v))
    top <- head(ranking$clave, n_top)

    pd_df |>
        mutate(es_top = clave %in% top,
               clave  = if_else(es_top, clave, "otros"),
               nombre = if_else(es_top, nombre, "Otros")) |>
        group_by(year = anio, es_top, clave, nombre) |>
        summarise(valor2 = sum(pd), .groups = "drop") |>
        group_by(year) |>
        mutate(tot = sum(valor2), valor_p = 100 * valor2 / tot) |>
        ungroup() |>
        left_join(tibble(clave = c(rev(top), "otros"), orden = seq_len(length(top) + 1)), by = "clave") |>
        arrange(year, orden)
}

variaciones <- function(pd_df) {
    pd_df |>
        filter(anio %in% c(ANIO_BASE, ANIO_COMP)) |>
        group_by(clave, nombre, anio) |>
        summarise(pd = sum(pd), .groups = "drop") |>
        pivot_wider(names_from = anio, values_from = pd, names_prefix = "pd_", values_fill = 0) |>
        rename(pd_base = !!glue("pd_{ANIO_BASE}"), pd_comp = !!glue("pd_{ANIO_COMP}")) |>
        mutate(dif_diaria = pd_comp - pd_base,
               var_pct         = if_else(pd_base > 0, 100 * (pd_comp / pd_base - 1), NA_real_),
               rank_comp       = min_rank(desc(pd_comp))) |>
        arrange(rank_comp)
}

nombre_archivo <- function(slide, seccion, descripcion) glue("slide_{sprintf('%02d', slide)}_{seccion}_{descripcion}")

# Ordena los cambios: primero las caidas (de la mayor a la menor) y despues los
# aumentos (del mayor al menor). Si no hay caidas, la lista queda solo con aumentos.
ordena_cambios <- function(d, n = Inf) {
    d |>
        mutate(grupo_signo = if_else(var_pct < 0, 1L, 2L)) |>
        arrange(grupo_signo, desc(abs(var_pct))) |>
        select(-grupo_signo) |>
        head(n)
}

# Titulo de la lista de la tarjeta segun los signos que contiene
titulo_cambios <- function(v) {
    if (length(v) == 0) return(NA_character_)
    if (all(v < 0)) "Ca\u00eddas m\u00e1s pronunciadas" else
        if (all(v >= 0)) "Aumentos m\u00e1s pronunciados" else "Cambios m\u00e1s pronunciados"
}

fila_cifra <- function(seccion, slide, campo, valor, texto = NA_character_) {
    tibble(seccion = seccion, slide = slide, campo = campo,
           valor = as.character(valor), texto_sugerido = as.character(texto))
}

# PROCESAMIENTO POR SECCION ___________________________________________________

procesar_seccion <- function(sec) {
    s0 <- sec$slide_inicial
    cat(bold$blue(glue("\n== {sec$etiqueta} (slides {s0} a {s0 + 4}) ==\n", .trim = FALSE)))

    m_sec <- filtrar_seccion(mensual, sec$niveles)
    t_sec <- filtrar_seccion(truncado, sec$niveles)
    if (nrow(t_sec) == 0) stop("La secci\u00f3n ", sec$seccion, " qued\u00f3 vac\u00eda.")

    tx <- TEXTOS_DESGLOSE |> filter(desglose == sec$desglose)
    t_sec <- t_sec |> mutate(clave = .data[[sec$desglose]],
                             nombre = .data[[paste0("nombre_", sec$desglose)]])
    if (sec$desglose == "eslabon") t_sec <- t_sec |> mutate(clave = nombre)   # sin claves numericas
    es_cap <- sec$desglose == "capitulo"
    sing <- tx$sing; plur <- tx$plur; art_del <- tx$art_del; otros_lb <- tx$otros_lb
    mas <- function(n) glue("{n} {if (n == 1) sing else plur} m\u00e1s")
    etiqueta_clave <- function(clave, nombre) {
        if (sec$desglose == "eslabon") nombre else
        if (sec$desglose == "subpartida") glue("{nombre} ({str_sub(clave, 1, 4)}.{str_sub(clave, 5, 6)})") else
        glue("{nombre} ({clave})")
    }

    n_fr <- n_distinct(t_sec$fraccion)
    cobertura <- if (is.na(sec$niveles)) {
        glue("las {n_fr} fracciones de electr\u00f3nicos que entrega la ANAM (cap\u00edtulos ",
             "{unir_lista(sort(unique(t_sec$capitulo)))})")
    } else {
        glue("las {n_fr} fracciones de {str_to_lower(sec$etiqueta)} de la canasta de bienes de IA")
    }
    nota_pd <- glue("Nota: {TX$nota_valor} ",
                    "Promedio diario del 1 de enero al {day(FECHA_CORTE)} de {MESES_LARGO[month(FECHA_CORTE)]} ",
                    "de cada a\u00f1o. Corte al {fecha_corta(FECHA_CORTE)}.")

    cifras <- list(); manif <- list()
    agrega_manif <- function(slide, tipo, desc, titulo) {
        archivo <- nombre_archivo(slide, sec$seccion, desc)
        tibble(slide = slide, seccion = sec$seccion, etiqueta_seccion = sec$etiqueta, tipo = tipo,
               titulo = titulo, tabulado = glue("{CARPETA_TAB}{archivo}.csv"),
               grafica = glue("{CARPETA_FIG}{archivo}"))
    }

    # ---- +0 Tendencia mensual ------------------------------------------------
    sl <- s0
    serie <- m_sec |>
        group_by(fecha, tipo) |>
        summarise(v = sum(valor, na.rm = TRUE) / 1e6, .groups = "drop") |>
        pivot_wider(names_from = tipo, values_from = v, values_fill = 0) |>
        complete(fecha = seq(min(fecha), max(fecha), by = "month"),
                 fill = list(Definitivas = 0, Temporales = 0)) |>
        transmute(fecha,
                  !!glue("{TX$Sust} totales") := Definitivas + Temporales,
                  !!glue("{TX$Sust} temporales") := Temporales,
                  !!glue("{TX$Sust} definitivas") := Definitivas) |>
        arrange(fecha)
    m_info <- agrega_manif(sl, "lineas_mensual", glue("tendencia_mensual_{FLUJO}"),
                           glue("Tendencia mensual de {FLUJO} ({TX$unidad_larga})"))
    write_csv(serie, m_info$tabulado)
    manif[[length(manif) + 1]] <- m_info

    tot_col <- serie[[glue("{TX$Sust} totales")]]
    imax <- which.max(tot_col)
    cifras[[length(cifras) + 1]] <- bind_rows(
        fila_cifra(sec$seccion, sl, glue("maximo_mensual_{U}"), round(tot_col[imax], 0),
                   glue("{str_to_title(MESES_LARGO[month(serie$fecha[imax])])} {year(serie$fecha[imax])}")),
        fila_cifra(sec$seccion, sl, glue("ultimo_mes_{U}"), round(tail(tot_col, 1), 0),
                   glue("{MES_INCOMPLETO}{if (HAY_INCOMPLETO) ' (mes incompleto)' else ''}")),
        fila_cifra(sec$seccion, sl, "nota_fuente", NA,
                   glue("Fuente: elaboraci\u00f3n propia con datos de la ANAM. Comprende {cobertura}, para ",
                        "{TX$cobertura_tipos}. {TX$nota_valor}",
                        "{if (HAY_INCOMPLETO) paste0(' ', MES_INCOMPLETO, ' es un mes incompleto (corte de la extracci\u00f3n).') else ''}"))
    )

    # ---- +1 Tendencia anual: PD definitivas vs temporales ---------------------
    sl <- s0 + 1
    anual <- pd_por(t_sec, tipo) |>
        select(year = anio, tipo, pd) |>
        pivot_wider(names_from = tipo, values_from = pd, values_fill = 0)
    for (col in c("Definitivas", "Temporales")) if (!col %in% names(anual)) anual[[col]] <- 0
    anual <- anual |> select(year, Definitivas, Temporales) |> arrange(year)
    a_info <- agrega_manif(sl, "lineas_anual", "tendencia_anual_pd_definitivas_temporales",
                           glue("Promedio diario anual, definitivas vs temporales ({TX$unidad})"))
    write_csv(anual, a_info$tabulado)
    manif[[length(manif) + 1]] <- a_info

    tot_anual <- anual |> mutate(total = Definitivas + Temporales)
    pd_comp   <- tot_anual$total[tot_anual$year == ANIO_COMP]
    prom_prev <- mean(tot_anual$total[tot_anual$year < ANIO_COMP])
    var_prom  <- 100 * (pd_comp / prom_prev - 1)
    pct_temp  <- 100 * tot_anual$Temporales[tot_anual$year == ANIO_COMP] / pd_comp
    cifras[[length(cifras) + 1]] <- bind_rows(
        fila_cifra(sec$seccion, sl, glue("pd_total_{tot_anual$year}"), round(tot_anual$total, 2)),
        fila_cifra(sec$seccion, sl, glue("pd_definitivas_{anual$year}"), round(anual$Definitivas, 2)),
        fila_cifra(sec$seccion, sl, glue("pd_temporales_{anual$year}"), round(anual$Temporales, 2)),
        fila_cifra(sec$seccion, sl, "var_pct_vs_promedio_previos", round(var_prom, 2),
                   glue("En {ANIO_COMP} se ha {TX$verbo} {fmt_pct(abs(var_prom), 2)} ",
                        "{if (var_prom < 0) 'menos' else 'm\u00e1s'} que el promedio de {ANIO_INICIO} a {ANIO_COMP - 1}.")),
        fila_cifra(sec$seccion, sl, "pct_temporales_anio_actual", round(pct_temp, 1),
                   glue("Las {FLUJO} temporales de {ANIO_COMP} equivalen al {fmt_pct(pct_temp, 1)} ",
                        "de las {FLUJO} totales.")),
        fila_cifra(sec$seccion, sl, "nota", NA, nota_pd)
    )

    # ---- +2 Barras por desglose ------------------------------------------------
    sl <- s0 + 2
    pd_desg <- pd_por(t_sec, clave, nombre)
    top_d   <- armar_top(pd_desg, TOP_N_DESGLOSE)

    otros_d <- pd_desg |>
        filter(anio == ANIO_COMP, !clave %in% top_d$clave) |>
        group_by(clave, nombre) |> summarise(v = sum(pd), .groups = "drop") |>
        filter(v > 0) |> arrange(desc(v))
    etq_otros_d <- glue("{otros_lb} ({mas(nrow(otros_d))}; incluyen ",
                        "{paste(str_to_lower(head(otros_d$nombre, N_EJEMPLOS_OTROS)), collapse = ', ')}, etc.)")

    claves_orden <- top_d |> distinct(clave, orden) |> arrange(orden) |> pull(clave)
    colores_d <- if (es_cap) asignar_colores(claves_orden, COLORES_CAPITULO) else
                             asignar_colores(claves_orden, por_posicion = COLORES_POSICION)
    top_d <- top_d |>
        left_join(tibble(clave = claves_orden, color = colores_d), by = "clave") |>
        mutate(etiqueta_leyenda = if_else(clave == "otros", as.character(etq_otros_d),
                                          as.character(etiqueta_clave(clave, nombre))))
    d_info <- agrega_manif(sl, "barras_apiladas", glue("barras_pd_por_{sec$desglose}"),
                           glue("Promedio diario por {sing} ({TX$unidad}), barras apiladas"))
    write_csv(top_d, d_info$tabulado)
    manif[[length(manif) + 1]] <- d_info

    var_d  <- variaciones(pd_desg)
    # Si hay caidas, la tarjeta reporta la mayor; si no, el mayor aumento
    caidas <- var_d |> filter(pd_base >= UMBRAL_PD_MIN, !is.na(var_pct)) |> ordena_cambios(N_CAIDAS)
    write_excel_csv(var_d |> rename(!!glue("dif_{U}_diarios") := dif_diaria), glue("{CARPETA_TAB}{nombre_archivo(sl, sec$seccion, glue('variaciones_por_{sec$desglose}'))}.csv"))
    ref1 <- if (nrow(caidas) > 0) etiqueta_clave(caidas$clave[1], caidas$nombre[1]) else ""
    texto_caida <- if (nrow(caidas) == 0) "Sin datos suficientes para calcular cambios." else
        glue("{if (caidas$var_pct[1] < 0) 'La ca\u00edda m\u00e1s grande' else 'El aumento m\u00e1s grande'} ",
             "entre {ANIO_BASE} y {ANIO_COMP} vino {art_del} {ref1}, ",
             "equivalente a {fmt_pct_signo(caidas$var_pct[1], 1)} de su valor.")
    cifras[[length(cifras) + 1]] <- bind_rows(
        fila_cifra(sec$seccion, sl, "total_pd_por_anio", round(top_d |> distinct(year, tot) |> pull(tot), 0),
                   glue("{top_d |> distinct(year, tot) |> pull(year)}")),
        fila_cifra(sec$seccion, sl, "caida_mas_grande", round(caidas$var_pct[1], 2), texto_caida),
        fila_cifra(sec$seccion, sl, "titulo_lista", NA, glue("{titulo_cambios(caidas$var_pct)} ({ANIO_BASE} a {ANIO_COMP})")),
        fila_cifra(sec$seccion, sl, glue("caidas_pronunciadas_{seq_len(nrow(caidas))}"), round(caidas$var_pct, 2),
                   glue("{etiqueta_clave(caidas$clave, caidas$nombre)}: {fmt_pct_signo(caidas$var_pct, 1)}")),
        fila_cifra(sec$seccion, sl, "leyenda_otros", nrow(otros_d), if (nrow(otros_d) > 0) etq_otros_d else NA),
        fila_cifra(sec$seccion, sl, "nota", NA, nota_pd)
    )

    # ---- +3 Barras por pais ---------------------------------------------------
    sl <- s0 + 3
    pd_pais <- pd_por(t_sec |> mutate(clave = pais, nombre = nombre_pais(pais)), clave, nombre)
    top_p   <- armar_top(pd_pais, TOP_N_PAISES)

    otros_p <- pd_pais |>
        filter(anio == ANIO_COMP, !clave %in% top_p$clave) |>
        group_by(clave, nombre) |> summarise(v = sum(pd), .groups = "drop") |>
        filter(v > 0) |> arrange(desc(v))
    etq_otros_p <- glue("Otros pa\u00edses ({nrow(otros_p)} pa\u00edses m\u00e1s; incluyen, en orden de volumen de ",
                        "{TX$sing}, a {unir_lista(head(otros_p$nombre, N_EJEMPLOS_OTROS))})")

    paises_orden <- top_p |> distinct(clave, nombre, orden) |> arrange(orden)
    top_p <- top_p |>
        left_join(tibble(clave = paises_orden$clave,
                         color = asignar_colores(if_else(paises_orden$clave == "otros", "otros", paises_orden$nombre),
                                                 COLORES_PAIS)), by = "clave") |>
        mutate(etiqueta_leyenda = if_else(clave == "otros", as.character(etq_otros_p), nombre))
    p_info <- agrega_manif(sl, "barras_apiladas", "barras_pd_por_pais",
                           glue("Promedio diario por pa\u00eds ({TX$unidad}), barras apiladas"))
    write_csv(top_p, p_info$tabulado)
    manif[[length(manif) + 1]] <- p_info

    var_p     <- variaciones(pd_pais)
    var_p_top <- var_p |> filter(clave %in% top_p$clave) |> ordena_cambios()
    write_excel_csv(var_p |> rename(!!glue("dif_{U}_diarios") := dif_diaria), glue("{CARPETA_TAB}{nombre_archivo(sl, sec$seccion, 'variaciones_por_pais')}.csv"))
    # Si algun pais del top cae, se reporta la mayor caida; si no, el mayor aumento
    hay_caida_p <- any(var_p_top$dif_diaria < 0)
    peor <- if (hay_caida_p) slice_min(var_p_top, dif_diaria, n = 1, with_ties = FALSE) else
                             slice_max(var_p_top, dif_diaria, n = 1, with_ties = FALSE)
    texto_pais <- if (hay_caida_p) {
        glue("La ca\u00edda m\u00e1s grande entre {ANIO_BASE} y {ANIO_COMP}, en los pa\u00edses con mayor volumen de ",
             "{TX$sing}, vino de {peor$nombre}, con {fmt_num(abs(peor$dif_diaria), 2)} {TX$unidad_min} menos ",
             "por d\u00eda respecto a {ANIO_BASE}.")
    } else {
        glue("El mayor aumento entre {ANIO_BASE} y {ANIO_COMP}, en los pa\u00edses con mayor volumen de ",
             "{TX$sing}, vino de {peor$nombre}, con {fmt_num(peor$dif_diaria, 2)} {TX$unidad_min} m\u00e1s ",
             "por d\u00eda respecto a {ANIO_BASE}.")
    }
    cifras[[length(cifras) + 1]] <- bind_rows(
        fila_cifra(sec$seccion, sl, glue("cambio_mas_grande_{U}_diarios"), round(peor$dif_diaria, 2), texto_pais),
        fila_cifra(sec$seccion, sl, "titulo_lista", NA,
                   glue("{titulo_cambios(var_p_top$var_pct)} por pa\u00eds ({ANIO_BASE} a {ANIO_COMP})")),
        fila_cifra(sec$seccion, sl, glue("caidas_por_pais_{seq_len(nrow(var_p_top))}"), round(var_p_top$var_pct, 2),
                   glue("{var_p_top$nombre} ({fmt_pct_signo(var_p_top$var_pct, 1)})")),
        fila_cifra(sec$seccion, sl, "leyenda_otros", nrow(otros_p), if (nrow(otros_p) > 0) etq_otros_p else NA),
        fila_cifra(sec$seccion, sl, "nota", NA, nota_pd)
    )

    # ---- +4 Tabla desglose x pais (ano actual, millones del periodo en la moneda de MONEDA) ---------------
    sl <- s0 + 4
    base_t <- t_sec |>
        filter(anio == ANIO_COMP) |>
        group_by(clave, nombre, pais) |>
        summarise(mill = sum(valor, na.rm = TRUE) / 1e6, .groups = "drop")
    filas_top <- base_t |> group_by(clave, nombre) |> summarise(v = sum(mill), .groups = "drop") |>
        slice_max(v, n = TOP_N_TABLA_FILAS, with_ties = FALSE)
    cols_top  <- base_t |> group_by(pais) |> summarise(v = sum(mill), .groups = "drop") |>
        slice_max(v, n = TOP_N_TABLA_PAISES, with_ties = FALSE)

    lbl_fila <- function(clave, nombre) {
        if (sec$desglose == "eslabon") as.character(nombre) else as.character(glue("[{clave}] {nombre}"))
    }
    orden_filas <- c(lbl_fila(filas_top$clave, filas_top$nombre),
                     if (n_distinct(base_t$clave) > nrow(filas_top)) otros_lb)
    orden_cols  <- c(nombre_pais(cols_top$pais), "Otros pa\u00edses")

    celdas <- base_t |>
        mutate(fila    = if_else(clave %in% filas_top$clave, lbl_fila(clave, nombre), otros_lb),
               columna = if_else(pais %in% cols_top$pais, nombre_pais(pais), "Otros pa\u00edses")) |>
        group_by(fila, columna) |> summarise(mill = sum(mill), .groups = "drop") |>
        complete(fila = orden_filas, columna = orden_cols, fill = list(mill = 0)) |>
        group_by(fila) |> mutate(total_fila = sum(mill)) |>
        group_by(columna) |> mutate(total_columna = sum(mill)) |> ungroup() |>
        mutate(pct_F = 100 * mill / total_fila,
               pct_C = 100 * mill / total_columna)
    gran_total <- sum(celdas$mill)

    tabla_larga <- bind_rows(
        celdas |> select(fila, columna, mill, pct_F, pct_C),
        celdas |> distinct(fila, mill = total_fila) |> mutate(columna = "TOTAL", pct_total = 100 * mill / gran_total),
        celdas |> distinct(columna, mill = total_columna) |> mutate(fila = "TOTAL", pct_total = 100 * mill / gran_total),
        tibble(fila = "TOTAL", columna = "TOTAL", mill = gran_total, pct_total = 100)
    ) |>
        mutate(fila    = factor(fila, levels = c(orden_filas, "TOTAL")),
               columna = factor(columna, levels = c(orden_cols, "TOTAL"))) |>
        arrange(fila, columna)

    tabla_ppt <- tabla_larga |>
        mutate(texto = if_else(is.na(pct_F),
                               glue("{fmt_num(mill, 0)}\n({fmt_pct(pct_total, 1)})"),
                               glue("{fmt_num(mill, 0)}\n({fmt_pct(pct_F, 1)})\n({fmt_pct(pct_C, 1)})"))) |>
        select(fila, columna, texto) |>
        pivot_wider(names_from = columna, values_from = texto) |>
        rename(!!str_to_title(sing) := fila)

    archivo_tabla <- nombre_archivo(sl, sec$seccion, glue("tabla_{sec$desglose}_por_pais_{ANIO_COMP}"))
    write_excel_csv(tabla_larga |> rename(!!U := mill), glue("{CARPETA_TAB}{archivo_tabla}_larga.csv"))
    write_excel_csv(tabla_ppt,   glue("{CARPETA_TAB}{archivo_tabla}_formato_ppt.csv"))

    n_otras_filas <- n_distinct(base_t$clave) - nrow(filas_top)
    ej_filas <- base_t |> filter(!clave %in% filas_top$clave) |> group_by(nombre) |>
        summarise(v = sum(mill)) |> arrange(desc(v)) |> head(N_EJEMPLOS_OTROS) |> pull(nombre)
    ej_paises <- base_t |> filter(!pais %in% cols_top$pais) |> group_by(pais) |>
        summarise(v = sum(mill)) |> arrange(desc(v)) |> head(N_EJEMPLOS_OTROS) |> pull(pais) |> nombre_pais()
    cifras[[length(cifras) + 1]] <- bind_rows(
        fila_cifra(sec$seccion, sl, glue("total_{U}_periodo"), round(gran_total, 0),
                   glue("Total {TX$verbo} del 1 de enero al {fecha_corta(FECHA_CORTE)} ({TX$unidad_larga})")),
        fila_cifra(sec$seccion, sl, "pie_otros_filas", n_otras_filas,
                   if (n_otras_filas > 0) glue("\"{otros_lb}\" contempla {mas(n_otras_filas) |> str_remove(' m\u00e1s')}, ",
                                               "incluyendo {paste(str_to_lower(ej_filas), collapse = ', ')}, etc.") else NA),
        fila_cifra(sec$seccion, sl, "pie_otros_paises", n_distinct(base_t$pais) - nrow(cols_top),
                   glue("Se contemplan {n_distinct(base_t$pais) - nrow(cols_top)} pa\u00edses en \"Otros\", ",
                        "incluyendo a {unir_lista(ej_paises)}.")),
        fila_cifra(sec$seccion, sl, "pie_F_C", NA,
                   glue("F: % que el pa\u00eds representa sobre el total {art_del}. ",
                        "C: % que {if (sing %in% c('partida', 'subpartida')) 'la' else 'el'} {sing} representa sobre el total del pa\u00eds.")),
        fila_cifra(sec$seccion, sl, "nota", NA,
                   glue("Nota: {TX$nota_valor} Corte al {fecha_corta(FECHA_CORTE)}."))
    )

    cat(green(glue("  PD {ANIO_COMP}: {fmt_num(pd_comp, 1)} {TX$unidad}/d\u00eda ({fmt_pct_signo(var_prom, 2)} vs promedio ",
                   "{ANIO_INICIO} a {ANIO_COMP - 1}); temporales {fmt_pct(pct_temp, 1)}\n", .trim = FALSE)))
    cat(green(glue("  {texto_caida}\n  {texto_pais}\n", .trim = FALSE)))

    list(cifras = bind_rows(cifras), manifiesto = bind_rows(manif))
}

# SCRIPT ______________________________________________________________________

# Textos de unidades segun MONEDA
UNI <- if (MONEDA == "usd") {
    list(unidad = "MDD", unidad_min = "mdd", unidad_larga = "MDD corrientes",
         eje_y = "Millones de d\u00f3lares corrientes",
         nota_imp = "Valor en aduana en d\u00f3lares corrientes, convertido con el tipo de cambio de cada operaci\u00f3n.",
         nota_exp = "Valor comercial en d\u00f3lares corrientes, convertido con el tipo de cambio de cada operaci\u00f3n.")
} else {
    list(unidad = "MDP", unidad_min = "mdp", unidad_larga = "MDP reales",
         eje_y = "Millones de pesos reales (enero de 2025)",
         nota_imp = glue("Valor en aduana en pesos reales, base {MES_BASE_TEXTO} (INPC)."),
         nota_exp = glue("Valor comercial en pesos reales, base {MES_BASE_TEXTO} (INPC)."))
}

U <- tolower(UNI$unidad)   # sufijo de los nombres de salida: "mdd" o "mdp"

TEXTOS_FLUJO <- list(
    importaciones = list(Sust = "Importaciones", sing = "importaci\u00f3n", verbo = "importado",
                         unidad = UNI$unidad, unidad_min = UNI$unidad_min, unidad_larga = UNI$unidad_larga,
                         eje_y = UNI$eje_y,
                         nota_valor = UNI$nota_imp,
                         cobertura_tipos = "importaciones definitivas (A1 y C1) y temporales (IN y AF, IMMEX)"),
    exportaciones = list(Sust = "Exportaciones", sing = "exportaci\u00f3n", verbo = "exportado",
                         unidad = UNI$unidad, unidad_min = UNI$unidad_min, unidad_larga = UNI$unidad_larga,
                         eje_y = UNI$eje_y,
                         nota_valor = UNI$nota_exp,
                         cobertura_tipos = "exportaciones definitivas (A1 y C1) y retornos de temporales (RT, IMMEX)")
)

eventos <- EVENTOS |>
    mutate(x_anual = year(fecha) + (yday(fecha) - 1) / ifelse(leap_year(fecha), 366, 365))

for (FLUJO in FLUJOS) {
    cat(bold$magenta(glue("\n######## {str_to_upper(FLUJO)} ########\n", .trim = FALSE)))
    TX <- TEXTOS_FLUJO[[FLUJO]]
    CARPETA_TAB <- glue("output/slides_contexto/{SUFIJO}/{FLUJO}/tabulados/")
    CARPETA_FIG <- glue("slides_contexto/{SUFIJO}/{FLUJO}/figuras/")   # relativo a output/
    dir.create(CARPETA_TAB, recursive = TRUE, showWarnings = FALSE)

    preparar <- function(d) {
        d |>
            filter(anio %in% ANIOS) |>
            (\(x) if (MONEDA == "mxn") deflactar(x, inpc, MES_BASE) else x)() |>
            left_join(canasta, by = c("fraccion", "nico")) |>
            mutate(nivel   = coalesce(nivel, "4_resto_electronicos"),
                   eslabon = if_else(nivel == "4_resto_electronicos", "4_resto_electronicos", eslabon)) |>
            left_join(CAT_CAPITULOS, by = "capitulo") |>
            left_join(nombres_partida, by = "partida") |>
            left_join(NOMBRES_SUBPARTIDA_MANUAL, by = "subpartida") |>
            mutate(nombre_capitulo   = coalesce(nombre_capitulo, glue("Cap\u00edtulo {capitulo}")),
                   nombre_eslabon    = coalesce(unname(NOMBRES_ESLABON[eslabon]), eslabon),
                   nombre_subpartida = coalesce(nombre_subpartida, nombre_partida))
    }

    mensual <- leer_consulta_anam(RUTAS_CONSULTA[[FLUJO]], FLUJO, MONEDA) |> preparar()
    if (!is.na(RUTAS_TRUNCADO[[FLUJO]])) {
        truncado <- leer_consulta_anam(RUTAS_TRUNCADO[[FLUJO]], FLUJO, MONEDA) |> preparar()
    } else {
        # SUPUESTO (sin consulta truncada): la consulta es mensual y se toma
        # enero al mes de corte de cada ano, suponiendo que el mes de corte
        # registra hasta el dia de FECHA_CORTE en todos los anos.
        message("  SUPUESTO: sin consulta truncada; ", MESES_LARGO[month(FECHA_CORTE)],
                " de cada a\u00f1o se toma como registrado al ", day(FECHA_CORTE), ".")
        truncado <- mensual |> filter(mes <= month(FECHA_CORTE))
    }

    # Validacion de la ventana del truncado: cada ano, enero al mes de corte
    ventana <- truncado |> group_by(anio) |> summarise(mes_max = max(mes), n_meses = n_distinct(mes))
    if (any(ventana$mes_max != month(FECHA_CORTE) | ventana$n_meses != month(FECHA_CORTE)) ||
        !setequal(ventana$anio, ANIOS)) {
        print(ventana)
        stop("El truncado de ", FLUJO, " no trae enero a ", MESES_LARGO[month(FECHA_CORTE)],
             " en todos los a\u00f1os. Revisa FECHA_CORTE o el insumo.")
    }
    # El mes de corte del truncado debe ser menor o igual al mismo mes completo
    chk <- inner_join(
        truncado |> filter(mes == month(FECHA_CORTE)) |> group_by(anio) |> summarise(v_trunc = sum(valor)),
        mensual  |> filter(mes == month(FECHA_CORTE)) |> group_by(anio) |> summarise(v_comp = sum(valor)),
        by = "anio")
    if (any(chk$v_trunc > chk$v_comp * 1.001)) {
        print(chk)
        warning("En ", FLUJO, ", el mes de corte del truncado supera al mes completo en alg\u00fan a\u00f1o.")
    }

    ULTIMO_MES     <- max(mensual$fecha)
    MES_INCOMPLETO <- glue("{str_to_title(MESES_LARGO[month(ULTIMO_MES)])} {year(ULTIMO_MES)}")
    HAY_INCOMPLETO <- ULTIMO_MES >= floor_date(FECHA_CORTE, "month")   # el mes de corte sale incompleto en la serie


    resultados <- SECCIONES |> split(seq_len(nrow(SECCIONES))) |> map(procesar_seccion)
    cifras     <- map_dfr(resultados, "cifras")
    manifiesto <- map_dfr(resultados, "manifiesto")

    write_excel_csv(cifras, glue("{CARPETA_TAB}cifras_tarjetas_{SUFIJO}.csv"))
    write_csv(manifiesto, glue("{CARPETA_TAB}manifiesto_graficas.csv"))
    write_csv(eventos, glue("{CARPETA_TAB}eventos_graficas.csv"))
    writeLines(TX$eje_y, glue("{CARPETA_TAB}eje_y.txt"), useBytes = FALSE)
    cat(green$bold(glue("\nListo {FLUJO}: {nrow(manifiesto)} tabulados y {nrow(cifras)} cifras en {CARPETA_TAB}\n",
                        .trim = FALSE)))
}

cat(green$bold("\nSiguiente paso: scripts/slides_contexto/2_graficas_importaciones.R\n"))
