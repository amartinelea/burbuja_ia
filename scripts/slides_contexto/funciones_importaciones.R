# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/slides_contexto/funciones_importaciones.R
# Objetivo:       Funciones de 1_tabulados_importaciones.R y
#                 2_graficas_importaciones.R. Adapta las de
#                 proyecto_textiles_anam/scripts/funciones_importaciones.R a las
#                 consultas de electronicos de la ANAM (importaciones y
#                 exportaciones; mensuales, sin aduana).
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          19/11/2026
#
# _____________________________________________________________________________

# CONVENCION DE TIPOS (la misma de textiles y autopartes) _____________________
#   Importaciones: Definitivas = A1 y C1; Temporales = IN y AF (IMMEX: insumos
#     y activo fijo), igual que el modelo (02_prepara_panel_anam.R)
#   Exportaciones: Definitivas = A1 y C1; Temporales = RT (retorno de IMMEX)
#   El resto de claves (V1, V5, F4, G1, etc.) no entra: son transferencias
#   virtuales, cambios de regimen o regularizaciones, sin cruce fisico.
CLAVES <- list(
    importaciones = list(operacion = "1", definitivas = c("A1", "C1"), temporales = c("IN", "AF"),
                         valor = "valor_aduana"),
    exportaciones = list(operacion = "2", definitivas = c("A1", "C1"), temporales = "RT",
                         valor = "valor_comercial")
)

# LECTURA _____________________________________________________________________

#' Lee una consulta de electronicos de la ANAM y deja una fila por mes,
#' clave, fraccion, NICO y pais, con el valor corriente en la moneda pedida.
#'   moneda = "usd": cada linea se convierte a dolares con el tipo de cambio
#'     que reporta ANAM en esa operacion (igual que el modelo). "mxn": pesos.
#'   flujo = "importaciones": valor en aduana. La consulta trae una linea por
#'     contribucion (IVA, IGI, DTA...) y el valor se repite en cada una. Regla
#'     (la de 02_prepara_panel_anam.R): en cada celda se suman las lineas de
#'     cada contribucion y se toma el mayor de esos totales (casi siempre IVA).
#'   flujo = "exportaciones": valor comercial en pesos (la consulta no trae
#'     valor en aduana); ya viene una linea por celda.
leer_consulta_anam <- function(ruta, flujo, moneda = "usd") {
    if (!file.exists(ruta)) stop("No existe ", ruta)
    cf <- CLAVES[[flujo]]
    d <- data.table::fread(ruta, colClasses = "character", encoding = "UTF-8",
                           na.strings = c("", "NA", "NULL"))
    message("  ", basename(ruta), ": ", format(nrow(d), big.mark = ","), " lineas")

    d <- d[clave_tipo_operacion == cf$operacion]
    otras <- setdiff(unique(d$clave_documento), c(cf$definitivas, cf$temporales))
    message("  Claves fuera de la convencion (se excluyen): ", paste(sort(otras), collapse = ", "))
    d <- d[clave_documento %in% c(cf$definitivas, cf$temporales)]
    d[, valor := as.numeric(get(cf$valor))]
    if (moneda == "usd") {
        if (!"tipo_cambio" %in% names(d)) stop("La consulta no trae tipo_cambio; no se puede pasar a dolares")
        d[, valor := valor / as.numeric(tipo_cambio)]
    }
    uni <- if (moneda == "usd") "MDD" else "MDP"

    celda <- c("fecha", "clave_documento", "fraccion_arancelaria", "subdivision_fraccion_arancelaria",
               "clave_pais_origen_destino", "clave_unidad_medida_comercial")
    if ("clave_contribucion" %in% names(d)) {
        por_contrib <- d[, .(valor = sum(valor, na.rm = TRUE)), by = c(celda, "clave_contribucion")]
        celdas <- por_contrib[, .(valor = max(valor)), by = celda]
        message("  Valor sin deduplicar: ", format(round(sum(d$valor, na.rm = TRUE) / 1e6), big.mark = ","),
                " ", uni, "; deduplicado: ", format(round(sum(celdas$valor) / 1e6), big.mark = ","), " ", uni)
    } else {
        celdas <- d[, .(valor = sum(valor, na.rm = TRUE)), by = celda]
    }

    celdas[, .(valor = sum(valor)),
           by = .(fecha, clave_documento, fraccion_arancelaria, subdivision_fraccion_arancelaria,
                  clave_pais_origen_destino)] |>
        tibble::as_tibble() |>
        dplyr::transmute(
            fecha    = as.Date(paste0(stringr::str_sub(fecha, 1, 7), "-01")),
            fraccion = stringr::str_pad(fraccion_arancelaria, 8, "left", "0"),
            nico     = stringr::str_pad(subdivision_fraccion_arancelaria, 2, "left", "0"),
            capitulo = stringr::str_sub(fraccion, 1, 2),
            partida  = stringr::str_sub(fraccion, 1, 4),
            subpartida = stringr::str_sub(fraccion, 1, 6),
            clave_documento,
            pais     = stringr::str_trim(clave_pais_origen_destino),
            valor,
            tipo     = dplyr::if_else(clave_documento %in% cf$definitivas, "Definitivas", "Temporales"),
            anio     = lubridate::year(fecha),
            mes      = lubridate::month(fecha)
        )
}

#' INPC general (serie SP1) descargado en cada corrida de la API del SIE de
#' Banxico, con BANXICO_TOKEN del .env (nunca se escribe el token aqui).
#' Guarda una copia en `ruta_copia` para dejar registro de la serie usada.
leer_inpc <- function(ruta_copia = "input/macro/inpc_banxico_sp1.csv", desde = "2021-01-01") {
    token <- leer_env()["BANXICO_TOKEN"]
    if (is.na(token) || token == "") stop("Falta BANXICO_TOKEN en el .env del proyecto")
    url <- glue::glue("https://www.banxico.org.mx/SieAPIRest/service/v1/series/SP1/datos/{desde}/{Sys.Date()}")
    h <- curl::new_handle()
    curl::handle_setheaders(h, "Bmx-Token" = unname(token), "Accept" = "application/json",
                            "User-Agent" = "Ariadna Martinez amartinelea@gmail.com")
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code != 200) stop("Banxico respondio ", resp$status_code, " al pedir el INPC (SP1)")
    js <- jsonlite::fromJSON(rawToChar(resp$content))
    datos <- js$bmx$series$datos[[1]]
    inpc <- tibble::tibble(fecha = as.Date(datos$fecha, "%d/%m/%Y"),
                           inpc  = as.numeric(gsub(",", "", datos$dato))) |>
        dplyr::filter(!is.na(inpc))
    message("  INPC (Banxico SP1): ", format(min(inpc$fecha), "%Y-%m"), " a ", format(max(inpc$fecha), "%Y-%m"))
    dir.create(dirname(ruta_copia), recursive = TRUE, showWarnings = FALSE)
    readr::write_csv(inpc, ruta_copia)
    inpc
}

#' Pasa a pesos reales del mes base. Los meses sin INPC publicado usan el
#' ultimo disponible (se avisa).
deflactar <- function(d, inpc, mes_base) {
    base <- inpc$inpc[inpc$fecha == mes_base]
    if (length(base) != 1) stop("No hay INPC para el mes base ", mes_base)
    ult <- max(inpc$fecha)
    sin <- sort(unique(d$fecha[d$fecha > ult]))
    if (length(sin) > 0) message("  AVISO: sin INPC para ", paste(format(sin, "%Y-%m"), collapse = ", "),
                                 "; se usa el de ", format(ult, "%Y-%m"))
    d |>
        dplyr::mutate(fecha_inpc = pmin(fecha, ult)) |>
        dplyr::left_join(inpc, by = c("fecha_inpc" = "fecha")) |>
        dplyr::mutate(valor = valor * base / inpc) |>
        dplyr::select(-fecha_inpc, -inpc)
}

#' Lee variables de un .env tolerando espacios alrededor del "=".
leer_env <- function(ruta = ".env") {
    if (!file.exists(ruta)) return(character(0))
    lineas <- readLines(ruta, warn = FALSE, encoding = "UTF-8")
    lineas <- stringr::str_trim(lineas[stringr::str_detect(lineas, "^\\s*[A-Za-z_]+\\s*=")])
    setNames(stringr::str_remove_all(stringr::str_remove(lineas, "^[^=]+=\\s*"), "^[\"']|[\"']$"),
             stringr::str_trim(stringr::str_remove(lineas, "=.*$")))
}

#' Lee el catalogo LIGIE/NICO (nombre con acento: se copia a un temporal
#' porque readxl falla con rutas acentuadas en algunos equipos).
leer_catalogo_ligie <- function(dir_input = "input") {
    archivo <- list.files(dir_input, pattern = "LIGIE.*\\.xlsx$", full.names = TRUE)[1]
    if (is.na(archivo)) stop("No encontr\u00e9 el cat\u00e1logo LIGIE en ", dir_input)
    tmp <- tempfile(fileext = ".xlsx")
    file.copy(archivo, tmp, overwrite = TRUE)
    readxl::read_excel(tmp, col_types = "text")
}

#' Normaliza texto para comparar (sin acentos, minusculas, sin espacios extra).
normalizar <- function(x) stringr::str_squish(stringr::str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII")))

# NOMBRES _____________________________________________________________________

CAT_CAPITULOS <- tibble::tribble(
    ~capitulo, ~nombre_capitulo,
    "84", "M\u00e1quinas, aparatos mec\u00e1nicos y de c\u00f3mputo",
    "85", "M\u00e1quinas y aparatos el\u00e9ctricos y electr\u00f3nicos",
    "90", "Instrumentos de \u00f3ptica, medida y m\u00e9dicos",
    "91", "Relojes y sus partes",
    "92", "Instrumentos musicales",
    "73", "Manufacturas de hierro o acero",
    "87", "Veh\u00edculos y sus partes",
    "95", "Juguetes y art\u00edculos de deporte",
    "70", "Vidrio y sus manufacturas"
)

# Niveles y eslabones de la canasta de bienes de IA
NOMBRES_NIVEL <- c("1_nucleo" = "N\u00facleo", "2_infraestructura" = "Infraestructura",
                   "3_mixta" = "Mixta", "4_resto_electronicos" = "Resto de electr\u00f3nicos")
NOMBRES_ESLABON <- c("servidores" = "Servidores", "semiconductores" = "Semiconductores", "redes" = "Redes",
                     "almacenamiento" = "Almacenamiento", "energia_enfriamiento" = "Energ\u00eda y enfriamiento",
                     "componentes" = "Componentes", "computo_general" = "C\u00f3mputo general",
                     "4_resto_electronicos" = "Resto de electr\u00f3nicos")

# Subpartidas de la canasta (nombres de la slide de la canasta)
NOMBRES_SUBPARTIDA_MANUAL <- tibble::tribble(
    ~subpartida, ~nombre_subpartida,
    "847150", "Unidades de proceso (servidores)",
    "847170", "Unidades de memoria",
    "847180", "Otras unidades de c\u00f3mputo",
    "847330", "Partes de computadoras (tarjetas, GPU)",
    "854231", "Procesadores y controladores",
    "854232", "Memorias",
    "851762", "Equipo de redes",
    "850440", "Convertidores est\u00e1ticos (fuentes)",
    "850760", "Bater\u00edas de iones de litio",
    "853710", "Tableros el\u00e9ctricos",
    "854442", "Conductores el\u00e9ctricos con conectores",
    "854449", "Otros conductores el\u00e9ctricos",
    "854470", "Cables de fibra \u00f3ptica",
    "853670", "Conectores de fibra \u00f3ptica",
    "841510", "Aire acondicionado de pared o ventana",
    "841581", "Aire acondicionado con bomba de calor",
    "841582", "Otros equipos de aire acondicionado",
    "841583", "Aire acondicionado sin refrigeraci\u00f3n",
    "841950", "Intercambiadores de calor",
    "854233", "Amplificadores",
    "854239", "Otros circuitos integrados",
    "853400", "Circuitos impresos",
    "852351", "Almacenamiento de estado s\u00f3lido",
    "847141", "Computadoras",
    "847149", "Sistemas de c\u00f3mputo",
    "847160", "Unidades de entrada o salida"
)

#' Recorta una descripcion del catalogo a un nombre corto.
nombre_corto <- function(desc, max_car = 55) {
    x <- stringr::str_remove(stringr::str_squish(desc), "\\.$")
    x <- stringr::str_split_i(x, ";|:| \\(|, (excepto|incluso|aunque)", 1)
    stringr::str_trunc(stringr::str_squish(x), max_car, ellipsis = "...")
}

NOMBRES_PAIS_MANUAL <- c(
    USA = "Estados Unidos", AUS = "Australia", CHN = "China", VNM = "Vietnam", TWN = "Taiw\u00e1n",
    KOR = "Corea del Sur", JPN = "Jap\u00f3n", MYS = "Malasia", THA = "Tailandia",
    MEX = "M\u00e9xico", DEU = "Alemania", IND = "India", PHL = "Filipinas",
    SGP = "Singapur", CAN = "Canad\u00e1", ITA = "Italia", FRA = "Francia",
    GBR = "Reino Unido", ESP = "Espa\u00f1a", IDN = "Indonesia", HKG = "Hong Kong",
    IRL = "Irlanda", ISR = "Israel", NLD = "Pa\u00edses Bajos", CHE = "Suiza",
    CZE = "Chequia", HUN = "Hungr\u00eda", POL = "Polonia", BRA = "Brasil",
    AUT = "Austria", BEL = "B\u00e9lgica", SWE = "Suecia", CRI = "Costa Rica"
)

nombre_pais <- function(iso3) {
    nm <- unname(NOMBRES_PAIS_MANUAL[iso3])
    if (requireNamespace("countrycode", quietly = TRUE)) {
        cc <- suppressWarnings(countrycode::countrycode(iso3, "iso3c", "cldr.name.es"))
        nm <- dplyr::coalesce(nm, cc)
    }
    dplyr::coalesce(nm, iso3)
}

# FORMATOS Y TEXTOS ___________________________________________________________

MESES_ABR <- c("ene.", "feb.", "mar.", "abr.", "may.", "jun.",
               "jul.", "ago.", "sep.", "oct.", "nov.", "dic.")
MESES_LARGO <- c("enero", "febrero", "marzo", "abril", "mayo", "junio", "julio",
                 "agosto", "septiembre", "octubre", "noviembre", "diciembre")

fecha_corta <- function(f) paste(lubridate::day(f), MESES_ABR[lubridate::month(f)], lubridate::year(f))

fmt_num <- function(x, dig = 1) {
    stringr::str_trim(formatC(round(x, dig), format = "f", digits = dig, big.mark = ","))
}
fmt_pct <- function(x, dig = 1) paste0(fmt_num(x, dig), "%")
fmt_pct_signo <- function(x, dig = 1) paste0(ifelse(x > 0, "+", ""), fmt_num(x, dig), "%")

unir_lista <- function(x) {
    x <- as.character(x)
    if (length(x) <= 1) return(paste(x, collapse = ""))
    ultimo <- x[length(x)]
    conj <- if (stringr::str_detect(stringr::str_to_lower(ultimo), "^h?[i\u00ed]")) "e" else "y"
    paste0(paste(x[-length(x)], collapse = ", "), " ", conj, " ", ultimo)
}

#' Dias naturales del 1 de enero al dia/mes de corte para cada ano.
dias_ventana <- function(anios, fecha_corte) {
    tibble::tibble(anio = anios) |>
        dplyr::mutate(
            inicio = lubridate::make_date(anio, 1, 1),
            fin    = lubridate::make_date(anio, lubridate::month(fecha_corte), lubridate::day(fecha_corte)),
            dias   = as.numeric(fin - inicio) + 1
        )
}
