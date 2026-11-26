# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/01_construye_datos_empresas.R
# Objetivo:       Construye la base de empresas IA: clasificacion, capex trimestral (SEC EDGAR)
#                 y valuacion en bolsa mensual aproximado (Yahoo Finance + acciones de EDGAR).
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          13/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Arma todo lo de empresas: gasto de capital (SEC), valuacion en bolsa
#     (Yahoo) y variables macro.
#   - Es el primer script; necesita internet. Si ya se bajo hoy, en
#     00_correr_todo poner correr_descargas <- FALSE.
#   - La SEC pide un user agent con correo; usa el personal, no el
#     institucional.
#   - Si una empresa sale sin gasto de capital, revisar capex_diagnostico.csv
#     (a veces cambian la etiqueta en EDGAR).
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Entrada : empresas_ia_clasificacion.csv (largo: empresa x fraccion_canal)
#   Salidas : datos/empresas/
#     empresas_ia.csv               una fila por empresa: clasificacion + CIK
#     empresas_canal_mx.csv         largo: empresa x canal_mexico x fraccion_canal
#     capex_trimestral_empresa.csv  empresa x trimestre (USD millones)
#     precios_mensuales.csv         ticker x mes (precio ajustado en USD)
#     mcap_mensual_empresa.csv      empresa x mes (valuacion en bolsa aprox, USD millones)
#     mcap_indices_grupo.csv        indice mensual por indicadora (base 100)
#     ratios_financieros_trimestral.csv  deuda/ingresos, deuda/EBITDA, capex/OCF, etc.
#     macro_trimestral.csv          tasa 10 anios, fondos federales, diferencial BAA,
#                                   VIX, S&P 500 y tipo de cambio FIX (seccion 7)
#
#   Token de Banxico (solo para el tipo de cambio): en el .env del proyecto
# _____________________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999, timeout = 120)
# Paquetes: instala los que falten y los carga
paquetes <- c("dplyr", "tidyr", "purrr", "stringr", "readr", "lubridate",
               "httr", "jsonlite", "quantmod", "zoo")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Parametros -------------------------------------------------------------
dir_base   <- getwd()                                   # AJUSTAR si hace falta
archivo_emp <- file.path(dir_base, "input", "empresas_ia_clasificacion.csv")
dir_out    <- file.path(dir_base, "datos", "empresas")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

fecha_ini  <- as.Date("2012-01-01")
ua_sec     <- "Ariadna Martinez amartinelea@gmail.com"

indicadoras <- c("es_hyperscaler", "es_semiconductores", "es_ensamble_equipo",
                 "es_datacenter", "es_infraestructura", "es_inteligencia",
                 "es_startup", "canal_directo_mx")

# ---- Utilidades -------------------------------------------------------------
sec_get <- function(url) {
  r <- tryCatch(GET(url, add_headers(`User-Agent` = ua_sec)), error = function(e) NULL)
  Sys.sleep(0.15)                       # limite SEC: 10 solicitudes/segundo
  if (is.null(r) || status_code(r) != 200) return(NULL)
  fromJSON(content(r, as = "text", encoding = "UTF-8"))
}

moneda_ticker <- function(tk) {
  case_when(str_detect(tk, "\\.TW$") ~ "TWD",
            str_detect(tk, "\\.KS$") ~ "KRW",
            str_detect(tk, "\\.HK$") ~ "HKD",
            str_detect(tk, "\\.PA$") ~ "EUR",
            TRUE                     ~ "USD")
}

# =============================================================================
# 1. Empresas + CIK
# =============================================================================
# El archivo viene en formato largo: una fila por empresa x fraccion_canal.
# Se separa en (a) canales (largo) y (b) empresas (una fila por empresa).
emp_raw <- read_csv(archivo_emp, locale = locale(encoding = "UTF-8"),
                    col_types = cols(fraccion_canal = col_character(),
                                     ticker = col_character()),
                    show_col_types = FALSE)

canales <- emp_raw %>%
  transmute(empresa, ticker, grupo, canal_mexico,
            fraccion_canal = na_if(str_trim(fraccion_canal), ""),
            canal_verificado = as.logical(canal_verificado))
write_excel_csv(canales, file.path(dir_out, "empresas_canal_mx.csv"))

empresas <- emp_raw %>%
  select(-canal_mexico, -fraccion_canal, -canal_verificado) %>%
  distinct(empresa, .keep_all = TRUE) %>%
  mutate(across(all_of(c(indicadoras, "cotiza")), as.logical),
         ticker = na_if(str_trim(ticker), ""),
         moneda = moneda_ticker(coalesce(ticker, "")))

mapa_sec <- sec_get("https://www.sec.gov/files/company_tickers.json")
mapa_sec <- if (is.null(mapa_sec)) tibble(ticker = character(), cik = character()) else
  bind_rows(mapa_sec) %>%
  transmute(ticker = toupper(ticker), cik = str_pad(cik_str, 10, pad = "0")) %>%
  distinct(ticker, .keep_all = TRUE)

empresas <- empresas %>%
  left_join(mapa_sec, by = "ticker") %>%
  # Alphabet antes de 2015 reportaba como Google Inc
  mutate(cik_previo = if_else(ticker %in% "GOOGL", "0001288776", NA_character_))

write_excel_csv(empresas, file.path(dir_out, "empresas_ia.csv"))
message("Empresas con CIK en SEC: ", sum(!is.na(empresas$cik)), " de ", nrow(empresas))

# =============================================================================
# 2. Capex trimestral (EDGAR XBRL). Solo reportes en USD.
#    Cash flow viene acumulado en el anio fiscal: se desacumula por fecha inicio.
# =============================================================================
tags_capex <- list(
  c("us-gaap",   "PaymentsToAcquirePropertyPlantAndEquipment"),
  c("us-gaap",   "PaymentsToAcquireProductiveAssets"),
  c("us-gaap",   "PaymentsToAcquireOtherPropertyPlantAndEquipment"),
  c("us-gaap",   "PaymentsForCapitalImprovements"),
  c("ifrs-full", "PurchaseOfPropertyPlantAndEquipmentClassifiedAsInvestingActivities"),
  # REITs de centros de datos (Digital Realty): reportan la inversion como
  # desarrollo de bienes raices. Van al final: solo se usan si la empresa no
  # reporta las etiquetas de PP&E en ese periodo.
  c("us-gaap",   "PaymentsToDevelopRealEstateAssets"),
  c("us-gaap",   "PaymentsToAcquireAndDevelopRealEstate"),
  c("us-gaap",   "PaymentsForConstructionInProcess"))

# Las empresas cambian de etiqueta con los anios (ej. Amazon despues de 2017,
# Nvidia despues de 2020): se bajan TODAS las etiquetas y se combinan.
# Si un mismo periodo aparece en varias, gana la etiqueta con menor prioridad.
baja_capex_cik <- function(cik) {
  map_dfr(seq_along(tags_capex), function(i) {
    tg <- tags_capex[[i]]
    j <- sec_get(sprintf("https://data.sec.gov/api/xbrl/companyconcept/CIK%s/%s/%s.json",
                         cik, tg[1], tg[2]))
    if (is.null(j) || is.null(j$units$USD)) return(NULL)
    as_tibble(j$units$USD) %>% mutate(tag = tg[2], prioridad = i)
  })
}

desacumula <- function(raw) {
  raw <- raw %>%
    filter(form %in% c("10-Q", "10-K", "20-F", "10-Q/A", "10-K/A"), !is.na(start)) %>%
    mutate(start = ymd(start), end = ymd(end), dias = as.numeric(end - start)) %>%
    filter(dias > 60, dias < 380)
  # Solo acumulados que empiezan al inicio del anio fiscal. Las empresas tambien
  # reportan periodos de 12 meses moviles u otros que empiezan a media anio
  # (Amazon): si entran, la resta da trimestres inflados. Los inicios fiscales
  # salen de los anuales del 17-K/20-F; el anio en curso empieza el dia
  # siguiente al cierre del ultimo anual.
  anuales <- raw %>% filter(form %in% c("10-K", "20-F", "10-K/A"), dias > 350)
  if (nrow(anuales) > 0) {
    # Tolerancia de 7 dias: con anios fiscales de 52/53 semanas (Nvidia) el
    # inicio puede variar unos dias entre el 17-K y los 10-Q
    inicios_fy <- unique(c(anuales$start, max(anuales$end) + 1))
    cerca_fy <- vapply(as.numeric(raw$start), function(d) any(abs(d - as.numeric(inicios_fy)) <= 7), logical(1))
    raw <- raw[cerca_fy, ]
  }
  raw %>%
    # 10-Q con ~12 meses = acumulado de 12 meses moviles (Amazon los reporta);
    # NO es acumulado del anio fiscal y rompe la resta. Se descartan.
    filter(!(str_starts(form, "10-Q") & dias > 300)) %>%
    arrange(prioridad, desc(filed)) %>%          # etiqueta preferida, cifra mas reciente
    distinct(start, end, .keep_all = TRUE) %>%
    arrange(start, end) %>%
    group_by(start) %>%
    mutate(fin_previo = lag(end, default = first(start) - 1),
           capex_q = val - lag(val, default = 0),
           dias_q = as.numeric(end - fin_previo)) %>%
    ungroup() %>%
    # Solo trimestres reales: si falta un acumulado intermedio, la resta junta
    # dos o mas trimestres (picos falsos en Intel, Nvidia). Se dejan como hueco.
    filter(dias_q >= 80, dias_q <= 100) %>%
    transmute(fin_periodo = end, capex_q) %>%
    distinct(fin_periodo, .keep_all = TRUE)
}

capex_empresa <- empresas %>%
  filter(!is.na(cik)) %>%
  select(empresa, ticker, cik, cik_previo) %>%
  mutate(datos = map2(cik, cik_previo, function(c1, c0) {
    r1 <- baja_capex_cik(c1)
    r0 <- if (!is.na(c0)) baja_capex_cik(c0) else NULL
    raw <- bind_rows(r1, r0)
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    desacumula(raw)
  })) %>%
  select(-cik_previo) %>%
  unnest(datos) %>%
  mutate(trimestre = floor_date(fin_periodo, "quarter"),
         capex_musd = capex_q / 1e6) %>%
  filter(trimestre >= fecha_ini, capex_musd > 0) %>%
  group_by(empresa, ticker, trimestre) %>%
  summarise(capex_musd = sum(capex_musd), .groups = "drop")

write_excel_csv(capex_empresa, file.path(dir_out, "capex_trimestral_empresa.csv"))

# Diagnostico: trimestres faltantes entre desde y hasta. Huecos > 0 = la
# empresa reporta anual (20-F, extranjeras) o cambio a etiqueta no cubierta.
diag_capex <- capex_empresa %>%
  group_by(empresa) %>%
  summarise(desde = min(trimestre), hasta = max(trimestre), n = n(), .groups = "drop") %>%
  mutate(esperados = as.integer(round(time_length(interval(desde, hasta), "month") / 3)) + 1,
         huecos = esperados - n) %>%
  arrange(desc(huecos))
print(diag_capex, n = Inf)
write_excel_csv(diag_capex, file.path(dir_out, "capex_diagnostico.csv"))
sin_capex <- setdiff(empresas$empresa[!is.na(empresas$cik)], capex_empresa$empresa)
if (length(sin_capex)) message("Empresas con CIK pero SIN capex en EDGAR (etiqueta propia o reportan en otra moneda): ",
                               paste(sin_capex, collapse = ", "))

# =============================================================================
# 3. Precios mensuales (Yahoo) convertidos a USD
# =============================================================================
baja_precio <- function(tk) {
  x <- tryCatch(getSymbols(tk, src = "yahoo", from = fecha_ini,
                           auto.assign = FALSE, warnings = FALSE),
                error = function(e) NULL)
  if (is.null(x)) { message("Sin precios: ", tk); return(NULL) }
  m <- to.monthly(Ad(x), indexAt = "firstof", OHLC = FALSE)
  tibble(mes = as.Date(index(m)), precio = as.numeric(m))
}

tickers <- empresas %>% filter(!is.na(ticker)) %>% distinct(ticker, moneda)

fx <- tibble(moneda = setdiff(unique(tickers$moneda), "USD")) %>%
  mutate(datos = map(moneda, ~ baja_precio(paste0(.x, "USD=X")))) %>%
  unnest(datos) %>%
  rename(usd_por_unidad = precio)

precios <- tickers %>%
  mutate(datos = map(ticker, baja_precio)) %>%
  unnest(datos) %>%
  left_join(fx, by = c("moneda", "mes")) %>%
  mutate(usd_por_unidad = if_else(moneda == "USD", 1, usd_por_unidad),
         precio_usd = precio * usd_por_unidad) %>%
  filter(!is.na(precio_usd))

write_excel_csv(precios, file.path(dir_out, "precios_mensuales.csv"))

# =============================================================================
# 4. Market cap aproximado = acciones actuales (EDGAR) x precio ajustado USD
#    El precio ajustado ya corrige splits, asi que se usan las acciones MAS
#    RECIENTES para toda la serie. Ignora recompras/emisiones: es aproximado.
#    Sin dato en EDGAR (extranjeras): llenar columna acciones_millones a mano
#    en empresas_ia_clasificacion.csv si se quiere incluir en el ponderado.
# =============================================================================
# 1) dei:EntityCommonStockSharesOutstanding (portada del 17-Q/10-K).
#    Empresas con varias clases (Alphabet, Meta, Dell, Palantir, CoreWeave)
#    lo reportan por clase con dimension y la API no lo trae.
# 2) Respaldo: us-gaap:WeightedAverageNumberOfDilutedSharesOutstanding del
#    ultimo trimestre, que es el TOTAL de todas las clases.
acciones_cik <- function(cik) {
  ult_valor <- function(j, unidad) {
    if (is.null(j) || is.null(j$units[[unidad]])) return(NA_real_)
    s <- as_tibble(j$units[[unidad]]) %>% mutate(end = ymd(end))
    if (!"start" %in% names(s)) s$start <- NA_character_
    s <- s %>% mutate(dias = as.numeric(end - ymd(start)))
    s <- s %>% filter(is.na(dias) | dias < 100)      # trimestral si hay rango
    if (nrow(s) == 0) return(NA_real_)
    ult <- s %>% filter(end == max(end)) %>% arrange(desc(filed)) %>% slice(1)
    ult$val / 1e6
  }
  a <- ult_valor(sec_get(sprintf("https://data.sec.gov/api/xbrl/companyconcept/CIK%s/dei/EntityCommonStockSharesOutstanding.json", cik)), "shares")
  if (is.na(a)) a <- ult_valor(sec_get(sprintf("https://data.sec.gov/api/xbrl/companyconcept/CIK%s/us-gaap/WeightedAverageNumberOfDilutedSharesOutstanding.json", cik)), "shares")
  a
}

# ADRs: el precio de Yahoo es por ADR, las acciones de EDGAR son ordinarias.
# acciones_adr = ordinarias / ordinarias_por_adr. TSMC: 1 ADR = 5 ordinarias.
ordinarias_por_adr <- c(TSM = 5)

empresas <- empresas %>%
  mutate(acciones_edgar = map_dbl(cik, ~ if (is.na(.x)) NA_real_ else acciones_cik(.x)),
         acciones_edgar = acciones_edgar /
           coalesce(unname(ordinarias_por_adr[ticker]), 1))
if (!"acciones_millones" %in% names(empresas)) empresas$acciones_millones <- NA_real_
empresas <- empresas %>%
  mutate(acciones_millones = coalesce(as.numeric(acciones_millones), acciones_edgar))
message("Empresas cotizadas sin acciones (no entran al indice ponderado): ",
        paste(empresas$empresa[!is.na(empresas$ticker) & is.na(empresas$acciones_millones)],
              collapse = ", "))
write_excel_csv(empresas, file.path(dir_out, "empresas_ia.csv"))

mcap <- precios %>%
  inner_join(empresas %>% select(empresa, ticker, grupo, all_of(indicadoras),
                                 acciones_millones),
             by = "ticker") %>%
  mutate(mcap_musd = precio_usd * acciones_millones)

write_excel_csv(mcap, file.path(dir_out, "mcap_mensual_empresa.csv"))

# =============================================================================
# 5. Indices mensuales por indicadora (base 100 = ene 2023)
#    ponderado: suma de valuacion en bolsa (solo empresas con acciones)
#    equiponderado: promedio de rendimientos log (todas con precio)
# =============================================================================
mes_base <- as.Date("2023-01-01")

# Indice ENCADENADO: cada mes solo usa empresas presentes ese mes y el anterior,
# asi la entrada de una empresa (ej. CoreWeave sale a bolsa en 2025) no
# produce un salto. Ponderado por valuacion en bolsa del mes anterior.
# Base = mes_base, o el primer mes del grupo si este empieza despues.
normaliza <- function(nivel, mes) {
  b <- nivel[mes == mes_base]
  if (length(b) == 0 || is.na(b)) b <- nivel[which(!is.na(nivel))[1]]
  100 * nivel / b
}

indice_grupo <- function(ind) {
  d <- mcap %>% filter(.data[[ind]])
  if (nrow(d) == 0) return(NULL)
  d <- d %>% group_by(ticker) %>% arrange(mes) %>%
    mutate(r = log(precio_usd) - lag(log(precio_usd)),
           w = lag(mcap_musd)) %>%
    ungroup()
  d %>%
    group_by(mes) %>%
    summarise(mcap_musd = sum(mcap_musd, na.rm = TRUE),
              n_empresas = sum(!is.na(precio_usd)),
              r_pond = if (any(!is.na(r) & !is.na(w))) weighted.mean(r[!is.na(r) & !is.na(w)], w[!is.na(r) & !is.na(w)]) else NA_real_,
              r_equi = if (any(!is.na(r))) mean(r, na.rm = TRUE) else NA_real_,
              .groups = "drop") %>%
    arrange(mes) %>%
    mutate(indicadora = ind,
           idx_ponderado     = normaliza(exp(cumsum(replace_na(r_pond, 0))), mes),
           idx_equiponderado = normaliza(exp(cumsum(replace_na(r_equi, 0))), mes)) %>%
    select(indicadora, mes, n_empresas, mcap_musd, idx_ponderado, idx_equiponderado)
}

indices <- map_dfr(indicadoras, indice_grupo)
write_excel_csv(indices, file.path(dir_out, "mcap_indices_grupo.csv"))


# =============================================================================
# 6. Ratios financieros trimestrales (EDGAR XBRL)
#    Los estados financieros NO reportan ratios: se calculan con sus partidas.
#    Flujos (ingresos, flujo operativo, utilidad operativa, depreciacion):
#      se desacumulan igual que el capex y se suman 4 trimestres (TTM).
#    Saldos (deuda): valor al cierre de cada trimestre.
#    Ratios (todos con TTM en el denominador o numerador de flujo):
#      deuda_ingresos = deuda / ingresos TTM          (lo pidieron)
#      deuda_ebitda   = deuda / EBITDA TTM            (apalancamiento clasico)
#      capex_ocf      = capex TTM / flujo operativo TTM (BofA: 50% en 2023, ~95% en 2026)
#      capex_ingresos = capex TTM / ingresos TTM
#      fcf_ingresos   = (flujo operativo - capex) TTM / ingresos TTM
#    EBITDA = utilidad operativa + depreciacion y amortizacion (no es partida GAAP).
#    Deuda = deuda de largo plazo (total o corriente + no corriente) + deuda de
#    corto plazo + papel comercial. NO incluye arrendamientos operativos.
# =============================================================================
conceptos_flujo <- list(
  ingresos  = c("Revenues", "RevenueFromContractWithCustomerExcludingAssessedTax",
                "SalesRevenueNet"),
  ocf       = c("NetCashProvidedByUsedInOperatingActivities"),
  util_oper = c("OperatingIncomeLoss"),
  dya       = c("DepreciationDepletionAndAmortization", "DepreciationAndAmortization",
                "DepreciationAmortizationAndAccretionNet", "Depreciation"))
# Cada concepto prueba varias etiquetas en orden de preferencia; por trimestre
# se queda la primera que exista. Las empresas no usan todas la misma (Oracle,
# CoreWeave y Applied Digital no reportan LongTermDebt).
conceptos_saldo <- list(
  deuda_lp_total = c("LongTermDebt",
                     "LongTermDebtAndCapitalLeaseObligationsIncludingCurrentMaturities",
                     "DebtLongtermAndShorttermCombinedAmount",
                     "DebtInstrumentCarryingAmount"),
  deuda_lp_nc    = c("LongTermDebtNoncurrent", "LongTermDebtAndCapitalLeaseObligations",
                     "LongTermNotesPayable", "SeniorNotes", "ConvertibleNotesPayable",
                     "OtherLongTermDebtNoncurrent"),
  # DebtCurrent va al final: puede incluir prestamos de corto plazo y papel
  # comercial; si la empresa tambien reporta esos, se cuentan dos veces.
  deuda_lp_c     = c("LongTermDebtCurrent", "LongTermDebtAndCapitalLeaseObligationsCurrent",
                     "NotesPayableCurrent", "DebtCurrent"),
  deuda_cp       = c("ShortTermBorrowings"),
  papel_com      = c("CommercialPaper"))

baja_tags <- function(cik, tags, unidad = "USD") {
  map_dfr(seq_along(tags), function(i) {
    j <- sec_get(sprintf("https://data.sec.gov/api/xbrl/companyconcept/CIK%s/us-gaap/%s.json",
                         cik, tags[i]))
    if (is.null(j) || is.null(j$units[[unidad]])) return(NULL)
    as_tibble(j$units[[unidad]]) %>% mutate(tag = tags[i], prioridad = i)
  })
}

flujo_trimestral <- function(cik, tags) {
  raw <- baja_tags(cik, tags)
  if (nrow(raw) == 0) return(NULL)
  raw <- desacumula(raw)
  if (nrow(raw) == 0) return(NULL)
  raw %>%
    mutate(trimestre = floor_date(fin_periodo, "quarter")) %>%
    group_by(trimestre) %>% summarise(valor = sum(capex_q), .groups = "drop")
}

saldo_trimestral <- function(cik, tags) {
  raw <- baja_tags(cik, tags)
  if (nrow(raw) == 0) return(NULL)
  raw %>%
    filter(form %in% c("10-Q", "10-K", "10-Q/A", "10-K/A")) %>%
    mutate(end = ymd(end), trimestre = floor_date(end, "quarter")) %>%
    arrange(prioridad, desc(filed)) %>%
    group_by(trimestre) %>% slice(1) %>% ungroup() %>%
    transmute(trimestre, valor = val)
}

emp_ratios <- empresas %>%
  filter(!is.na(cik), es_hyperscaler | es_datacenter) %>%
  select(empresa, ticker, cik)

partidas <- emp_ratios %>%
  mutate(datos = map(cik, function(ck) {
    # Si una empresa no reporta un concepto (ej. ShortTermBorrowings), queda vacio
    etiqueta <- function(d, nombre) if (is.null(d) || nrow(d) == 0) NULL else mutate(d, concepto = nombre)
    f <- imap(conceptos_flujo, ~ etiqueta(flujo_trimestral(ck, .x), .y)) %>% bind_rows()
    s <- imap(conceptos_saldo, ~ etiqueta(saldo_trimestral(ck, .x), .y)) %>% bind_rows()
    bind_rows(f, s)
  })) %>%
  select(-cik) %>%
  unnest(datos) %>%
  filter(trimestre >= fecha_ini) %>%
  pivot_wider(names_from = concepto, values_from = valor)

for (v in c(names(conceptos_flujo), names(conceptos_saldo)))
  if (!v %in% names(partidas)) partidas[[v]] <- NA_real_

ratios <- partidas %>%
  left_join(capex_empresa %>% select(empresa, trimestre, capex_musd), by = c("empresa", "trimestre")) %>%
  arrange(empresa, trimestre) %>%
  group_by(empresa) %>%
  complete(trimestre = seq(min(trimestre), max(trimestre), by = "quarter")) %>%
  fill(ticker, .direction = "downup") %>%
  mutate(
    capex   = capex_musd * 1e6,
    ebitda  = util_oper + dya,
    ing_ttm   = zoo::rollsumr(ingresos, 4, fill = NA),
    ocf_ttm   = zoo::rollsumr(ocf,      4, fill = NA),
    ebitda_ttm= zoo::rollsumr(ebitda,   4, fill = NA),
    capex_ttm = zoo::rollsumr(capex,    4, fill = NA),
    # Sin la parte de largo plazo no se arma la deuda: sumar solo la porcion
    # corriente la subestima (Oracle salia con deuda/ingresos de 0.11)
    deuda_lp_suma = if_else(is.na(deuda_lp_nc), NA_real_,
                            deuda_lp_nc + coalesce(deuda_lp_c, 0)),
    deuda = coalesce(deuda_lp_total, deuda_lp_suma) +
            coalesce(deuda_cp, 0) + coalesce(papel_com, 0),
    deuda_ingresos = deuda / ing_ttm,
    # Con EBITDA o flujo operativo <= 0 el ratio no tiene interpretacion: NA
    deuda_ebitda   = if_else(ebitda_ttm > 0, deuda / ebitda_ttm, NA_real_),
    capex_ocf      = if_else(ocf_ttm > 0, capex_ttm / ocf_ttm, NA_real_),
    capex_ingresos = capex_ttm / ing_ttm,
    fcf_ingresos   = (ocf_ttm - capex_ttm) / ing_ttm) %>%
  ungroup()

write_excel_csv(ratios, file.path(dir_out, "ratios_financieros_trimestral.csv"))
# Diagnostico: en cuantos de los ultimos 8 trimestres hay dato de cada partida
diag_ratios <- ratios %>% group_by(empresa) %>% slice_max(trimestre, n = 8) %>%
  summarise(across(c(ingresos, ocf, ebitda, deuda, capex_ocf, deuda_ingresos), ~ sum(!is.na(.x))),
            .groups = "drop")
cat("\nTrimestres con dato (de los ultimos 8) por partida:\n"); print(diag_ratios, n = Inf)
write_excel_csv(diag_ratios, file.path(dir_out, "ratios_diagnostico.csv"))
cat("\nRatios del ultimo trimestre disponible por empresa:\n")
print(ratios %>% group_by(empresa) %>% filter(!is.na(capex_ocf)) %>% slice_max(trimestre, n = 1) %>%
        select(empresa, trimestre, deuda_ingresos, deuda_ebitda, capex_ocf, capex_ingresos) %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))), n = Inf)

# ---- Ingresos de empresas de semiconductores (variable alternativa del paso 2, script 15) ----
# Nvidia, AMD, Broadcom, etc.: sus ventas son la demanda de hardware de IA ya
# realizada. Mismas reglas de desacumulacion que el capex. Las que reportan en
# IFRS (20-F, ej. TSMC o ASML) no tienen estas etiquetas y quedan fuera.
emp_semiconductores <- empresas %>% filter(!is.na(cik), es_semiconductores) %>% select(empresa, ticker, cik)
ingresos_semiconductores <- map_dfr(seq_len(nrow(emp_semiconductores)), function(k) {
  d <- tryCatch(flujo_trimestral(emp_semiconductores$cik[k], conceptos_flujo$ingresos), error = function(e) NULL)
  if (is.null(d) || nrow(d) == 0) return(NULL)
  d %>% transmute(empresa = emp_semiconductores$empresa[k], ticker = emp_semiconductores$ticker[k],
                  trimestre, ingresos_musd = valor / 1e6)
})
if (nrow(ingresos_semiconductores) > 0) {
  ingresos_semiconductores <- ingresos_semiconductores %>% filter(trimestre >= fecha_ini, ingresos_musd > 0)
  write_excel_csv(ingresos_semiconductores, file.path(dir_out, "ingresos_trimestral_semiconductores.csv"))
  cat("\nIngresos de semiconductores: trimestres con dato por empresa\n")
  print(ingresos_semiconductores %>% count(empresa, name = "trimestres"), n = Inf)
} else message("No se obtuvieron ingresos de empresas de semiconductores")

message("Listo. Archivos en: ", dir_out)

# =============================================================================
# 7. Variables macro trimestrales (controles del paso 1 y del paso 2)
#    FRED no pide clave. Banxico pide token: se lee del .env del proyecto
#    (BANXICO_TOKEN=tu_token). No escribirlo en el script.
#    Si una descarga falla, el script busca input/macro/ o input/raw/<serie>.csv con dos
#    columnas (fecha, valor), que se puede bajar a mano de la pagina de la fuente.
#    tasa10      : bono del Tesoro a 10 anios (FRED DGS10), %
#    fedfunds    : tasa de fondos federales efectiva (FRED FEDFUNDS), %
#    spread_baa  : diferencial BAA corporativo vs bono a 10 anios (FRED BAA10Y), pp
#    vix         : volatilidad implicita del S&P 500 (FRED VIXCLS)
#    sp500       : S&P 500 (Yahoo ^GSPC), nivel
#    tc_fix      : tipo de cambio FIX pesos por dolar (Banxico SIE SF43718)
# =============================================================================
lee_local <- function(id) {
  f <- file.path(dir_base, "input", c("macro", "raw"), paste0(id, ".csv"))
  f <- f[file.exists(f)][1]
  if (is.na(f)) { message("  tampoco esta input/macro/ ni input/raw/", id, ".csv"); return(NULL) }
  message("  leo ", f)
  d <- read_csv(f, show_col_types = FALSE, na = c(".", "", "N/E"),
                locale = locale(encoding = "UTF-8"))
  names(d)[1:2] <- c("fecha", "valor")
  d %>% transmute(fecha = as.Date(fecha), valor = as.numeric(valor))
}

baja_fred <- function(id) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id,
                "&cosd=", fecha_ini)
  # HTTP/1.1: con HTTP/2 FRED corta la conexion (INTERNAL_ERROR) en algunos equipos
  r <- tryCatch(GET(url, config(http_version = 2), timeout(60)), error = function(e) NULL)
  d <- if (!is.null(r) && status_code(r) == 200)
    tryCatch(read_csv(content(r, as = "text", encoding = "UTF-8"), show_col_types = FALSE,
                      na = c(".", "")), error = function(e) NULL) else NULL
  if (is.null(d)) { message("FRED ", id, ": fallo la descarga; busco archivo local"); return(lee_local(id)) }
  names(d)[1:2] <- c("fecha", "valor")
  d %>% transmute(fecha = as.Date(fecha), valor = as.numeric(valor))
}

baja_banxico <- function(id) {
  token <- Sys.getenv("BANXICO_TOKEN")
  if (token == "") { message("Sin BANXICO_TOKEN; busco input/macro/", id, ".csv"); return(lee_local(id)) }
  url <- sprintf("https://www.banxico.org.mx/SieAPIRest/service/v1/series/%s/datos/%s/%s",
                 id, fecha_ini, Sys.Date())
  r <- tryCatch(GET(url, add_headers(`Bmx-Token` = token, Accept = "application/json"), timeout(60)),
                error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) {
    message("Banxico ", id, ": fallo la descarga; busco input/macro/"); return(lee_local(id))
  }
  x <- fromJSON(content(r, as = "text", encoding = "UTF-8"))$bmx$series$datos[[1]]
  tibble(fecha = dmy(x$fecha), valor = suppressWarnings(as.numeric(gsub(",", "", x$dato))))
}

baja_yahoo_nivel <- function(tk) {
  x <- tryCatch(getSymbols(tk, src = "yahoo", from = fecha_ini, auto.assign = FALSE),
                error = function(e) NULL)
  if (is.null(x)) { message("Yahoo ", tk, ": fallo; busco input/macro/"); return(lee_local(gsub("\\^", "", tk))) }
  tibble(fecha = index(x), valor = as.numeric(Ad(x)))
}

# Tokens (BANXICO_TOKEN, INEGI_TOKEN) desde el .env del proyecto, formato CLAVE=valor
archivo_env <- file.path(dir_base, ".env")
if (file.exists(archivo_env)) readRenviron(archivo_env)

series_macro <- list(
  tasa10     = baja_fred("DGS10"),
  fedfunds   = baja_fred("FEDFUNDS"),
  spread_baa = baja_fred("BAA10Y"),
  vix        = baja_fred("VIXCLS"),
  sp500      = baja_yahoo_nivel("^GSPC"),
  tc_fix     = baja_banxico("SF43718"))

faltan_macro <- names(series_macro)[map_lgl(series_macro, ~ is.null(.x) || nrow(.x) == 0)]
if (length(faltan_macro)) warning("Series macro no disponibles: ", paste(faltan_macro, collapse = ", "))

macro_trim <- series_macro[!names(series_macro) %in% faltan_macro] %>%
  imap_dfr(~ .x %>% mutate(serie = .y)) %>%
  filter(!is.na(valor)) %>%
  mutate(trimestre = floor_date(fecha, "quarter")) %>%
  group_by(serie, trimestre) %>%
  summarise(valor = mean(valor), .groups = "drop") %>%      # promedio del trimestre
  pivot_wider(names_from = serie, values_from = valor) %>%
  arrange(trimestre)

write_excel_csv(macro_trim, file.path(dir_out, "macro_trimestral.csv"))
cat("\nUltimos trimestres de las variables macro:\n")
print(tail(macro_trim, 6))
message("Listo. Variables macro en: ", file.path(dir_out, "macro_trimestral.csv"))
