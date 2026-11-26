# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/02_prepara_panel_anam.R
# Objetivo:       Limpia el panel de importaciones de la ANAM y lo deja listo para el modelo.
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
#   - Deja limpia la consulta de importaciones de la ANAM; todo el modelo de
#     importaciones sale de aqui.
#   - Correr cada vez que llegue una consulta nueva de la ANAM
#     (input/raw/consulta_electro_imp.csv).
#   - Quita el mes en curso segun la fecha de la computadora: si se corre un
#     mes despues, revisar que el ultimo mes este completo.
#   - Para validar, comparar 02_valor_anual_para_validar.csv contra Data Mexico
#     (diagnostico_data_mexico_2024.R).
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Problemas que resuelve:
#     0. El valor se toma de valor_aduana (pesos) convertido a dolares con el
#        tipo_cambio de la linea; en adelante "valor_dolares" = valor en aduana en USD.
#     1. Cada celda (mes, clave, fraccion, NICO, pais, unidad) aparece una vez por
#        contribucion (IVA, IGI, DTA...), con el valor repetido. Sumar
#        valor_dolares directo lo duplica.
#     2. Las claves de pedimento mezclan entradas fisicas con operaciones
#        virtuales y cambios de regimen que vuelven a contar la misma mercancia.
#        La clasificacion esta en input/claves_pedimento.csv (Anexo 22, apendice 2).
#     3. La fraccion viene como entero y la subdivision (NICO) sin ceros.
#
#   Entradas:  input/raw/<panel mensual de ANAM>.csv (fecha AAAA-MM)
#              input/Catalogo LIGIE 2025 NICO - 2026.11.10.xlsx
#              input/claves_pedimento.csv
#   Salidas (output/panel/):
#     00_diagnostico_panel.csv         filas, fechas, duplicacion
#     02_valor_anual_para_validar.csv  totales anuales para cruzar con INEGI
#     01_valor_por_clave.csv           valor por clave de pedimento y tipo de operacion
#     panel_fraccion_pais_mes.csv      valor en USD, flujo fisico, por NICO (fraccion + subdivision)-pais-mes
#     panel_eua_trimestre.csv          fraccion x trimestre con EUA (insumo del paso 2)
#     recaudacion_fraccion_mes.csv     pagos en efectivo por contribucion
#     arancel_fraccion_pais_mes.csv    tasa de IGI efectiva (control del paso 2)
# _____________________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

# Paquetes: instala los que falten y los carga
paquetes <- c("data.table", "readxl", "lubridate")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

ruta_panel    <- file.path("input", "raw", "consulta_electro_imp.csv")  # consulta de ANAM actualizada al 20/11/2026
# Se busca por patrón para no depender del acento en el nombre (Mac y Windows)
ruta_catalogo <- list.files("input", pattern = "LIGIE.*NICO.*\\.xlsx$", full.names = TRUE)[1]
if (is.na(ruta_catalogo)) stop("No encuentro el catálogo LIGIE NICO en input/")
ruta_claves   <- file.path("input", "claves_pedimento.csv")
dir_out       <- file.path("output", "panel")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
escribe <- function(x, nombre) fwrite(x, file.path(dir_out, nombre), bom = TRUE)

# Claves que cuentan como flujo físico. Por ahora solo hay importaciones:
#   definitivas A1 y C1; temporales IMMEX IN (insumos) y AF (activo fijo).
# Robustez: agregar D1, I1, M3, F2, G1, C3 (entradas físicas menores) o A3.
claves_flujo <- c("A1", "C1", "IN", "AF")

# Variable de valor. Para importaciones se pide valor_aduana (base gravable:
# incluye flete y seguro), que viene en PESOS; se pasa a dolares con el
# tipo_cambio de cada linea para que este en la misma moneda que el capex.
# Alternativa: "valor_dolares" (valor comercial en dolares).
variable_valor <- "valor_aduana"

# ---- 1. Lectura ----------------------------------------------------------------
panel <- fread(ruta_panel, encoding = "UTF-8", colClasses = list(character = c(
  "fecha", "clave_documento", "clave_pais_origen_destino")))
panel <- panel[, !duplicated(names(panel)), with = FALSE]
if ("V1" %in% names(panel)) panel[, V1 := NULL]

panel[, `:=`(
  mes            = as.Date(paste0(substr(fecha, 1, 7), "-01")),   # "2022-01" -> 2022-01-01
  fraccion       = sprintf("%08d", as.integer(fraccion_arancelaria)),
  nico           = sprintf("%02d", as.integer(subdivision_fraccion_arancelaria)),
  pais           = toupper(trimws(clave_pais_origen_destino)),
  clave_documento = toupper(trimws(clave_documento)),
  regimen        = fifelse(toupper(trimws(clave_documento)) %in% c("IN", "AF"), "temporal", "definitiva"),
  tipo_operacion = fifelse(clave_tipo_operacion == 1L, "importacion",
                   fifelse(clave_tipo_operacion == 2L, "exportacion", NA_character_))
)]
panel[, trimestre := floor_date(mes, "quarter")]
if (variable_valor == "valor_aduana") {
  panel[, valor_usd_linea := fifelse(!is.na(tipo_cambio) & tipo_cambio > 0,
                                     valor_aduana / tipo_cambio, NA_real_)]
  sin_tc <- panel[is.na(valor_usd_linea), .N]
  if (sin_tc) message("Lineas sin tipo de cambio valido (se usa valor_dolares): ", sin_tc)
  panel[is.na(valor_usd_linea), valor_usd_linea := valor_dolares]
} else {
  panel[, valor_usd_linea := valor_dolares]
}
if (anyNA(panel$mes)) stop("Hay fechas que no tienen formato AAAA-MM")
# El mes en curso viene incompleto (la consulta se corta el dia de extraccion):
# se descarta para que no parezca una caida. Cambiar a FALSE si el panel ya
# viene cerrado.
quitar_mes_en_curso <- TRUE
mes_corte <- floor_date(Sys.Date(), "month")
if (quitar_mes_en_curso && any(panel$mes >= mes_corte)) {
  message("Se descarta el mes en curso o posterior: ", paste(sort(unique(format(panel$mes[panel$mes >= mes_corte], "%Y-%m"))), collapse = ", "))
  panel <- panel[mes < mes_corte]
}

claves <- fread(ruta_claves, encoding = "UTF-8")
claves[, flujo_fisico := clave %in% claves_flujo]
faltan <- setdiff(unique(panel$clave_documento), claves$clave)
if (length(faltan)) warning("Claves sin clasificar (se tratan como no físicas): ",
                            paste(faltan, collapse = ", "))
panel <- merge(panel, claves[, .(clave_documento = clave, grupo_clave = grupo,
                                 flujo_fisico, cuenta_recaudacion = recaudacion)],
               by = "clave_documento", all.x = TRUE)
panel[is.na(flujo_fisico), `:=`(flujo_fisico = FALSE, cuenta_recaudacion = FALSE,
                                grupo_clave = "sin_clasificar")]

# ---- 2. Una fila por celda -------------------------------------------------------
# El panel ya viene agregado por mes. Cada celda trae una línea por contribución
# (y por tasa y forma de pago dentro de cada contribución). El valor se repite
# entre contribuciones, con diferencias de redondeo, pero dentro de una misma
# contribución las líneas se reparten el valor. Regla: valor de la celda = el
# mayor de los totales por contribución (la que declararon todas las mercancías,
# casi siempre IVA).
celda <- c("tipo_operacion", "regimen", "clave_documento", "grupo_clave", "flujo_fisico",
           "fraccion", "nico", "pais", "clave_unidad_medida_comercial", "mes", "trimestre")
# valor_mxn: la misma variable de valor en pesos (valor_aduana), para reportar
# el efecto en pesos sin pasar por un tipo de cambio promedio.
por_contrib <- panel[, .(valor_dolares = sum(valor_usd_linea),
                         valor_mxn = sum(as.numeric(valor_aduana), na.rm = TRUE),
                         cantidad = sum(cantidad_mercancia_unidades_medida_tarifa)),
                     by = c(celda, "clave_contribucion")]
celdas <- por_contrib[order(-valor_dolares), .SD[1], by = celda]
celdas[, contribucion_base := clave_contribucion][, clave_contribucion := NULL]

# ---- 3. Diagnóstico ----------------------------------------------------------------
iva <- por_contrib[clave_contribucion == 3L, .(valor_iva = valor_dolares), by = celda]
chk <- merge(celdas, iva, by = celda, all.x = TRUE)
diag <- data.table(
  indicador = c("filas", "celdas", "lineas por celda", "mes minimo", "mes maximo", "meses",
                "fracciones", "paises", "valor USD sin deduplicar (millones)",
                "valor USD deduplicado (millones)", "% celdas sin linea de IVA",
                "% celdas donde otra contribucion supera al IVA en mas de 1%"),
  valor = c(nrow(panel), nrow(celdas), round(nrow(panel) / nrow(celdas), 2),
            as.character(min(panel$mes)), as.character(max(panel$mes)), uniqueN(panel$mes),
            uniqueN(panel$fraccion), uniqueN(panel$pais),
            round(sum(panel$valor_usd_linea, na.rm = TRUE) / 1e6, 1),
            round(sum(celdas$valor_dolares, na.rm = TRUE) / 1e6, 1),
            round(100 * mean(is.na(chk$valor_iva)), 2),
            round(100 * mean(chk$valor_dolares > 1.01 * chk$valor_iva, na.rm = TRUE), 2)))
diag <- rbind(diag, data.table(indicador = "variable de valor", valor = variable_valor))
escribe(diag, "00_diagnostico_panel.csv")
print(diag)

por_clave <- celdas[, .(celdas = .N, valor_musd = sum(valor_dolares) / 1e6),
                    by = .(tipo_operacion, clave_documento, grupo_clave, flujo_fisico)]
por_clave[, pct_valor := round(100 * valor_musd / sum(valor_musd), 2), by = tipo_operacion]
setorder(por_clave, tipo_operacion, -valor_musd)
escribe(por_clave, "01_valor_por_clave.csv")
print(por_clave)

# Valor anual con EUA y total, para comparar con la balanza de INEGI
anual <- celdas[flujo_fisico == TRUE, .(total_musd = sum(valor_dolares) / 1e6,
                                        eua_musd = sum(valor_dolares[pais == "USA"]) / 1e6),
                by = .(tipo_operacion, regimen, anio = year(mes))]
escribe(anual, "02_valor_anual_para_validar.csv")

# ---- 4. Panel fracción x país x mes (flujo físico) ---------------------------------
cat_lig <- as.data.table(read_xlsx(ruta_catalogo))
# Catalogo a nivel NICO (10 digitos = fraccion + subdivision)
cat_lig <- unique(cat_lig[, .(fraccion = substr(fraccion_nico, 1, 8), nico = substr(fraccion_nico, 9, 10),
                              subpartida, desc_fraccion, desc_nico)], by = c("fraccion", "nico"))

panel_fpm <- celdas[flujo_fisico == TRUE,
                      .(valor_usd = sum(valor_dolares), valor_mxn = sum(valor_mxn), cantidad = sum(cantidad)),
                      by = .(tipo_operacion, regimen, fraccion, nico, pais, mes)]
panel_fpm <- merge(panel_fpm, cat_lig, by = c("fraccion", "nico"), all.x = TRUE)
panel_fpm[, fraccion_nico := paste0(fraccion, nico)]
escribe(panel_fpm, "panel_fraccion_pais_mes.csv")

# Fracción x trimestre con EUA, con ceros explícitos (PPML los necesita)
eua <- celdas[flujo_fisico == TRUE & pais == "USA",
                .(valor_usd = sum(valor_dolares)), by = .(tipo_operacion, regimen, fraccion, nico, trimestre)]
malla <- CJ(tipo_operacion = unique(eua$tipo_operacion), regimen = c("definitiva", "temporal"),
            fraccion_nico = unique(paste0(eua$fraccion, eua$nico)), trimestre = sort(unique(celdas$trimestre)))
malla[, `:=`(fraccion = substr(fraccion_nico, 1, 8), nico = substr(fraccion_nico, 9, 10))]
eua <- merge(malla, eua, by = c("tipo_operacion", "regimen", "fraccion", "nico", "trimestre"), all.x = TRUE)
eua[is.na(valor_usd), valor_usd := 0]
eua <- merge(eua, cat_lig, by = c("fraccion", "nico"), all.x = TRUE)
escribe(eua, "panel_eua_trimestre.csv")

# ---- 5. Recaudación y arancel efectivo ---------------------------------------------
contrib <- c(`1` = "DTA", `2` = "CC", `3` = "IVA", `6` = "IGI_IGE", `12` = "TMEC_2.5")
rec <- panel[cuenta_recaudacion == TRUE & clave_forma_pago == 0L,
             .(importe_mxn = sum(importe_pago)),
             by = .(tipo_operacion, fraccion, nico, pais, mes, clave_contribucion)]
rec[, contribucion := fcoalesce(contrib[as.character(clave_contribucion)], "OTRAS")]
escribe(rec, "recaudacion_fraccion_pais_mes.csv")

# IVA cubierto con credito fiscal IMMEX (formas 21 y 22, Apendice 13). No es
# recaudacion (no entra a la Tesoreria); se guarda aparte para reportarlo.
cred <- panel[cuenta_recaudacion == TRUE & clave_forma_pago %in% c(21L, 22L),
              .(importe_mxn = sum(importe_pago)),
              by = .(tipo_operacion, fraccion, nico, pais, mes, clave_contribucion)]
escribe(cred, "iva_credito_fraccion_pais_mes.csv")

# Tasa de IGI ponderada por valor: varía por fracción, país y mes, así que no la
# absorben los efectos fijos de trimestre (control del paso 2).
igi <- panel[clave_contribucion == 6L & clave_tipo_tasa == 1L,
             .(tasa_igi = weighted.mean(tasa_contribucion, valor_usd_linea)),
             by = .(fraccion, nico, pais, mes)]
escribe(igi, "arancel_fraccion_pais_mes.csv")

cat("\nListo. Revisa 00_diagnostico_panel.csv antes de correr el paso 2.\n")
