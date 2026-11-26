# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/diagnostico_data_mexico_2024.R
# Objetivo:       Compara las importaciones y exportaciones de 2024 de las consultas de ANAM
#                 contra Data Mexico (INEGI) por partida, para ver si la consulta de
#                 importaciones solo trae pagos en efectivo.
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          24/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Diagnostico puntual: compara las consultas de la ANAM contra Data Mexico
#     2024. No lo corre 00_correr_todo.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Fuente Data Mexico: perfiles de producto, 2024 (millones de USD)
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(width = 200)
paquetes <- c("data.table")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

# ---- Data Mexico 2024 (millones de USD) ------------------------------------
# 8471: intercambio total 48,258 menos exportaciones 34,891
dm_imp <- c("8542" = 25778, "8473" = 16159, "8471" = 48258 - 34891, "8517" = 17405)
dm_exp <- c("8542" = 3388,  "8473" = 5046,  "8471" = 34891,         "8517" = 14920)

# ---- Importaciones ANAM 2024 -------------------------------------------------
imp <- fread("input/raw/consulta_electro_imp.csv", colClasses = "character", encoding = "UTF-8")
imp <- imp[substr(fecha, 1, 4) == "2024"]
cat("Formas de pago en la consulta de importaciones:", paste(unique(imp$clave_forma_pago), collapse = ", "), "\n")
imp[, valor_dolares := as.numeric(valor_dolares)]
imp[, hs4 := substr(sprintf("%08s", fraccion_arancelaria), 1, 4)]
imp[, hs4 := gsub(" ", "0", hs4)]
# El valor se repite en cada contribucion: por celda se toma el mayor total por contribucion
celda <- c("fecha", "clave_documento", "fraccion_arancelaria", "subdivision_fraccion_arancelaria",
           "clave_pais_origen_destino", "clave_unidad_medida_comercial", "hs4")
imp <- imp[, .(v = sum(valor_dolares, na.rm = TRUE)), by = c(celda, "clave_contribucion")][, .(v = max(v)), by = celda]
anam_imp <- imp[hs4 %in% names(dm_imp), .(
  todas_claves = sum(v) / 1e6,
  total_imp = sum(v[clave_documento %in% c("A1", "C1", "IN", "AF")]) / 1e6,
  temporales   = sum(v[clave_documento %in% c("IN", "AF")]) / 1e6), by = hs4]

# ---- Exportaciones ANAM 2024 -------------------------------------------------
ex <- fread("input/raw/consulta_electro_exp.csv", colClasses = "character", encoding = "UTF-8")
ex <- ex[substr(fecha, 1, 4) == "2024" & clave_tipo_operacion == "2"]
ex[, hs4 := gsub(" ", "0", substr(sprintf("%08s", fraccion_arancelaria), 1, 4))]
ex[, v := as.numeric(valor_comercial) / as.numeric(tipo_cambio)]
anam_exp <- ex[hs4 %in% names(dm_exp), .(
  todas_claves = sum(v, na.rm = TRUE) / 1e6,
  a1_c1_rt     = sum(v[clave_documento %in% c("A1", "C1", "RT")], na.rm = TRUE) / 1e6), by = hs4]

# ---- Comparacion -------------------------------------------------------------
comp_imp <- merge(data.table(hs4 = names(dm_imp), data_mexico = dm_imp), anam_imp, by = "hs4", all.x = TRUE)
comp_imp[, `:=`(diferencia = total_imp - data_mexico, cobertura_pct = round(100 * total_imp / data_mexico, 1))]
comp_exp <- merge(data.table(hs4 = names(dm_exp), data_mexico = dm_exp), anam_exp, by = "hs4", all.x = TRUE)
comp_exp[, `:=`(diferencia = a1_c1_rt - data_mexico, cobertura_pct = round(100 * a1_c1_rt / data_mexico, 1))]

cat("\nIMPORTACIONES 2024 (millones de USD): ANAM contra Data Mexico\n")
redondea <- function(d) { d <- as.data.frame(d); num <- setdiff(names(d)[sapply(d, is.numeric)], "cobertura_pct"); d[num] <- round(d[num]); d }
print(redondea(comp_imp), row.names = FALSE)
cat("\nEXPORTACIONES 2024 (millones de USD): ANAM contra Data Mexico\n")
print(redondea(comp_exp), row.names = FALSE)
cat("\nSi la cobertura de importaciones es muy baja y la de exportaciones cercana a 100%,",
    "\nla consulta de importaciones solo trae lo pagado en efectivo.\n")
