# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/diagnostico_valor_aduana_vs_dolares.R
# Objetivo:       Compara el valor de las importaciones de ANAM medido de tres formas:
#                 aduana_usd    = valor_aduana / tipo_cambio   (lo que usa el proyecto)
#                 dolares       = valor_dolares                 (dolares que reporta ANAM)
#                 comercial_usd = valor_comercial / tipo_cambio
#                 valor_dolares es valor comercial en dolares; valor_aduana incluye ademas
#                 flete, seguro y otros cargos. Comparar comercial_usd contra dolares aisla el
#                 efecto del tipo de cambio; aduana_usd contra comercial_usd, el de los cargos.
#                 Solo claves de flujo fisico (A1, C1, IN, AF), igual que el modelo.
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      ariadna.martinez@transformaciondigital.gob.mx
#
# Fecha:          25/11/2026
#
# Ultima
# actualizacion:  26/11/2026
#
# NOTAS PARA EL EQUIPO:
#   - Diagnostico puntual: cuanto cambia el valor si se usa valor en aduana en
#     dolares o valor_dolares. No lo corre 00_correr_todo.
#   - Resultado de referencia: la diferencia es menor a 1% y viene de flete y
#     seguro, no del tipo de cambio.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Salida: consola y output/panel/diagnostico_valor_aduana_vs_dolares.csv
# _____________________________________________________________________________

rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(width = 200, scipen = 999)
paquetes <- c("data.table")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

claves_fisicas <- c("A1", "C1", "IN", "AF")
dir_out <- file.path("output", "panel")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

# ---- Lectura ---------------------------------------------------------------------
imp <- fread("input/raw/consulta_electro_imp.csv", colClasses = "character", encoding = "UTF-8")
imp <- imp[clave_documento %in% claves_fisicas]
for (v in c("valor_aduana", "valor_dolares", "valor_comercial", "tipo_cambio"))
  set(imp, j = v, value = as.numeric(imp[[v]]))
imp <- imp[!is.na(tipo_cambio) & tipo_cambio > 0]
imp[, `:=`(aduana_usd = valor_aduana / tipo_cambio, comercial_usd = valor_comercial / tipo_cambio,
           anio = substr(fecha, 1, 4),
           regimen = fifelse(clave_documento %in% c("IN", "AF"), "temporal", "definitiva"),
           hs4 = substr(formatC(as.integer(fraccion_arancelaria), width = 8, flag = "0"), 1, 4))]

# ---- Una fila por celda -------------------------------------------------------------
# El valor se repite en cada contribucion: por celda se toma la contribucion con
# mayor valor (la misma regla del 02) y de ella salen las tres medidas.
celda <- c("fecha", "anio", "clave_documento", "regimen", "fraccion_arancelaria",
           "subdivision_fraccion_arancelaria", "clave_pais_origen_destino",
           "clave_unidad_medida_comercial", "hs4")
pc <- imp[, .(aduana_usd = sum(aduana_usd, na.rm = TRUE), dolares = sum(valor_dolares, na.rm = TRUE),
              comercial_usd = sum(comercial_usd, na.rm = TRUE)), by = c(celda, "clave_contribucion")]
setorder(pc, -aduana_usd)
ce <- pc[, .SD[1], by = celda]

# ---- Resumen -------------------------------------------------------------------------
resume <- function(d, por) {
  r <- d[, .(aduana_usd = sum(aduana_usd) / 1e6, dolares = sum(dolares) / 1e6,
             comercial_usd = sum(comercial_usd) / 1e6), by = por]
  r[, `:=`(dif_aduana_vs_dolares_pct = round(100 * (aduana_usd / dolares - 1), 2),
           dif_tipo_cambio_pct = round(100 * (comercial_usd / dolares - 1), 2),
           dif_cargos_pct = round(100 * (aduana_usd / comercial_usd - 1), 2))]
  r[, c("aduana_usd", "dolares", "comercial_usd") := lapply(.SD, round, 0),
    .SDcols = c("aduana_usd", "dolares", "comercial_usd")]
  setorderv(r, por)
  r[]
}
por_anio    <- resume(ce, "anio")[, nivel := "total"]
por_regimen <- resume(ce, c("anio", "regimen"))[, nivel := "regimen"]
ult <- max(ce$anio[ce$anio < max(ce$anio)])     # ultimo anio completo
top_hs4 <- ce[anio == ult, .(v = sum(aduana_usd)), by = hs4][order(-v)][1:10, hs4]
por_hs4 <- resume(ce[anio == ult & hs4 %in% top_hs4], c("anio", "hs4"))[, nivel := "hs4"]

cat("\nIMPORTACIONES (A1, C1, IN, AF), millones de USD\n")
cat("dif_aduana_vs_dolares_pct: cuanto mas da valor_aduana/tipo_cambio que valor_dolares\n")
cat("  = efecto del tipo de cambio (comercial/tc vs dolares) + cargos (aduana vs comercial)\n\n")
cat("Por anio:\n");                 print(por_anio[, !"nivel"], row.names = FALSE)
cat("\nPor anio y regimen:\n");     print(por_regimen[, !"nivel"], row.names = FALSE)
cat("\nPartidas con mas valor en", ult, ":\n"); print(por_hs4[, !"nivel"], row.names = FALSE)

fwrite(rbind(por_anio, por_regimen, por_hs4, fill = TRUE),
       file.path(dir_out, "diagnostico_valor_aduana_vs_dolares.csv"), bom = TRUE)
cat("\nGuardado en", file.path(dir_out, "diagnostico_valor_aduana_vs_dolares.csv"), "\n")
