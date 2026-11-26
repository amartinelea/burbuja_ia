# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         scripts/03_equivalencia_clasificacion.R
# Objetivo:       Equivalencia entre las fracciones de ANAM (con su descripcion) y la
#                 clasificacion de la canasta IA (nivel y eslabon por subpartida).
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
#   - Decide que fracciones son nucleo, infraestructura, mixta o resto de
#     electronicos.
#   - Si se quiere mover un bien de nivel, se edita
#     input/canasta_ia_subpartidas.csv o input/canasta_ia_ajustes_fraccion.csv,
#     no este script.
#   - La hoja 3_candidatas del Excel sirve para revisar a mano bienes que
#     podrian ser de centros de datos.
#   - Sin acentos en comentarios ni textos para que corra igual en Mac y Windows.
#
# DETALLE TECNICO:
#   Entradas: input/<Catalogo LIGIE ... NICO ...>.xlsx (catalogo completo de fracciones)
#             output/panel/panel_fraccion_pais_mes.csv (script 02: valor por fraccion)
#             input/canasta_ia_subpartidas.csv     (regla por subpartida)
#             input/canasta_ia_ajustes_fraccion.csv (excepciones por fraccion, opcional)
#   Unidad: NICO (10 digitos = fraccion + subdivision). Los ajustes pueden ir a
#             8 digitos (toda la fraccion) o a 10 (un NICO); el de NICO gana.
#   Salidas : input/canasta_ia_fracciones.csv  clasificacion publicable a nivel
#             NICO (solo la canasta IA y lo que un ajuste saco), con su origen
#             output/choque_ia/equivalencia_clasificacion.xlsx con 5 hojas
#     1_equivalencia   cada NICO del catalogo LIGIE (cap. 84, 85 y 90) con su
#                      clasificacion y su valor importado en los ultimos 12 meses
#     2_cobertura      cada subpartida de la canasta: cuantos NICO tiene en el
#                      catalogo, cuantos tienen comercio en el panel y su valor
#     3_candidatas     NICO en resto cuya descripcion sugiere uso en centros de
#                      datos (revisar a mano)
#     3b_los_demas     NICO y fracciones residuales ("Los demas") con comercio
#                      desde EUA, ordenados por valor: donde buscar bienes de IA
#                      que hoy quedan escondidos en categorias genericas
#     4_glosario       la tabla de la canasta tal cual
#   Orden: correr DESPUES del script 02 (necesita el panel limpio).
# _____________________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola

options(encoding = "UTF-8", scipen = 999)
# Paquetes: instala los que falten y los carga
paquetes <- c("dplyr", "stringr", "readr", "readxl", "writexl", "stringi")
faltan_paq <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan_paq)) install.packages(faltan_paq, repos = "https://cloud.r-project.org")
suppressPackageStartupMessages(invisible(lapply(paquetes, library, character.only = TRUE)))

dir_base   <- getwd()
# El catalogo se busca por patron para no depender del acento (Mac y Windows)
archivo_cat <- list.files(file.path(dir_base, "input"), pattern = "LIGIE.*NICO.*\\.xlsx$",
                          full.names = TRUE)[1]
if (is.na(archivo_cat)) stop("No encuentro el catalogo LIGIE NICO en input/")
archivo_pn <- file.path(dir_base, "output", "panel", "panel_fraccion_pais_mes.csv")
if (!file.exists(archivo_pn)) stop("Falta ", archivo_pn, ": corre antes el script 05")
archivo_cn <- file.path(dir_base, "input", "canasta_ia_subpartidas.csv")
archivo_aj <- file.path(dir_base, "input", "canasta_ia_ajustes_fraccion.csv")
dir_out    <- file.path(dir_base, "output", "choque_ia")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)
capitulos  <- c("84", "85", "90")   # universo de electronicos para la equivalencia

# ---- Insumos ----------------------------------------------------------------
# Catalogo: una fila por NICO
catalogo <- read_xlsx(archivo_cat) %>%
  mutate(fraccion_nico = gsub("[^0-9]", "", as.character(fraccion_nico)),
         fraccion_nico = str_pad(fraccion_nico, 10, pad = "0"),
         fraccion = str_sub(fraccion_nico, 1, 8), nico = str_sub(fraccion_nico, 9, 10)) %>%
  filter(str_sub(fraccion, 1, 2) %in% capitulos) %>%
  distinct(fraccion_nico, .keep_all = TRUE) %>%
  transmute(fraccion_nico, fraccion, nico,
            descripcion_fraccion = desc_fraccion, descripcion_nico = desc_nico,
            descripcion_completa, igi_ligie = igi_imp)

# Valor importado de los ultimos 12 meses completos (USD), total y desde EUA
panel <- read_csv(archivo_pn, locale = locale(encoding = "UTF-8"),
                  col_types = cols(fraccion = col_character(), nico = col_character(),
                                   pais = col_character(), .default = col_guess()),
                  show_col_types = FALSE) %>%
  mutate(fraccion = str_pad(fraccion, 8, pad = "0"), nico = str_pad(coalesce(nico, "00"), 2, pad = "0"),
         fraccion_nico = paste0(fraccion, nico), mes = as.Date(mes))
ult12 <- sort(unique(panel$mes), decreasing = TRUE)[1:12]
valores <- panel %>%
  filter(tipo_operacion == "importacion") %>%
  group_by(fraccion_nico) %>%
  summarise(valor_12m_musd     = sum(valor_usd[mes %in% ult12]) / 1e6,
            valor_12m_eua_musd = sum(valor_usd[mes %in% ult12 & pais == "USA"]) / 1e6,
            valor_12m_temporal_musd = sum(valor_usd[mes %in% ult12 & regimen == "temporal"]) / 1e6,
            meses_con_comercio = n_distinct(mes), .groups = "drop")
message("Valores de ", format(min(ult12), "%Y-%m"), " a ", format(max(ult12), "%Y-%m"))

canasta <- read_csv(archivo_cn, locale = locale(encoding = "UTF-8"),
                    col_types = cols(subpartida = col_character()),
                    show_col_types = FALSE) %>%
  mutate(subpartida = str_pad(subpartida, 6, pad = "0"))

# Ajustes a 8 digitos (fraccion) o 10 (NICO)
ajustes <- if (file.exists(archivo_aj))
  read_csv(archivo_aj, locale = locale(encoding = "UTF-8"),
           col_types = cols(.default = col_character()), show_col_types = FALSE) %>%
  mutate(fraccion = gsub("[^0-9]", "", fraccion),
         fraccion = if_else(nchar(fraccion) > 8, str_pad(fraccion, 10, pad = "0"), str_pad(fraccion, 8, pad = "0"))) else
  tibble(fraccion = character(), nivel = character(), eslabon = character(), motivo = character())
aj10 <- ajustes %>% filter(nchar(fraccion) == 10) %>%
  transmute(fraccion_nico = fraccion, nivel_aj10 = nivel, eslabon_aj10 = eslabon, motivo_aj10 = motivo)
aj8 <- ajustes %>% filter(nchar(fraccion) == 8) %>%
  transmute(fraccion, nivel_aj8 = nivel, eslabon_aj8 = eslabon, motivo_aj8 = motivo)

# ---- 1. Equivalencia NICO -> clasificacion ----------------------------------------
# Universo: catalogo LIGIE (capitulos de electronicos) + cualquier NICO del panel
# que no este en el catalogo (codigos de otra version de la LIGIE)
universo <- catalogo %>%
  full_join(valores, by = "fraccion_nico") %>%
  mutate(fraccion = coalesce(fraccion, str_sub(fraccion_nico, 1, 8)),
         nico = coalesce(nico, str_sub(fraccion_nico, 9, 10)),
         en_panel = coalesce(meses_con_comercio, 0L) > 0,
         en_catalogo = fraccion_nico %in% catalogo$fraccion_nico,
         subpartida = str_sub(fraccion, 1, 6),
         partida    = str_sub(fraccion, 1, 4))

equivalencia <- universo %>%
  left_join(canasta %>% rename(descripcion_subpartida = descripcion_sa), by = "subpartida") %>%
  left_join(aj10, by = "fraccion_nico") %>%
  left_join(aj8, by = "fraccion") %>%
  mutate(origen_clasificacion = case_when(!is.na(nivel_aj10) ~ "ajuste_nico",
                                          !is.na(nivel_aj8)  ~ "ajuste_fraccion",
                                          !is.na(nivel)      ~ "regla_subpartida",
                                          TRUE               ~ "sin_regla_resto"),
         motivo_ajuste = coalesce(motivo_aj10, motivo_aj8),
         nivel   = coalesce(nivel_aj10, nivel_aj8, nivel, "4_resto_electronicos"),
         eslabon = coalesce(eslabon_aj10, eslabon_aj8, eslabon, "4_resto_electronicos"),
         en_canasta_ia = nivel != "4_resto_electronicos") %>%
  select(fraccion_nico, fraccion, nico, descripcion_fraccion, descripcion_nico, partida, subpartida,
         descripcion_subpartida, nivel, eslabon, origen_clasificacion, motivo_ajuste,
         any_of(c("uso_en_data_center", "contaminacion_no_ia", "nota")),
         en_canasta_ia, en_panel, en_catalogo, igi_ligie,
         valor_12m_musd, valor_12m_eua_musd, valor_12m_temporal_musd, meses_con_comercio,
         descripcion_completa) %>%
  arrange(desc(en_canasta_ia), nivel, eslabon, fraccion_nico)

# ---- 2. Cobertura por subpartida ----------------------------------------------
cobertura <- canasta %>%
  left_join(
    equivalencia %>%
      filter(subpartida %in% canasta$subpartida) %>%
      group_by(subpartida) %>%
      summarise(nico_catalogo   = sum(en_catalogo),
                nico_excluidos  = sum(!en_canasta_ia),
                nico_en_canasta = sum(en_canasta_ia),
                nico_en_panel   = sum(en_canasta_ia & en_panel),
                valor_12m_musd     = sum(valor_12m_musd[en_canasta_ia], na.rm = TRUE),
                valor_12m_eua_musd = sum(valor_12m_eua_musd[en_canasta_ia], na.rm = TRUE),
                .groups = "drop"),
    by = "subpartida") %>%
  mutate(across(starts_with("nico_"), ~ coalesce(as.integer(.x), 0L)),
         estado = case_when(nico_catalogo == 0 ~ "no existe en catalogo LIGIE: revisar codigo",
                            nico_en_canasta == 0 ~ "todos sus NICO excluidos por ajuste",
                            nico_en_panel == 0 ~ "sin comercio en el panel",
                            nico_en_panel < nico_en_canasta ~ "parcial",
                            TRUE ~ "completa")) %>%
  select(subpartida, nivel, eslabon, descripcion_sa, estado, nico_catalogo,
         nico_excluidos, nico_en_canasta, nico_en_panel, valor_12m_musd, valor_12m_eua_musd)

# ---- 3. Candidatas a revisar ----------------------------------------------------
# NICO en resto cuya descripcion (fraccion, NICO o completa) contiene palabras
# de centros de datos. Se compara sin acentos y en minusculas.
palabras <- c("servidor", "procesador", "acelerador", "grafic", "memoria", "fibra", "optic",
              "transceptor", "convertidor", "almacenamiento", "estado solido", "conmutador",
              "enrutador", "router", "switch", "unidades de proceso", "circuitos integrados",
              "tarjeta", "refrigeracion", "enfriamiento", "acondicionamiento de aire", "intercambiador",
              "rack", "gabinete", "tratamiento de datos", "transmision de datos", "semiconductor",
              "fuente de poder", "fuente de alimentacion", "no interrumpible", "ups")
patron <- paste0("\\b(", paste(palabras, collapse = "|"), ")")
norm <- function(x) str_to_lower(stri_trans_general(coalesce(x, ""), "Latin-ASCII"))

candidatas <- equivalencia %>%
  filter(!en_canasta_ia) %>%
  mutate(texto = paste(norm(descripcion_nico), norm(descripcion_fraccion), norm(descripcion_completa)),
         palabra_encontrada = str_extract(texto, patron)) %>%
  filter(!is.na(palabra_encontrada)) %>%
  select(fraccion_nico, descripcion_fraccion, descripcion_nico, palabra_encontrada, en_panel,
         valor_12m_musd, valor_12m_eua_musd) %>%
  arrange(desc(coalesce(valor_12m_eua_musd, 0)))

# ---- 3b. Residuales "Los demas" ---------------------------------------------------
# Fracciones o NICO genericos ("Los demas", "Las demas") con comercio desde EUA.
# Ahi pueden quedar bienes de IA sin categoria propia (ej. 8542.39: circuitos a
# la medida, aceleradores, semiconductores de red). Se ordenan por valor desde EUA para
# revisar primero los que pesan. Lo que se recupere va como ajuste a 10 digitos.
es_residual <- function(x) str_detect(norm(x), "^\\s*(los|las) dem")
los_demas <- equivalencia %>%
  filter(es_residual(descripcion_nico) | es_residual(descripcion_fraccion),
         coalesce(valor_12m_eua_musd, 0) > 0) %>%
  mutate(subpartida_en_canasta = subpartida %in% canasta$subpartida,
         residual_en = case_when(es_residual(descripcion_nico) & es_residual(descripcion_fraccion) ~ "fraccion y NICO",
                                 es_residual(descripcion_nico) ~ "NICO",
                                 TRUE ~ "fraccion")) %>%
  select(fraccion_nico, subpartida, descripcion_subpartida, descripcion_fraccion, descripcion_nico,
         residual_en, nivel, eslabon, subpartida_en_canasta, valor_12m_eua_musd, valor_12m_musd,
         descripcion_completa) %>%
  arrange(desc(valor_12m_eua_musd))

# ---- Salida ---------------------------------------------------------------------
canasta_fracciones <- equivalencia %>%
  filter(en_canasta_ia | origen_clasificacion %in% c("ajuste_fraccion", "ajuste_nico")) %>%
  select(fraccion_nico, fraccion, nico, descripcion_fraccion, descripcion_nico, subpartida,
         nivel, eslabon, origen_clasificacion, motivo_ajuste,
         any_of("contaminacion_no_ia"), en_panel, valor_12m_musd, valor_12m_eua_musd)
write_excel_csv(canasta_fracciones, file.path(dir_base, "input", "canasta_ia_fracciones.csv"))

write_xlsx(list(`1_equivalencia` = equivalencia,
                `1b_canasta_nico` = canasta_fracciones,
                `2_cobertura`    = cobertura,
                `3_candidatas`   = candidatas,
                `3b_los_demas`   = los_demas,
                `4_glosario`     = canasta),
           file.path(dir_out, "equivalencia_clasificacion.xlsx"))

message("NICO en canasta IA: ", sum(equivalencia$en_canasta_ia),
        " (con comercio en el panel: ", sum(equivalencia$en_canasta_ia & equivalencia$en_panel), ")")
fuera_cat <- sum(!equivalencia$en_catalogo)
if (fuera_cat) message("Aviso: ", fuera_cat, " NICO del panel no estan en el catalogo LIGIE ",
                       "(codigos de otra version); revisar en la hoja 1_equivalencia (en_catalogo = FALSE)")
print(count(cobertura, estado))
print(count(canasta_fracciones, nivel, origen_clasificacion))
message("Candidatas a revisar: ", nrow(candidatas), "; residuales 'Los demas' con comercio desde EUA: ", nrow(los_demas))
cat("\nResiduales 'Los demas' con mas valor desde EUA (millones USD, 12 meses):\n")
print(los_demas %>% select(fraccion_nico, residual_en, nivel, valor_12m_eua_musd, descripcion_subpartida) %>%
        mutate(descripcion_subpartida = str_trunc(coalesce(descripcion_subpartida, ""), 50)) %>% head(20))
