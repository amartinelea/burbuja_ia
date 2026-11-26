# _____________________________________________________________________________
#
# Proyecto:       boom_ia
#
# Script:         main.R
# Objetivo:
#
# Autor:          Ariadna Martinez Leal
# Correo(s):      amartinelea@gmail.com
#
# Fecha:          13/11/2026
# 
# Última
# actualización:  13/11/2026
#
# _____________________________________________________________________________



# PREAMBULO ___________________________________________________________________

# Limpiar entorno de trabajo
rm(list = ls())       # Limpiar entorno de trabajo
# source("~/.Rprofile") # Cargar configuraciones globales
cat("\014")           # Limpiar consola


# Carga de paquetes
pacman::p_load(tidyverse, readxl, writexl, janitor, lubridate, jsonlite, fst,
    openxlsx, scales, glue)




# exploracion de datos anam 

detalle_fraccion <- read_xlsx("input/raw/PROSEC_Sectores_Importaciones_2025_2026.xlsx", sheet = 4)
sectores_prosec <- read_xlsx("input/raw/PROSEC_Sectores_Importaciones_2025_2026.xlsx", sheet = 5)
glimpse(detalle_fraccion)
glimpse(sectores_prosec)

## nuevo insumo 
panel_mensual <- fread("input/raw/consulta_electro_imp.csv")
catalogo_panel_mensual <- read_xlsx("input/Catálogo LIGIE 2025 NICO - 2026.11.10.xlsx")

glimpse(panel_mensual)
glimpse(catalogo_panel_mensual)



#3 orden en el que corremos los codigos

# El orden completo de los scripts esta en scripts/00_correr_todo.R
# (se renumeraron el 24/11/2026 en el orden en que se corren).
# source("scripts/00_correr_todo.R")
