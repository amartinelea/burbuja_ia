# -*- coding: utf-8 -*-
# ==============================================================================
# Proyecto:   boom_ia
# Script:     scripts/slides_contexto/2_graficas_importaciones.py
# Objetivo:   Genera las gráficas de importaciones (PNG + SVG) con las
#             funciones de pabloplot (scripts/1_funciones), igual
#             que graficas_081026.py de proyecto_autopartes.
#
# NO SE CORRE A MANO: lo lanza scripts/slides_contexto/2_graficas_importaciones.R con
# system(), pasándole todo por variables de entorno (Sys.setenv):
#   IMP_RAIZ           raíz del proyecto (el proceso debe correr parado ahí)
#   IMP_DIR_FUNCIONES  carpeta con barras_verticales.py, lineas.py, funciones.py
#   IMP_MANIFIESTO     manifiesto_graficas.csv (lo escribe 1_tabulados_importaciones.R)
#   IMP_EVENTOS        eventos_graficas.csv (líneas verticales)
#   IMP_SOLO_SLIDES    opcional, "4,6,18": genera solo esas slides
#   IMP_EJE_Y          titulo del eje y de la serie mensual (pesos o dolares)
#
# Cada renglón del manifiesto es una gráfica: tipo (lineas_mensual,
# lineas_anual, barras_apiladas), tabulado de entrada y nombre de salida.
# exportar_grafica() guarda en output/<nombre>; el nombre ya trae
# "<ddmmaa>/figuras/slide_XX_<seccion>_<descripcion>".
# ==============================================================================

import os
import sys
import locale
import textwrap
import traceback
from pathlib import Path

import matplotlib
matplotlib.use("Agg")                          # sin ventanas: corre desde R
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import font_manager
import pandas as pd


# ---- PARÁMETROS QUE LLEGAN DESDE R --------------------------------------------
def env(nombre, defecto=None):
    valor = os.environ.get(nombre, defecto)
    if valor is None or valor == "":
        if defecto is None:
            sys.exit(f"ERROR: falta la variable de entorno {nombre}. Corre este script desde "
                     f"scripts/slides_contexto/2_graficas_importaciones.R")
    return valor


RAIZ          = Path(env("IMP_RAIZ")).resolve()
DIR_FUNCIONES = Path(env("IMP_DIR_FUNCIONES")).resolve()
MANIFIESTO    = Path(env("IMP_MANIFIESTO")).resolve()
EVENTOS       = Path(env("IMP_EVENTOS")).resolve()
EJE_Y         = env("IMP_EJE_Y", "Millones de pesos reales (enero de 2025)")
SOLO_SLIDES   = [int(s) for s in env("IMP_SOLO_SLIDES", "").replace(" ", "").split(",") if s]

# exportar_grafica() guarda en "output/<nombre>" RELATIVO a la carpeta actual.
# R lanza este proceso parado en la raíz; aquí solo se verifica (sin os.chdir).
if Path.cwd().resolve() != RAIZ:
    sys.exit(f"ERROR: el proceso corre en {Path.cwd()} y debe correr en {RAIZ}.")

# ---- LOCALE A PRUEBA DE WINDOWS ------------------------------------------------
# funciones.py pide locale 'es_ES.UTF-8', que no existe en Windows. Se prueban
# variantes y, si ninguna existe, se deja el locale actual sin tronar. Las
# etiquetas de meses de este script se escriben a mano, así que no dependen
# del locale.
_setlocale_original = locale.setlocale


def _setlocale_seguro(categoria, loc=None):
    if loc is None:
        return _setlocale_original(categoria)
    for candidato in [loc, "es_MX.UTF-8", "es_ES.UTF-8", "es_MX", "es_ES",
                      "Spanish_Mexico.1252", "Spanish_Spain.1252", "Spanish"]:
        try:
            return _setlocale_original(categoria, candidato)
        except locale.Error:
            continue
    return _setlocale_original(categoria)


locale.setlocale = _setlocale_seguro

# ---- FUNCIONES DE PABLOPLOT Y FUENTES ------------------------------------------
sys.path.insert(0, str(DIR_FUNCIONES))
from barras_verticales import barras_verticales   # noqa: E402
from lineas import lineas                          # noqa: E402
from funciones import mostrar_guardar_figura       # noqa: E402

for archivo_fuente in font_manager.findSystemFonts(fontpaths=[str(DIR_FUNCIONES.parent / "0_fonts")]):
    font_manager.fontManager.addfont(archivo_fuente)

# Montserrat en TODO el texto, incluida la leyenda (toma la fuente global)
mpl.rcParams["font.family"] = "Montserrat"
mpl.rcParams["svg.fonttype"] = "none"              # texto editable en el SVG
# Negritas dentro de la leyenda (nombre del capitulo en negritas, clave sin
# negritas): se usa mathtext con la misma Montserrat.
mpl.rcParams["mathtext.fontset"] = "custom"
mpl.rcParams["mathtext.rm"] = "Montserrat"
mpl.rcParams["mathtext.bf"] = "Montserrat:bold"
mpl.rcParams["mathtext.default"] = "regular"
mpl.rcParams["mathtext.it"] = "Montserrat"
mpl.rcParams["mathtext.sf"] = "Montserrat"
mpl.rcParams["mathtext.cal"] = "Montserrat"

# ---- ESTILO (mismo que autopartes) ---------------------------------------------
MESES_ABR = ["Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"]

COLOR_TOTAL       = "#002f2aff"   # verde oscuro: importaciones totales
COLOR_TEMPORALES  = "#bfbfbfff"   # gris claro: importaciones temporales (serie de fondo)
COLOR_DEFINITIVAS = "#9b2247ff"
COLOR_TEMP_ANUAL  = "#011738ff"
COLOR_LINEA_ANIO  = "#dc8f12"     # líneas punteadas de cambio de año
COLOR_EVENTO      = "#9b2247"     # líneas de aranceles / reforma

ANCHO_BARRA          = 0.7
ALTO_BARRAS          = 16.5       # cm; igual que autopartes (leyenda dentro de la imagen)
ANCHO_ETIQUETA       = 48         # caracteres por renglón en la leyenda
UMBRAL_PCT_DESGLOSE  = 4.5        # segmentos menores a este % quedan sin número
UMBRAL_PCT_PAISES    = 3.0

TAMANOS = {                       # cm, medidos de las imágenes de la PPT
    "lineas_mensual": (30.07, 13.61),
    "lineas_anual":   (20.07, 10.12),
    "barras_desglose": 19.76,
    "barras_pais":     19.39,
}

eventos = pd.read_csv(EVENTOS, parse_dates=["fecha"], encoding="utf-8-sig")


def _negritas(texto):
    """Escribe un texto en negritas con mathtext, escapando lo especial."""
    for c in ["\\", "%", "$", "_", "^", "{", "}", "#", "&"]:
        texto = texto.replace(c, "\\" + c)
    texto = texto.replace(",", "{,}")          # sin espacio extra tras la coma
    return r"$\bf{" + texto.replace(" ", r"\ ") + "}$"


def etiqueta_negritas(texto, ancho):
    """Convención de las barras por capítulo/partida: el nombre en negritas y
    lo que va entre paréntesis (clave o detalle de "Otros") sin negritas.
    Respeta el corte de renglón de textwrap."""
    corte = texto.find(" (")
    fin_nombre = len(texto) if corte < 0 else corte
    renglones, pos = [], 0
    for renglon in textwrap.wrap(texto, ancho):
        ini = texto.index(renglon, pos)
        fin = ini + len(renglon)
        pos = fin
        negro = texto[ini:min(fin, fin_nombre)]
        normal = texto[max(ini, fin_nombre):fin].replace("$", r"\$")
        renglones.append((_negritas(negro) if negro.strip() else "") + normal)
    return "\n".join(renglones)


# ---- GRÁFICAS -------------------------------------------------------------------
def grafica_mensual(fila):
    """Serie mensual: importaciones totales con cápsulas (inicio, máximo de cada
    año y último mes) y temporales como línea tenue de fondo."""
    df = pd.read_csv(fila.tabulado, parse_dates=["fecha"], encoding="utf-8-sig")
    col_total = next(c for c in df.columns if c.endswith("totales"))
    col_temp = next(c for c in df.columns if c.endswith("temporales"))
    total = df[["fecha", col_total]].copy()

    anio = total["fecha"].dt.year
    capsulas = sorted(set(
        [0, len(total) - 1] + total.groupby(anio)[col_total].idxmax().tolist()
    ))
    etiquetas_x = [
        f"{MESES_ABR[f.month - 1]}\n{f.year}" if f.month == 1 else
        (MESES_ABR[f.month - 1] if f.month in (4, 7, 10) else "")
        for f in total["fecha"]
    ]
    verticales = [{"x": pd.Timestamp(a, 1, 1), "color": COLOR_LINEA_ANIO, "linewidth": 1,
                   "linestyle": "--", "alpha": 0.8} for a in sorted(anio.unique())[1:]]
    verticales += [{"x": f, "color": COLOR_EVENTO, "linewidth": 1.5, "linestyle": "-"}
                   for f in eventos.loc[eventos["en_mensual"], "fecha"]]
    techo = max(df[col_total].max(), df[col_temp].max()) * 1.18

    ancho, alto = TAMANOS["lineas_mensual"]
    fig, ax = lineas(total,
                     nombre=fila.grafica,
                     ancho=ancho, alto=alto,
                     colores=[COLOR_TOTAL],
                     puntos=False,
                     mostrar_capsu=capsulas,
                     capsu_ini=True, capsu_max=True, capsu_fin=True,
                     algoritmo_posicionamiento="algoritmo_posicionamiento_1",   # cápsulas arriba del punto, sin encimarse
                     sustituir_valor_ejex=etiquetas_x,
                     marcas_graduacion_ejex=False,
                     tam_valor_ejex=9, tam_valor_ejey=10, tam_valor_capsu=11,
                     nombre_ejey=EJE_Y,
                     tam_nombre_ejey=9,
                     limites_ejey=(0, techo),
                     lineas_verticales=verticales,
                     leyenda=False,
                     guardar=False, mostrar=False)

    # Temporales: línea tenue sin cápsulas, debajo de la serie total
    ax.plot(df["fecha"], df[col_temp], color=COLOR_TEMPORALES,
            linewidth=1.3, zorder=1)
    ax.set_ylim(0, techo)
    mostrar_guardar_figura(fig=fig, ax=ax, nombre_df=fila.grafica, guardar_fig=True,
                           mostrar_fig=False, limpiar_svg_con_scour=True, exact_dim=True)
    plt.close("all")


def grafica_anual(fila):
    """Promedio diario anual: definitivas vs temporales (como temp_lineas de autopartes)."""
    df = pd.read_csv(fila.tabulado, encoding="utf-8-sig")
    df = df[["year", "Definitivas", "Temporales"]]
    verticales = [{"x": x, "color": COLOR_EVENTO, "linewidth": 1.2, "linestyle": "--"}
                  for x in eventos.loc[eventos["en_anual"], "x_anual"]]

    ancho, alto = TAMANOS["lineas_anual"]
    lineas(df,
           nombre=fila.grafica,
           ancho=ancho, alto=alto,
           sin_capsu=0,
           # Igual que autopartes: allá se pasó 'algorimto_posicionamiento_1' (sin
           # reacomodo de cápsulas); None reproduce ese mismo comportamiento.
           algoritmo_posicionamiento=None,
           ajustar_pos_capsu=0.1,
           leyenda=False,
           lineas_verticales=verticales,
           colores=[COLOR_DEFINITIVAS, COLOR_TEMP_ANUAL],
           guardar=True, mostrar=False)
    plt.close("all")


def grafica_barras(fila):
    """Barras apiladas de PD por capítulo/partida o por país (como
    barras_partidas_pd_081026 / barras_paises_pd_081026 de autopartes)."""
    df = pd.read_csv(fila.tabulado, dtype={"clave": str}, encoding="utf-8-sig")
    es_pais = "por_pais" in fila.tabulado

    orden = (df[df["year"] == df["year"].max()]
             .sort_values("orden")[["etiqueta_leyenda", "color"]])
    ancho_df = (df.pivot(index="year", columns="etiqueta_leyenda", values="valor2")
                  .reset_index())
    ancho_df.columns.name = None
    ancho_df["year"] = ancho_df["year"].astype(str)
    ancho_df = ancho_df[["year"] + orden["etiqueta_leyenda"].tolist()].fillna(0)
    formato = (lambda c: textwrap.fill(c, ANCHO_ETIQUETA)) if es_pais else \
              (lambda c: etiqueta_negritas(c, ANCHO_ETIQUETA))
    ancho_df.columns = ["year"] + [formato(c) for c in ancho_df.columns[1:]]

    umbral = UMBRAL_PCT_PAISES if es_pais else UMBRAL_PCT_DESGLOSE
    areamin = umbral / 100 * ancho_df.drop(columns="year").sum(axis=1).max() * ANCHO_BARRA

    barras_verticales(ancho_df,
                      nombre=fila.grafica,
                      leyenda=True,
                      ancho=TAMANOS["barras_pais" if es_pais else "barras_desglose"],
                      alto=ALTO_BARRAS,
                      num_columnas_leyenda=2,
                      posicion_leyenda={"loc": "upper center", "bbox_to_anchor": (0.5, -0.1)},
                      tam_letra_leyenda=8,
                      quitar_capsu=True,
                      colores=orden["color"].tolist(),
                      porce_barra=True,
                      valor_barra=False,
                      ancho_barra=ANCHO_BARRA,
                      areamin=areamin,
                      opcion_areamin="sin etiquetas",
                      ordenar="x",
                      mostrar=False)
    plt.close("all")


GRAFICADORES = {
    "lineas_mensual":  grafica_mensual,
    "lineas_anual":    grafica_anual,
    "barras_apiladas": grafica_barras,
}

# ---- EJECUCIÓN -------------------------------------------------------------------
manifiesto = pd.read_csv(MANIFIESTO, encoding="utf-8-sig")
if SOLO_SLIDES:
    manifiesto = manifiesto[manifiesto["slide"].isin(SOLO_SLIDES)]

errores = []
for fila in manifiesto.itertuples(index=False):
    print(f"\n>> Slide {fila.slide:02d} | {fila.etiqueta_seccion} | {fila.titulo}", flush=True)
    try:
        GRAFICADORES[fila.tipo](fila)
        print(f"   OK: output/{fila.grafica}.png", flush=True)
    except Exception as e:                       # sigue con las demás y reporta al final
        errores.append((fila.slide, fila.titulo, repr(e)))
        traceback.print_exc()
        plt.close("all")

print(f"\nGráficas generadas: {len(manifiesto) - len(errores)} de {len(manifiesto)}", flush=True)
if errores:
    for slide, titulo, err in errores:
        print(f"   FALLÓ slide {slide:02d} ({titulo}): {err}", flush=True)
    sys.exit(1)
