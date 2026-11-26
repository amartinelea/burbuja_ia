# -*- coding: utf-8 -*-
# ==============================================================================
# Proyecto:   boom_ia
# Script:     24_graficas_resultados.py
# Objetivo:   Graficas de resultados para la presentacion con pabloplot
#             (barras_horizontales), sin titulos: el titulo y la lectura van en
#             la lamina.
#               01 impacto total por escenario (B a D): canasta de IA + efecto de arrastre
#               02 eta del paso 2 por prueba (script 09)
#               03 epsilon por nivel (capex -> lo que EUA importa)
#               04 impacto por eslabon, una grafica por escenario (B, C, D)
#               05 escenarios de anexo (E y F): total y nucleo
#               06 version anterior del paso 2: eta del nucleo (anexo 8)
#
# NO SE CORRE A MANO: lo lanza 24_graficas_resultados.R con system(), que pasa
# todo por variables de entorno:
#   BIA_RAIZ, BIA_DIR_FUNCIONES, BIA_DIR_FUENTES, BIA_DIR_19, BIA_DIR_18,
#   BIA_DIR_04, BIA_SALIDA (relativa a output/), BIA_VERSION
# Autora:     Ariadna Martinez Leal (ariadna.martinez@transformaciondigital.gob.mx)
# Version:    2026-11-25
# ==============================================================================

import os
import sys
import types
import locale
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib as mpl
import matplotlib.pyplot as plt
from matplotlib import font_manager
import numpy as np
import pandas as pd


def env(nombre, defecto=None):
    valor = os.environ.get(nombre, defecto)
    if valor is None or valor == "":
        sys.exit(f"ERROR: falta la variable de entorno {nombre}. Corre este script desde 24_graficas_resultados.R")
    return valor


RAIZ          = Path(env("BIA_RAIZ")).resolve()
DIR_FUNCIONES = Path(env("BIA_DIR_FUNCIONES")).resolve()
DIR_FUENTES   = Path(env("BIA_DIR_FUENTES")).resolve()
DIR_19        = Path(env("BIA_DIR_19"))
DIR_18        = Path(env("BIA_DIR_18"))
DIR_04        = Path(env("BIA_DIR_04"))
SALIDA        = env("BIA_SALIDA", "graficas_resultados")
VERSION       = env("BIA_VERSION", "principal")

# exportar_grafica() guarda en output/<nombre> relativo a la carpeta actual
if Path.cwd().resolve() != RAIZ:
    sys.exit(f"ERROR: el proceso corre en {Path.cwd()} y debe correr en {RAIZ}.")

# Locale a prueba de Windows (funciones.py pide es_ES.UTF-8)
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

# ---- PABLOPLOT Y FUENTES -------------------------------------------------------
sys.path.insert(0, str(DIR_FUNCIONES))
from funciones import mostrar_guardar_figura  # noqa: E402,F401


def cargar_barras_horizontales():
    """Carga barras_horizontales.py sin modificar el archivo. Corrige en memoria
    dos detalles: asignar_etiquetas (usaba la posicion en el eje en lugar del
    numero de barra) y la capsula de barras solo negativas."""
    ruta = DIR_FUNCIONES / "barras_horizontales.py"
    fuente = ruta.read_text(encoding="utf-8")
    fuente = fuente.replace("etiquetas_finales.iloc[pos]", "etiquetas_finales.iloc[idx_row]")
    # Si una barra solo tiene valores negativos y capsu_cero=False, la variable
    # is_simple_or_divergent no se define antes de la capsula negativa
    fuente = fuente.replace(
        "                color_una_barra_capsu_neg = None\n                if is_simple_or_divergent:",
        "                color_una_barra_capsu_neg = None\n"
        "                is_simple_or_divergent = len(columnas_barras) == 1 or (es_divergente and len(columnas_barras) == 2)\n"
        "                if is_simple_or_divergent:")
    # Las capsulas negativas quitaban el ".0" (-6.0 salia como -6); se respeta el numero de decimales
    fuente = fuente.replace(
        "elimina_ceros_no_significativos(formato_decimal.format(abs(valor_negativo_total)))",
        "formato_decimal.format(abs(valor_negativo_total))")
    # Numeros dentro de las barras solo en las barras indicadas en _FILAS_VALOR
    # (None = en todas, como pabloplot)
    fuente = fuente.replace(
        "debe_dibujar_dentro = area_barra >= areamin\n",
        "debe_dibujar_dentro = area_barra >= areamin and (_FILAS_VALOR is None or entidad in _FILAS_VALOR)\n")
    fuente = fuente.replace(
        "texto_valor = elimina_ceros_no_significativos(formato_decimal.format(abs(current_val)))",
        "texto_valor = (\"-\" if (current_val < 0 and _SUFIJO_VALOR) else \"\") + elimina_ceros_no_significativos(formato_decimal.format(abs(current_val))) + _SUFIJO_VALOR")
    # Sufijo (p. ej. "%") tambien en las capsulas negativas, que no usan asignar_etiquetas
    fuente = fuente.replace(
        'texto_a_mostrar_neg = f"{prefijo_neg}{formato_decimal.format(abs(valor_negativo_total))}"',
        'texto_a_mostrar_neg = f"{prefijo_neg}{formato_decimal.format(abs(valor_negativo_total))}{_SUFIJO_CAPSU}"')
    modulo = types.ModuleType("barras_horizontales_bia")
    modulo._FILAS_VALOR = None
    modulo._SUFIJO_VALOR = ""
    modulo._SUFIJO_CAPSU = ""
    modulo.__file__ = str(ruta)
    exec(compile(fuente, str(ruta), "exec"), modulo.__dict__)
    return modulo.barras_horizontales


barras_horizontales = cargar_barras_horizontales()
def sufijo_capsula(sufijo=""):
    barras_horizontales.__globals__["_SUFIJO_CAPSU"] = sufijo

for archivo_fuente in font_manager.findSystemFonts(fontpaths=[str(DIR_FUENTES)]):
    font_manager.fontManager.addfont(archivo_fuente)
mpl.rcParams["font.family"] = "Montserrat"
mpl.rcParams["svg.fonttype"] = "none"

# ---- ESTILO ---------------------------------------------------------------------
GUINDA   = "#9b2247"
GRIS_OSC = "#6f6e70"
GRIS     = "#bfbfbf"
VERDE    = "#1e5b4f"

ESCENARIOS = {
    "B. Ajuste de expectativas (neoclouds)": "B. Ajuste de expectativas",
    "C. Contraccion de la industria": "C. Contracción de la industria",
    "D. Auge (aceleracion)": "D. Auge (aceleración)",
    "E. Alza de tasas (+100 pb)": "E. Alza de tasas",
    "F. Transicion a monetizacion": "F. Monetización",
}
NIVELES = {"1_nucleo": "Núcleo", "2_infraestructura": "Infraestructura", "3_mixta": "Mixta",
           "4_resto_electronicos": "Resto de electrónicos"}
ESLABONES = {"servidores": "Servidores", "almacenamiento": "Almacenamiento", "redes": "Redes",
             "semiconductores": "Semiconductores", "computo_general": "Cómputo general", "componentes": "Componentes",
             "energia_enfriamiento": "Energía y enfriamiento", "4_resto_electronicos": "Resto de electrónicos"}

# Tamanos de letra, capsulas y grosor de barra: los de pabloplot por defecto
COMUNES = dict(guardar_fig=True, mostrar_fig=False)


def lee(ruta):
    return pd.read_csv(ruta, encoding="utf-8-sig")


def estrellas(p):
    if pd.isna(p):
        return ""
    return "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.10 else ""


def num(x, d=1):
    return f"{x:,.{d}f}".replace("-", "−")


def pct(x, d=1):
    return num(x, d) + "%"


def destacada(df, col_etq, col_val, es_destacada, col_lab=None):
    """Una barra por fila con dos columnas (destacada / resto) para darle otro
    color a la barra que se resalta; la capsula muestra col_lab."""
    salida = pd.DataFrame({col_etq: df[col_etq],
                           "Destacada": np.where(es_destacada, df[col_val], 0.0),
                           "Resto": np.where(es_destacada, 0.0, df[col_val])})
    if col_lab is not None:
        salida["__lab"] = df[col_lab].values
    return salida


def nombre(archivo):
    return f"{SALIDA}/{archivo}"


hechas = []

# ==============================================================================
# 01 Impacto total por escenario (B a D): canasta de IA + efecto de arrastre
# ==============================================================================
# Solo la barra con efecto de arrastre (C) lleva los numeros de cada parte dentro; las
# demas solo la capsula con el total. Limites del eje segun los datos.
def impacto_por_escenario(dir_res, archivo):
    esc = lee(dir_res / "03_escenarios_supuestos.csv")
    tot = lee(dir_res / "04_resultado_total.csv")
    d1 = (tot[(tot["version_beta"] == VERSION) & tot["escenario"].str.match(r"^[B-D]\.")]
          .merge(esc[["escenario", "arrastre"]], on="escenario", how="left"))
    d1["total"] = (100 * d1["var_pct"]).round(1)
    d1["Efecto de arrastre"] = (100 * (np.exp(d1["arrastre"].fillna(0)) - 1)).round(1)
    d1["Canasta de bienes de IA"] = d1["total"] - d1["Efecto de arrastre"]
    d1["Escenario"] = d1["escenario"].map(ESCENARIOS)
    d1["__lab"] = [pct(t) for t in d1["total"]]
    con_partes = d1.loc[d1["Efecto de arrastre"] != 0, "Escenario"].tolist()
    d1 = d1.sort_values("escenario")[["Escenario", "Canasta de bienes de IA", "Efecto de arrastre", "__lab"]]
    lo, hi = d1[["Canasta de bienes de IA", "Efecto de arrastre"]].clip(upper=0).sum(axis=1).min(), d1["Canasta de bienes de IA"].clip(lower=0).max()
    barras_horizontales.__globals__["_FILAS_VALOR"] = con_partes
    barras_horizontales.__globals__["_SUFIJO_VALOR"] = "%"
    barras_horizontales(d1, nombre=nombre(archivo), colores=[GUINDA, GRIS],
                        ancho=22, alto=9, ordenar=None, decimales=1, valor_barra=True,
                        areamin=2.5, opcion_areamin="sin etiquetas",
                        asignar_etiquetas="__lab", capsu_cero=False, solo_ejex_positivo=False,
                        simbolo_negativo_capsu=True, color_valor_capsu="#434343", color_borde_capsu="#434343",
                        limites_ejex=(np.floor(lo * 1.25 / 5) * 5, max(np.ceil(hi * 1.6 / 5) * 5, 5)),
                        leyenda=True, posicion_leyenda=(0.45, -0.22), num_columnas_leyenda=2, **COMUNES)
    barras_horizontales.__globals__["_FILAS_VALOR"] = None
    barras_horizontales.__globals__["_SUFIJO_VALOR"] = ""
    plt.close("all")
    hechas.append(archivo)

sufijo_capsula("%")
impacto_por_escenario(DIR_19, "01_impacto_por_escenario")
# Misma grafica para exportaciones (script 12), si ya se corrio
dir_exp = RAIZ / "output" / "choque_ia_exportaciones"
if (dir_exp / "04_resultado_total.csv").exists():
    impacto_por_escenario(dir_exp, "01b_impacto_por_escenario_exportaciones")

sufijo_capsula("")

# ==============================================================================
# 02 eta del paso 2 por prueba (script 09)
# ==============================================================================
pruebas = [("1_tendencia", "x", "Principal: tendencia por subpartida"),
           ("1_base", "x", "Sin tendencia"),
           ("1_tendencia_cluster_hs6_y_trimestre", "x", "Errores por subpartida y trimestre"),
           ("4_sin_2022", "x", "Sin 2022"), ("4_sin_2023", "x", "Sin 2023"), ("4_sin_2024", "x", "Sin 2024"),
           ("4_sin_2025", "x", "Sin 2025"), ("4_sin_2026", "x", "Sin 2026"),
           ("5_sin_top5_hs6", "x", "Sin las 5 subpartidas mayores"),
           ("7_rezagos_distribuidos", "suma_0_a_3", "Rezagos distribuidos (0 a 3)"),
           ("6_cambios_anuales", "dx", "Cambios de 4 trimestres"),
           ("2_nucleo_menos_resto", "x:nucleo", "Núcleo menos resto")]
r18 = lee(DIR_18 / "01_pruebas.csv")
d2 = (pd.DataFrame(pruebas, columns=["prueba", "termino", "Prueba"])
      .merge(r18, on=["prueba", "termino"], how="inner"))
d2["lab"] = [f"{e:.2f}{estrellas(p)}" for e, p in zip(d2["eta"], d2["p"])]
d2 = destacada(d2, "Prueba", "eta", d2["prueba"].eq("1_tendencia"), "lab")
barras_horizontales(d2, nombre=nombre("02_eta_paso2_por_prueba"), colores=[GUINDA, GRIS_OSC], color_borde_capsu=[GUINDA, GRIS_OSC], color_valor_capsu=[GUINDA, GRIS_OSC],
                    ancho=20, alto=16, ordenar=None, decimales=2, valor_barra=False,
                    asignar_etiquetas="__lab", capsu_cero=False, leyenda=None, **COMUNES)
plt.close("all")
hechas.append("02_eta_paso2_por_prueba")

# ==============================================================================
# 03 epsilon por nivel
# ==============================================================================
el = lee(DIR_19 / "01_elasticidades_eua.csv")
d3 = el[el["paso"] == "capex_a_eua"].copy()
d3["Nivel"] = d3["grupo"].map(NIVELES)
d3["lab"] = [f"{e:.2f}{estrellas(p)}" for e, p in zip(d3["est"], d3["p"])]
d3 = d3.sort_values("grupo")
d3 = destacada(d3, "Nivel", "est", ~d3["grupo"].eq("4_resto_electronicos"), "lab")
barras_horizontales(d3, nombre=nombre("03_epsilon_por_nivel"), colores=[GUINDA, GRIS_OSC], color_borde_capsu=[GUINDA, GRIS_OSC], color_valor_capsu=[GUINDA, GRIS_OSC],
                    ancho=18, alto=8, ordenar=None, decimales=2, valor_barra=False,
                    asignar_etiquetas="__lab", capsu_cero=False, leyenda=None, **COMUNES)
plt.close("all")
hechas.append("03_epsilon_por_nivel")

# ==============================================================================
# 04 Impacto por eslabon: una grafica por escenario, mismo orden (el de C)
# ==============================================================================
es = lee(DIR_19 / "05b_resultado_por_eslabon.csv")
es = es[es["version_beta"] == VERSION].copy()
es["pct"] = 100 * es["var_pct"]
es["Eslabón"] = es["eslabon"].map(ESLABONES)
orden = (es[es["escenario"].str.startswith("C.")].sort_values("pct")["Eslabón"].tolist())
sufijo_capsula("%")
for letra, archivo in [("B", "04_impacto_eslabon_b"), ("C", "04_impacto_eslabon_c"), ("D", "04_impacto_eslabon_d")]:
    d4 = es[es["escenario"].str.startswith(letra + ".")].set_index("Eslabón").loc[orden].reset_index()
    d4["lab"] = [pct(v) for v in d4["pct"]]
    d4 = destacada(d4, "Eslabón", "pct", ~d4["Eslabón"].eq("Resto de electrónicos"), "lab")
    color = VERDE if letra == "D" else GUINDA
    v = d4["Destacada"] + d4["Resto"]
    limites = (min(v.min() * 1.5, 0), max(v.max() * 1.4, 0) or 0.5)   # espacio para las capsulas
    # Los nombres de los eslabones solo van en el primer panel (B); C y D comparten el orden
    etq = None if letra == "B" else [""] * len(d4)
    barras_horizontales(d4, nombre=nombre(archivo), colores=[color, GRIS_OSC], limites_ejex=limites,
                        color_borde_capsu=[color, GRIS_OSC], color_valor_capsu=[color, GRIS_OSC],
                        sustituir_valor_ejey=etq,
                        ancho=14 if letra == "B" else 10, alto=14, ordenar=None, decimales=1, valor_barra=False,
                        asignar_etiquetas="__lab", capsu_cero=False, solo_ejex_positivo=False,
                        simbolo_negativo_capsu=True, leyenda=None, **COMUNES)
    plt.close("all")
    hechas.append(archivo)

# Version unida: los tres paneles lado a lado, alineados por arriba
paneles = [plt.imread(str(RAIZ / "output" / SALIDA / f"04_impacto_eslabon_{x}.png")) for x in "bcd"]
alto_max = max(p.shape[0] for p in paneles)
fig_u, ejes = plt.subplots(1, 3, figsize=(sum(p.shape[1] for p in paneles) / 200, alto_max / 200),
                           gridspec_kw={"width_ratios": [p.shape[1] for p in paneles], "wspace": 0.08})
for eje, p in zip(ejes, paneles):
    eje.imshow(p); eje.set_xlim(0, p.shape[1]); eje.set_ylim(alto_max, 0); eje.axis("off")
fig_u.savefig(RAIZ / "output" / SALIDA / "04_impacto_eslabon_bcd.png", dpi=200, transparent=True, bbox_inches="tight", pad_inches=0)
plt.close("all")
hechas.append("04_impacto_eslabon_bcd")

# ==============================================================================
# 05 Escenarios de anexo (E y F): total y nucleo
# ==============================================================================
ic = lee(DIR_19 / "04b_intervalos_impacto.csv")
ic = ic[(ic["version_beta"] == VERSION) & ic["escenario"].str.match(r"^[EF]\.")
        & ic["nivel"].isin(["total", "1_nucleo"])].copy()
ic["Grupo"] = [f"{ESCENARIOS[e]}: {'total' if n == 'total' else 'núcleo'}" for e, n in zip(ic["escenario"], ic["nivel"])]
ic["pct"] = 100 * ic["var_pct_p50"]
ic["lab"] = [pct(v) for v in ic["pct"]]
ic = ic.sort_values(["escenario", "nivel"], ascending=[True, False])
d5 = destacada(ic, "Grupo", "pct", ic["nivel"].eq("1_nucleo"), "lab")
barras_horizontales(d5, nombre=nombre("05_anexo_tasas_monetizacion"), colores=[GUINDA, GRIS_OSC], color_borde_capsu=[GUINDA, GRIS_OSC], color_valor_capsu=[GUINDA, GRIS_OSC],
                    limites_ejex=(min(ic["pct"].min() * 1.6, -1), 0.5),
                    ancho=18, alto=8, ordenar=None, decimales=1, valor_barra=False,
                    asignar_etiquetas="__lab", capsu_cero=False, solo_ejex_positivo=False,
                    simbolo_negativo_capsu=True, leyenda=None, **COMUNES)
plt.close("all")
hechas.append("05_anexo_tasas_monetizacion")
sufijo_capsula("")

# ==============================================================================
# 06 Version anterior del paso 2 (anexo 8)
# ==============================================================================
archivo_04 = DIR_04 / "robustez_betas.csv"
if archivo_04.exists():
    specs = [("base", "Base (PPML, EUA)", "Base"),
             ("solo_definitivas", "Solo definitivas", "En niveles"),
             ("rezago_1t", "Capex del trimestre anterior", "En niveles"),
             ("sin_top3_nucleo", "Sin 3 mayores del núcleo", "En niveles"),
             ("continuos_ppml", "Solo NICO con comercio continuo", "En niveles"),
             ("ols_log", "OLS en logaritmos", "En niveles"),
             ("todos_origenes", "Todos los orígenes", "En niveles"),
             ("tendencia_grupo", "Con tendencia por grupo", "Con tendencia o en cambios"),
             ("continuos_tend_ols", "Continuos, OLS con tendencia", "Con tendencia o en cambios"),
             ("continuos_4t", "Continuos, cambios de 4 trim.", "Con tendencia o en cambios")]
    rb = lee(archivo_04)
    rb = rb[rb["grupo"] == "1_nucleo"]
    d6 = pd.DataFrame(specs, columns=["especificacion", "Especificación", "tipo"]).merge(rb, on="especificacion")
    z = (d6["beta_relativo"] / d6["ee_relativo"]).abs()
    from math import erf, sqrt
    p = [2 * (1 - 0.5 * (1 + erf(v / sqrt(2)))) for v in z]
    d6["lab"] = [f"{b:.2f}{estrellas(q)}" for b, q in zip(d6["beta_relativo"], p)]
    tabla = pd.DataFrame({"Especificación": d6["Especificación"]})
    for t in ["Base", "En niveles", "Con tendencia o en cambios"]:
        tabla[t] = np.where(d6["tipo"].eq(t), d6["beta_relativo"], 0.0)
    tabla["__lab"] = d6["lab"].values
    barras_horizontales(tabla, nombre=nombre("06_anexo_eta_capex_version_anterior"),
                        colores=[GUINDA, GRIS_OSC, GRIS], ancho=20, alto=15, ordenar=None,
                        color_borde_capsu=[GUINDA, GRIS_OSC, GRIS_OSC], color_valor_capsu=[GUINDA, GRIS_OSC, GRIS_OSC],
                        decimales=2, valor_barra=False, asignar_etiquetas="__lab", capsu_cero=False,
                        leyenda=True, posicion_leyenda=(0.45, -0.15), num_columnas_leyenda=3, **COMUNES)
    plt.close("all")
    hechas.append("06_anexo_eta_capex_version_anterior")
else:
    print(f"No esta {archivo_04}: se omite la grafica 06 (correr el script 04)", flush=True)

print("Graficas hechas: " + ", ".join(hechas), flush=True)
