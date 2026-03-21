import pandas as pd
import janitor
import geopandas as gpd
import matplotlib.pyplot as plt


base = pd.read_csv("data/processed/usig/usig_marzo.csv")

personas_cud = len(base)

tipo_def_pct = (
    base["tipo_de_deficiencia_simple_multiple"]
    .value_counts(normalize=True) * 100
).round()


base["edad_al_inicio_del_dano"].describe()

###piramido poblacional
# ===== 1. Filtrar y agrupar =====
piramide = (
    base[base["sexo"].isin(["Masculino", "Femenino"])]
    .groupby(["grupos_quinquenales", "sexo"])
    #.groupby(["edad_a_la_solicitud_del_certificado", "sexo"])
    #.groupby(["edad_al_inicio_del_dano", "sexo"])    
    .size()
    .unstack(fill_value=0)
)

# ===== 2. Asegurar columnas =====
piramide = piramide.reindex(columns=["Masculino", "Femenino"], fill_value=0)

# ===== 3. Hombres en negativo =====
piramide["Masculino"] = -piramide["Masculino"]

# ===== 4. Ordenar edades (si hace falta) =====
#piramide = piramide.sort_index(ascending=True)

import re

# función para extraer el primer número del rango
def extraer_edad(x):
    match = re.search(r"\d+", str(x))
    return int(match.group()) if match else 999

# ordenar índice correctamente
piramide = piramide.loc[
    sorted(piramide.index, key=extraer_edad)
]

# ===== 5. Gráfico =====
plt.figure()

plt.barh(piramide.index, piramide["Masculino"], label="Masculino")
plt.barh(piramide.index, piramide["Femenino"], label="Femenino")

# Línea central
plt.axvline(0)

# Títulos
plt.title("Pirámide poblacional")
plt.xlabel("Cantidad de personas")
plt.ylabel("Grupos quinquenales")

# Leyenda
plt.legend()

# Mejorar estética
plt.tight_layout()

# Guardar
#plt.savefig("piramide.png")
#plt.close()



resumen = {
    "total_cud": len(base),
    "edad_promedio": base["edad_a_la_solicitud_del_certificado"].mean(),
    "edad_mediana": base["edad_a_la_solicitud_del_certificado"].median(),
    "porc_vivienda_colectiva": (
        (base["vivienda_particular_o_colectiva"] == "Colectiva").mean() * 100
    ),
    "porc_cobertura_publica":
        (base["cobertura_de_salud"].isin(["Pública", "Programa Nacional y/o Provincial de Salud"])
    .mean() * 100)
    }

print(resumen)


#cargo comunas 

base = gpd.GeoDataFrame(
    base,
    geometry=gpd.points_from_xy(base["lon"], base["lat"]),
    crs="EPSG:4326"
)


url_comunas_caba = (
    "https://wms.ign.gob.ar/geoserver/ows?"
    "service=WFS&version=1.1.0&request=GetFeature&"
    "typeName=ign:departamento&outputFormat=application/json&"
    "cql_filter=gna='Comuna'"
)

comunas_caba = gpd.read_file(url_comunas_caba)

comunas_caba["comuna"] = comunas_caba["nam"]
comunas_caba = comunas_caba[["comuna", "geometry"]]

base = gpd.sjoin(
    base,
    comunas_caba,
    how="left",          
    predicate="intersects"  
)

# filtrar comuna no nula
base = base[base["comuna"].notna()]

# filtrar geometrías no vacías
base = base[~base.geometry.is_empty]


#proyeccion por comuna segun indec
url = "https://www.indec.gob.ar/ftp/cuadros/poblacion/base_estimaciones_pob_deptos_2022_2035.csv"
df = pd.read_csv(url, sep = ';')
df = df.clean_names()

df = df[
    (df["fecha"] == 2025) & 
    (df["nombre_jurisdiccion"] == "CABA")
    ]


df['poblacion_proyectada'] = df['poblacion']

df = (
    df
    .groupby("nombre_departamento", as_index=False)["poblacion_proyectada"]
    .sum()
)


base = base.merge(df,
    left_on="comuna",                # columna en tu base
    right_on="nombre_departamento", # columna en la otra tabla
    how="left"
)


#Porcentaje de personas con CUD sobre población total
#  → indicador de prevalencia en la población general 
# ver si tiene sentido este indicador porque es personas con cud, no con discapacidad


prevalencia_cud = personas_cud / df.poblacion_proyectada.sum() * 100





