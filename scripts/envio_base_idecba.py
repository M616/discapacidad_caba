from numpy import right_shift
import pandas as pd
import janitor
import hashlib
import re
import openpyxl

base = pd.read_csv('data/Anonimizado CUD_vigentesCABA2026. 4 de MARZO(CABA).csv', sep = ';') 
base = base.clean_names()
#base = base[["domicilio","numero_domicilio","tipo_de_vivienda"]]

# --- normalización simple ---
def normalizar(texto):
    if pd.isna(texto):
        return ""
    texto = str(texto).lower().strip()
    texto = re.sub(r'\s+', ' ', texto)  # espacios múltiples → uno
    return texto

# --- hash ---
def hash_string(s):
    return hashlib.md5(s.encode()).hexdigest()

# --- generar IDs ---
def generar_ids(row):
    # RAW (sin tocar)
    raw = f"{row['domicilio']}_{row['numero_domicilio']}"
    
    # CLEAN (normalizado)
    calle_clean = normalizar(row['domicilio'])
    altura_clean = str(row['numero_domicilio']).strip()
    clean = f"{calle_clean}_{altura_clean}"
    
    return pd.Series({
        'id_raw': hash_string(raw),
        'id_clean': hash_string(clean)
    })

base[['id_raw', 'id_clean']] = base.apply(generar_ids, axis=1)
base_envio = base[['id_raw', 'domicilio', 'numero_domicilio']]

import zipfile

# guardar csv
ruta_csv = "data/processed/domicilios.csv"



base_envio.to_csv(ruta_csv, index=False)
#p = pd.read_csv("data/processed/domicilios.csv") 

# comprimir a zip
with zipfile.ZipFile("data/processed/domicilios.zip", "w", compression=zipfile.ZIP_DEFLATED) as z:
    z.write(ruta_csv)


###aca cuando me llegue la base

#Estimado Marcos,
#Espero que te encuentre bien.
#Te escribo para adjuntarte la base de datos de DICA correspondiente a la Ciudad Autónoma de Buenos Aires. Hemos procesado la base que nos proporcionaste para su​discapacidad_entrega1 GEO.xlsx​ georreferenciación, logrando un total de 143,841 registros georreferenciados de un total de 150,416.
#De estos registros, 142,878 cuentan con coordenadas X e Y. Los 963 restantes no pudieron ser georreferenciados, posiblemente debido a que pertenecen a la provincia de Buenos Aires o presentan domicilios poco claros.
#Adicionalmente, aún tenemos 6,575 registros pendientes que estamos revisando si corresponden a BaPIS. Te los estaremos enviando a la brevedad.
#Quedo a tu disposición para cualquier consulta o comentario que pudieras tener al respecto.
#Saludos cordiales,
#Martín


#base=pd.read_csv("data/processed/domicilios.csv") 
base_devuelta = pd.read_excel("data/raw/idecba/discapacidad_entrega1 GEO.xlsx", engine="openpyxl")

base_devuelta = base_devuelta.clean_names()

## se repiten los ids. hay ids que tienen mas de un par de coordenadas lat long, otros que no y se repiten. 
resumen = (
    base_devuelta
    .drop_duplicates(subset=['id_raw_c_254', 'longitude_n_19_11', 'latitude_n_19_11'])
    .groupby('id_raw_c_254')
    .size()
    .reset_index(name='n_coords_unicas')
)


ids_conflictivos = resumen.loc[
    resumen['n_coords_unicas'] > 1,
    'id_raw_c_254'
]

casos = (
    base_devuelta[base_devuelta['id_raw_c_254'].isin(ids_conflictivos)]
    .sort_values(['id_raw_c_254'])
)


# me quedo solo con los ids que tienen solo un par de coordenadas y un solo id por fila

# 1. quedarte con combinaciones únicas id + coords
tmp = base_devuelta.drop_duplicates(
    subset=['id_raw_c_254', 'longitude_n_19_11', 'latitude_n_19_11']
)

# 2. contar cuántas coords únicas tiene cada id
conteo = (
    tmp
    .groupby('id_raw_c_254')
    .size()
    .reset_index(name='n_coords')
)

# 3. quedarte solo con ids que tienen 1 coordenada
ids_validos = conteo.loc[
    conteo['n_coords'] == 1,
    'id_raw_c_254'
]

# 4. filtrar dataset original
base_filtrada = base_devuelta[
    base_devuelta['id_raw_c_254'].isin(ids_validos)
]

# 5. asegurarte 1 fila por id
base_filtrada = base_filtrada.drop_duplicates(subset='id_raw_c_254')



merged = base.merge(
    base_filtrada,
    left_on ='id_raw',
    right_on = 'id_raw_c_254',
    how='left',
    indicator=True
)


merged = merged[merged['provincia_de_residencia'] == 'Ciudad Autónoma de Buenos Aires']



###si hay casos rotos o faltantes
faltantes = merged[merged['_merge'] == 'left_only']

base_devuelta['id_clean'] = base_devuelta.apply(
    lambda row: hash_string(
        f"{normalizar(row['domicilio_c_254'])}_{str(row['numero_dom_c_254']).strip()}"
    ),
    axis=1
)

recuperados = faltantes.drop(columns=base_devuelta.columns, errors='ignore').merge(
    base_devuelta,
    on='id_clean',
    how='left'
)

##exporto
merged = merged.drop(columns=['id_clean'])

ruta_csv = "data/processed/idecba_geocodificacion_domicilios.csv"
merged.to_csv(ruta_csv, index=False)
import zipfile
# comprimir a zip
with zipfile.ZipFile("data/processed/idecba_domicilios.zip", "w", compression=zipfile.ZIP_DEFLATED) as z:
    z.write(ruta_csv)