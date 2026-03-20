import pandas as pd
import janitor
import hashlib
import re

base = pd.read_csv('data/Anonimizado CUD_vigentesCABA2026. 4 de MARZO(CABA).csv', sep = ';') 
base = base.clean_names()
base = base[["domicilio","numero_domicilio","tipo_de_vivienda"]]

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
p = pd.read_csv("data/processed/domicilios.csv") 

# comprimir a zip
with zipfile.ZipFile("data/processed/domicilios.zip", "w", compression=zipfile.ZIP_DEFLATED) as z:
    z.write(ruta_csv)


###aca cuando me llegue la base
base = pd.read_csv('data/Anonimizado CUD_vigentesCABA2026. 4 de MARZO(CABA).csv', sep = ';') 
base = base.clean_names()
base[['id_raw', 'id_clean']] = base.apply(generar_ids, axis=1)

merged = base.merge(
    base_devuelta,
    on='id_raw',
    how='left',
    indicator=True
)

###si hay casos rotos o faltantes
faltantes = merged[merged['_merge'] == 'left_only']

base_devuelta['id_clean'] = base_devuelta.apply(
    lambda row: hash_string(
        f"{normalizar(row['domicilio'])}_{str(row['numero_domicilio']).strip()}"
    ),
    axis=1
)

recuperados = faltantes.drop(columns=base_devuelta.columns, errors='ignore').merge(
    base_devuelta,
    on='id_clean',
    how='left'
)