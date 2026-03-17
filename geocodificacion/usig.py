## aca esta la documentacion
'https://servicios.usig.buenosaires.gob.ar/normalizar/'

import asyncio
import aiohttp
import pandas as pd
import janitor
from tqdm.asyncio import tqdm
import requests


URL = "https://servicios.usig.buenosaires.gob.ar/normalizar/"

PARAMS_BASE = {
    "geocodificar": "true",
    "maxOptions": 1,
    "tipoResultado": "calle_altura",
    "srid": 4326
}

MAX_CONCURRENT = 10
RETRIES = 3

sem = asyncio.Semaphore(MAX_CONCURRENT)


async def geocode(session, direccion):

    params = PARAMS_BASE.copy()
    params["direccion"] = direccion

    for intento in range(RETRIES):

        try:
            async with sem:
                async with session.get(URL, params=params, timeout=10) as resp:

                    if resp.status != 200:
                        await asyncio.sleep(1)
                        continue

                    data = await resp.json()

                    res = data["direccionesNormalizadas"][0]

                    return {
                        "direccion_input": direccion,
                        "direccion_norm": res["direccion"],
                        "lon": float(res["coordenadas"]["x"]),
                        "lat": float(res["coordenadas"]["y"]),
                        "tipo": res["tipo"]
                    }

        except Exception:
            await asyncio.sleep(1 + intento)

    return {
        "direccion_input": direccion,
        "direccion_norm": None,
        "lon": None,
        "lat": None,
        "tipo": None
    }


async def geocode(session, item):

    idx, direccion = item

    params = PARAMS_BASE.copy()
    params["direccion"] = direccion

    for intento in range(RETRIES):

        try:
            async with sem:
                async with session.get(URL, params=params, timeout=10) as resp:

                    if resp.status != 200:
                        await asyncio.sleep(1)
                        continue

                    data = await resp.json()

                    res = data["direccionesNormalizadas"][0]

                    return {
                        "id": idx,
                        "direccion_input": direccion,
                        "direccion_norm": res["direccion"],
                        "lon": float(res["coordenadas"]["x"]),
                        "lat": float(res["coordenadas"]["y"]),
                        "tipo": res["tipo"]
                    }

        except Exception:
            await asyncio.sleep(1 + intento)

    return {
        "id": idx,
        "direccion_input": direccion,
        "direccion_norm": None,
        "lon": None,
        "lat": None,
        "tipo": None
    }


base = pd.read_csv('data/Anonimizado CUD_vigentesCABA2026. 4 de MARZO(CABA).csv', sep = ';') 
base = base.clean_names()

import re

def limpiar_calle(x):

    x = x.upper()

    # eliminar barrios comunes
    x = re.sub(r'\b(VILLA|BARRIO|B°)\s+\w+', '', x)

    # eliminar barrios sueltos conocidos
    barrios = [
        "RECOLETA","CABALLITO","PALERMO","BELGRANO",
        "DEVOTO","FLORES","ALMAGRO","BOEDO", "SAN TELMO",
        "SAN NICOLAS"
    ]

    for b in barrios:
        x = x.replace(b, "")

    x = x.replace("AV.", "AV")

    return x.strip()

base["calle"] = base["domicilio"].apply(limpiar_calle)

base[["calle","numero_domicilio"]].sample(20)

base["direccion_usig"] = (
    base["calle"] + " " +
    base["numero_domicilio"].astype(str) +
    ", CABA"
)

direcciones = list(zip(base.index, base["direccion_usig"]))

async def run(direcciones):

    async with aiohttp.ClientSession() as session:

        tasks = [geocode(session, d) for d in direcciones]

        results = []

        for task in tqdm(asyncio.as_completed(tasks), total=len(tasks)):
            res = await task
            results.append(res)

    return results


#results = await run(direcciones)

results = await run(direcciones)

df = pd.DataFrame(results)

print(df)

df.to_csv("data/processed/usig/usig_marzo.csv", encoding="utf-8")

df_final = base.merge(
    df,
    left_index=True,
    right_on="id",
    how="left"
)

df_final.to_csv("data/processed/usig/usig_marzo.csv", encoding="utf-8")


import geopandas as gpd

gdf = gpd.GeoDataFrame(
    df_final,
    geometry=gpd.points_from_xy(df_final["lon"], df_final["lat"]),
    crs="EPSG:4326"
)

#cargo comunas 
url_comunas_caba = (
    "https://wms.ign.gob.ar/geoserver/ows?"
    "service=WFS&version=1.1.0&request=GetFeature&"
    "typeName=ign:departamento&outputFormat=application/json&"
    "cql_filter=gna='Comuna'"
)

comunas_caba = gpd.read_file(url_comunas_caba)

comunas_caba["comuna"] = comunas_caba["nam"]
comunas_caba = comunas_caba[["comuna", "geometry"]]
base = gpd.read_file ("data/processed/usig/usig_direcciones_cud_marzo.gpkg")

base = gpd.sjoin(
    base,
    comunas_caba,
    how="left",          
    predicate="intersects"  
)

total = len(base)

tabla = pd.DataFrame({
    "tipo": ["vacia", "nula", "valida"],
    "cantidad": [
        base.geometry.is_empty.sum(),
        base.geometry.isna().sum(),
        (~base.geometry.is_empty & ~base.geometry.isna()).sum()
    ]
})

tabla["porcentaje"] = (tabla["cantidad"] / total) * 100


base.to_file("data/processed/usig/usig_direcciones_cud_marzo.gpkg", driver="GPKG")
