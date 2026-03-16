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


async def run(direcciones):

    async with aiohttp.ClientSession() as session:

        tasks = [geocode(session, d) for d in direcciones]

        results = []

        for task in tqdm(asyncio.as_completed(tasks), total=len(tasks)):
            res = await task
            results.append(res)

    return results


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

direcciones = base["direccion_usig"].tolist()


results = await run([direcciones[0]])

df = pd.DataFrame(results)

print(df)
