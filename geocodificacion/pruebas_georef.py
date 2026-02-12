import polars as pl
import geopandas as gpd

df = pl.read_excel(
    "data/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx",
    read_options={
        "skip_rows": 1  # startRow = 2 → saltea la primera fila
    }
)

df["Tipo de vivienda"].value_counts(sort=True)
df["Tipo de calle"].value_counts(sort=True)


gdf = gpd.read_file(
    "/home/marcos/Documentos/discapacidad_caba/data/georef/base2.gpkg"
)

gdf = gdf[["id", "geometry"]]

print(df.schema)
print(gdf.dtypes)

gdf["id"] = gdf["id"].astype("int64")
df = df.rename({'Número': "id"})
pdf = df.to_pandas()

gdf_final = gdf.merge(
    pdf,
    on="id",
    how="left"
)
gdf_final = gdf_final[
    gdf_final.geometry.notnull() & 
    ~gdf_final.geometry.is_empty
]


m = gdf_final.explore(
    column="Tipo de vivienda",
    categorical=True,
    legend=True,
    tooltip=["id", "Tipo de calle", "Tipo de vivienda"]
)

m

#calles de tierra
gdf_tierra = gdf_final[gdf_final["Tipo de calle"] == "Tierra"]


m = gdf_tierra.explore(
    color="red",
    tooltip=["id", "Tipo de vivienda", "Tipo de calle"],
    tiles="CartoDB positron"
)

m

## es rarisimo las calles de tierra que aparecen, parece estar mal la variable


