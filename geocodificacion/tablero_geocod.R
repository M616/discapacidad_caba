{library(rsconnect)
library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(openxlsx)
library(janitor)
library(DataExplorer)
library(shiny)
library(leaflet)
}


base <- read.xlsx(
  #'data/cud_anonimizada_octubre25.xlsx',
  #'data/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx',
  'data/raw/dgippd/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx', 
#cols = 1:3,
#startRow = 2
)

base <- janitor::clean_names(base)

base2 <- st_read('data/processed/georef/base2_georef.gpkg')

base2 <- 
  base2 |> 
  select(id, direccion_georef, lon_georef, lat_georef) |> 
  left_join(base, by = c('id' = 'numero'))


indicadores_geo <- list(
  total_casos = nrow(base2),
  respuestas_api = sum(!st_is_empty(base2$geom)),
  tasa_respuesta_api = round(
  mean(!st_is_empty(base2$geom)) * 100,
  1)
)

saveRDS(indicadores_geo, "data/processed/indicadores_geo.rds")

#DataExplorer::create_report(st_drop_geometry(base2))

# URL con filtro CQL por el atributo 'gna' = 'Comuna'
url_comunas_caba <- "https://wms.ign.gob.ar/geoserver/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=ign:departamento&outputFormat=application/json&cql_filter=gna='Comuna'"
# Descargar solo las comunas
comunas_caba <- st_read(url_comunas_caba)

comunas_caba <- 
  comunas_caba |> 
  mutate(comuna = nam) |> 
  select(comuna)

geom_comunas <- comunas_caba

base2 <- 
  base2 |> 
  select(
    edad_actual,
    edad_de_inicio_del_dano,
    tipo_de_vivienda,
    nivel_de_hacinamiento,
    vivienda_adaptada,
    tipo_de_convivencia,
    cobertura_de_salud,
    tipo_de_deficiencia_simple_multiple,
    vivienda_particular_o_colectiva,
    convivencia_con_pcd,
    equipamiento)

comunas_caba <- 
  st_join(comunas_caba,
        base2)


tabla_resumen <- 
  comunas_caba %>%
  st_drop_geometry() |> 
  group_by(comuna) %>%
  summarise(
    total_personas = n(),
    pct_vivienda_adaptada = mean(vivienda_adaptada == "Sí", na.rm = TRUE) * 100,
    pct_cobertura_publica = mean(cobertura_de_salud %in%  c('Programa Nacional y/o Provincial de Salud','Pública'), na.rm = TRUE) * 100,
    pct_hacinamiento = mean(nivel_de_hacinamiento %in% c('Nivel Crítico', 'Nivel Moderado'), na.rm = TRUE) * 100,
    pct_colectiva = mean(vivienda_particular_o_colectiva == 'Colectiva', na.rm = TRUE) * 100,
    pct_intelectual = mean(tipo_de_deficiencia_simple_multiple == 'Intelectual', na.rm = TRUE) * 100,
    pct_equipamiento = mean(equipamiento == 'Con equipamiento', na.rm = TRUE) * 100,
    edad_inicio_del_dano_promedio = mean(edad_de_inicio_del_dano, na.rm = TRUE),
    edad_promedio = mean(edad_actual, na.rm = TRUE)
  )

tabla_mapa <- geom_comunas %>%
  left_join(tabla_resumen, by = "comuna")

saveRDS(tabla_mapa, "data/processed/tabla_mapa.rds")
saveRDS(tabla_mapa, "app_geocode/data/tabla_mapa.rds")
saveRDS(indicadores_geo, "app_geocode/data/indicadores_geo.rds")