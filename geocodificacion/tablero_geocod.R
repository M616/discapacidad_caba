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

saveRDS(indicadores_geo, "app_geocode/data/indicadores_geo.rds")