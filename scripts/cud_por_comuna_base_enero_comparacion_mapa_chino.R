library(tidyverse)
library(sf)

base <-read_csv("data/processed/usig/usig_marzo.csv")


base_sf <- st_as_sf(
  base,
  coords = c("lon", "lat"),
  crs = 4326,
  na.fail = FALSE
)

# =====================================================
# CARGAR COMUNAS CABA
# =====================================================
url_comunas_caba <- paste0(
  "https://wms.ign.gob.ar/geoserver/ows?",
  "service=WFS&version=1.1.0&request=GetFeature&",
  "typeName=ign:departamento&outputFormat=application/json&",
  "cql_filter=gna='Comuna'"
)

comunas_caba <- st_read(url_comunas_caba, quiet = TRUE) %>%
  mutate(comuna = nam) %>%
  select(comuna, geometry)

# =====================================================
# JOIN ESPACIAL
# =====================================================
base_sf <- st_join(base_sf, comunas_caba, left = TRUE)

# filtrar
base_sf <- base_sf %>%
  filter(!is.na(comuna)) %>%
  filter(!st_is_empty(geometry))

base_sf |> 
  st_drop_geometry() |> 
  group_by(comuna) |> summarise(cantidad_personas = n()) |> 
  arrange(desc(cantidad_personas))


#############base mayo (corte junio 2026)

base <-read_csv("data/processed/usig/usig_mayo.csv")
table

base$comuna <- NULL

base_sf <- st_as_sf(
  base,
  coords = c("lon", "lat"),
  crs = 4326,
  na.fail = FALSE
)

# =====================================================
# CARGAR COMUNAS CABA
# =====================================================
url_comunas_caba <- paste0(
  "https://wms.ign.gob.ar/geoserver/ows?",
  "service=WFS&version=1.1.0&request=GetFeature&",
  "typeName=ign:departamento&outputFormat=application/json&",
  "cql_filter=gna='Comuna'"
)

comunas_caba <- st_read(url_comunas_caba, quiet = TRUE) %>%
  mutate(comuna = nam) %>%
  select(comuna, geometry)

# =====================================================
# JOIN ESPACIAL
# =====================================================
base_sf <- st_join(base_sf, comunas_caba, left = TRUE)

# filtrar
base_sf <- base_sf %>%
  filter(!is.na(comuna)) %>%
  filter(!st_is_empty(geometry))

base_sf |> 
  st_drop_geometry() |> 
  group_by(comuna) |> summarise(cantidad_personas = n()) |> 
  arrange(desc(cantidad_personas))


