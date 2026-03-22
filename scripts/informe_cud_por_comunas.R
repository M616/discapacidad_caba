# =====================================================
# LIBRERÍAS
# =====================================================
library(tidyverse)
library(janitor)
library(sf)
library(patchwork)

# =====================================================
# CARGA DE DATOS
# =====================================================
base <- read_csv("data/processed/usig/usig_marzo.csv")

personas_cud <- nrow(base)

# =====================================================
# TIPO DE DEFICIENCIA (%)
# =====================================================
tipo_def_pct <- base %>%
  tabyl(tipo_de_deficiencia_simple_multiple) %>%
  mutate(pct = round(100 * n / sum(n), 0))

# =====================================================
# RESUMEN VARIABLE NUMÉRICA
# =====================================================
resumen_edad_certificado <- base %>%
  summarise(
    count = sum(!is.na(edad_a_la_solicitud_del_certificado)),
    mean = mean(edad_a_la_solicitud_del_certificado, na.rm = TRUE),
    median = median(edad_a_la_solicitud_del_certificado, na.rm = TRUE),
    sd = sd(edad_a_la_solicitud_del_certificado, na.rm = TRUE),
    min = min(edad_a_la_solicitud_del_certificado, na.rm = TRUE),
    max = max(edad_a_la_solicitud_del_certificado, na.rm = TRUE)
  )

# =====================================================
# PIRÁMIDE POBLACIONAL
# =====================================================

# 1. Agrupar
piramide <- base %>%
  filter(sexo %in% c("Masculino", "Femenino")) %>%
  count(grupos_quinquenales, sexo) %>%
  pivot_wider(names_from = sexo, values_from = n, values_fill = 0)

# 2. Hombres en negativo
piramide <- piramide %>%
  mutate(Masculino = -Masculino)

# 3. Ordenar edades
extraer_edad <- function(x) {
  as.numeric(stringr::str_extract(x, "\\d+"))
}

piramide <- piramide %>%
  arrange(extraer_edad(grupos_quinquenales)) %>%
  mutate(
    grupos_quinquenales = factor(
      grupos_quinquenales,
      levels = unique(grupos_quinquenales)
    )
  )

# 4. Gráfico
piramide_long <- piramide %>%
  pivot_longer(cols = c(Masculino, Femenino),
               names_to = "sexo",
               values_to = "cantidad")

ggplot(piramide_long, aes(x = cantidad, y = grupos_quinquenales, fill = sexo)) +
  geom_col() +
  geom_vline(xintercept = 0) +
  labs(
    title = "Pirámide poblacional",
    x = "Cantidad de personas",
    y = "Grupos quinquenales"
  ) +
  theme_minimal()

# =====================================================
# RESUMEN GENERAL
# =====================================================
resumen <- list(
  total_cud = nrow(base),
  edad_promedio = mean(base$edad_a_la_solicitud_del_certificado, na.rm = TRUE),
  edad_mediana = median(base$edad_a_la_solicitud_del_certificado, na.rm = TRUE),
  porc_vivienda_colectiva = mean(base$vivienda_particular_o_colectiva == "Colectiva", na.rm = TRUE) * 100,
  porc_cobertura_publica = mean(
    base$cobertura_de_salud %in% c("Pública", "Programa Nacional y/o Provincial de Salud"),
    na.rm = TRUE
  ) * 100
)

print(resumen)

# =====================================================
# GEO: CREAR sf
# =====================================================
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

# =====================================================
# POBLACIÓN INDEC
# =====================================================
url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/base_estimaciones_pob_deptos_2022_2035.csv"

df <- read_csv2(url) %>%  # sep=";"
  clean_names() %>%
  filter(
    fecha == 2025,
    nombre_jurisdiccion == "CABA"
  ) %>%
  mutate(poblacion_proyectada = poblacion) %>%
  group_by(nombre_departamento) %>%
  summarise(poblacion_proyectada = sum(poblacion_proyectada), .groups = "drop")

# =====================================================
# MERGE
# =====================================================
base_sf <- base_sf %>%
  left_join(df, by = c("comuna" = "nombre_departamento"))

# =====================================================
# PREVALENCIA
# =====================================================
prevalencia_cud <- personas_cud / sum(df$poblacion_proyectada, na.rm = TRUE) * 100

print(prevalencia_cud)

#############mapa
base_proj <- st_transform(base_sf, 3857)
comunas_proj <- st_transform(comunas_caba, 3857)

coords <- st_coordinates(base_proj)
df_coords <- as.data.frame(coords)

ggplot() +
  
  geom_density_2d_filled(
    data = df_coords,
    aes(
      X, Y,
      fill = after_stat(level),
      alpha = after_stat(as.numeric(level))   # 👈 ACÁ
    ),
    bins = 10,
    adjust = 1.2,
    show.legend = FALSE
  ) +
  
  scale_fill_viridis_d(
    option = "magma",
    direction = -1
  ) +
  
  scale_alpha(
    range = c(0, 1),   # 👈 y esto
    guide = "none"
  ) +
  
  geom_sf(
    data = comunas_proj,
    fill = NA,
    color = "white",
    size = 0.3    
  ) +
   
  geom_sf(
    data = st_union(comunas_proj),
    fill = NA,
    color = "black",
    size = 0.4
  ) +
  
  coord_sf(expand = FALSE) +
  
  theme_void()