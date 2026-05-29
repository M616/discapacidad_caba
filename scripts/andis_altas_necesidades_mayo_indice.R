{library(httr)
library(jsonlite)
library(tidyverse)
#library(Rinmoscrap)
library(stringr)
library(tictoc)
library(googledrive)
library(here)
library(sf)
library(mapview)
library(googlesheets4)
#library(readxl)
library(openxlsx)
library(janitor)
}

dir.create('data')
#base de marzo2026
drive_download('https://drive.google.com/file/d/1RcwuOIMC94PnhgrCysF1wXjdvPd2DzAv/view?usp=drive_link', 
path = "DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv")

base <- read_delim(
  'DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv')
base <- clean_names(base)

cie_vars <- paste0("cie10_", 1:8)


tabla <- base |>
  select(all_of(cie_vars)) |>
  pivot_longer(
    cols = everything(),
    values_to = "cie10"
  ) |>
  filter(!is.na(cie10), cie10 != "") |>
  count(cie10, sort = TRUE)

print(tabla)

tabla <- as.data.frame( tabla)

#write_csv(tabla,'data/tabla.csv')


table(base$ley_de_acompanante, useNA = "ifany")
table(base$vivienda_particular_o_colectiva, useNA = "ifany")
table(base$tipo_de_deficiencia_simple_multiple, useNA = "ifany")
table(base$tipo_de_deficiencia, useNA = "ifany")


table(base$equipamiento_1)
# ============================================================
# DICCIONARIO CIE10 PARA ÍNDICE DE
# ALTAS NECESIDADES DE APOYO (ANA)
# ============================================================
#
# OBJETIVO
# ------------------------------------------------------------
# Este diccionario clasifica códigos CIE10 según
# su asociación potencial con:
#
# - dependencia funcional,
# - necesidad de apoyos intensos,
# - apoyos frecuentes/permanentes,
# - complejidad funcional,
# - apoyos humanos/institucionales.
#
# IMPORTANTE:
# ------------------------------------------------------------
# NO clasifica "gravedad médica".
#
# Un diagnóstico puede ser grave clínicamente
# y NO implicar ANA.
#
# El foco está en:
# - intensidad de apoyos requeridos,
# - dependencia,
# - funcionalidad cotidiana.
#
# ============================================================



diccionario_cie <- tribble(

# ============================================================
# 1. DEPENDENCIA FUNCIONAL / APOYO DIRECTO
# ============================================================

~cie10,   ~descripcion,                                   ~grupo_ana,                     ~peso_ana,

"Z74.0",  "Movilidad reducida",                           "dependencia_funcional",        3,
"Z74.1",  "Necesidad de ayuda para cuidado personal",     "dependencia_funcional",        3,
"Z74.2",  "Necesidad de asistencia domiciliaria",         "dependencia_funcional",        3,
"Z74.3",  "Necesidad de supervision continua",            "dependencia_funcional",        4,
"Z74.8",  "Otros problemas relacionados cuidado",         "dependencia_funcional",        2,
"Z74.9",  "Problema relacionado cuidado no especificado", "dependencia_funcional",        2,

# ============================================================
# 2. DEPENDENCIA DE DISPOSITIVOS / APOYOS
# ============================================================

"Z99.0",  "Dependencia aspirador",                        "dependencia_dispositivos",     4,
"Z99.1",  "Dependencia respirador",                       "dependencia_dispositivos",     4,
"Z99.2",  "Dependencia dialisis",                         "dependencia_dispositivos",     3,
"Z99.3",  "Dependencia silla ruedas",                     "dependencia_dispositivos",     4,
"Z99.8",  "Dependencia otros dispositivos",               "dependencia_dispositivos",     2,
"Z99.9",  "Dependencia dispositivos NE",                  "dependencia_dispositivos",     2,

# ============================================================
# 3. PARÁLISIS / MOTORA SEVERA
# ============================================================

"G80",    "Paralisis cerebral",                           "motora_severa",               4,
"G80.0",  "Paralisis cerebral espastica",                 "motora_severa",               4,
"G80.1",  "Diplegia espastica",                           "motora_severa",               4,
"G80.2",  "Hemiplejia infantil",                          "motora_severa",               4,
"G80.3",  "Paralisis cerebral discinetica",               "motora_severa",               4,
"G80.4",  "Paralisis cerebral ataxica",                   "motora_severa",               4,
"G80.8",  "Otras paralisis cerebrales",                   "motora_severa",               4,
"G80.9",  "Paralisis cerebral NE",                        "motora_severa",               4,

"G81",    "Hemiplejia",                                   "motora_severa",               3,
"G81.0",  "Hemiplejia flacida",                           "motora_severa",               3,
"G81.1",  "Hemiplejia espastica",                         "motora_severa",               3,
"G81.9",  "Hemiplejia NE",                                "motora_severa",               3,

"G82",    "Paraplejia/tetraplejia",                       "motora_severa",               4,
"G82.0",  "Paraplejia flacida",                           "motora_severa",               4,
"G82.1",  "Paraplejia espastica",                         "motora_severa",               4,
"G82.2",  "Paraplejia NE",                                "motora_severa",               4,
"G82.3",  "Tetraplejia flacida",                          "motora_severa",               4,
"G82.4",  "Tetraplejia espastica",                        "motora_severa",               4,
"G82.5",  "Tetraplejia NE",                               "motora_severa",               4,

"G83",    "Otros sindromes paraliticos",                  "motora_severa",               3,
"G83.2",  "Monoplejia miembro superior",                  "motora_severa",               2,

# ============================================================
# 4. DEMENCIAS / DETERIORO COGNITIVO
# ============================================================

"F00",    "Demencia Alzheimer",                           "neurodegenerativa",           4,
"F00.0",  "Alzheimer inicio temprano",                    "neurodegenerativa",           4,
"F00.1",  "Alzheimer inicio tardio",                      "neurodegenerativa",           4,
"F00.2",  "Alzheimer atipico",                            "neurodegenerativa",           4,

"F01",    "Demencia vascular",                            "neurodegenerativa",           4,
"F01.3",  "Demencia mixta",                               "neurodegenerativa",           4,

"F02",    "Demencia otras enfermedades",                  "neurodegenerativa",           4,
"F02.0",  "Demencia Pick",                                "neurodegenerativa",           4,
"F02.3",  "Demencia Parkinson",                           "neurodegenerativa",           4,
"F02.8",  "Otras demencias clasificadas",                 "neurodegenerativa",           4,

"F03",    "Demencia NE",                                  "neurodegenerativa",           4,

"G20",    "Parkinson",                                    "neurodegenerativa",           3,
"G21",    "Parkinsonismo secundario",                     "neurodegenerativa",           3,
"G35",    "Esclerosis multiple",                          "neurodegenerativa",           3,

# ============================================================
# 5. DISCAPACIDAD INTELECTUAL
# ============================================================

"F70",    "DI leve",                                      "intelectual",                 1,
"F71",    "DI moderada",                                  "intelectual",                 2,
"F72",    "DI severa",                                    "intelectual",                 4,
"F73",    "DI profunda",                                  "intelectual",                 4,
"F78",    "Otra DI",                                      "intelectual",                 2,
"F79",    "DI NE",                                        "intelectual",                 2,

# ============================================================
# 6. TEA / NEURODESARROLLO
# ============================================================

"F84",    "TEA",                                          "neurodesarrollo",             2,
"F84.0",  "Autismo infantil",                             "neurodesarrollo",             2,
"F84.1",  "Autismo atipico",                              "neurodesarrollo",             2,

# ============================================================
# 7. EPILEPSIAS COMPLEJAS
# ============================================================

"G40",    "Epilepsia",                                    "neurologica_compleja",        2,
"G40.2",  "Epilepsia focal compleja",                     "neurologica_compleja",        3,

# ============================================================
# 8. TRASTORNOS FUNCIONALES IMPORTANTES
# ============================================================

"R26",    "Trastornos marcha",                            "limitacion_funcional",        1,
"R26.0",  "Marcha ataxica",                               "limitacion_funcional",        1,
"R26.2",  "Dificultad caminar",                           "limitacion_funcional",        1,
"R26.8",  "Otros trastornos marcha",                      "limitacion_funcional",        1,

"R27",    "Falta coordinacion",                           "limitacion_funcional",        1,

"R32",    "Incontinencia urinaria",                       "limitacion_funcional",        1,

"R47",    "Alteraciones habla",                           "limitacion_funcional",        1,
"R47.1",  "Disartria",                                    "limitacion_funcional",        1,

# ============================================================
# 9. APOYOS VITALES / RESPIRATORIOS
# ============================================================

"J96",    "Insuficiencia respiratoria",                   "apoyo_vital",                 4,
"J96.0",  "Insuficiencia respiratoria aguda",             "apoyo_vital",                 4,

# ============================================================
# 10. OSTOMÍAS / TRAQUEOSTOMÍAS
# ============================================================

"Z93",    "Aberturas artificiales",                       "apoyo_vital",                 3,

# ============================================================
# 11. LESIONES MEDULARES / AMPUTACIONES MAYORES
# ============================================================

"S48.1",  "Amputacion brazo",                             "motora_severa",               2,
"S98.2",  "Amputacion pie bilateral",                     "motora_severa",               3,
"S98.3",  "Amputacion otros pies",                        "motora_severa",               2,
"Z89.5",  "Ausencia adquirida pierna",                    "motora_severa",               2,
"Z89.6",  "Ausencia adquirida piernas",                   "motora_severa",               3

)

# ============================================================
# OBSERVACIÓN METODOLÓGICA
# ============================================================
#
# Este diccionario es:
#
# - EXPLORATORIO
# - OPERACIONAL
# - PERFECTIBLE
#
# Debe:
# - revisarse empíricamente,
# - validarse con expertos,
# - contrastarse con casos reales.
#
# No reemplaza evaluación clínica ni funcional.
#
# ============================================================

cie_vars <- paste0("cie10_", 1:8)

cie_long <- base |>

  mutate(id = row_number()) |>

  pivot_longer(
    cols = all_of(cie_vars),
    names_to = "cie_var",
    values_to = "cie10"
  ) |>

  filter(
    !is.na(cie10),
    cie10 != ""
  )


cie_long <- cie_long |>

  mutate(

    cie10 = str_replace_all(cie10, "\\*", ""),

    cie10_base =
      str_extract(cie10, "^[A-Z][0-9]{2}(\\.[0-9])?")

  )



cie_long <- cie_long |>

  left_join(
    diccionario_cie,
    by = c("cie10_base" = "cie10")
  )


p <- cie_long |>

  filter(is.na(grupo_ana)) |>

  count(cie10_base, sort = TRUE) |> slice(1:40)
