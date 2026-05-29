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
#drive_download('https://drive.google.com/file/d/1RcwuOIMC94PnhgrCysF1wXjdvPd2DzAv/view?usp=drive_link', 
#path = "DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv")

base <- read_delim(
  'DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv')

# =========================================================
# EXPLORACIÓN CIE10 PARA ALTAS NECESIDADES DE APOYO
# =========================================================
#
# Objetivo:
# Construir una estructura analítica de CIE10:
#
# 1. limpiar códigos
# 2. pasar la base a formato largo
# 3. generar códigos base
# 4. identificar capítulos
# 5. clasificar tipos funcionales
# 6. analizar frecuencias
# 7. preparar co-ocurrencias
#
# =========================================================


# ---------------------------------------------------------
# 1. nombres limpios
# ---------------------------------------------------------

base <- base |>
  clean_names()

# ---------------------------------------------------------
# 2. variables CIE
# ---------------------------------------------------------

cie_vars <- paste0("cie10_", 1:8)

# ---------------------------------------------------------
# 3. pasar a formato largo
# ---------------------------------------------------------
#
# Cada fila pasa a ser:
#
# persona | variable_cie | codigo
#
# Esto es FUNDAMENTAL para:
# - frecuencias
# - co-ocurrencias
# - MCA
# - clustering
#
# ---------------------------------------------------------

base_cie <- base |>

  # crear id único
  mutate(
    id_persona = row_number()
  ) |>

  # pasar CIE a largo
  pivot_longer(

    cols = all_of(cie_vars),

    names_to = "variable_cie",
    values_to = "cie10"

  ) |>

  # eliminar vacíos
  filter(
    !is.na(cie10),
    cie10 != ""
  )

# ---------------------------------------------------------
# 4. limpiar códigos CIE
# ---------------------------------------------------------
#
# Ejemplos:
#
# F00*   -> F00
# H90.3  -> H90.3
#
# ---------------------------------------------------------

base_cie <- base_cie |>

  mutate(

    # sacar asteriscos
    cie10 =
      str_replace_all(cie10, "\\*", ""),

    # mayúsculas
    cie10 =
      str_to_upper(cie10)

  )

# ---------------------------------------------------------
# 5. generar niveles analíticos
# ---------------------------------------------------------
#
# cie10_detalle:
#   conserva decimal
#
# cie10_base:
#   reduce a 3 caracteres
#
# capitulo:
#   primera letra ICD
#
# ---------------------------------------------------------

base_cie <- base_cie |>

  mutate(

    # código detallado
    cie10_detalle =
      str_extract(
        cie10,
        "^[A-Z][0-9]{2}(\\.[0-9])?"
      ),

    # código base
    cie10_base =
      str_extract(
        cie10,
        "^[A-Z][0-9]{2}"
      ),

    # capítulo ICD
    capitulo =
      str_sub(cie10_base, 1, 1)

  )

# ---------------------------------------------------------
# 6. clasificación funcional preliminar
# ---------------------------------------------------------
#
# Esto NO es todavía el índice ANA.
#
# Es una clasificación exploratoria
# para entender perfiles funcionales.
#
# ---------------------------------------------------------

base_cie <- base_cie |>

  mutate(

    grupo_funcional = case_when(

      # ---------------------------------------------------
      # discapacidad intelectual / neurodesarrollo
      # ---------------------------------------------------

      cie10_base %in% c(
        paste0("F", 70:79),
        "F80",
        "F81",
        "F84",
        "Q90"
      ) ~ "di_neurodesarrollo",

      # ---------------------------------------------------
      # salud mental severa
      # ---------------------------------------------------

      cie10_base %in% c(
        "F20",
        "F25",
        "F29",
        "F31",
        "F33"
      ) ~ "salud_mental_severa",

      # ---------------------------------------------------
      # neurodegenerativas
      # ---------------------------------------------------

      cie10_base %in% c(
        "G20",
        "G30",
        "G31",
        "F00",
        "F01",
        "F02",
        "F03"
      ) ~ "neurodegenerativas",

      # ---------------------------------------------------
      # neurológico motor severo
      # ---------------------------------------------------

      cie10_base %in% c(
        "G80",
        "G81",
        "G82",
        "G83",
        "G35",
        "I69",
        "B91"
      ) ~ "neurologico_motor",

      # ---------------------------------------------------
      # sensorial visual
      # ---------------------------------------------------

      cie10_base %in% c(
        "H53",
        "H54"
      ) ~ "sensorial_visual",

      # ---------------------------------------------------
      # sensorial auditiva
      # ---------------------------------------------------

      cie10_base %in% c(
        "H90"
      ) ~ "sensorial_auditiva",

      # ---------------------------------------------------
      # osteoarticular / movilidad
      # ---------------------------------------------------

      cie10_base %in% c(
        "M15",
        "M16",
        "M17",
        "M41",
        "M47",
        "M51",
        "M99"
      ) ~ "osteoarticular_movilidad",

      # ---------------------------------------------------
      # funcionalidad / síntomas
      # ---------------------------------------------------

      cie10_base %in% c(
        "R26",
        "R13",
        "R32",
        "R47"
      ) ~ "funcionalidad",

      # ---------------------------------------------------
      # dependencia / apoyos
      # ---------------------------------------------------

      cie10_base %in% c(
        "Z74",
        "Z99"
      ) ~ "dependencia_apoyos",

      TRUE ~ "otros"

    )

  )

# =========================================================
# EXPLORACIONES
# =========================================================

# ---------------------------------------------------------
# 7. frecuencia de códigos base
# ---------------------------------------------------------

tabla_cie <- base_cie |>

  count(cie10_base, sort = TRUE)

tabla_cie

# ---------------------------------------------------------
# 8. frecuencia grupos funcionales
# ---------------------------------------------------------

tabla_grupos <- base_cie |>

  count(grupo_funcional, sort = TRUE)

tabla_grupos

# ---------------------------------------------------------
# 9. frecuencia capítulos ICD
# ---------------------------------------------------------

tabla_capitulos <- base_cie |>

  count(capitulo, sort = TRUE)

tabla_capitulos

# ---------------------------------------------------------
# 10. cantidad de CIE por persona
# ---------------------------------------------------------
#
# Muy útil para complejidad.
#
# ---------------------------------------------------------

cie_por_persona <- base_cie |>

  distinct(
    id_persona,
    cie10_base
  ) |>

  count(
    id_persona,
    name = "n_cie"
  )

# ---------------------------------------------------------
# 11. cantidad de grupos funcionales por persona
# ---------------------------------------------------------

grupos_por_persona <- base_cie |>

  distinct(
    id_persona,
    grupo_funcional
  ) |>

  count(
    id_persona,
    name = "n_grupos_funcionales"
  )

# ---------------------------------------------------------
# 12. matriz persona x CIE
# ---------------------------------------------------------
#
# Esto sirve para:
#
# - co-ocurrencia
# - clustering
# - MCA
# - redes
#
# ---------------------------------------------------------

####################esto explota
#matriz_cie <- base_cie |>

#  distinct(
#    id_persona,
#    cie10_base
#  ) |>


#mutate(valor = 1) |>

#  pivot_wider(

#    names_from = cie10_base,
 #   values_from = valor,
  #  values_fill = 0

  #)

# ---------------------------------------------------------
# 13. personas con DI/neurodesarrollo
# ---------------------------------------------------------

personas_di <- base_cie |>

  filter(
    grupo_funcional == "di_neurodesarrollo"
  ) |>

  distinct(id_persona)

# ---------------------------------------------------------
# 14. base final enriquecida
# ---------------------------------------------------------

base_final <- base |>

  mutate(
    id_persona = row_number()
  ) |>

  left_join(
    cie_por_persona,
    by = "id_persona"
  ) |>

  left_join(
    grupos_por_persona,
    by = "id_persona"
  )

# =========================================================
# OBJETOS IMPORTANTES
# =========================================================
#
# base_cie
#   base larga CIE
#
# tabla_cie
#   frecuencias CIE
#
# tabla_grupos
#   grupos funcionales
#
# matriz_cie
#   matriz persona x CIE
#
# base_final
#   base original enriquecida
#
# =========================================================

#dentro de cada gurpo de discapacidad que cie10 predominan
tabla_prop <- base_cie |>

  count(
    tipo_de_deficiencia_simple_multiple,
    cie10_base
  ) |>

  group_by(
    tipo_de_deficiencia_simple_multiple
  ) |>

  mutate(
    prop = n / sum(n)
  ) |>

  arrange(
    tipo_de_deficiencia_simple_multiple,
    desc(prop)
  )


#tabla de contingencia
tabla_ca <- base_cie |>

  count(
    tipo_de_deficiencia_simple_multiple,
    cie10_base
  ) |>

  pivot_wider(

    names_from = cie10_base,
    values_from = n,
    values_fill = 0

  )

###analisis de correspondencias
library(factoextra)
library(FactoMineR)

rownames(tabla_ca) <-
  tabla_ca$tipo_de_deficiencia_simple_multiple

tabla_ca <-
  tabla_ca |>
  select(-tipo_de_deficiencia_simple_multiple)

ca <- CA(
  tabla_ca,
  graph = FALSE
)

fviz_ca_biplot(ca)


