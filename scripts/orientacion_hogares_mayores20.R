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

orientaciones <- c(
  "Hogar",
  "Pequeño hogar",
  "Residencia"
)

base <- 
  base |>
 mutate(
    fecha_de_nacimiento = dmy(fecha_de_nacimiento),

    edad =
      interval(
        fecha_de_nacimiento,
        today()-1
        
      ) / years(1)
  )


base_tabla <- 
base   |>

  # -----------------------------
  # filtrar mayores de 20
  # -----------------------------
  filter(
    edad > 20
  ) |>
  # id único por persona
  mutate(
    id_persona = row_number(),

    # indicador Z74.3 en cualquiera de los CIE
    z743 = if_else(

      rowSums(
        across(
          all_of(cie_vars),
          ~ .x == "Z74.3"
        ),
        na.rm = TRUE
      ) > 0,

      "Z74.3 Sí",
      "Z74.3 No"
    )
  ) |>

  # pasar orientaciones a largo
  pivot_longer(

    cols = starts_with("tipo_de_orientacion_prestacional_"),

    names_to = "tipo_orientacion",
    values_to = "orientacion"

  ) |>

  # quedarnos solo con las orientaciones deseadas
  filter(
    orientacion %in% orientaciones
  ) |>

  # evitar duplicados por persona
  distinct(
    id_persona,
    orientacion,
    z743
  ) |>

  # contar
  count(
    orientacion,
    z743
  ) |>

  # ancho
  pivot_wider(

    names_from = z743,
    values_from = n,
    values_fill = 0

  ) |>

  mutate(
    `Total n` = `Z74.3 No` + `Z74.3 Sí`
  )  


bind_rows(base_tabla,
  base_tabla |>

      summarise(

        orientacion = "Total n",

        `Z74.3 No` = sum(`Z74.3 No`),
        `Z74.3 Sí` = sum(`Z74.3 Sí`),
        `Total n`  = sum(`Total n`)

      ))
base |> filter(edad>20) |> nrow()
