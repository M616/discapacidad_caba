{library(janitor)
library(tidyverse)
library(googledrive)
library(here)
library(googlesheets4)
library(openxlsx)
}

dir.create('data')
#drive_download('https://docs.google.com/spreadsheets/d/1sAfzu82spdX3bNjg7vS0wRQrnoIIuAOh/edit?usp=sharing&ouid=118184938313502858180&rtpof=true&sd=true', path = 'data/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx')

base <- read.xlsx(
  #'data/cud_anonimizada_octubre25.xlsx',
  'data/raw/dgippd/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx') 

base <- clean_names(base)  
base |> count(provincia_de_residencia)

base <- base |> filter(provincia_de_residencia == 'Ciudad Autónoma de Buenos Aires')

names(base)

base |>
summarise(
  registros = n(),
  personas_unicas = n_distinct(numero)) #179561 casos

#cantidad de registros por vivienda part o colect
base |> count(vivienda_particular_o_colectiva)

base_cud |>
  summarise(
    registros = n(),
    personas_unicas = n_distinct(id_persona),
    diferencia = registros - personas_unicas,
    registros_por_persona = registros / personas_unicas
  )
