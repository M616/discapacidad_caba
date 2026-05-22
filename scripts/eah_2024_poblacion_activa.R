{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(flextable)}

options(scipen = 999)

enlace <- 'https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2025/04/eah2024_bu_ampliada.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto", 
              mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, full.names = TRUE)
archivos_descomprimidos

diseno <- readxl::read_excel(archivos_descomprimidos[4])
calculo_cv <- readxl::read_excel(archivos_descomprimidos[2])

base <- 
  read_delim(archivos_descomprimidos[5],
             delim = ';')

base |> 
  filter(dd_con_dif == 1 & edad >= 18 & edad <= 60) |> 
  #filter(dd_con_dif == 1 & dd15 !=4) |> 
  group_by(estado) |> 
  summarise(cantidad = sum(fexp)) |> 
  mutate(porcentaje = (cantidad / sum(cantidad)*100))


base |> 
  filter(dd_con_dif == 1) |> 
  group_by(dd15) |> 
  summarise(cantidad = sum(fexp)) |> 
  mutate(porcentaje = (cantidad / sum(cantidad)*100))


table(base$dd_con_dif)