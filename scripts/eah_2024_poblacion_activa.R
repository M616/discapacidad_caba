{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(flextable)
library(googledrive)
library(janitor)}

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


#tasa de actividad de las personas con alguna dificultad. 1= ocupado, 2=desocupado, 3= inactivo

##################funcion para calcular tasas
calcular_tasas <- function(datos) {
  datos %>%
    filter(
      edad >= 14,
      edad <= 65,
      !is.na(estado),
      !is.na(fexp)
    ) %>%
    summarise(
      poblacion_referencia = sum(fexp, na.rm = TRUE),
      ocupados = sum(fexp[estado == 1], na.rm = TRUE),
      desocupados = sum(fexp[estado == 2], na.rm = TRUE),
      pea = ocupados + desocupados,

      tasa_actividad = round(
        100 * pea / poblacion_referencia, 1
      ),

      tasa_empleo = round(
        100 * ocupados / poblacion_referencia, 1
      ),

      tasa_desempleo = round(
        100 * desocupados / pea, 1
      )
    )
}


tasas_total <- base %>%
  calcular_tasas() %>%
  mutate(poblacion = "Población total")

tasas_dificultad <- base %>%
  filter(dd_con_dif == 1) %>%
  calcular_tasas() %>%
  mutate(poblacion = "Población con alguna dificultad")


tasas_comparadas <- bind_rows(
  tasas_total,
  tasas_dificultad
) %>%
  select(
    poblacion,
    poblacion_referencia,
    ocupados,
    desocupados,
    pea,
    tasa_actividad,
    tasa_empleo,
    tasa_desempleo
  )

tasas_comparadas



###base andis
dir.create('data')
#base de marzo2026
drive_download('https://drive.google.com/file/d/1RcwuOIMC94PnhgrCysF1wXjdvPd2DzAv/view?usp=drive_link', 
path = "data/DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv",
overwrite = FALSE)
andis <- read_delim('data/DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv')
andis<-clean_names(andis)
andis <- andis |> filter(provincia_de_residencia == 'Ciudad Autónoma de Buenos Aires') 

andis |> count(condicion_de_actividad) 

#no corresponde?

