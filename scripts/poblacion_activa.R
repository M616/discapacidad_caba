{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(flextable)
library(googledrive)
library(janitor)}

options(scipen = 999)


###pedido original: personas activas en la base andis, pero se reformulo a personas que estén en condiciones de trabajar
#entre 18 y 60 años



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


###base andis
dir.create('data')
#base de marzo2026
drive_download('https://drive.google.com/file/d/1RcwuOIMC94PnhgrCysF1wXjdvPd2DzAv/view?usp=drive_link', 
path = "data/DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv",
overwrite = FALSE)
andis <- read_delim('data/DGIND-DA-CUD_ABRIL_v1(CABA)_recibida 21-05.csv')
andis<-clean_names(andis)
andis <- andis |> filter(provincia_de_residencia == 'Ciudad Autónoma de Buenos Aires') 


andis <- andis |>
  mutate(
    fecha_de_nacimiento = dmy(fecha_de_nacimiento),
    edad = time_length(
      interval(fecha_de_nacimiento, ymd("2026-04-01")),
      "years"
    )
  )

andis |>
  filter(edad >= 18 & edad <= 60) |>
  count(condicion_de_actividad) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) 

#no corresponde?
andis |> filter(condicion_de_actividad == 'No corresponde') |> 
  summarise(edad_maxima = max(edad))


andis |>
  filter(condicion_de_actividad == "No corresponde") |>
  summarise(
    n = n(),
    min = min(edad, na.rm = TRUE),
    q1 = quantile(edad, 0.25, na.rm = TRUE),
    mediana = median(edad, na.rm = TRUE),
    media = mean(edad, na.rm = TRUE),
    q3 = quantile(edad, 0.75, na.rm = TRUE),
    max = max(edad, na.rm = TRUE),
    sd = sd(edad, na.rm = TRUE)
  )

andis |> filter(edad >=18 & edad <=60 & condicion_de_actividad == 'No corresponde') |>
  count(tipo_de_deficiencia_simple_multiple) |> arrange(desc(n))



