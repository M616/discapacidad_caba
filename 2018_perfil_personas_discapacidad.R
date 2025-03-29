{library(tidyverse)
}

enlace <- 'https://www.indec.gob.ar/ftp/cuadros/menusuperior/enpd/base_estudio_discapacidad_2018.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto", 
              mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
base <- read.csv(archivos_descomprimidos[1],
                 sep =';')

#falta ponderar
base |> count(hogar_dificultad )
base |> count(dificultad_total)
base |> count(dificultad_6ymas )
base |> count(tipo_dificultad)
