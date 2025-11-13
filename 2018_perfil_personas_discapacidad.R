{
library(tidyverse)
library(srvyr)
}

dir.create('data')
dir.create('data/enppd')

enlace <- 'https://www.indec.gob.ar/ftp/cuadros/menusuperior/enpd/base_estudio_discapacidad_2018.zip'
#temp_dir <- tempdir()
temp_zip <- file.path('data/enppd', "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto", 
              mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = 'data/enppd')

#archivos_descomprimidos <- list.files('data/enppd', pattern = "\\.csv$", full.names = TRUE)
base <- read.csv('data/enppd/base_estudio_discapacidad_2018.csv',
                 sep =';')


#cargo base pesos replicados

enlace <- 'https://www.indec.gob.ar/ftp/cuadros/menusuperior/enpd/base_replicas_estudio_discapacidad_2018.zip'
#temp_dir <- tempdir()
temp_zip <- file.path('data/enppd', "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto", 
              mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = 'data/enppd')

#archivos_descomprimidos <- list.files('data/enppd', pattern = "\\.csv$", full.names = TRUE)
replicas <- read.csv('data/enppd/base_replicas_estudio_discapacidad_2018.csv',
                 sep =';')


###### preparo base, ver https://www.indec.gob.ar/ftp/cuadros/menusuperior/enpd/estudio_discapacidad_nota_tecnica.pdf
#falta ponderar
base |> count(hogar_dificultad )
base |> count(dificultad_total)
base |> count(dificultad_6ymas )
base |> count(tipo_dificultad)
