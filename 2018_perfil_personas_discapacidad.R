{
library(tidyverse)
library(survey)
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
base <- 
  merge(base,
        replicas)

disenio <- 
  svrepdesign(data=base,
              weights=~pondera,
              repweights="w_rep[1-9]+",
              type='bootstrap',
              mse=T)

#personas con alguna dificultad
dificultad_total <- 
  svymean(~dificultad_total,
          #~pondera,
          disenio)

round(dificultad_total[1]*100,1)

#personas con alguna dificultad de mas de 6 años
svymean(~dificultad_6ymas,
        subset(disenio, !is.na(dificultad_6ymas)))


### pregunta dificultades, aca voy a tomar los que tienen mas de 
##3 dificultades como proxy de discapacidad intelectual

#total de personas con 3 dificultades o mas
svytotal(~I(dificultades == 3),
         subset(disenio, !is.na(dificultades)))


#porcentaje de personas con 3 dificultades o mas
svymean(~I(dificultades == 3),
        subset(disenio, !is.na(dificultades)))



### dentro de los que tienen dificultade == 3, o sea, los que tienen 
#mas de 3 dificultades, calculo el procentaje de personas con 
# certificado de discapacidad

svymean(
  ~I(certificado == 1),
  subset(disenio, dificultades == 3 & !is.na(certificado))
)




#falta ponderar
base |> count(hogar_dificultad )
base |> count(dificultad_total)
base |> count(dificultad_6ymas )
base |> count(tipo_dificultad)
