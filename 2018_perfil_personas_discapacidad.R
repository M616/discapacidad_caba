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

#0.10178


### pregunta dificultades, aca voy a tomar los que tienen mas de 
##3 dificultades como proxy de discapacidad intelectual

#total de personas con 3 dificultades o mas
#svytotal(~I(dificultades == 3),
#         subset(disenio, !is.na(dificultades)))




#porcentaje de personas con 3 dificultades o mas sobre personas con dificultades, conservador
ana_conservador_prop <- 
  svymean(~I(dificultades == 3),
        subset(disenio, dificultad_6ymas == 1 ))

ana_conservador_prop


###escenario central
ana_ampliado_prop <- 
  svymean(~I(
  dificultades == 3 |
    (tipo_dificultad %in% c(4, 5, 6) )), subset(disenio, dificultad_6ymas == 1)
)

ana_ampliado_prop



#--------------------------------------------------
# Función para evaluar calidad de estimaciones INDEC
#--------------------------------------------------
eval_calidad_indec <- function(est_obj) {
  
  est <- coef(est_obj)[2]       # proporción TRUE
  se  <- SE(est_obj)[2]
  cv  <- se / est
  
  calidad <- case_when(
    cv > 0.333 ~ "No confiable",
    cv > 0.166 ~ "Poco confiable",
    TRUE       ~ "Confiable"
  )
  
  tibble(
    estimacion = est,
    ee = se,
    cv = cv,
    calidad = calidad
  )
}


eval_calidad_indec(ana_conservador_prop)
eval_calidad_indec(ana_ampliado_prop)

###de las personas del escenario conservador, que proporcion tiene cudcertificado(¿unico?)
cud <-  svymean(~I(certificado == 1),
        subset(disenio,dificultad_6ymas == 1 & dificultades == 3))

eval_calidad_indec(cud)



####ver variable pc03, hay una categoria que dice causa disca
#probablemente asociado a vejez

##ver que pasa en los hogares con mas de dos personas con discapacidad, 
#serian los prioritarios?