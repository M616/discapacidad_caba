{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)}

options(scipen = 999)

enlace <- 'https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2019/12/eah2018_bu_ampliada.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto", 
              mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, full.names = TRUE)
archivos_descomprimidos

base <- 
  read_delim(archivos_descomprimidos[4],
             delim = ';')


#altas necesidades de apoyo segun definicion laxo
base <- base %>%
  mutate(
    alta_necesidad_apoyo_laxo = if_else(
      d14n_2 == 1 |
      d14n_3 == 1 |
      d14n_4 == 1 |
      d14n_10 == 1,
      1, 0
    )
  )


###altas necesidades de apoyo conservador
base <- base |>
  mutate(
    n_avd_basicas =
      (d14n_2 == 1) +
      (d14n_3 == 1) +
      (d14n_4 == 1) +
      (d14n_10 == 1),

    alta_necesidad_apoyo_cons = n_avd_basicas >= 2
  )


### altas necesidades de apoyo tomando a todas las personas que 
### necesitan apoyo en todas las de d14_n

base <- 
  base |>
  mutate(
        alta_necesidad_apoyo_todo_d14 =
      if_all(starts_with("d14n_") & !ends_with("_11"), ~ .x == 1)
  )

### motivos por el cual no tienen cud (con altas necesidades de apoyo)

base$d4n_f <- factor(
  base$d4n,
  levels = c(1,2,3,4,5,6,7,8,9,10),
  labels = c(
    "No sabe que existe",
    "No sabe para qué sirve",
    "No sabe cómo obtenerlo / es complicado",
    "Le queda lejos el lugar",
    "No lo quiere",
    "No lo necesita",
    "No lo renovó",
    "Cree que lo puede perjudicar",
    "Se lo denegaron",
    "Otro motivo"
  )
)


##porcentaje de la poblacion con mas de 3 necesidades
base$alta_necesidad_apoyo_3mas <- base$dd_tipo_dif == 9


disenio <- svydesign(ids = ~1,    # se usa ~1 si no hay conglomerados
                    weights = ~fexp,    # columna de factores de expansión
                    data = base)

###porcentaje de personas con alta necesidad de apoyo
svymean(~alta_necesidad_apoyo_laxo,  
  subset(disenio,
      edad >= 6),
      na.rm = TRUE)

#poblacion 6 y más con discapacidad 
disenio_pcd_6mas <- subset(
  disenio,
  dd_con_dif == 1 & edad >= 6
)

##si hago esto me promedia las categorias de dd_con_dif. esta como numerica, saca el promedio
svymean(~dd_con_dif,  
  subset(disenio,
      edad >= 6),
      na.rm = TRUE)

svymean(~I(dd_con_dif == 1),
        subset(disenio, edad >= 6),
        na.rm = TRUE)



#dentro de la poblacion con discapacidad mayor a 6 años, porcentaje de altas necesidades de apoyo
svymean(~alta_necesidad_apoyo_laxo, disenio_pcd_6mas, na.rm = TRUE)



###porcentaje de personas con alta necesidad de apoyo
svymean(~alta_necesidad_apoyo_cons,  
  subset(disenio,
      edad >= 6),
      na.rm = TRUE)

###porcentaje de personas con alta necesidad de apoyo, todas las necesidades
svymean(~alta_necesidad_apoyo_todo_d14,   
  subset(disenio,
      edad >= 6),
      na.rm = TRUE)



#dentro de la poblacion con discapacidad mayor a 6 años, porcentaje de altas necesidades de apoyo
svymean(~alta_necesidad_apoyo_cons, disenio_pcd_6mas, na.rm = TRUE)

#agrego criterio severidad estructural de la discapacidad
##porcentaje de la poblacion con mas de 3 necesidades
svymean(
  ~alta_necesidad_apoyo_3mas,
  subset(disenio, edad >= 6 ),
  na.rm = TRUE
)

svymean(
  ~I(dd_tipo_dif == 9),
  subset(disenio, edad >= 6 ),
  na.rm = TRUE
)

##porcentaje de la poblacion con discapacidad con más de 3 discapacidades
svymean(
    ~I(dd_tipo_dif == 9 ),
  subset(disenio, edad >= 6 & dd_con_dif == 1  ),
  na.rm = TRUE
)



###pruebo a abrir por comuna, tiene baja precisión
res_comuna <- svyby(
  ~alta_necesidad_apoyo_cons,
  ~comuna,
  subset(disenio, edad >= 6),
  svymean,
  na.rm = TRUE,
  vartype = "se"
)

res_comuna

res_comuna$cv_true <- with(
  res_comuna,
  se.alta_necesidad_apoyo_consTRUE /
    alta_necesidad_apoyo_consTRUE * 100
)

###para ver la poblacion con alta necesidad de apoyo que tiene certificado (todo d14_n)
disenio_ana_cons <- subset(
  disenio,
  edad >= 6 & alta_necesidad_apoyo_todo_d14 == TRUE
)

svymean(
  ~factor(dd15),
  disenio_ana_cons,
  na.rm = TRUE
)
#Al desagregar el escenario conservador de altas necesidades de apoyo según tenencia del 
# Certificado Único de Discapacidad, se observa que el 44,6% cuenta con CUD vigente (CV=9,0%),
#  mientras que un 49,6% no posee CUD (CV=8,1%). Los grupos con certificado vencido
#  (1,8%; CV=58,7%) o en trámite (4,0%; CV=38,3%) presentan coeficientes de variación 
# elevados, asociados a su baja frecuencia relativa, por lo que sus estimaciones deben 
# interpretarse con cautela. Estos resultados evidencian una brecha relevante entre 
# la necesidad de apoyos intensivos y el acceso efectivo al reconocimiento 
# administrativo de la discapacidad.




disenio_ana_cons_sin_cud <- subset(
  disenio,
  edad >= 6 &
  alta_necesidad_apoyo_cons == TRUE &
  dd15 != 1
)


res_d4n <- svymean(
  ~d4n_f,
  disenio_ana_cons_sin_cud,
  na.rm = TRUE
)
res_d4n 

tabla_d4n <- data.frame(
  motivo = names(coef(res_d4n)),
  prop   = coef(res_d4n),
  se     = SE(res_d4n)
)

tabla_d4n$cv <- tabla_d4n$se / tabla_d4n$prop * 100

tabla_d4n


