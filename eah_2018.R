{library(tidyverse)
library(survey)
library(srvyr)
}

enlace <- 'https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2019/12/eah2018_bu_ampliada.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto", 
              mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, full.names = TRUE)

base <- read_delim(archivos_descomprimidos[4],
                 delim = ';')

#ponderar con 'fexp'
#total poblacion
diseño <- svydesign(ids = ~1,    # si no tienes conglomerados, usa ~1
                    #strata = ~estrato,   # si tienes estratificación
                    weights = ~fexp,    # columna de factores de expansión
                    data = base)    # el dataframe con tus datos


prop.table(svytable(~dd_con_dif, diseño))*100

#poblacion mayor a 6
diseño6 <- svydesign(ids = ~1,    # si no tienes conglomerados, usa ~1
                    #strata = ~estrato,   # si tienes estratificación
                    weights = ~fexp,    # columna de factores de expansión
                    data = base[base$edad >=6,])    # el dataframe con tus datos



prop.table(svytable(~dd_con_dif, diseño6))*100
svyby(~dd_con_dif,by = ~comuna, design = diseño6,FUN = svymean)


# Calcular el porcentaje de 'fexp' dentro de cada comuna para cada 'dd_con_dif'
resultados <- base |> 
  filter(edad >= 6) |>  # Filtrar las edades mayores o iguales a 6
  group_by(comuna, dd_con_dif) |>  # Agrupar por 'comuna' y 'dd_con_dif'
  summarise(suma_fexp = sum(fexp), .groups = "drop") |>  # Calcular la suma de 'fexp' por grupo
  group_by(comuna) |>  # Agrupar nuevamente por 'comuna' para calcular el total dentro de la comuna
  mutate(total_fexp_comuna = sum(suma_fexp)) |>  # Calcular el total de 'fexp' por comuna
  mutate(porcentaje = (suma_fexp / total_fexp_comuna) * 100) |>  # Calcular el porcentaje
  select(comuna, dd_con_dif, porcentaje)  # Seleccionar las columnas de interés

# Mostrar los resultados
print(resultados)

#poblacion con alguna dificultad
base_survey <- base %>%
  as_survey(weights = fexp)

#poblacion total
base_survey %>%
  group_by(dd_con_dif) %>%
  summarise(prop = survey_prop())

#mayor a 6total
base_survey %>%
  filter(edad > 5) |> 
  group_by(dd_con_dif) %>%
  summarise(prop = survey_prop())


##proporcion de personas con al menos una dificultad que tiene certificado de discapacidad
#mayor a 6
base_survey %>%
  filter(edad > 5 & dd_con_dif == 1 ) |> 
  group_by(dd15) %>%
  summarise(prop = survey_prop())

#Sólo eel 40% de la población mayor a 6 años con alguna dificultad tiene el CUD 

pob_mayor6 <- sum(base[base$edad >= 6, 'fexp'])

base[base$dif]
base |> count(hogar_dificultad )
base |> count(dificultad_total)
base |> count(dificultad_6ymas )
base |> count(tipo_dificultad)
