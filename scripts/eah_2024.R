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

base <- base |>
  mutate(
    grupo_edad = case_when(
      edad >= 6 & edad <= 14 ~ "6-14",
      edad >= 15 & edad <= 29 ~ "15-29",
      edad >= 30 & edad <= 44 ~ "30-44",
      edad >= 45 & edad <= 64 ~ "45-64",
      edad >= 65 ~ "65+"
    )
  )

disenio <- svydesign(ids = ~1,    # se usa ~1 si no hay conglomerados
                    weights = ~fexp,    # columna de factores de expansión
                    data = base)

###porcentaje de personas con alta necesidad de apoyo
svymean(~alta_necesidad_apoyo_laxo,  
  #subset(disenio,
  #    edad >= 6),
  design = disenio,
      na.rm = TRUE)

base |>
  filter(alta_necesidad_apoyo_laxo == 1) |>
  summarise(
    n_absoluto = n(),
    total_expandido = sum(fexp, na.rm = TRUE)
  )
###porcentaje de personas con alta necesidad de apoyo dentro de la poblacion con discapacidad
svymean(~alta_necesidad_apoyo_laxo,  
  subset(disenio,
      dd_con_dif == 1),
      na.rm = TRUE)

#poblacion 6 y más con discapacidad 
disenio_pcd_6mas <- subset(
  disenio,
  dd_con_dif == 1 & edad >= 6
)


svymean(~I(dd_con_dif == 1),
        #subset(disenio, edad >= 6),
        design = disenio,
        na.rm = TRUE)



#dentro de la poblacion con discapacidad mayor a 6 años, porcentaje de altas necesidades de apoyo
svymean(~alta_necesidad_apoyo_laxo, disenio_pcd_6mas, na.rm = TRUE)



###porcentaje de personas con alta necesidad de apoyo
svymean(~alta_necesidad_apoyo_cons,  
  #subset(disenio,      edad >= 6),
  design = disenio,
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



#-------------------------------------------------
# TABLA SÍNTESIS ESCENARIOS DE APOYO
#-------------------------------------------------

# Escenario laxo
laxo_n <- base |>
  filter(alta_necesidad_apoyo_laxo == 1) |>
  nrow()

laxo_exp <- base |>
  filter(alta_necesidad_apoyo_laxo == 1) |>
  summarise(total = sum(fexp, na.rm = TRUE)) |>
  pull(total)

laxo_prop <- svymean(
  ~alta_necesidad_apoyo_laxo,
  disenio,
  na.rm = TRUE
)[1]

# Escenario conservador
cons_n <- base |>
  filter(alta_necesidad_apoyo_cons == TRUE) |>
  nrow()

cons_exp <- base |>
  filter(alta_necesidad_apoyo_cons == TRUE) |>
  summarise(total = sum(fexp, na.rm = TRUE)) |>
  pull(total)

cons_prop <- svymean(
  ~alta_necesidad_apoyo_cons,
  disenio,
  na.rm = TRUE
)[2]

# Escenario alta intensidad
alta_n <- base |>
  filter(alta_necesidad_apoyo_todo_d14 == TRUE) |>
  nrow()

alta_exp <- base |>
  filter(alta_necesidad_apoyo_todo_d14 == TRUE) |>
  summarise(total = sum(fexp, na.rm = TRUE)) |>
  pull(total)

alta_prop <- svymean(
  ~alta_necesidad_apoyo_todo_d14,
  disenio,
  na.rm = TRUE
)[2]

#-------------------------------------------------
# ARMADO TABLA
#-------------------------------------------------

tabla_escenarios <- tibble(
  `Escenario operativo` = c(
    "Laxo",
    "Conservador",
    "Alta intensidad"
  ),

  Definicion = c(
    "Necesidad de apoyo en ≥1 actividad básica",
    "Necesidad de apoyo en ≥2 actividades básicas",
    "Necesidad de apoyo en la totalidad de las actividades relevadas"
  ),

  `Casos relevados (n)` = c(
    laxo_n,
    cons_n,
    alta_n
  ),

  `Total expandido` = c(
    round(laxo_exp),
    round(cons_exp),
    round(alta_exp)
  ),

  `% población` = c(
    round(laxo_prop * 100, 1),
    round(cons_prop * 100, 1),
    round(alta_prop * 100, 2)
  ),

  `CV aproximado` = c(
    "~5,6%",
    "~7%",
    "~15%"
  )
)

#-------------------------------------------------
# FLEXTABLE
#-------------------------------------------------

tabla_ft <- tabla_escenarios |>
  flextable() |>
  autofit() |>
  theme_booktabs() |>
  bg(part = "header", bg = "#153244") |>
  color(part = "header", color = "white") |>
  align(align = "center", part = "all") |>
  valign(valign = "center", part = "all") |>
  fontsize(size = 10, part = "all") |>
  bold(part = "header") |>
  set_caption(
    caption = "Tabla 1. Escenarios operativos de altas necesidades de apoyo. EAH 2024"
  )

tabla_ft





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
#Al desagregar el escenario ccon todas las dificultades de altas necesidades de apoyo según tenencia del 
# Certificado Único de Discapacidad, se observa que el 44,6% cuenta con CUD vigente (CV=9,0%),
#  mientras que un 49,6% no posee CUD (CV=8,1%). Los grupos con certificado vencido
#  (1,8%; CV=58,7%) o en trámite (4,0%; CV=38,3%) presentan coeficientes de variación 
# elevados, asociados a su baja frecuencia relativa, por lo que sus estimaciones deben 
# interpretarse con cautela. Estos resultados evidencian una brecha relevante entre 
# la necesidad de apoyos intensivos y el acceso efectivo al reconocimiento 
# administrativo de la discapacidad.


##engo entonces esta encuesta del 2024.. ya vi lo del cud.. pero capaz puedo hacer algunos cruces de variables
#  sociales basicas? entonces despues mostraria esto del cud, y creo que me puede dar pie para luego ir por el 
# apartado de la base cud de andis, para tomar mas esos datos que puedo incluso desagregarlos por comuna

#edad
#sexo
#coertura de salud
#nivel educativo
#condicion actividad

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


svymean(
  ~factor(grupo_edad),
  subset(
    disenio,
    edad >= 6 &
    alta_necesidad_apoyo_todo_d14 == TRUE
  ),
  na.rm = TRUE
)




## =========================================================
## CASOS ABSOLUTOS (SIN PONDERAR)
## PERSONAS CON ALTAS NECESIDADES DE APOYO
## =========================================================

## criterio principal
base_ana <- base |>
  filter(
    #edad >= 6,
    alta_necesidad_apoyo_todo_d14 == TRUE
  )



## =========================================================
## TOTAL DE CASOS RELEVADOS
## =========================================================

nrow(base_ana)



## =========================================================
## SEXO
## =========================================================

table(base_ana$sexo)

prop.table(table(base_ana$sexo)) * 100



## =========================================================
## GRUPOS DE EDAD
## =========================================================

base_ana <- base_ana |>
  mutate(
    grupo_edad = case_when(
      edad < 6 ~ "0-5",
      edad >= 6  & edad <= 14 ~ "6-14",
      edad >= 15 & edad <= 29 ~ "15-29",
      edad >= 30 & edad <= 44 ~ "30-44",
      edad >= 45 & edad <= 64 ~ "45-64",
      edad >= 65              ~ "65+"
    )
  )

table(base_ana$grupo_edad)

prop.table(table(base_ana$grupo_edad)) * 100



## =========================================================
## NIVEL EDUCATIVO
## (ajustar nombre variable segun EAH)
## =========================================================

#asiste a establecimiento educativo
table(base_ana$e2)

prop.table(table(base_ana$nivel)) * 100



## =========================================================
## CONDICION DE ACTIVIDAD
## (ajustar variable)
## =========================================================

table(base_ana$estado)

prop.table(table(base_ana$estado)) * 100



## =========================================================
## COBERTURA DE SALUD
## (ajustar variable)
## =========================================================

#tipo cobertura de salud
table(base_ana$tipcob2_2 )

prop.table(table(base_ana$tipcob2_2)) * 100



## =========================================================
## TENENCIA DE CUD
## =========================================================

table(base_ana$dd15)

prop.table(table(base_ana$dd15)) * 100



## =========================================================
## MOTIVOS DE NO TENENCIA DE CUD
## =========================================================

base_ana_sin_cud <- base_ana |>
  filter(dd15 != 1)

table(base_ana_sin_cud$d4n_f)

prop.table(table(base_ana_sin_cud$d4n_f)) * 100


