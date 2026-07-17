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




base_ana <- base_ana |>
  mutate(
    e2_f = factor(
      e2,
      levels = c(1,2,3),
      labels = c(
        "Asiste",
        "No asiste pero asistió",
        "Nunca asistió"
      )
    )
  )

tabla_edu_edad <- base_ana |>
  count(grupo_edad, e2_f) |>
  group_by(grupo_edad) |>
  mutate(
    porcentaje = round(n / sum(n) * 100, 1)
  )

tabla_edu_edad


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





# ============================================================
# EAH 2024 - Módulo discapacidad
# Altas necesidades de apoyo - CABA
# Marcos May
# ============================================================

library(tidyverse)
library(survey)
library(srvyr)
library(flextable)
library(readxl)

options(scipen = 999)
options(survey.lonely.psu = "adjust")

# ------------------------------------------------------------
# 1. Descarga y carga de base
# ------------------------------------------------------------

enlace <- "https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2025/04/eah2024_bu_ampliada.zip"

temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "eah2024_bu_ampliada.zip")

download.file(
  url = enlace,
  destfile = temp_zip,
  method = "auto",
  mode = "wb"
)

unzip(zipfile = temp_zip, exdir = temp_dir)

archivos <- list.files(temp_dir, full.names = TRUE)

print(archivos)

# Ajustar si el orden cambia
diseno_registros <- read_excel(archivos[4])
calculo_cv       <- read_excel(archivos[2])

base <- read_delim(
  archivos[5],
  delim = ";",
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 2. Chequeo de variables clave
# ------------------------------------------------------------

vars_clave <- c(
  "id", "nhogar", "miembro",
  "comuna", "dominio",
  "edad", "sexo",
  "estado", "nivel", "e2",
  "tipcob2_2",
  "dd_con_dif", "dd_tipo_dif", "dd15",
  "d4n",
  paste0("d14n_", 1:11),
  "fexp"
)

data.frame(
  variable = vars_clave,
  existe = vars_clave %in% names(base)
) |> print(row.names = FALSE)

# ------------------------------------------------------------
# 3. Recodificaciones
# ------------------------------------------------------------

base <- base |>
  mutate(
    # ---------------------------------------
    # Sexo
    # ---------------------------------------
    sexo_f = factor(
      sexo,
      levels = c(1, 2),
      labels = c("Varón", "Mujer")
    ),

    # ---------------------------------------
    # Grupo de edad
    # ---------------------------------------
    grupo_edad = case_when(
      edad < 6 ~ "0-5",
      edad >= 6  & edad <= 14 ~ "6-14",
      edad >= 15 & edad <= 29 ~ "15-29",
      edad >= 30 & edad <= 44 ~ "30-44",
      edad >= 45 & edad <= 64 ~ "45-64",
      edad >= 65 ~ "65+",
      TRUE ~ NA_character_
    ),

    grupo_edad = factor(
      grupo_edad,
      levels = c("0-5", "6-14", "15-29", "30-44", "45-64", "65+")
    ),

    # ---------------------------------------
    # Persona con dificultad / discapacidad
    # dd_con_dif:
    # 1 = Tiene al menos una dificultad/discapacidad
    # 2 = No tiene dificultad / Ns/Nc
    # ---------------------------------------
    pcd = if_else(dd_con_dif == 1, 1, 0, missing = 0),

    pcd_6mas = if_else(dd_con_dif == 1 & edad >= 6, 1, 0, missing = 0),

    # ---------------------------------------
    # Certificado de discapacidad
    # dd15:
    # 1 vigente, 2 vencido, 3 en trámite,
    # 4 no tiene, 9 Ns/Nc
    # ---------------------------------------
    dd15_f = factor(
      dd15,
      levels = c(1, 2, 3, 4, 9),
      labels = c(
        "Certificado vigente",
        "Certificado vencido",
        "En trámite",
        "No tiene certificado",
        "Ns/Nc"
      )
    ),

    tiene_certificado_vigente = if_else(dd15 == 1, 1, 0, missing = 0),

    # ---------------------------------------
    # Motivo de no tenencia de certificado
    # d4n
    # ---------------------------------------
    d4n_f = factor(
      d4n,
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
    ),

    # ---------------------------------------
    # Condición de actividad
    # estado:
    # 1 ocupado, 2 desocupado, 3 inactivo
    # ---------------------------------------
    estado_f = factor(
      estado,
      levels = c(1, 2, 3),
      labels = c("Ocupado", "Desocupado", "Inactivo")
    ),

    # ---------------------------------------
    # Tipo de cobertura de salud
    # ---------------------------------------
    cobertura_f = factor(
      tipcob2_2,
      levels = c(1, 2, 3, 4, 5, 9),
      labels = c(
        "Sólo sistema público",
        "Sólo obra social",
        "Sólo prepaga o mutual vía obra social",
        "Sólo prepaga por contratación voluntaria",
        "Otros / dos o más sistemas",
        "Ns/Nc"
      )
    ),

    # ---------------------------------------
    # Asistencia educativa
    # e2:
    # 1 asiste, 2 no asiste pero asistió, 3 nunca asistió
    # ---------------------------------------
    e2_f = factor(
      e2,
      levels = c(1, 2, 3, 9),
      labels = c(
        "Asiste",
        "No asiste pero asistió",
        "Nunca asistió",
        "Ns/Nc"
      )
    ),

    # ---------------------------------------
    # Máximo nivel educativo
    # nivel5
    # ---------------------------------------
    nivel_f = factor(
      nivel,
      levels = c(0,1,2,3,4,5,6,7,8,9),
      labels = c(
        "Otras escuelas especiales",
        "Inicial",
        "Primario incompleto",
        "Primario completo",
        "Secundario incompleto",
        "Secundario completo",
        "Superior incompleto",
        "Superior completo",
        "Sin instrucción",
        "Ns/Nc"
      )
    )
  )

# ------------------------------------------------------------
# 4. Construcción de escenarios de altas necesidades de apoyo
# ------------------------------------------------------------

# Variables d14n_1 a d14n_10: asistencia habitual en actividades de la vida diaria.
d14_principales <- paste0("d14n_", 1:10)

base <- base |>
  mutate(
    # AVD básicas seleccionadas:
    # comer/beber, higienizarse, vestirse, medicación/visitas médicas
    n_avd_basicas = rowSums(
      across(
        c(d14n_2, d14n_3, d14n_4, d14n_10),
        ~ .x == 1
      ),
      na.rm = TRUE
    ),

    # Escenario laxo:
    # necesidad de asistencia en al menos una AVD básica
    ana_laxo = if_else(n_avd_basicas >= 1, 1, 0, missing = 0),

    # Escenario conservador:
    # necesidad de asistencia en dos o más AVD básicas
    ana_conservador = if_else(n_avd_basicas >= 2, 1, 0, missing = 0),

    # Escenario alta intensidad:
    # requiere asistencia en todas las actividades d14n_1 a d14n_10
    ana_alta_intensidad = if_else(
      if_all(all_of(d14_principales), ~ .x == 1),
      1, 0, missing = 0
    ),

    # Severidad estructural:
    # dd_tipo_dif == 9 = tres o más dificultades
    ana_3mas_dificultades = if_else(dd_tipo_dif == 9, 1, 0, missing = 0)
  )

# ------------------------------------------------------------
# 5. Diseño muestral
# ------------------------------------------------------------

disenio <- svydesign(
  ids = ~1,
  weights = ~fexp,
  data = base
)

disenio_6mas <- subset(disenio, edad >= 6)

disenio_pcd_6mas <- subset(
  disenio,
  edad >= 6 & dd_con_dif == 1
)

# ------------------------------------------------------------
# 6. Funciones auxiliares
# ------------------------------------------------------------

cv_svy <- function(x) {
  as.numeric(SE(x) / coef(x) * 100)
}

estimar_binaria <- function(var, design, etiqueta = NULL) {

  formula_var <- as.formula(paste0("~", var))

  prop <- svymean(formula_var, design, na.rm = TRUE)
  total <- svytotal(formula_var, design, na.rm = TRUE)

  tibble(
    variable = var,
    escenario = if_else(is.null(etiqueta), var, etiqueta),
    proporcion = as.numeric(coef(prop)[1]),
    porcentaje = proporcion * 100,
    se_pct = as.numeric(SE(prop)[1]) * 100,
    cv_pct = as.numeric(cv_svy(prop)[1]),
    total_expandido = as.numeric(coef(total)[1]),
    total_se = as.numeric(SE(total)[1]),
    total_cv_pct = as.numeric(cv_svy(total)[1])
  )
}

tabla_categorica <- function(var, design, etiqueta = NULL) {

  f <- as.formula(paste0("~factor(", var, ")"))

  prop <- svymean(f, design, na.rm = TRUE)
  total <- svytotal(f, design, na.rm = TRUE)

  categorias <- names(coef(prop)) |>
    str_replace_all(paste0("factor\\(", var, "\\)"), "") |>
    str_trim()

  tibble(
    variable = if_else(is.null(etiqueta), var, etiqueta),
    categoria = categorias,
    proporcion = as.numeric(coef(prop)),
    porcentaje = proporcion * 100,
    se_pct = as.numeric(SE(prop)) * 100,
    cv_pct = as.numeric(SE(prop) / coef(prop) * 100),
    total_expandido = as.numeric(coef(total)),
    total_se = as.numeric(SE(total)),
    total_cv_pct = as.numeric(SE(total) / coef(total) * 100)
  ) |>
    arrange(desc(total_expandido))
}

# ------------------------------------------------------------
# 7. Tabla síntesis de escenarios sobre población total CABA
# ------------------------------------------------------------

tabla_escenarios_total <- bind_rows(
  estimar_binaria(
    "ana_laxo",
    disenio,
    "Laxo: asistencia en ≥1 AVD básica"
  ),
  estimar_binaria(
    "ana_conservador",
    disenio,
    "Conservador: asistencia en ≥2 AVD básicas"
  ),
  estimar_binaria(
    "ana_alta_intensidad",
    disenio,
    "Alta intensidad: asistencia en todas las actividades d14n_1 a d14n_10"
  ),
  estimar_binaria(
    "ana_3mas_dificultades",
    disenio_6mas,
    "Severidad estructural: tres o más dificultades"
  )
)

tabla_escenarios_total

# ------------------------------------------------------------
# 8. Tabla de escenarios sobre población con discapacidad 6+
# ------------------------------------------------------------

tabla_escenarios_pcd_6mas <- bind_rows(
  estimar_binaria(
    "ana_laxo",
    disenio_pcd_6mas,
    "Laxo: asistencia en ≥1 AVD básica"
  ),
  estimar_binaria(
    "ana_conservador",
    disenio_pcd_6mas,
    "Conservador: asistencia en ≥2 AVD básicas"
  ),
  estimar_binaria(
    "ana_alta_intensidad",
    disenio_pcd_6mas,
    "Alta intensidad: asistencia en todas las actividades d14n_1 a d14n_10"
  ),
  estimar_binaria(
    "ana_3mas_dificultades",
    disenio_pcd_6mas,
    "Severidad estructural: tres o más dificultades"
  )
)

tabla_escenarios_pcd_6mas

# ------------------------------------------------------------
# 9. Casos absolutos sin ponderar por escenario
# ------------------------------------------------------------

casos_escenarios <- base |>
  summarise(
    n_total_base = n(),
    n_pcd = sum(dd_con_dif == 1, na.rm = TRUE),
    n_laxo = sum(ana_laxo == 1, na.rm = TRUE),
    n_conservador = sum(ana_conservador == 1, na.rm = TRUE),
    n_alta_intensidad = sum(ana_alta_intensidad == 1, na.rm = TRUE),
    n_3mas_dificultades = sum(ana_3mas_dificultades == 1, na.rm = TRUE)
  )

casos_escenarios

# ------------------------------------------------------------
# 10. Tabla compacta para informe
# ------------------------------------------------------------

tabla_informe_escenarios <- tibble(
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
    sum(base$ana_laxo == 1, na.rm = TRUE),
    sum(base$ana_conservador == 1, na.rm = TRUE),
    sum(base$ana_alta_intensidad == 1, na.rm = TRUE)
  ),

  `Total expandido` = c(
    tabla_escenarios_total$total_expandido[tabla_escenarios_total$variable == "ana_laxo"],
    tabla_escenarios_total$total_expandido[tabla_escenarios_total$variable == "ana_conservador"],
    tabla_escenarios_total$total_expandido[tabla_escenarios_total$variable == "ana_alta_intensidad"]
  ),

  `% población` = c(
    tabla_escenarios_total$porcentaje[tabla_escenarios_total$variable == "ana_laxo"],
    tabla_escenarios_total$porcentaje[tabla_escenarios_total$variable == "ana_conservador"],
    tabla_escenarios_total$porcentaje[tabla_escenarios_total$variable == "ana_alta_intensidad"]
  ),

  `CV aproximado` = c(
    tabla_escenarios_total$cv_pct[tabla_escenarios_total$variable == "ana_laxo"],
    tabla_escenarios_total$cv_pct[tabla_escenarios_total$variable == "ana_conservador"],
    tabla_escenarios_total$cv_pct[tabla_escenarios_total$variable == "ana_alta_intensidad"]
  )
) |>
  mutate(
    `Total expandido` = round(`Total expandido`, 0),
    `% población` = round(`% población`, 2),
    `CV aproximado` = round(`CV aproximado`, 1)
  )

tabla_informe_escenarios

# ------------------------------------------------------------
# 11. Flextable para informe
# ------------------------------------------------------------

tabla_ft <- tabla_informe_escenarios |>
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
    caption = "Tabla. Escenarios operativos de altas necesidades de apoyo. EAH 2024"
  )

tabla_ft

# ------------------------------------------------------------
# 12. Caracterización básica del escenario de alta intensidad
# ------------------------------------------------------------

disenio_alta <- subset(
  disenio,
  ana_alta_intensidad == 1
)

base_alta <- base |>
  filter(ana_alta_intensidad == 1)

# Casos relevados
nrow(base_alta)

# Caracterización ponderada
tabla_alta_sexo <- tabla_categorica("sexo_f", disenio_alta, "Sexo")
tabla_alta_edad <- tabla_categorica("grupo_edad", disenio_alta, "Grupo de edad")
tabla_alta_certificado <- tabla_categorica("dd15_f", disenio_alta, "Certificado")
tabla_alta_cobertura <- tabla_categorica("cobertura_f", disenio_alta, "Cobertura de salud")
tabla_alta_actividad <- tabla_categorica("estado_f", disenio_alta, "Condición de actividad")
tabla_alta_educacion <- tabla_categorica("nivel_f", disenio_alta, "Máximo nivel educativo")

tabla_alta_sexo
tabla_alta_edad
tabla_alta_certificado
tabla_alta_cobertura
tabla_alta_actividad
tabla_alta_educacion

# Caracterización sin ponderar, útil por baja cantidad de casos
tabla_alta_no_ponderada <- list(
  sexo = base_alta |> count(sexo_f) |> mutate(pct = n / sum(n) * 100),
  edad = base_alta |> count(grupo_edad) |> mutate(pct = n / sum(n) * 100),
  certificado = base_alta |> count(dd15_f) |> mutate(pct = n / sum(n) * 100),
  cobertura = base_alta |> count(cobertura_f) |> mutate(pct = n / sum(n) * 100),
  actividad = base_alta |> count(estado_f) |> mutate(pct = n / sum(n) * 100),
  educacion = base_alta |> count(nivel_f) |> mutate(pct = n / sum(n) * 100)
)

tabla_alta_no_ponderada

# ------------------------------------------------------------
# 13. Motivos de no tenencia de certificado
# ------------------------------------------------------------

disenio_alta_sin_cert <- subset(
  disenio,
  ana_alta_intensidad == 1 &
    dd15 != 1 &
    !is.na(d4n)
)

tabla_motivos_sin_cert <- tabla_categorica(
  "d4n_f",
  disenio_alta_sin_cert,
  "Motivo de no tenencia de certificado vigente"
)

tabla_motivos_sin_cert

# ------------------------------------------------------------
# 14. Exportar resultados
# ------------------------------------------------------------

dir.create("data/salidas_eah2024", showWarnings = FALSE)

write_csv(tabla_escenarios_total, "data/salidas_eah2024/tabla_escenarios_total.csv")
write_csv(tabla_escenarios_pcd_6mas, "data/salidas_eah2024/tabla_escenarios_pcd_6mas.csv")
write_csv(tabla_informe_escenarios, "data/salidas_eah2024/tabla_informe_escenarios.csv")
write_csv(casos_escenarios, "data/salidas_eah2024/casos_escenarios.csv")

write_csv(tabla_alta_sexo, "data/salidas_eah2024/alta_intensidad_sexo.csv")
write_csv(tabla_alta_edad, "data/salidas_eah2024/alta_intensidad_edad.csv")
write_csv(tabla_alta_certificado, "data/salidas_eah2024/alta_intensidad_certificado.csv")
write_csv(tabla_alta_cobertura, "data/salidas_eah2024/alta_intensidad_cobertura.csv")
write_csv(tabla_alta_actividad, "data/salidas_eah2024/alta_intensidad_actividad.csv")
write_csv(tabla_alta_educacion, "data/salidas_eah2024/alta_intensidad_educacion.csv")
write_csv(tabla_motivos_sin_cert, "data/salidas_eah2024/motivos_sin_certificado.csv")
