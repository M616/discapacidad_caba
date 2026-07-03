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

###genero la variable de certificado binaria
base <- base %>%
  mutate(
    certificado_bin = case_when(
      certificado == 1 ~ 1,
      certificado == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

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

##personas de 0 a 5 años con alguna dificultad(para primera infancia, paper de Leila)
svymean(~dificultad_total,
        subset(disenio, edad_agrupada == 1))

##personas de 0 a 5 años con alguna dificultad con certificado(para primera infancia, paper de Leila)
svymean(
  ~certificado_bin,
  subset(
    disenio,
    edad_agrupada == 1 &
    dificultad_total == 1 &
    !is.na(certificado_bin)
  ),
  vartype = "ci"
)


#“Entre las niñas y niños de 0 a 5 años que presentan al menos una dificultad funcional, el 69,0 % 
# cuenta con certificado de discapacidad (IC 95 %: 52,3–85,7).”



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












# ============================================================
# ENPPD 2018 / Estudio Nacional sobre el Perfil de las Personas
# con Discapacidad
#
# Tablas nacionales para informe de beca
# Proyecto: Discapacidad intelectual + altas necesidades de apoyo
# Autor: Marcos May
# ============================================================

# ------------------------------------------------------------
# 0. Paquetes
# ------------------------------------------------------------

# install.packages(c("data.table", "dplyr", "survey", "stringr", "tidyr"))

library(data.table)
library(survey)
library(stringr)


options(survey.lonely.psu = "adjust")
options(scipen = 999)

# ------------------------------------------------------------
# 1. Rutas de archivos
# ------------------------------------------------------------

# Ajustar los nombres según cómo tengas guardados los archivos.
# La base usuario del INDEC suele venir como CSV separado por ;
# La base de réplicas también viene como CSV separado por ;

# Carpeta de salida
out_dir <- "data/salidas_enppd_2018"
dir.create(out_dir, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Carga de datos
# ------------------------------------------------------------

base <- fread('data/enppd/base_estudio_discapacidad_2018.csv',
  sep = ";",
  encoding = "UTF-8",
  na.strings = c("", "NA", "NaN")
)

reps <- fread(
  'data/enppd/base_replicas_estudio_discapacidad_2018.csv',
  sep = ";",
  encoding = "UTF-8",
  na.strings = c("", "NA", "NaN")
)

# Pasar nombres a minúscula para evitar problemas
setnames(base, names(base), tolower(names(base)))
setnames(reps, names(reps), tolower(names(reps)))

# Chequeos mínimos
stopifnot("id" %in% names(base))
stopifnot("id" %in% names(reps))
stopifnot("pondera" %in% names(base))

# Unir base usuario con base de réplicas
df <- merge(base, reps, by = "id", all.x = TRUE)

# Identificar pesos replicados
rep_cols <- grep("^w_rep[0-9]+$", names(df), value = TRUE)

if (length(rep_cols) == 0) {
  stop("No se encontraron columnas de pesos replicados con patrón w_rep1, w_rep2, ..., w_rep300.")
}

message("Cantidad de pesos replicados encontrados: ", length(rep_cols))

# ------------------------------------------------------------
# 3. Recodificaciones generales
# ------------------------------------------------------------

df <- df %>%
  mutate(
    # --------------------------------------------------------
    # Identificación de población con dificultad
    # --------------------------------------------------------
    pcd_total = if_else(dificultad_total == 1, 1, 0, missing = 0),
    pcd_6ymas = if_else(dificultad_6ymas == 1, 1, 0, missing = 0),

    # --------------------------------------------------------
    # Sexo
    # --------------------------------------------------------
    sexo_lab = case_when(
      pa04 == 1 ~ "Varón",
      pa04 == 2 ~ "Mujer",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Grupos de edad
    # --------------------------------------------------------
    edad_lab = case_when(
      edad_agrupada == 1 ~ "0 a 5 años",
      edad_agrupada == 2 ~ "6 a 13 años",
      edad_agrupada == 3 ~ "14 a 39 años",
      edad_agrupada == 4 ~ "40 a 64 años",
      edad_agrupada == 5 ~ "65 años y más",
      TRUE ~ NA_character_
    ),

    edad_grandes_lab = case_when(
      edad_grandes_grupos == 1 ~ "0 a 14 años",
      edad_grandes_grupos == 2 ~ "15 a 64 años",
      edad_grandes_grupos == 3 ~ "65 años y más",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Certificado de discapacidad vigente
    # --------------------------------------------------------
    tiene_certificado = if_else(certificado == 1, 1, 0, missing = 0),

    certificado_lab = case_when(
      certificado == 1 ~ "Tiene certificado vigente",
      certificado == 2 ~ "No tiene certificado",
      certificado == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Cantidad de dificultades
    # --------------------------------------------------------
    cant_dific_lab = case_when(
      dificultades == 1 ~ "Una dificultad",
      dificultades == 2 ~ "Dos dificultades",
      dificultades == 3 ~ "Tres dificultades o más",
      dificultades == 4 ~ "Solo certificado",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Tipo de dificultad
    # --------------------------------------------------------
    tipo_dific_lab = case_when(
      tipo_dificultad == 1 ~ "Solo motora",
      tipo_dificultad == 2 ~ "Solo visual",
      tipo_dificultad == 3 ~ "Solo auditiva",
      tipo_dificultad == 4 ~ "Solo mental-cognitiva",
      tipo_dificultad == 5 ~ "Solo cuidado de sí",
      tipo_dificultad == 6 ~ "Solo habla y comunicación",
      tipo_dificultad == 7 ~ "Dos dificultades",
      tipo_dificultad == 8 ~ "Tres dificultades o más",
      tipo_dificultad == 9 ~ "Solo certificado",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Inicio de la dificultad
    # pc01:
    # 1 = Desde el nacimiento
    # 2 = Después del nacimiento
    # 9 = Ignorado
    # --------------------------------------------------------
    inicio_lab = case_when(
      pc01 == 1 ~ "Desde el nacimiento",
      pc01 == 2 ~ "Después del nacimiento",
      pc01 == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # Edad_inicio:
    # 0 = Todavía no había cumplido el año
    # 1 = 1 a 14 años
    # 2 = 15 a 39 años
    # 3 = 40 a 64 años
    # 4 = 65 años y más
    # 999 = Ignorado
    inicio_temprano = if_else(
      pc01 == 1 | edad_inicio %in% c(0, 1),
      1,
      0,
      missing = 0
    ),

    edad_inicio_lab = case_when(
      edad_inicio == 0 ~ "Menos de 1 año",
      edad_inicio == 1 ~ "1 a 14 años",
      edad_inicio == 2 ~ "15 a 39 años",
      edad_inicio == 3 ~ "40 a 64 años",
      edad_inicio == 4 ~ "65 años y más",
      edad_inicio == 999 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Causa de la dificultad
    # pc03:
    # 1 = Accidente
    # 2 = Problemas en parto / enfermedad de la madre durante embarazo
    # 3 = Problemas asociados a la vejez
    # 4 = Enfermedad / síndrome
    # 5 = Otra causa
    # 9 = Ignorado
    # --------------------------------------------------------
    causa_lab = case_when(
      pc03 == 1 ~ "Accidente",
      pc03 == 2 ~ "Parto / embarazo",
      pc03 == 3 ~ "Problemas asociados a la vejez",
      pc03 == 4 ~ "Enfermedad / síndrome",
      pc03 == 5 ~ "Otra causa",
      pc03 == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    causa_desarrollo = if_else(
      pc03 %in% c(2, 4),
      1,
      0,
      missing = 0
    ),

    # --------------------------------------------------------
    # Cobertura de salud
    # pc04:
    # 1 = Obra social o PAMI
    # 2 = Prepaga
    # 3 = Programa o plan estatal
    # 4 = No tiene obra social, prepaga o plan estatal
    # --------------------------------------------------------
    cobertura_lab = case_when(
      pc04 == 1 ~ "Obra social o PAMI",
      pc04 == 2 ~ "Prepaga",
      pc04 == 3 ~ "Programa o plan estatal",
      pc04 == 4 ~ "Solo sistema público",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Previsión social
    # --------------------------------------------------------
    recibe_jub_pension_lab = case_when(
      pc05 == 1 ~ "Recibe jubilación o pensión",
      pc05 == 2 ~ "No recibe jubilación o pensión",
      TRUE ~ NA_character_
    ),

    tipo_beneficio_lab = case_when(
      pc06 == 1 ~ "Solo jubilación",
      pc06 == 2 ~ "Solo pensión por fallecimiento",
      pc06 == 3 ~ "Jubilación y pensión",
      pc06 == 4 ~ "Solo pensión por invalidez/discapacidad",
      pc06 == 5 ~ "Solo pensión de otro tipo",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Educación
    # --------------------------------------------------------
    alfabetizacion_lab = case_when(
      pc07 == 1 ~ "Sabe leer y escribir",
      pc07 == 2 ~ "No sabe leer y escribir",
      TRUE ~ NA_character_
    ),

    asistencia_educativa_lab = case_when(
      pc08 == 1 ~ "Cursa actualmente",
      pc08 == 2 ~ "No cursa actualmente, pero cursó",
      pc08 == 3 ~ "Nunca cursó",
      TRUE ~ NA_character_
    ),

    modalidad_educativa_lab = case_when(
      mod_edu == 1 ~ "Solo común",
      mod_edu == 2 ~ "Solo especial",
      mod_edu == 3 ~ "Común y especial",
      mod_edu == 9 ~ "Ignorado",
      TRUE ~ NA_character_
    ),

    max_nivel_educativo_lab = case_when(
      mnea == 1 ~ "Hasta primario completo",
      mnea == 2 ~ "Secundario incompleto",
      mnea == 3 ~ "Educación integral",
      mnea == 4 ~ "Secundario completo",
      mnea == 5 ~ "Superior / universitario / posgrado",
      mnea == 6 ~ "No especifica / ignorado",
      TRUE ~ NA_character_
    ),

    # --------------------------------------------------------
    # Condición de actividad
    # --------------------------------------------------------
    actividad_lab = case_when(
      estado_ocup == 1 ~ "Ocupado",
      estado_ocup == 2 ~ "Desocupado",
      estado_ocup == 3 ~ "Inactivo",
      TRUE ~ NA_character_
    )
  )

# ------------------------------------------------------------
# 4. Construcción de escenarios exploratorios
# ------------------------------------------------------------

df <- df %>%
  mutate(
    # --------------------------------------------------------
    # Escenario 1:
    # Severidad estructural: tres o más dificultades
    # --------------------------------------------------------
    esc_severidad_estructural = if_else(
      pcd_6ymas == 1 & dificultades == 3,
      1,
      0,
      missing = 0
    ),

    # --------------------------------------------------------
    # Escenario 2:
    # Dominios críticos:
    # mental-cognitiva, cuidado de sí, habla/comunicación,
    # o tres dificultades o más.
    # --------------------------------------------------------
    esc_dominios_criticos = if_else(
      pcd_6ymas == 1 & tipo_dificultad %in% c(4, 5, 6, 8),
      1,
      0,
      missing = 0
    ),

    # --------------------------------------------------------
    # Escenario 3:
    # Proxy DI estricto:
    # solo mental-cognitiva + inicio temprano.
    # --------------------------------------------------------
    esc_proxy_di_estricto = if_else(
      pcd_6ymas == 1 &
        tipo_dificultad == 4 &
        inicio_temprano == 1,
      1,
      0,
      missing = 0
    ),

    # --------------------------------------------------------
    # Escenario 4:
    # Proxy DI / desarrollo ampliado:
    # mental-cognitiva o dominios críticos con inicio temprano
    # o causa compatible con desarrollo.
    # --------------------------------------------------------
    esc_proxy_di_ampliado = if_else(
      pcd_6ymas == 1 &
        (
          tipo_dificultad == 4 |
            (tipo_dificultad %in% c(4, 5, 6, 8) & inicio_temprano == 1) |
            (tipo_dificultad %in% c(4, 5, 6, 8) & causa_desarrollo == 1)
        ),
      1,
      0,
      missing = 0
    ),

    # --------------------------------------------------------
    # Escenario 5:
    # Proxy DI + potenciales altas necesidades de apoyo.
    # Requiere proxy DI ampliado y algún indicador de mayor
    # complejidad funcional o administrativa:
    # - tres o más dificultades
    # - certificado vigente
    # - cuidado de sí
    # - habla y comunicación
    # - tres dificultades o más en tipo_dificultad
    # --------------------------------------------------------
    esc_proxy_di_anda = if_else(
      esc_proxy_di_ampliado == 1 &
        (
          dificultades == 3 |
            tiene_certificado == 1 |
            tipo_dificultad %in% c(5, 6, 8)
        ),
      1,
      0,
      missing = 0
    )
  )

# ------------------------------------------------------------
# 5. Diseño muestral con réplicas bootstrap
# ------------------------------------------------------------

# Convertir a data.frame simple para survey
df_svy <- as.data.frame(df)

design_enppd <- svrepdesign(
  weights = ~pondera,
  repweights = df_svy[, rep_cols],
  data = df_svy,
  type = "bootstrap",
  combined.weights = TRUE,
  mse = TRUE
)

# Subuniverso: población de 6 años y más con dificultad
design_pcd_6ymas <- subset(design_enppd, pcd_6ymas == 1)

# ------------------------------------------------------------
# 6. Funciones auxiliares
# ------------------------------------------------------------

cv_svy <- function(x) {
  out <- SE(x) / coef(x) * 100
  as.numeric(out)
}

limpiar_nombre_categoria <- function(x, var) {
  x <- gsub(paste0("factor\\(", var, "\\)"), "", x)
  x <- gsub(var, "", x)
  x <- gsub("^", "", x, fixed = TRUE)
  x
}

# ------------------------------------------------------------
# Función para estimar una variable binaria 0/1 sobre total población
# ------------------------------------------------------------

estimar_binaria <- function(var, design = design_enppd, etiqueta = NULL) {

  f <- as.formula(paste0("~", var))

  prop <- svymean(f, design, na.rm = TRUE)
  tot  <- svytotal(f, design, na.rm = TRUE)

  res <- tibble(
    variable = var,
    indicador = ifelse(is.null(etiqueta), var, etiqueta),
    proporcion = as.numeric(coef(prop)[1]),
    proporcion_pct = proporcion * 100,
    error_estandar_pct = as.numeric(SE(prop)[1]) * 100,
    cv_pct = as.numeric(cv_svy(prop)[1]),
    total_expandido = as.numeric(coef(tot)[1]),
    total_error_estandar = as.numeric(SE(tot)[1]),
    total_cv_pct = as.numeric(cv_svy(tot)[1])
  )

  res
}

# ------------------------------------------------------------
# Función para tabla categórica con porcentaje, error estándar y CV
# ------------------------------------------------------------

tabla_categorica <- function(var, design, etiqueta_var = NULL) {

  f <- as.formula(paste0("~factor(", var, ")"))

  prop <- svymean(f, design, na.rm = TRUE)
  tot  <- svytotal(f, design, na.rm = TRUE)

  categorias <- names(coef(prop))
  categorias_limpias <- categorias %>%
    str_replace_all(paste0("factor\\(", var, "\\)"), "") %>%
    str_trim()

  tibble(
    variable = ifelse(is.null(etiqueta_var), var, etiqueta_var),
    categoria = categorias_limpias,
    proporcion = as.numeric(coef(prop)),
    porcentaje = proporcion * 100,
    error_estandar_pct = as.numeric(SE(prop)) * 100,
    cv_pct = as.numeric(SE(prop) / coef(prop) * 100),
    total_expandido = as.numeric(coef(tot)),
    total_error_estandar = as.numeric(SE(tot)),
    total_cv_pct = as.numeric(SE(tot) / coef(tot) * 100)
  ) %>%
    arrange(desc(total_expandido))
}

# ------------------------------------------------------------
# 7. Tabla 1: prevalencia nacional
# ------------------------------------------------------------

tabla_01_prevalencia <- bind_rows(
  estimar_binaria(
    var = "pcd_total",
    design = design_enppd,
    etiqueta = "Población con dificultad - total población"
  ),
  estimar_binaria(
    var = "pcd_6ymas",
    design = design_enppd,
    etiqueta = "Población de 6 años y más con dificultad"
  )
) %>%
  select(
    indicador,
    proporcion_pct,
    error_estandar_pct,
    cv_pct,
    total_expandido,
    total_error_estandar,
    total_cv_pct
  )

print(tabla_01_prevalencia)

# ------------------------------------------------------------
# 8. Tabla 2: distribución por tipo de dificultad
# Universo: población de 6 años y más con dificultad
# ------------------------------------------------------------

tabla_02_tipo_dificultad <- tabla_categorica(
  var = "tipo_dific_lab",
  design = design_pcd_6ymas,
  etiqueta_var = "Tipo de dificultad"
)

print(tabla_02_tipo_dificultad)

# ------------------------------------------------------------
# 9. Tabla 3: cantidad de dificultades
# Universo: población de 6 años y más con dificultad
# ------------------------------------------------------------

tabla_03_cantidad_dificultades <- tabla_categorica(
  var = "cant_dific_lab",
  design = design_pcd_6ymas,
  etiqueta_var = "Cantidad de dificultades"
)

print(tabla_03_cantidad_dificultades)

# ------------------------------------------------------------
# 10. Tabla 4: tenencia de certificado
# Universo: población de 6 años y más con dificultad
# ------------------------------------------------------------

tabla_04_certificado <- tabla_categorica(
  var = "certificado_lab",
  design = design_pcd_6ymas,
  etiqueta_var = "Tenencia de certificado"
)

print(tabla_04_certificado)

# ------------------------------------------------------------
# 11. Tabla 5: escenarios sobre total población nacional
# ------------------------------------------------------------

tabla_05_escenarios_total <- bind_rows(
  estimar_binaria(
    "esc_severidad_estructural",
    design_enppd,
    "Severidad estructural: tres o más dificultades"
  ),
  estimar_binaria(
    "esc_dominios_criticos",
    design_enppd,
    "Dominios críticos: mental-cognitiva, cuidado de sí, habla/comunicación o 3+ dificultades"
  ),
  estimar_binaria(
    "esc_proxy_di_estricto",
    design_enppd,
    "Proxy DI estricto: mental-cognitiva + inicio temprano"
  ),
  estimar_binaria(
    "esc_proxy_di_ampliado",
    design_enppd,
    "Proxy DI / desarrollo ampliado"
  ),
  estimar_binaria(
    "esc_proxy_di_anda",
    design_enppd,
    "Proxy DI + potenciales altas necesidades de apoyo"
  )
) %>%
  select(
    indicador,
    proporcion_pct,
    error_estandar_pct,
    cv_pct,
    total_expandido,
    total_error_estandar,
    total_cv_pct
  )

print(tabla_05_escenarios_total)

# ------------------------------------------------------------
# 12. Tabla 6: escenarios sobre población con dificultad
# ------------------------------------------------------------

tabla_06_escenarios_sobre_pcd <- bind_rows(
  estimar_binaria(
    "esc_severidad_estructural",
    design_pcd_6ymas,
    "Severidad estructural: tres o más dificultades"
  ),
  estimar_binaria(
    "esc_dominios_criticos",
    design_pcd_6ymas,
    "Dominios críticos"
  ),
  estimar_binaria(
    "esc_proxy_di_estricto",
    design_pcd_6ymas,
    "Proxy DI estricto"
  ),
  estimar_binaria(
    "esc_proxy_di_ampliado",
    design_pcd_6ymas,
    "Proxy DI / desarrollo ampliado"
  ),
  estimar_binaria(
    "esc_proxy_di_anda",
    design_pcd_6ymas,
    "Proxy DI + potenciales altas necesidades de apoyo"
  )
) %>%
  rename(
    porcentaje_sobre_poblacion_con_dificultad = proporcion_pct
  ) %>%
  select(
    indicador,
    porcentaje_sobre_poblacion_con_dificultad,
    error_estandar_pct,
    cv_pct,
    total_expandido,
    total_error_estandar,
    total_cv_pct
  )

print(tabla_06_escenarios_sobre_pcd)

# ------------------------------------------------------------
# 13. Caracterización básica del escenario proxy DI + ANdA
# ------------------------------------------------------------

design_proxy_di_anda <- subset(design_enppd, esc_proxy_di_anda == 1)

# Sexo
tabla_07_proxy_sexo <- tabla_categorica(
  var = "sexo_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Sexo - Proxy DI + ANdA"
)

# Edad
tabla_08_proxy_edad <- tabla_categorica(
  var = "edad_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Edad - Proxy DI + ANdA"
)

# Certificado
tabla_09_proxy_certificado <- tabla_categorica(
  var = "certificado_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Certificado - Proxy DI + ANdA"
)

# Cobertura de salud
tabla_10_proxy_cobertura <- tabla_categorica(
  var = "cobertura_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Cobertura de salud - Proxy DI + ANdA"
)

# Condición de actividad
tabla_11_proxy_actividad <- tabla_categorica(
  var = "actividad_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Condición de actividad - Proxy DI + ANdA"
)

print(tabla_07_proxy_sexo)
print(tabla_08_proxy_edad)
print(tabla_09_proxy_certificado)
print(tabla_10_proxy_cobertura)
print(tabla_11_proxy_actividad)

# ------------------------------------------------------------
# 14. Tablas adicionales opcionales
# ------------------------------------------------------------

# Inicio de la dificultad dentro del proxy
tabla_12_proxy_inicio <- tabla_categorica(
  var = "inicio_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Inicio de la dificultad - Proxy DI + ANdA"
)

# Edad de inicio
tabla_13_proxy_edad_inicio <- tabla_categorica(
  var = "edad_inicio_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Edad de inicio - Proxy DI + ANdA"
)

# Causa de la dificultad
tabla_14_proxy_causa <- tabla_categorica(
  var = "causa_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Causa de la dificultad - Proxy DI + ANdA"
)

# Modalidad educativa
tabla_15_proxy_modalidad_educativa <- tabla_categorica(
  var = "modalidad_educativa_lab",
  design = design_proxy_di_anda,
  etiqueta_var = "Modalidad educativa - Proxy DI + ANdA"
)

print(tabla_12_proxy_inicio)
print(tabla_13_proxy_edad_inicio)
print(tabla_14_proxy_causa)
print(tabla_15_proxy_modalidad_educativa)

# ------------------------------------------------------------
# 15. Exportar todas las tablas
# ------------------------------------------------------------

fwrite(
  tabla_01_prevalencia,
  file.path(out_dir, "tabla_01_prevalencia_nacional.csv")
)

fwrite(
  tabla_02_tipo_dificultad,
  file.path(out_dir, "tabla_02_tipo_dificultad_pcd_6ymas.csv")
)

fwrite(
  tabla_03_cantidad_dificultades,
  file.path(out_dir, "tabla_03_cantidad_dificultades_pcd_6ymas.csv")
)

fwrite(
  tabla_04_certificado,
  file.path(out_dir, "tabla_04_certificado_pcd_6ymas.csv")
)

fwrite(
  tabla_05_escenarios_total,
  file.path(out_dir, "tabla_05_escenarios_total_poblacion.csv")
)

fwrite(
  tabla_06_escenarios_sobre_pcd,
  file.path(out_dir, "tabla_06_escenarios_sobre_pcd.csv")
)

fwrite(
  tabla_07_proxy_sexo,
  file.path(out_dir, "tabla_07_proxy_di_anda_sexo.csv")
)

fwrite(
  tabla_08_proxy_edad,
  file.path(out_dir, "tabla_08_proxy_di_anda_edad.csv")
)

fwrite(
  tabla_09_proxy_certificado,
  file.path(out_dir, "tabla_09_proxy_di_anda_certificado.csv")
)

fwrite(
  tabla_10_proxy_cobertura,
  file.path(out_dir, "tabla_10_proxy_di_anda_cobertura.csv")
)

fwrite(
  tabla_11_proxy_actividad,
  file.path(out_dir, "tabla_11_proxy_di_anda_actividad.csv")
)

fwrite(
  tabla_12_proxy_inicio,
  file.path(out_dir, "tabla_12_proxy_di_anda_inicio.csv")
)

fwrite(
  tabla_13_proxy_edad_inicio,
  file.path(out_dir, "tabla_13_proxy_di_anda_edad_inicio.csv")
)

fwrite(
  tabla_14_proxy_causa,
  file.path(out_dir, "tabla_14_proxy_di_anda_causa.csv")
)

fwrite(
  tabla_15_proxy_modalidad_educativa,
  file.path(out_dir, "tabla_15_proxy_di_anda_modalidad_educativa.csv")
)

# ------------------------------------------------------------
# 16. Exportar resumen metodológico de escenarios
# ------------------------------------------------------------

diccionario_escenarios <- tibble(
  escenario = c(
    "Severidad estructural",
    "Dominios críticos",
    "Proxy DI estricto",
    "Proxy DI / desarrollo ampliado",
    "Proxy DI + potenciales altas necesidades de apoyo"
  ),
  definicion_operativa = c(
    "Población de 6 años y más con dificultad y tres o más dificultades funcionales.",
    "Población de 6 años y más con dificultad mental-cognitiva, cuidado de sí, habla/comunicación o tres dificultades o más.",
    "Población de 6 años y más con solo dificultad mental-cognitiva e inicio temprano de la dificultad.",
    "Población de 6 años y más con dificultad mental-cognitiva o dominios críticos combinados con inicio temprano o causa compatible con condición del desarrollo.",
    "Subconjunto del proxy DI/desarrollo ampliado que presenta certificado, tres o más dificultades, dificultad de cuidado de sí, habla/comunicación o multidificultad."
  ),
  interpretacion = c(
    "Aproxima alta complejidad funcional, sin distinguir tipo diagnóstico.",
    "Aproxima dominios funcionales relevantes para autonomía, comunicación y conducta adaptativa.",
    "Aproxima un núcleo más específico compatible con discapacidad intelectual o del desarrollo, aunque no diagnóstico.",
    "Aproxima un universo más inclusivo de discapacidad intelectual o del desarrollo.",
    "Aproxima perfiles con mayor probabilidad de requerir apoyos intensos o generalizados."
  ),
  limitacion = c(
    "No identifica discapacidad intelectual.",
    "No mide intensidad, frecuencia ni tipo de apoyo.",
    "Puede subestimar casos con discapacidad intelectual combinada con otras dificultades.",
    "Puede incluir situaciones no equivalentes a discapacidad intelectual.",
    "Es un proxy exploratorio; requiere validación con registros administrativos como CUD-ANDIS."
  )
)

fwrite(
  diccionario_escenarios,
  file.path(out_dir, "diccionario_escenarios_enppd.csv")
)

print(diccionario_escenarios)

# ------------------------------------------------------------
# 17. Mensaje final
# ------------------------------------------------------------

message("Procesamiento finalizado.")
message("Tablas exportadas en la carpeta: ", out_dir)
