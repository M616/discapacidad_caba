library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(kableExtra)
library(officer)
library(rvg)
library(googledrive)
options(scipen = 999)
library(openxlsx2)
library(mschart)

enlace <- 'https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2025/04/eah2024_bu_ampliada.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")

download.file(url = enlace, destfile = temp_zip, method = "auto", mode = "wb")
unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, full.names = TRUE)

base0 <- read_delim(archivos_descomprimidos[5], delim = ';')
hogares <- read_delim(archivos_descomprimidos[3], delim = ';')

vivienda <- hogares %>%
  select(id, v2_2, v4, hacinam_2) %>%
  distinct()

base0 <- base0 %>%
  left_join(vivienda, by = "id") %>%
  mutate(
    sexo = factor(sexo, levels = c(1, 2), labels = c("Varon", "Mujer"))
  )

base <- base0 %>%
  filter(edad < 6) %>%
  filter(entrea_md == 1)



base |> count(dd_con_dif)

base |> filter(dd_con_dif == 2) select()


base %>%
  filter(dd_con_dif == 2) %>%   # los que NO tienen dificultad
  select(
    dd1, dd2, dd3,
    dd4, dd5, dd6,
    dd7, dd8, dd9,
    dd10, dd11, dd12,
    dd13, dd14, dd15,
    dd_con_dif
  )




tabla_n_dif <- base %>%
  select(
    motora = dd_tipo_dif1,
    visual = dd_tipo_dif2,
    auditiva = dd_tipo_dif3,
    habla = dd_tipo_dif4,
    cognitiva = dd_tipo_dif5,
    cuidado = dd_tipo_dif6
  ) %>%
  mutate(
    n_dificultades = rowSums(across(everything()), na.rm = TRUE)
  ) %>%
  count(n_dificultades) %>%
  arrange(n_dificultades)

tabla_n_dif


base %>% filter(dd_cant_dif > 1) %>% nrow()

base %>%  filter(sexo == 'Varon') |> 
  mutate(
    grupo = case_when(
      dd_tipo_dif4 == 1 & dd_tipo_dif5 == 1 ~ "Ambas",
      dd_tipo_dif4 == 1 & dd_tipo_dif5 == 0 ~ "Solo habla",
      dd_tipo_dif5 == 1 & dd_tipo_dif4 == 0 ~ "Solo mental"
    )
  ) %>%
  filter(!is.na(grupo)) %>%   # 👈 esto elimina "Ninguna"
  count(grupo)




base$sexo
