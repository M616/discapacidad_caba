## -----------------------------------------------------------------------------------------
library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(kableExtra)

options(scipen = 999)

enlace <- 'https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2025/04/eah2024_bu_ampliada.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")

download.file(url = enlace, destfile = temp_zip, method = "auto", mode = "wb")
unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, full.names = TRUE)

base0 <- read_delim("/tmp/Rtmp45OxJz/eah2024_bu_ampliada_ind.txt", delim = ';')
hogares <- read_delim("/tmp/Rtmp45OxJz/eah2024_bu_ampliada_hog.txt", delim = ';')

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


## -----------------------------------------------------------------------------------------
base |> count(edad) |> kable()


## -----------------------------------------------------------------------------------------
base |> count(sexo) |> kable()


## -----------------------------------------------------------------------------------------
base %>%
  select(
    sexo,
    motora = dd_tipo_dif1,
    visual = dd_tipo_dif2,
    auditiva = dd_tipo_dif3,
    habla_comunicacion = dd_tipo_dif4,
    mental_cognitiva = dd_tipo_dif5,
    cuidado_si = dd_tipo_dif6
  ) %>%
  pivot_longer(
    cols = -sexo,
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  filter(valor == 1) %>%
  count(tipo, sexo) %>%
  group_by(tipo) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(tipo = reorder(tipo, total)) %>%
  ggplot(aes(x = n, y = tipo, fill = sexo)) +
  geom_col() +
  labs(
    title = "Tipos de dificultad por sexo",
    x = "Cantidad de niños/as",
    y = "",
    fill = "Sexo"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()



## -----------------------------------------------------------------------------------------
base %>%
  mutate(
    tipo_vivienda = case_when(
      v2_2 == 1 ~ "Casa",
      v2_2 == 2 ~ "Departamento",
      v2_2 == 9 ~ "Inquilinato / conventillo",
      v2_2 == 10 ~ "Hotel / pension",
      v2_2 == 5 ~ "No destinada a vivienda",
      v2_2 == 8 ~ "Otro",
      TRUE ~ NA_character_
    )
  ) %>%
  count(tipo_vivienda) %>%
  mutate(tipo_vivienda = reorder(tipo_vivienda, n)) %>%
  ggplot(aes(x = n, y = tipo_vivienda, fill = tipo_vivienda)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Distribución del tipo de vivienda",
    x = "Cantidad de casos",
    y = ""
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal()
base %>%
  mutate(
    tipo_vivienda = case_when(
      v2_2 == 1 ~ "Casa",
      v2_2 == 2 ~ "Departamento",
      v2_2 == 9 ~ "Inquilinato / conventillo",
      v2_2 == 10 ~ "Hotel / pension",
      v2_2 == 5 ~ "No destinada a vivienda",
      v2_2 == 8 ~ "Otro",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    tipo_vivienda,
    motora = dd_tipo_dif1,
    visual = dd_tipo_dif2,
    auditiva = dd_tipo_dif3,
    habla_comunicacion = dd_tipo_dif4,
    mental_cognitiva = dd_tipo_dif5,
    cuidado_si = dd_tipo_dif6
  ) %>%
  pivot_longer(
    cols = -tipo_vivienda,
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  filter(valor == 1) %>%
  count(tipo, tipo_vivienda) %>%
  group_by(tipo) %>%
  mutate(total = sum(n)) %>%
  ungroup() %>%
  mutate(tipo = reorder(tipo, total)) %>%
  ggplot(aes(x = n, y = tipo, fill = tipo_vivienda)) +
  geom_col() +
  labs(
    title = "Tipos de dificultad por tipo de vivienda",
    x = "Cantidad de niños/as",
    y = "",
    fill = "Tipo de vivienda"
  ) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()



## -----------------------------------------------------------------------------------------
base %>%
  mutate(
    dominio_rec = case_when(
      dominio == 3 ~ "Villa",
      dominio == 4 ~ "Resto de la ciudad",
      TRUE ~ NA_character_
    )
  ) %>%
  count(dominio_rec) %>%
  mutate(dominio_rec = reorder(dominio_rec, n)) %>%
  ggplot(aes(x = n, y = dominio_rec, fill = dominio_rec)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Distribución territorial",
    x = "Cantidad de casos",
    y = ""
  ) +
  scale_fill_brewer(palette = "Accent") +
  theme_minimal()


## -----------------------------------------------------------------------------------------
base %>%
  mutate(
    hacinamiento = case_when(
      hacinam_2 == 1 ~ "Sin hacinamiento",
      hacinam_2 == 2 ~ "Hacinamiento no critico",
      hacinam_2 == 3 ~ "Hacinamiento critico",
      hacinam_2 == 0 ~ "Sin habitaciones de uso exclusivo",
      TRUE ~ NA_character_
    )
  ) %>%
  count(hacinamiento) %>%
  mutate(hacinamiento = reorder(hacinamiento, n)) %>%
  ggplot(aes(x = n, y = hacinamiento, fill = hacinamiento)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Condiciones de hacinamiento",
    x = "Cantidad de casos",
    y = ""
  ) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal()


## -----------------------------------------------------------------------------------------
base %>%
  mutate(
    dominio_rec = case_when(
      dominio == 3 ~ "Villa",
      dominio == 4 ~ "Resto de la ciudad",
      TRUE ~ NA_character_
    ),
    hacinamiento = case_when(
      hacinam_2 == 1 ~ "Sin",
      hacinam_2 == 2 ~ "Moderado",
      hacinam_2 == 3 ~ "Critico",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(hacinamiento)) %>%
  count(dominio_rec, hacinamiento) %>%
  ggplot(aes(x = dominio_rec, y = n, fill = hacinamiento)) +
  geom_col(position = "dodge") +
  labs(
    title = "Hacinamiento según dominio",
    x = "",
    y = "Cantidad de casos",
    fill = "Hacinamiento"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()
base %>%
  mutate(
    tipo_vivienda = case_when(
      v2_2 == 1 ~ "Casa",
      v2_2 == 2 ~ "Departamento",
      TRUE ~ "Otros"
    ),
    hacinamiento = case_when(
      hacinam_2 == 1 ~ "Sin",
      hacinam_2 == 2 ~ "Moderado",
      hacinam_2 == 3 ~ "Critico",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(hacinamiento)) %>%
  count(tipo_vivienda, hacinamiento) %>%
  ggplot(aes(x = tipo_vivienda, y = n, fill = hacinamiento)) +
  geom_col(position = "dodge") +
  labs(
    title = "Tipo de vivienda y hacinamiento",
    x = "",
    y = "Cantidad de casos",
    fill = "Hacinamiento"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

