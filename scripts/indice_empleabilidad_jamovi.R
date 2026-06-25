library(tidyverse)
library(jmvReadWrite)
library(janitor)
library(scales)

jamovi <- read_omv("data/empleabilidad/Base Empleabilidad JAMOVI.omv")
attributes(jamovi$IndiceEmpleabilidad)



attributes(jamovi$Capital)

datos <- clean_names(jamovi)

datos <- 
  datos |>
  filter(
    (sexo == "Masculino" &
       edad_a_la_solicitud_del_certificado >= 18 &
       edad_a_la_solicitud_del_certificado < 66) |
    (sexo == "Femenino" &
       edad_a_la_solicitud_del_certificado >= 18 &
       edad_a_la_solicitud_del_certificado < 61)
  )

datos |>
  count(condicion_de_actividad) |>
  mutate(
    pct = scales::percent(n / sum(n), accuracy = 0.1)
  )

#Teniendo en cuenta el cortre etario, en la base original el 32 % de las personas no trabaja

datos |>
  filter(condicion_de_actividad == "Trabaja" & !is.na(indice_empleabilidad)) |>
  summarise(
    minimo = min(indice_empleabilidad),
    p25 = quantile(indice_empleabilidad, 0.25),
    mediana = median(indice_empleabilidad),
    p75 = quantile(indice_empleabilidad, 0.75),
    media = mean(indice_empleabilidad),
    maximo = max(indice_empleabilidad))


datos |>
  filter(!is.na(indice_empleabilidad)) |>
  group_by(condicion_de_actividad) |>
  summarise(media_indice = mean(indice_empleabilidad))


datos <- datos |>
  mutate(
    categoria_indice = case_when(
      indice_empleabilidad < 5 ~ "Bajo",
      indice_empleabilidad >= 5 & indice_empleabilidad <= 6 ~ "Medio",
      indice_empleabilidad > 6 ~ "Alto"
    )  )

datos |>
  filter(condicion_de_actividad %in% c("Trabaja", "No trabaja")) |>
  count(categoria_indice, condicion_de_actividad) |>
  group_by(categoria_indice) |>
  mutate(porcentaje = n / sum(n) * 100)


datos |>
  filter(condicion_de_actividad %in% c("Trabaja", "No trabaja")) |>
  mutate(trabaja_bin = if_else(condicion_de_actividad == "Trabaja", 1, 0)) |>
  group_by(categoria_indice) |>
  summarise(
    prob_trabajar = mean(trabaja_bin)
  )


datos |>
  filter(condicion_de_actividad %in% c("Trabaja", "No trabaja")) |>
  mutate(trabaja_bin = if_else(condicion_de_actividad == "Trabaja", 1, 0)) |>
  group_by(indice_empleabilidad) |>
  summarise(prob_trabajar = mean(trabaja_bin))

datos |>
  filter(condicion_de_actividad %in% c("Trabaja", "No trabaja")) |>
  mutate(trabaja_bin = if_else(condicion_de_actividad == "Trabaja", 1, 0)) |>
  group_by(indice_empleabilidad) |>
  summarise(prob_trabajar = mean(trabaja_bin)) |>
  ggplot(aes(x = indice_empleabilidad, y = prob_trabajar)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Índice de empleabilidad",
    y = "Probabilidad de trabajar"
  )


lm(indice_empleabilidad ~ autonomia + severidad_clinica + capital, data = datos)
lm(indice_empleabilidad ~ autonomia_inversa + severidad_clinica_inverso + capital, data = datos)
