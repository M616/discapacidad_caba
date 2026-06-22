library(tidyverse)
library(jmvReadWrite)
library(janitor)
library(scales)

jamovi <- read_omv("data/empleabilidad/Base Empleabilidad JAMOVI.omv")
attributes(jamovi$IndiceEmpleabilidad)
library(dplyr)

tibble(
  variable = names(jamovi),
  clase = sapply(datos, \(x) class(x)[1]),
  atributos = sapply(datos, \(x) paste(names(attributes(x)), collapse = ", "))
)

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

ocup <- datos |> filter(condicion_de_actividad == 'Trabaja')
summary(ocup$indice_empleabilidad )
summary(datos$indice_empleabilidad )
boxplot(ocup$indice_empleabilidad )

ocup |> select(alfabetizacion,tipo_de_vivienda, indice_empleabilidad)



