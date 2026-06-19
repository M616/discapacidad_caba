{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(flextable)
library(googledrive)
library(janitor)
library(readxl)}

options(scipen = 999)


###base andis mayo
dir.create('data')
#base de marzo2026
drive_download('https://drive.google.com/file/d/1ZZfm2AuTceTKN_TeMjI6bbHofDXjHO2V/view?usp=sharing', 
path = "data/DGINC-BASE_MAYO_Recibida_16-6-20260.csv", overwrite = TRUE)
library(data.table)
andis<-fread("data/DGINC-BASE_MAYO_Recibida_16-6-20260.csv", encoding = 'Latin-1')
andis <- as.data.frame(andis)
names(andis)
andis<-clean_names(andis)
andis <- andis |> filter(provincia_de_residencia == 'Ciudad Autónoma de Buenos Aires') 
names(andis)

cie_vars <- paste0("cie10_", 1:8)

tabla_trabajan <- andis |>
  filter(condicion_de_actividad == "Trabaja") |>
  select(all_of(cie_vars)) |>
  pivot_longer(
    cols = everything(),
    values_to = "cie10"
  ) |>
  filter(!is.na(cie10), cie10 != "") |>
  count(cie10, sort = TRUE)

print(tabla_trabajan)


library(dplyr)

equip_vars <- c(
  "equipamiento",
  paste0("equipamiento_", 1:4)
)

andis <- andis %>%
  mutate(
    n_equip = rowSums(!is.na(select(., all_of(equip_vars))) &
                        select(., all_of(equip_vars)) != ""),
    score_autonomia = scales::rescale(n_equip, to = c(0,1))
  )


summary(andis$score_autonomia)



andis <- andis %>%
  mutate(
    score_vivienda = case_when(
      tipo_de_vivienda == "Persona viviendo en calle" ~ 0,
      tipo_de_vivienda %in% c(
        "Hospital, Clínica, Sanatorio"
      ) ~ 0,
      tipo_de_vivienda %in% c(
        "Prisión, Comisaría e Instituto de Menores",
        "Residencia, Hogar y Pequeño Hogar (Ley N°24.901 y Resolución Ministerial 1328/06)"
      ) ~ 0.1,
      tipo_de_vivienda %in% c(
        "Hogar (para personas mayores, religiosos, para niños, niñas y adolescentes)"
      ) ~ 0.2,
      tipo_de_vivienda %in% c(
        "Pieza en inquilinato",
        "Pieza en hotel familiar o pensión"
      ) ~ 0.5,
      tipo_de_vivienda %in% c(
        "Casilla", "Rancho"
      ) ~ 0.6,
      tipo_de_vivienda %in% c(
        "Departamento", "Casa"
      ) ~ 1,
      TRUE ~ 0.7
    )
  )










tabla <- as.data.frame( tabla)




andis <- andis |>
  mutate(
    fecha_de_nacimiento = dmy(fecha_de_nacimiento),
    edad = time_length(
      interval(fecha_de_nacimiento, ymd("2026-04-01")),
      "years"
    )
  )

andis |>
  filter(edad >= 18 & edad <= 60) |>
  count(condicion_de_actividad) |>
  mutate(
    porcentaje = n / sum(n) * 100
  ) 

#no corresponde?
andis |> filter(condicion_de_actividad == 'No corresponde') |> 
  #summarise(edad_maxima = max(edad))
  summarise(max(edad_a_la_solicitud_del_certificado))
  


andis |>
  filter(condicion_de_actividad == "No corresponde") |>
  summarise(
    n = n(),
    min = min(edad, na.rm = TRUE),
    q1 = quantile(edad, 0.25, na.rm = TRUE),
    mediana = median(edad, na.rm = TRUE),
    media = mean(edad, na.rm = TRUE),
    q3 = quantile(edad, 0.75, na.rm = TRUE),
    max = max(edad, na.rm = TRUE),
    sd = sd(edad, na.rm = TRUE)
  )

andis |> filter(edad >=18 & edad <=60 & condicion_de_actividad == 'No corresponde') |>
  count(tipo_de_deficiencia_simple_multiple) |> arrange(desc(n))



