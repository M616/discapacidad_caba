{library(tidyverse)
library(survey)
library(srvyr)
  library(ggthemes)}

enlace <- 'https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2025/07/eah2024_bu_ampliada.zip'
temp_dir <- tempdir()
temp_zip <- file.path(temp_dir, "archivo_descargado.zip")
download.file(url = enlace, 
              destfile = temp_zip, 
              method = "auto") 
              #mode = "wb") # 'wb' para Windows, importante para archivos binarios

unzip(zipfile = temp_zip, exdir = temp_dir)

archivos_descomprimidos <- list.files(temp_dir, full.names = TRUE)
archivos_descomprimidos

base <- 
  read_delim(archivos_descomprimidos[5],
             delim = ';')

#ponderar con 'fexp'
#total poblacion
diseño <- svydesign(ids = ~1,    # si no tienes conglomerados, usa ~1
                    #strata = ~estrato,   # si tienes estratificación
                    weights = ~fexp,    # columna de factores de expansión
                    data = base)    # el dataframe con tus datos



#la encuesta 2024 no tiene el modulo por discapacidad publicado
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
base_survey <- 
  base %>%
  as_survey(weights = fexp)

#poblacion total
base_survey <- 
  base_survey %>%
  mutate( alguna_dificultad = case_when(dd_con_dif == 1 ~ 'si',
                                        TRUE ~ 'no')) 

base_survey |> 
  group_by(alguna_dificultad) %>%
  summarise(prop = survey_prop(level = 0.95, 
                               vartype = 'cv')
            *100)

#mayor a 6total

base_survey <- 
  base_survey %>% 
  mutate( alguna_dificultad = 
            case_when(dd_con_dif == 1 ~ 'si',
                      TRUE ~ 'no')) 
  
base_survey |> 
  filter(edad > 5 ) |> 
  group_by(alguna_dificultad) %>%
  summarise(prop = survey_prop()*100)


##proporcion de personas con al menos una dificultad que tiene certificado de discapacidad
#mayor a 6

base |> 
  count(dd15)

base_survey <- 
  base_survey %>%
  mutate(certificado = 
           case_when (dd15 == 1 ~ 'si',
                      dd15 == 9 ~ NA_character_,
                      TRUE ~ 'no') )
  
base_survey |> 
  filter(edad > 5 & alguna_dificultad == 'si' ) |> 
  group_by(certificado) %>%
  summarise(prop = survey_prop()*100)

#el cud no es el unico certificado
#Sólo eel 40% de la población mayor a 6 años con alguna dificultad tiene 
#algún tipo de certificado de discapacidad. 

#### tipo certificado ####
#base_survey <- 
  base_survey |> 
  filter(d2a %in% c(1,2) ) |> 
  group_by(d2a) |> 
  summarise(total = survey_total(),
            porcentaje = survey_prop()*100)

## de las personas con certificado (91 mil), el 85% (78 mil) de las personas tiene CUD


df_plot <- 
  as.data.frame(svytable(~tipo_dificultad, 
                         design = base_survey))

colnames(df_plot) <- c("tipo_dificultad", "n")



#### tipo dificultad
base_survey <- 
  base_survey |> 
  filter( dd_tipo_dif != 0  ) |> 
  mutate(tipo_dificultad = case_when( dd_tipo_dif == 1  ~ 'Sólo dificultad motora',
                                      dd_tipo_dif == 2  ~ 'Sólo dificultad visual',
                                      dd_tipo_dif == 3  ~ 'Sólo dificultad auditiva',
                                      dd_tipo_dif == 4  ~ 'Sólo dificultad del habla y comunicación',
                                      dd_tipo_dif == 5  ~ 'Sólo dificultad mental cognitiva',
                                      dd_tipo_dif == 6  ~ 'Sólo dificultad del cuidado de sí mismo',
                                      dd_tipo_dif == 7  ~ 'Sólo certificado de discapacidad',
                                      dd_tipo_dif == 8  ~ '2 dificultades',
                                      dd_tipo_dif == 9  ~ '3 o más dificultades'))



###### tipo de dificultad ####

df_plot <- 
  as.data.frame(svytable(~tipo_dificultad, 
                         design = base_survey))

colnames(df_plot) <- c("tipo_dificultad", "n")

# Calcular porcentaje
df_plot <- df_plot %>%
  mutate(percentage = (n / sum(n)) * 100) %>%  # Convertir a porcentaje
  arrange(desc(n))

# Crear el gráfico
ggplot(df_plot, 
       aes(y = reorder(tipo_dificultad, n),
           x = n, 
           fill = tipo_dificultad)) +
  geom_bar(stat = "identity", fill = "#003f5c", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(percentage), "%")),  # Etiquetas con porcentaje
            hjust = -0.1, size = 5)+
  scale_fill_viridis_d() +  
  labs(
    title = "Distribución de Tipos de Dificultades",
    y = "Tipo de Dificultad",
    x = "Cantidad de personas"
  ) +
  theme_economist (base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotar etiquetas si fuera necesario









pob_mayor6 <- sum(base[base$edad >= 6, 'fexp'])

base[base$dif]
base |> count(hogar_dificultad )
base |> count(dificultad_total)
base |> count(dificultad_6ymas )
base |> count(tipo_dificultad)
