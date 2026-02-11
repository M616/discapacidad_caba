{library(httr)
library(jsonlite)
library(tidyverse)
library(Rinmoscrap)
library(stringr)
library(tictoc)
library(googledrive)
library(here)
library(sf)
library(mapview)
library(googlesheets4)
#library(readxl)
library(openxlsx)
}

dir.create('data')
#drive_download('https://docs.google.com/spreadsheets/d/1sAfzu82spdX3bNjg7vS0wRQrnoIIuAOh/edit?usp=sharing&ouid=118184938313502858180&rtpof=true&sd=true', path = 'data/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx')

base <- read.xlsx(
  #'data/cud_anonimizada_octubre25.xlsx',
  'data/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx', 
#rows = 1:2,
cols = 1:3,
startRow = 2,
colNames = FALSE)

#base <- base |> st_drop_geometry()
#base <- base[,1:3]

names(base) <- c('id', 'altura', 'calle')

#base$id <- as.integer(base$id)


bloque_size <- 500 
n <- nrow(base)
#n <- 100
bloques <- ceiling(n / bloque_size)
respuestas <- list()

for (i in seq_len(bloques)) {
  cat(sprintf("Procesando bloque %d de %d...\n", i, bloques))
  
  tic(sprintf("Bloque %d", i))  # ⏱ INICIO medición de tiemp
  
  # Índices del bloque
  inicio <- (i - 1) * bloque_size + 1
  fin <- min(i * bloque_size, n)
  
  # Subset del bloque correcto
  bloque_df <- base[inicio:fin, ]
  ids_bloque <- bloque_df$id
  
  # Construir lista de direcciones
  direcciones_list <- lapply(seq_len(nrow(bloque_df)), function(j) {
    list(
      direccion = paste(bloque_df$calle[j],bloque_df$altura[j]),
      provincia = "Ciudad Autónoma de Buenos Aires",
      #departamento = bloque_df$nombre_arba[j],
      aplanar = TRUE,
      campos = "completo",
      #max = 5,
      max = 1,
      inicio = 0
      #exacto = FALSE,
      
    )
  })
  
  # Estructura JSON correcta
  json_data <- toJSON(
    list(
      direcciones = direcciones_list#,
      #campos = "estandar",
      #max = 1,
      #inicio = 0,
      #exacto = FALSE,
      #aplanar = TRUE
    ),
    auto_unbox = TRUE,
    pretty = TRUE
  )
  
  sum(duplicated(base$id))
  
  # Enviar POST
  url <- "https://apis.datos.gob.ar/georef/api/direcciones"
  
  result <- tryCatch({
    response <- POST(
      url,
      add_headers(
        "Content-Type" = "application/json",
        "Accept" = "application/json"
      ),
      body = json_data,
      encode = "raw"
    )
    
    if (http_error(response)) {
      cat(sprintf("❌ Error HTTP %d en bloque %d\n", status_code(response), i))
      print(content(response, "text", encoding = "UTF-8"))
      return(NULL)
    }
    
    
    
    response_text <- content(response, "text", encoding = "UTF-8")
    response_json <- fromJSON(response_text, flatten = TRUE)
    response_json
  }, error = function(e) {
    cat(sprintf("Error en el bloque %d: %s\n", i, e$message))
    NULL
  })
  
  
  respuestas[[i]] <- result
  respuestas[[i]]$resultados$id <- as.integer (ids_bloque)
  toc(log = TRUE)  # ⏱ FIN medición + guarda en log
  Sys.sleep(0.5)
}


dir.create('data/georef')
#save(respuestas, file = 'data/georef/respuestas_sm.Rda')
save(respuestas, file = 'data/georef/respuestas_max1.Rda')
#load('data/georef/respuestas_sm.Rda')

resultados_largos <- map_dfr(respuestas, function(res) {
  
  if (is.null(res)) return(NULL)  # bloque vacío
  
  ids_bloque <- as.integer (res$resultados$id)
  direcciones <- res$resultados$direcciones
  
  map2_dfr(direcciones, ids_bloque, function(df_dir, id) {
    
    # Si no es data.frame o tiene 0 filas, devolver NA
    if (!is.data.frame(df_dir) || nrow(df_dir) == 0) {
      tibble(
        id = id,
        direccion_georef = NA_character_,
        lon_georef = NA_real_,
        lat_georef = NA_real_,
        localidad_censal = NA_character_
      )
    } else {
      df_dir %>%
        mutate(
          id = id,
          direccion_georef = nomenclatura,  
          lon_georef = ubicacion_lon,
          lat_georef = ubicacion_lat,
          localidad_censal = localidad_censal_nombre
        ) %>%
        select(id, direccion_georef, lon_georef, lat_georef)
    }
  })
})

#save(resultados_largos, file = 'data/georef/resultados_largos.Rda')
save(resultados_largos, file = 'data/georef/resultados_largos_max1.Rda')

#load('data/georef/resultados_largos.Rda')
#load('data/inmo.Rda')

base2 <- 
  base |> 
  #left_join(resultados_largos,
  right_join(resultados_largos,
            by = 'id')

base2 <- 
  st_as_sf(base2,
         coords = c('lon_georef',
                    'lat_georef'),
         na.fail = FALSE,
         remove = FALSE)

base2 <- 
  st_set_crs(base2, 4326)

#inmo |> 
#  filter(!st_is_empty(geometry)) |> 
#  mapview()

##estos son los que tengo que ver de que localidades son
sum(duplicated(base$id))

base2 <- 
  base2 |> 
  filter(!st_is_empty(geometry))


mapview(base2)
#####################


library(tidygeocoder)
library(geoAr)

bbox_sm <-
  geoAr::get_geo('BUENOS AIRES') |>
  filter(coddepto_censo == 760) |>
  st_bbox()

bbox_string <-
  paste(bbox_sm[c("xmin",
                      "ymin",
                      "xmax",
                      "ymax")],
        collapse = ",")



nominatim <-
  inmo |> 
  filter(is.na(geocodificador)) |> 
  mutate(direccion_simple = paste0(direccion_limpia,
                                   ', ',
                                   'Mar del Plata',
                                   ', ',
                                   'Argentina')) |> 

  tidygeocoder::geocode(address = direccion_limpia,
                        limit = 1,
                        method = "osm",
                        full_results = TRUE,
                        custom_query = list(
                          viewbox = bbox_string ,
                          bounded = 1
                        ))
save(nominatim, file = here('data/nominatim.Rda'))

nominatim_filtrado <-
  nominatim |>
  dplyr::filter( type == 'house',
                 class == 'place')

#####solo 173 observaciones con nominatim, pruebo mapbox
mapbox <-
  inmo |> 
  filter(is.na(geocodificador)) |> 
  mutate(direccion_simple = paste0(direccion_limpia,
                                   ', ',
                                   'Mar del Plata',
                                   ', ',
                                   'Argentina')) |> 
  
  tidygeocoder::geocode(address = direccion_limpia,
                        limit = 1,
                        method = "mapbox",
                        full_results = TRUE,
                        custom_query = list(
                          bbox = bbox_string,
                          types = "address"
                        ))
save(mapbox, file = here('data/mapbox.Rda'))


#relevance. Fuente chatgpt
#Es un valor numérico entre 0 y 1.
#Cuanto más cercano a 1, mayor coincidencia entre la cadena de texto que geocodificaste 
#(la dirección original) y el resultado encontrado.
#Está calculado en función de varios factores:
#Coincidencia de texto (por ejemplo, si escribiste “Calle Falsa 123” y existe exactamente esa calle).
#Nivel jerárquico correcto (por ejemplo, la localidad y el país coinciden).
#Tipo de entidad (por ejemplo, si buscabas una dirección y encontró una calle o barrio, 
#la relevancia será menor).

summary(mapbox$relevance)

mapbox |> 
  filter(relevance < 0.6) |> 
  select(direccion_limpia)




