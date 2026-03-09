{library(httr)
library(jsonlite)
library(tidyverse)
#library(Rinmoscrap)
library(stringr)
library(tictoc)
library(googledrive)
library(here)
library(sf)
library(mapview)
library(googlesheets4)
#library(readxl)
library(openxlsx)
library(janitor)
}

dir.create('data')
#base de marzo2026
drive_download('https://drive.google.com/file/d/1SsGHY_AuNLBjHNlpzC0nhl9pepNUVGtO/view?usp=drive_link', path = 'data/Anonimizado CUD_vigentesCABA2026. 4 de MARZO(CABA).csv')

base <- read_delim(
'data/Anonimizado CUD_vigentesCABA2026. 4 de MARZO(CABA).csv')
base <- clean_names(base)
base$numero_domicilio <- as.integer(base$numero_domicilio)
base$id <- 1:nrow(base)

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
  bloque_df <- base[inicio:fin,c('id','domicilio','numero_domicilio') ]
  ids_bloque <- bloque_df$id
  
  # Construir lista de direcciones
  direcciones_list <- lapply(seq_len(nrow(bloque_df)), function(j) {
    list(
      direccion = paste(bloque_df$domicilio[j],bloque_df$numero_domicilio[j]),
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
  respuestas[[i]]$resultados$id <- ids_bloque
  toc(log = TRUE)  # ⏱ FIN medición + guarda en log
  Sys.sleep(0.5)
}


dir.create('data/georef')
save(respuestas, file = 'data/georef/cud_marzo_respuestas_max1.Rda')
#load('data/georef/cud_enero_respuestas_max1.Rda')

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

save(resultados_largos, file = 'data/georef/cud_marzo26_resultados_largos_max1.Rda')
#load('data/georef/cud_marzo26_resultados_largos_max1.Rda')


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


base2[is.na(base2$vivienda_adaptada), "vivienda_adaptada" ]  <- 'No corresponde'
base2$vivienda_adaptada <- factor(base2$vivienda_adaptada)
base2$tipo_de_deficiencia_simple_multiple <- factor(base2$tipo_de_deficiencia_simple_multiple)

##poner tipo de vivienda colectiva o individual 
#hay 186 personas viviendo en la calle pero con dimicilio. son paradores sociales?
base2 |> st_drop_geometry() |> count(vivienda_particular_o_colectiva)
base2 |> st_drop_geometry() |> count(tipo_de_vivienda)

base2 |> 
  st_drop_geometry() |> 
  filter(tipo_de_vivienda == 'Persona viviendo en calle') |> 
  select(domicilio, numero_domicilio, vivienda_particular_o_colectiva)

base2$vivienda_particular_o_colectiva <- 
  factor(base2$vivienda_particular_o_colectiva )


#hacer jopin espacial con comunas caba
# URL con filtro CQL por el atributo 'gna' = 'Comuna'
url_comunas_caba <- "https://wms.ign.gob.ar/geoserver/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=ign:departamento&outputFormat=application/json&cql_filter=gna='Comuna'"
# Descargar solo las comunas
comunas_caba <- st_read(url_comunas_caba)

comunas_caba <- 
  comunas_caba |> 
  mutate(comuna = nam) |> 
  select(comuna)

#base2 <- st_read('data/georef/andis_marzo_georef.gpkg')

base2 <- 
  st_join(base2,
        comunas_caba,
        left = TRUE
            )


st_write(base2, 'data/georef/andis_marzo_georef.gpkg', append = FALSE)

base <- 
  base2 |> 
  filter(!st_is_empty(geom) )|> 
    dplyr::select(
    comuna,
    tipo_de_deficiencia_simple_multiple,
    vivienda_particular_o_colectiva,
    grupos_quinquenales,
    domicilio,
    numero_domicilio,
    geom
  )


st_write(base, 'app_puntos_domicilios/data/andis_marzo_georef.gpkg', append = FALSE)
