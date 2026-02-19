{library(rsconnect)
library(tidyverse)
library(stringr)
library(sf)
library(mapview)
library(openxlsx)
library(janitor)
library(DataExplorer)
library(shiny)
library(leaflet)
}


base <- read.xlsx(
  #'data/cud_anonimizada_octubre25.xlsx',
  'data/CUD vigentes residentes en CABA anonimizada 1-10-2025.xlsx', 
#cols = 1:3,
#startRow = 2
)

base <- janitor::clean_names(base)

base2 <- st_read('data/georef/base2_georef.gpkg')

base2 <- 
  base2 |> 
  select(id, direccion_georef, lon_georef, lat_georef) |> 
  left_join(base, by = c('id' = 'numero'))


indicadores_geo <- list(
  total_casos = nrow(base2),
  respuestas_api = sum(!st_is_empty(base2$geom)),
  tasa_respuesta_api = round(
  mean(!st_is_empty(base2$geom)) * 100,
  1)
)

saveRDS(indicadores_geo, "data/processed/indicadores_geo.rds")


#DataExplorer::create_report(st_drop_geometry(base2))

# URL con filtro CQL por el atributo 'gna' = 'Comuna'
url_comunas_caba <- "https://wms.ign.gob.ar/geoserver/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=ign:departamento&outputFormat=application/json&cql_filter=gna='Comuna'"
# Descargar solo las comunas
comunas_caba <- st_read(url_comunas_caba)

comunas_caba <- 
  comunas_caba |> 
  mutate(comuna = nam) |> 
  select(comuna)

geom_comunas <- comunas_caba



####to do: agregar vivienda particular o colectiva; tipo de vivienda
base2 <- 
  base2 |> 
  select(
    edad_actual,
    edad_de_inicio_del_dano,
    tipo_de_vivienda,
    nivel_de_hacinamiento,
    vivienda_adaptada,
    tipo_de_convivencia,
    cobertura_de_salud,
    tipo_de_deficiencia_simple_multiple,
    equipamiento,
    vivienda_particular_o_colectiva,
    tipo_de_vivienda)

comunas_caba <- 
  st_join(comunas_caba,
        base2)


tabla_resumen <- 
  comunas_caba %>%
  st_drop_geometry() |> 
  group_by(comuna) %>%
  summarise(
    total_personas = n(),
    pct_vivienda_adaptada = mean(vivienda_adaptada == "Sí", na.rm = TRUE) * 100,
    pct_cobertura_publica = mean(cobertura_de_salud %in%  c('Programa Nacional y/o Provincial de Salud','Pública'), na.rm = TRUE) * 100,
    pct_hacinamiento = mean(nivel_de_hacinamiento %in% c('Nivel Crítico', 'Nivel Moderado'), na.rm = TRUE) * 100,
    edad_promedio = mean(edad_actual, na.rm = TRUE),
    pct_vivienda_colectiva = mean(vivienda_particular_o_colectiva == 'Colectiva', na.rm = TRUE) * 100
  )

tabla_mapa <- geom_comunas %>%
  left_join(tabla_resumen, by = "comuna")

saveRDS(tabla_mapa, "data/processed/tabla_mapa.rds")


#tabla_mapa <- readRDS("data/processed/tabla_mapa.rds")

server <- function(input, output, session) {
  
  output$mapa <- renderLeaflet({
    
    variable <- tabla_mapa[[input$indicador]]
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = variable,
      na.color = "#f0f0f0"
    )
    
    leaflet(tabla_mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(tabla_mapa[[input$indicador]]),
        weight = 1,
        color = "white",
        fillOpacity = 0.8,
        popup = ~paste0(
          "<strong>", comuna, "</strong><br>",
          input$indicador, ": ",
          round(tabla_mapa[[input$indicador]], 1)
        )
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = variable,
        title = input$indicador
      )
  })
  
}


ui <- navbarPage(
  "Indicadores territoriales - Personas con CUD en CABA",
  
  # ------------------------
  # Pestaña 1: Mapa
  # ------------------------
  tabPanel(
    "Mapa de Indicadores",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "indicador",
          "Seleccionar indicador:",
          choices = c(
            "Total de personas" = "total_personas",
            "Edad promedio" = "edad_promedio",
            "% Vivienda adaptada" = "pct_vivienda_adaptada",
            "% Cobertura pública" = "pct_cobertura_publica",
            "% Hacinamiento" = "pct_hacinamiento",
            "% Viviendas colectivas" = "pct_vivienda_colectiva"            
          ),
          selected = "total_personas"
        )
      ),
      
      mainPanel(
        leafletOutput("mapa", height = 600)
      )
    )
  ),
  
  # ------------------------
  # Pestaña 2: Metodología
  # ------------------------
  tabPanel(
    "Notas metodológicas",
    
    fluidPage(
      h3("Fuente de datos"),
      p("Base anonimizada de personas con CUD vigentes residentes en CABA en octubre de 2025."),
      
      h3("Georreferenciación"),
      p(
  "Las comunas fueron asignadas mediante geocodificación automática de domicilios, utilizando la API Georef (",
  tags$a(
    href = "https://www.argentina.gob.ar/georef",
    "https://www.argentina.gob.ar/georef",
    target = "_blank"
  ),
  ")."
),
      p("El porcentaje de respuesta de la API fue del 80%."),
      p("La respuesta exitosa de la API no implica exactitud."),
      
      h3("Construcción de indicadores"),
      tags$ul(
        tags$li("Total de personas: conteo por comuna."),
        tags$li("% Vivienda adaptada: porcentaje de respuestas 'Sí'."),
        tags$li("% Cobertura pública: incluye Programa Nacional/Provincial y Pública."),
        tags$li("% Hacinamiento: incluye nivel crítico y moderado."),
        tags$li("Edad promedio: promedio simple por comuna."),
        tags$li("% Viviendas colectivas: porcentaje de viviendas colectivas.")
      ),

      h3("Sugerencias sobre la carga del formulario CUD"),
      tags$ul(
        tags$li("El 66% de la base habita en departamento. No se está cargando piso y departamento, solo se está cargando calle y altura. 
        Se sugiere incorporarlo al formulario. No queda claro de donde sale el 
        dato de calle y numero, ya que en el formulario solo se releva domicilio 
        (una sola variable)"),
        tags$li("Aproximadamente el 80% de los registros matchea con georef, 
        sin saber los falsos positivos. Se sugiere que el operador cargue la 
        dirección y el sistema le muestre la ubicación geográfica, para que el 
        operador pueda validar si es correcta o no.")
      ),


    )
  )
)

shinyApp(ui, server)