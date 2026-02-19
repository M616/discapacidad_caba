

#tabla_mapa <- readRDS("app_geocode/data/tabla_mapa.rds")

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