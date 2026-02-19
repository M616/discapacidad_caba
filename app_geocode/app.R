# =========================
# Librerías
# =========================
library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# =========================
# Cargar datos procesados
# =========================
tabla_mapa <- readRDS("data/tabla_mapa.rds")
#tabla_mapa <- readRDS("app_geocode/data/tabla_mapa.rds")

# =========================
# Server
# =========================
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

# =========================
# UI
# =========================
ui <- navbarPage(
  title = div(
    style = "font-size: 26px; font-weight: bold;",
    "Tablero Territorial – Personas con CUD"
  ),

  tabPanel(
    "Mapa de Indicadores",
    
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "indicador",
          "Seleccionar indicador:",
          choices = c(
            "Total personas con CUD" = "total_personas",
            "Edad promedio" = "edad_promedio",
            #"Promedio edad inicio del daño" = "edad_inicio_del_dano_promedio",
            "% Vivienda adaptada" = "pct_vivienda_adaptada",
            "% Cobertura pública" = "pct_cobertura_publica",
            "% Hacinamiento" = "pct_hacinamiento",
            "% Viviendas colectivas" = "pct_colectiva",
            "% Personas con equipamiento" = "pct_equipamiento",
            "% Personas con discapacidad intelectual" = "pct_intelectual"
          ),
          selected = "total_personas"
        )
      ),
      
      mainPanel(
        leafletOutput("mapa", height = "70vh")
      )
    )
  ),
  
  tabPanel(
    "Notas metodológicas",
    
    fluidPage(
      
      h3("Fuente de datos"),
      p("Base anonimizada de personas con CUD vigentes residentes en CABA en octubre de 2025."),
      
      h3("Georreferenciación"),
      tags$ul(
        tags$li("Las comunas fueron asignadas mediante geocodificación automática de domicilios. La geocodificación automática es un proceso mediante el cual una dirección escrita se transforma en coordenadas geográficas (latitud y longitud). Estas coordenadas permiten ubicar el domicilio dentro de una comuna determinada. Por ejemplo: si se ingresa 'Av. Cabildo 500', el sistema devuelve un punto geográfico. Ese punto se cruza con el mapa oficial de comunas. De esa intersección se determina a qué comuna pertenece el domicilio."),
        tags$li("Se utilizó la API Georef del Estado Nacional (",
                tags$a(
                  href = "https://www.argentina.gob.ar/georef",
                  "https://www.argentina.gob.ar/georef",
                  target = "_blank"
                ),
                ")."),
        tags$li("El porcentaje de respuesta exitosa fue aproximadamente del 80%."),
        tags$li("Una respuesta exitosa indica que la API pudo asignar coordenadas, pero no garantiza exactitud absoluta.")
      ),
      
      h3("Protección de datos y confidencialidad"),
      p("El análisis se presenta exclusivamente a nivel agregado por comuna. No se visualizan domicilios individuales ni información que permita identificar personas."),
      tags$ul(
        tags$li("La agregación territorial preserva la confidencialidad de los datos personales."),
        tags$li("No se publican coordenadas individuales ni direcciones exactas."),
        tags$li("Los indicadores reflejan tendencias colectivas y no situaciones particulares."),
        tags$li("El tablero cumple con criterios de resguardo de datos sensibles en el marco de la normativa vigente.")
      ),

     

      h3("Sugerencias sobre la carga del formulario CUD"),
      tags$ul(
        tags$li("El 66% de la base habita en departamento. No se está cargando piso y departamento, solo calle y altura. Se sugiere incorporarlo al formulario."),
        tags$li("Aproximadamente el 80% de los registros matchea con georef, sin conocer falsos positivos. Se sugiere validación visual por parte del operador.")
      )
      
    )
  )
)


# =========================
# Lanzar app
# =========================
shinyApp(ui, server)