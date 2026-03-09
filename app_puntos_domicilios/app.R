library(shiny)
library(leaflet)
library(sf)
library(leafgl)

base <- st_read('app_puntos_domicilios/data/andis_marzo_georef.gpkg')
#base <- st_read('data/andis_marzo_georef.gpkg')

base <- base[!sf::st_is_empty(base), ]
base <- sf::st_cast(base, "POINT")

coords <- sf::st_coordinates(base)

base$lng <- coords[,1]
base$lat <- coords[,2]

#base$color = "#8fd19e"

base$color <- ifelse(
  base$vivienda_particular_o_colectiva == "Colectiva",
  "#d73027",   # rojo
  "#8fd19e"    # verde
)

# popup liviano
base$popup <- paste0(
  "<b>Comuna:</b> ", base$comuna, "<br>",
  "<b>Domicilio:</b> ", base$domicilio, " ", base$numero_domicilio, "<br>",
  #"<b>Vivienda adaptada:</b> ", base$vivienda_adaptada, "<br>",
  "<b>Tipo de vivienda:</b> ", base$vivienda_particular_o_colectiva, "<br>",
  "<b>Grupo quinquenal:</b> ", base$grupos_quinquenales
)

ui <- fluidPage(

  titlePanel("Georreferenciación de domicilios"),

  tabsetPanel(

    tabPanel(
  "Mapa",

  selectInput(
    "comuna",
    "Filtrar por comuna:",
    choices = c("Todas", sort(unique(base$comuna))),
    selected = "Todas"
  ),

  leafletOutput("mapa", height = "800px")
),

    tabPanel(
      "Notas metodológicas",

      fluidPage(

        h3("Fuente de datos"),
        p("Base anonimizada de personas con CUD vigentes residentes en CABA en marzo de 2026."),

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

          tags$li("Puede existir subrepresentación de población residente en villas o asentamientos debido a la ausencia de direcciones formales (calle y altura), lo que limita la geocodificación automática."),

          tags$li("Una respuesta exitosa indica que la API pudo asignar coordenadas, pero no garantiza exactitud absoluta.")

        ),

        h3("Protección de datos y confidencialidad"),

        p("El análisis se presenta con fines exploratorios. Para preservar la confidencialidad de la información personal, el mapa no expone información identificatoria de las personas."),

        tags$ul(
          tags$li("Por cuestiones de eficiencia computacional y para evitar problemas de rendimiento en la visualización, el mapa despliega únicamente un conjunto reducido de variables de ejemplo."),
          tags$li("Estas variables pueden visualizarse haciendo click sobre cada punto del mapa."),
          tags$li("La información presentada no permite identificar personas específicas.")
        )

      )

    )

  )

)

server <- function(input, output, session){

  datos_filtrados <- reactive({

  if (input$comuna == "Todas") {
    base
  } else {
    base[base$comuna == input$comuna, ]
  }

})

  output$contador <- renderText({
    paste("Domicilios visualizados:", nrow(datos_filtrados()))
  })

  output$mapa <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addLegend(
        position = "bottomright",
        colors = c("#8fd19e", "#d73027"),
        labels = c("Vivienda particular", "Vivienda colectiva"),
        title = "Tipo de vivienda"
      )
  })

  observe({

  datos <- datos_filtrados()

  leafletProxy("mapa") %>%
    clearMarkers() %>%
    leafgl::addGlPoints(
      lng = datos$lng,
      lat = datos$lat,
      popup = datos$popup,
      fillColor = datos$color,
      radius = 9
    )

})

}

shinyApp(ui, server)
