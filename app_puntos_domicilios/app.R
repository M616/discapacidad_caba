library(shiny)
library(leaflet)
library(sf)
library(leafgl)

#base <- st_read('app_puntos_domicilios/data/andis_marzo_georef.gpkg')
base <- st_read('data/andis_marzo_georef.gpkg')

comunas <- unique(base$comuna)
comunas <- comunas[order(as.numeric(gsub("Comuna ", "", comunas)))]

# popup liviano
base$popup <- paste0(
  "<b>Comuna:</b> ", base$comuna, "<br>",
  "<b>Domicilio:</b> ", base$domicilio, " ", base$numero_domicilio, "<br>",
  "<b>Grupo quinquenal:</b> ", base$grupos_quinquenales, "<br>",
  "<b>Tipo de deficiencia:</b> ", base$tipo_de_deficiencia_simple_multiple, "<br>",
  "<b>Tipo de vivienda:</b> ", base$vivienda_particular_o_colectiva, "<br>"
  
)

ui <- fluidPage(

  titlePanel("Georreferenciación de domicilios"),

  tabsetPanel(

    tabPanel(
  "Mapa",

  selectInput(
  "comuna",
  "Filtrar por comuna",
  choices = c("Todas", comunas),
  selected = "Todas"
),

  leafletOutput("mapa", height = "800px")
),

    tabPanel(
  "Notas metodológicas",

  fluidPage(

    h3("Fuente de datos"),
    p("Base anonimizada de personas con CUD vigentes residentes en la Ciudad Autónoma de Buenos Aires en marzo de 2026."),

    h3("Uso del mapa interactivo"),

    tags$ul(

      tags$li("El mapa muestra la localización georreferenciada de domicilios correspondientes a personas con CUD vigentes residentes en la Ciudad Autónoma de Buenos Aires."),

      tags$li("Cada punto del mapa representa un domicilio georreferenciado. Al hacer click sobre un punto se despliega información descriptiva básica asociada al registro."),

      tags$li("El selector ubicado en la parte superior del mapa permite filtrar los domicilios según la comuna de residencia."),

      tags$li("Al seleccionar una comuna, el mapa se centra automáticamente en el área correspondiente y se visualizan únicamente los domicilios registrados en esa comuna."),

      tags$li("La opción 'Todas' permite visualizar el conjunto completo de domicilios georreferenciados en la ciudad."),

      tags$li("Los colores de los puntos representan el tipo de vivienda declarado (vivienda particular, vivienda colectiva o sin datos).")

    ),

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

      tags$li("Una respuesta exitosa indica que la API pudo asignar coordenadas, pero no garantiza exactitud absoluta. Por ejemplo, si se ingresa únicamente 'Perón' como nombre de calle, la API puede devolver una coordenada asociada a una calle con ese nombre. Sin embargo, en la Ciudad de Buenos Aires existen calles como 'Eva Perón' y 'Juan Domingo Perón'. Si el domicilio original corresponde a 'Eva Perón' pero se carga únicamente 'Perón', el sistema puede asignar una ubicación válida pero incorrecta. Este tipo de ambigüedades en la información de origen puede afectar la precisión de la geocodificación.")

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

  output$mapa <- renderLeaflet({

    datos <- datos_filtrados()

    mapa <- leaflet() %>%
      addProviderTiles("CartoDB.Positron")

    if (nrow(datos) > 0) {

      mapa <- mapa %>%
        setView(
          lng = mean(datos$lng),
          lat = mean(datos$lat),
          zoom = ifelse(input$comuna == "Todas", 11, 13)
        ) %>%
        leafgl::addGlPoints(
          data = datos,
          lng = datos$lng,
          lat = datos$lat,
          popup = datos$popup,
          fillColor = datos$color,
          radius = 9
        )

    }

    mapa %>%
      addLegend(
        position = "bottomright",
        colors = c("#8fd19e", "#d73027", "#ffffff"),
        labels = c("Vivienda particular", "Vivienda colectiva", 'Sin datos'),
        title = "Tipo de vivienda"
      )

  })

}

shinyApp(ui, server)