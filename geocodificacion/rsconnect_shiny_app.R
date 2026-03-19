library(rsconnect)

options(rsconnect.packrat = FALSE)
options(rsconnect.useRenviron = FALSE)
#options(rsconnect.packrat = FALSE)
rsconnect::deployApp(
  #appDir = "app_geocode",
  appDir = "app_puntos_domicilios",
  appName = "domicilios",
 appFiles = c("app.R",
     "data/usig_direcciones_cud_marzo.gpkg"),
  forceUpdate = TRUE
)
