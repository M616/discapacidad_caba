library(rsconnect)
rsconnect::deployApp(
  appDir = "app_puntos_domicilios",
  appName = "domicilios",
  appFiles = c("app.R",
     "data/usig_direcciones_cud_marzo.gpkg"),
  forceUpdate = TRUE
)
