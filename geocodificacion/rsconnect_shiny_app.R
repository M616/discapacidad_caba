library(rsconnect)
rsconnect::deployApp(
  appDir = "app_geocode",
  appName = "CUD_geocode",
  appFiles = c("app.R",
     "data/tabla_mapa.rds"),
  forceUpdate = TRUE
)
