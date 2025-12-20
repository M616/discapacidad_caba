{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(utils)}

enlace <- "https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/01/Base_Usuarios_con_IOS_EDER_CABA_2019_rev1.rar"

getwd()

destino <- file.path("data", "eder")
dir.create(destino, recursive = TRUE, showWarnings = FALSE)

# nombre local del rar
archivo_rar <- file.path(destino, "eder_2019.rar")

# descargar
download.file(
  url = enlace,
  destfile = archivo_rar,
  mode = "wb"
)

# verificar que unrar exista
if (Sys.which("unrar") == "") {
  stop("unrar no está instalado en el sistema")
}

# descomprimir dentro de data/eder
system(
  paste(
    "unrar x -o+",
    shQuote(archivo_rar),
    shQuote(destino)
  )
)

list.files("data/eder", recursive = TRUE)