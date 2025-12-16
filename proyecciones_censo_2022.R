{library(httr)
library(tidyverse)
library(readxl)
}

url <- "https://www.indec.gob.ar/ftp/cuadros/poblacion/proyecciones_jurisdicciones_2022_2040_c1.xlsx"

dir.create('data')
dir.create('data/censo_2022')
archivo_local <- "data/censo_2022/proyecciones_poblacion_INDEC.xlsx"

response <- GET(url)

# Verificar que la petición fue exitosa
if(status_code(response) == 200) {
  # Guardar el contenido
  writeBin(content(response, "raw"), archivo_local)
  message("Archivo descargado exitosamente: ", archivo_local)
} else {
  message("Error en la descarga. Código de estado: ", status_code(response))
}


proyecciones <- read_excel("data/censo_2022/proyecciones_poblacion_INDEC.xlsx", 
                                           sheet = "02-CABA", range = "a5:d24")

caba_2025 <- proyecciones[proyecciones$Año == 2025, 2][[1]]



## esta proporcion sale del estudio sobre poblacion con discapacidad
##de indec, ver el otro scripts
0.094 * caba_2025

##mismo estudio, proporcion estimada de altas necesidades de apoyo = 0.012 (pero 6 años y mas)
## la poblacion mayor o igual a 6 años es 0.948
0.012 * (caba_2025*0.948)

## esto tomando 0.6 % (todos somos todos, pagina33)
0.006 * caba_2025

## esto tomando 1.5 según escenario conservador calculado a partir
#de eah2018
0.015 * (caba_2025*0.948)

