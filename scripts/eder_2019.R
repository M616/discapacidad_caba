{library(tidyverse)
library(survey)
library(srvyr)
library(ggthemes)
library(utils)}

enlace <- "https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/01/Base_Usuarios_con_IOS_EDER_CABA_2019_rev1.rar"

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

####eder usuario retro. 
#cada registro representa un año calendario donde se suceden los eventos
# biográficos de al menos 1 año de duracion

eder_retro = 
  read.table('data/eder/eder2019_usuarios_retro.txt',
header = TRUE, 
sep = '\t',
fileEncoding = 'UCS-2LE')


#batería preg p12 sobre discapacidad. ver cuales de estas combinaciones sirve de proxy
# para identificar discapacidad intelectual y altas necesidades de apoyo


 eder_retro |> 
  filter(p12_1a_dis == 1 | 
         p12_1b_dis == 1 | 
         p12_1c_dis == 1 | 
         p12_1d_dis == 1 | 
         p12_1e_dis == 1 | 
         p12_1f_dis == 1 | 
         p12_3_dis == 1)

#tengo base de hijos y conyugue. se puede ver hijos con discapacidad ntelectual
# que murieron?

eder_retro_hijo = 
  read.table('data/eder/eder2019_usuarios_retro_hijo.txt',
header = TRUE, 
sep = '\t',
fileEncoding = 'UCS-2LE')

names(eder_retro_hijo)
####eder usuario ios
#cada registro es una persona (poblacion objetivo). brinda informacion
#sobre cada hogar  identificaday caracterizacion sociodemografica de 
# los miembros del hogar.

#nota: El IOS es una medida relativa que indica la posición de la persona
#encuestada en la estratificación social a los 14 años, en relación con
#las personas que integran su cohorte de nacimiento. Para ampliar la
#información sobre el desarrollo del IOS, consulte el documento meto-
#dológico disponible en el sitio web de la DGEyC.


eder_ios <- read.delim2('data/eder/eder2019_usuarios_IOS.txt')
names(eder_ios)
